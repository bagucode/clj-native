(ns clj-native.core
  (:use [clojure.contrib.seq-utils :only [indexed]])
  (:import [clojure.asm ClassVisitor MethodVisitor
            ClassWriter Opcodes]
           [java.util UUID]))

(defn get-root-loader
  "Gets the root DynamicClassLoader"
  []
  (loop [#^ClassLoader cl (clojure.lang.RT/baseLoader)]
    (let [parent (.getParent cl)]
      (if (not= (class parent) clojure.lang.DynamicClassLoader)
        cl
        (recur parent)))))

(defn load-code
  "Loads bytecode (a class definition) using the root DynamicClassLoader"
  [#^String classname #^"[B" bytecodes]
  (.defineClass #^ClassLoader (get-root-loader) classname bytecodes))

(defn word-bits
  "Returns number of bits per machine word on current architecture."
  []
  (Integer/parseInt (System/getProperty "sun.arch.data.model")))

(def type-map
  {'byte Byte/TYPE
   'short Integer/TYPE
   'int (condp = (word-bits)
          32 Integer/TYPE
          64 Long/TYPE)
   'size_t (condp = (word-bits)
             32 Integer/TYPE
             64 Long/TYPE)
   'long Long/TYPE
   'i8 Byte/TYPE
   'i16 Integer/TYPE
   'i32 Integer/TYPE
   'i64 Long/TYPE
   'float Float/TYPE
   'double Double/TYPE
   'pointer com.sun.jna.Pointer
   'void Void/TYPE})

(defn descriptor
  "Get internal name type descriptor for class"
  [#^Class clazz]
  (clojure.asm.Type/getDescriptor clazz))

(defn make-native-func
  "Create a static method stub for a native function"
  [#^ClassVisitor cv fndesc]
  (let [name (:name fndesc)
        rettype  (type-map (:rettype fndesc))
        argtypes (map type-map (:argtypes fndesc))
        opcodes (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC Opcodes/ACC_NATIVE)
        desc (str "(" (apply str (map descriptor argtypes))
                  ")" (descriptor rettype))]
    (-> cv
        (.visitMethod (int opcodes) name desc nil nil)
        (.visitEnd))))

(defn make-native-lib-stub
  "Create the class needed for JNA direct mapping."
  [lib fn-descriptors & options]
  (let [defaults {:pkg (.replaceAll (name (ns-name *ns*)) "-" "_")
                  :name lib}
        opts (merge defaults (apply array-map options))
        {:keys [pkg name]} opts]
    (let [#^ClassVisitor cv (clojure.asm.ClassWriter. 0)]
      (.visit cv Opcodes/V1_5 Opcodes/ACC_PUBLIC
              (str pkg \/ name)
              nil "java/lang/Object", (make-array String 0))
      ;; Class static block, must call Native.register here
      (doto #^MethodVisitor (.visitMethod cv Opcodes/ACC_STATIC
                                          "<clinit>" "()V" nil nil)
        (.visitCode)
        (.visitLdcInsn (if (string? lib) lib (name lib)))
        (.visitMethodInsn Opcodes/INVOKESTATIC "com/sun/jna/Native"
                          "register" "(Ljava/lang/String;)V")
        (.visitInsn Opcodes/RETURN)
        (.visitMaxs 1 0)
        (.visitEnd))
      ;; Default constructor, just delegate to Object
      (doto #^MethodVisitor (.visitMethod cv Opcodes/ACC_PUBLIC
                                          "<init>" "()V" nil nil)
        (.visitCode)
        (.visitVarInsn Opcodes/ALOAD 0)
        (.visitMethodInsn Opcodes/INVOKESPECIAL
                          "java/lang/Object" "<init>" "()V")
        (.visitInsn Opcodes/RETURN)
        (.visitMaxs 1 1)
        (.visitEnd))
      ;; Native function stubs
      (doseq [fndesc fn-descriptors]
        (make-native-func cv fndesc))
      ;; All done
      (.visitEnd cv)
      (.toByteArray cv))))

(defn check-type
  [t]
  (when (not-any? #(= t %) (keys type-map))
    (throw (Exception. (str "Unknown type: " t))))
  t)

(defn parse
  "Parses input to defclib and returns a library specification map"
  [lib & body]
  (when-not (symbol? lib)
    (throw (Exception. "lib must be a symbol"))) ;; overkill?
  (let [third (fn [coll] (first (next (next coll))))
        drop-until (fn [pred coll] (drop-while #(not (pred %)) coll))
        functions
        (for [fdef body]
          (if-not (list? fdef)
            (throw (Exception. (str "invalid function description: " fdef)))
            (let [name (first fdef)
                  doc (when (string? (second fdef)) (second fdef))
                  cljname (cond (symbol? (second fdef)) (second fdef)
                                (and (string? (second fdef))
                                     (symbol? (third fdef))) (third fdef)
                                :else name)
                  argvec (first (drop-until #(vector? %) fdef))
                  rettype (second (drop-until #(vector? %) fdef))]
              (when-not (symbol? name)
                (throw (Exception.
                        (str "expected symbol as function name: " fdef))))
              (when-not (vector? argvec)
                (throw (Exception. (str "argument vector missing: " fdef))))
              {:name (clojure.core/name name)
               :rettype (check-type (or rettype 'void))
               :argtypes (vec (for [a argvec] (check-type a)))
               :doc doc
               :cljname cljname})))]
    {:lib lib
     :fns functions
     :pkg (symbol (.replaceAll (name (ns-name *ns*)) "-" "_"))
     :classname (symbol (.replaceAll
                         (str "clj_native_" (UUID/randomUUID)) "-" "_"))}))

(defn argnames
  "Create unique names for a seq of argument types"
  [argtypes]
  (for [[i t] (indexed argtypes)]
    (symbol (str t i))))

(defn load-fn
  "Generates code for a function that loads a native library."
  [lib]
  (let [clsname (str (:pkg lib) \. (:classname lib))]
    `(fn [] (load-code ~clsname
                       (make-native-lib-stub
                        ~(name (:lib lib))
                        '~(:fns lib)
                        :name '~(:classname lib)
                        :pkg '~(:pkg lib))))))

(defn clj-stub
  "Creates a stub clojure function that will load library on demand
  and replace itself with a version that calls the library function."
  [m n native args lib loadfn]
  (let [clsname (str (:pkg lib) \. (:classname lib))]
    `(do
       (try
        (Class/forName ~clsname)
        (catch ClassNotFoundException cnfe#
          (try
           (~loadfn)
           ;; LinkageError happens if the class is already loaded.
           (catch LinkageError le#))))
       ;; Have to use eval because the class name is only known
       ;; by class loaders at runtime after loadfn has run
       (eval ~(list 'quote
                    (list 'def (with-meta n m)
                          (list `fn (vec args)
                                (list* (symbol (str clsname \/ native)) args)))))
       ;; call new version of self
       ~(list* n args))))

;; TODO: type tags and arglists in meta, also type tags on arguments
(defn make-clj-stubs
  "Creates clojure function stubs that will load the library on
  demand and replace themselves with versions that only call the
  library functions."
  [libspec loadfn]
  (for [fdef (:fns libspec)]
    (let [m (if (:doc fdef)
              {:doc (:doc fdef)}
              {})
          m (if (not= (:rettype fdef) 'void)
              (assoc m :tag (type-map (:rettype fdef)))
              m)
          m (assoc m :arglists (list 'quote (list (:argtypes fdef))))
          n (:cljname fdef)
          args (argnames (:argtypes fdef))
          fdecl (clj-stub m n (:name fdef) args libspec loadfn)]
      (list 'def (with-meta n m)
            (list `fn (vec args) fdecl)))))

;; --------------- PUBLIC INTERFACE ---------------
;; Use other stuff at own peril

(defmacro defclib
  "Create C library bindings.
  Only functions are supported at this time (structs and unions
  may be handled as opaque pointers).
  lib is a symbol naming the native library to link eg. 'c' for
  linking against the standard C runtime.
  body is any number of function descriptions of the form:

  (name docstring? clojure-name? [argtypes*] returntype?)

  Where name must match the name of the C function. The only
  other required component is the vector containing the argument
  types, even if the function takes no arguments. If the type
  of the return value is left out, void is assumed.
  A clojure function mapped to a C function that returns void
  will always return nil.
  clojure-name can be used to give the function a different name
  within the clojure runtime, useful eg. for getting rid of annoying
  prefixes used by C libraries to ensure unique names.

  This macro will create clojure functions in the current
  namespace corresponding to the imported C functions. The first
  time these functions are called they will dynamically load
  the C library and then replace themselves with versions that
  simply call their C library namesakes.
  Because of this black magic self-replacement and the loading
  of the native library, it will probably break things horribly
  if any of the functions are called during compile time.
  You have been warned."
  [lib & body]
  (let [lib (apply parse lib body)
        loadfn (gensym "loadfn")]
    `(let [~loadfn ~(load-fn lib)]
       ~@(make-clj-stubs lib loadfn))))

(comment

(use 'clj-native)

(defclib
 m
 (sin [double] double)
 (cos [double] double))

)
