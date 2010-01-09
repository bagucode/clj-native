;;   Copyright (c) Markus Gustavsson. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

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

(defn native-long
  "Returns the java type corresponding to
  the bitsize of a native long."
  []
  (condp = com.sun.jna.NativeLong/SIZE
    4 Integer/TYPE
    8 Long/TYPE))

(defn native-long-buffer
  "Returns the type of nio buffer appropriate
  to store arrays of native longs."
  []
  (condp = com.sun.jna.NativeLong/SIZE
    4 java.nio.IntBuffer
    8 java.nio.LongBuffer))

(def type-map
     {'char Byte/TYPE
      'wchar_t Character/TYPE
      'byte Byte/TYPE
      'short Short/TYPE
      'int Integer/TYPE
      'enum Integer/TYPE
      'BOOL Boolean/TYPE
      'bool Boolean/TYPE
      'size_t native-long
      'long native-long
      'longlong Long/TYPE
      '__int64 Long/TYPE
      'i8 Byte/TYPE
      'i16 Short/TYPE
      'i32 Integer/TYPE
      'i64 Long/TYPE
      'float Float/TYPE
      'double Double/TYPE
      'void Void/TYPE
      'void* com.sun.jna.Pointer
      'byte* java.nio.ByteBuffer
      'char* java.nio.ByteBuffer
      'constchar* String
      'wchar_t* java.nio.CharBuffer
      'constwchar_t* com.sun.jna.WString
      'short* java.nio.ShortBuffer
      'int* java.nio.IntBuffer
      'long* native-long-buffer
      'size_t* native-long-buffer
      'longlong* java.nio.LongBuffer
      '__int64* java.nio.LongBuffer
      'i8* java.nio.ByteBuffer
      'i16* java.nio.ShortBuffer
      'i32* java.nio.IntBuffer
      'i64* java.nio.LongBuffer
      'float* java.nio.FloatBuffer
      'double* java.nio.DoubleBuffer
      ;; the types below are not yet supported using direct mapping in jna
      ;;       'char** (class (make-array String 0))
      ;;       'wchar_t** (class (make-array com.sun.jna.WString 0))
      ;;       'void** (class (make-array com.sun.jna.Pointer 0))
      'struct com.sun.jna.Structure
      'struct* com.sun.jna.Structure
      'union com.sun.jna.Union
      ;; No idea how anyone will ever be able to write struct[] :)
      (symbol "struct[]") (class (make-array com.sun.jna.Structure 0))
      'structs (class (make-array com.sun.jna.Structure 0))
      'struct-array (class (make-array com.sun.jna.Structure 0))
      'fn com.sun.jna.Callback
      'fn* com.sun.jna.Callback
      'function com.sun.jna.Callback
      'function* com.sun.jna.Callback
      'callback com.sun.jna.Callback
      'callback* com.sun.jna.Callback})

(defn descriptor
  "Get internal name type descriptor for t"
  [t]
  (cond
   (class? t) (clojure.asm.Type/getDescriptor t)
   (fn? t) (clojure.asm.Type/getDescriptor (t))))

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
              nil "java/lang/Object" (make-array String 0))
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
              ;; Can't support Buffers as return types since there is no way
              ;; of knowing the size of the returned memory block.
              ;; User must manually use the .asByteBuffer of the Pointer class.
              (when (and rettype
                         (.endsWith (str rettype) "*")
                         (not= 'void* rettype))
                (throw (Exception.
                        (str
                         "typed pointers are not supported as return types: "
                         fdef))))
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
  (let [clsname (str (:pkg lib) \. (:classname lib))
        ns (ns-name *ns*)
        v (gensym)] ;; function var
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
       ;; TODO: special handling of types that need conversion?
       (eval
        ~(list 'quote
               (list 'let [v `(ns-resolve '~ns '~n)]
                     `(.bindRoot
                       ~v
                       ~(list `fn (vec args)
                              (list* (symbol (str clsname \/ native))
                                     args))))))
       ;; call new version of self
       ~(list* n args))))

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
  This can also cause trouble if the functions are used as
  arguments to higher order functions since the replacement
  only modifies the function's var. That means that any reference
  directly to the function (such as a local in another function)
  will not be updated. It is therefore best not to use the
  functions as arguments to higher order functions until after
  they have been called at least once.
  You have been warned."
  [lib & body]
  (let [lib (apply parse lib body)
        loadfn (gensym "loadfn")]
    `(let [~loadfn ~(load-fn lib)]
       ~@(make-clj-stubs lib loadfn))))

(comment

  (use ['clj-native.core :only ['defclib]])

  (defclib
    m
    (sin [double] double)
    (cos [double] double))

  (sin 1) ;; => 0.8414709848078965

  ;; Typed pointers are represented as nio buffers.
  ;; It's also possible to use java arrays with jna but I chose
  ;; nio buffers because they are faster (less copying) and
  ;; safer (direct buffers are not moved by GC so it's safe for
  ;; native code to hold on to their pointers).

  (defclib
    c
    (malloc [size_t] void*)
    (free [void*])
    (memset [byte* int size_t] void*))

  (def mem (malloc 100))
  (def view (.getByteBuffer mem 0 100))
  (memset view 10 100)
  (.get view 20) ;; => 10
  (def int-view (.asIntBuffer view)) ;; 25 ints (100/4)
  (memset view 1 100)
  (.get int-view 20) ;; => 16843009 (four bytes, each with their lsb set)
  (free mem) ;; => nil
  (.get int-view 0) ;; => Undefined. Don't use freed memory ;)

)
