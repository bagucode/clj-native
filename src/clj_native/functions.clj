;;   Copyright (c) Markus Gustavsson. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;;; ***************************************************************************
;;;
;;; -----===== Functions =====-----
;;;
;;; ***************************************************************************

(ns clj-native.functions
  (:use [clj-native direct-util])
  (:import [clojure.asm ClassVisitor MethodVisitor ClassWriter Opcodes]))

(defn make-native-func
  "Create a static method stub for a native function"
  [#^ClassVisitor cv fndesc]
  (let [name (:name fndesc)
        rettype (resolve-type (:rettype fndesc))
        argtypes (map resolve-type (:argtypes fndesc))
        opcodes (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC Opcodes/ACC_NATIVE)
        desc (str "(" (apply str (map descriptor argtypes))
                  ")" (descriptor rettype))]
    (-> cv
        (.visitMethod (int opcodes) name desc nil nil)
        (.visitEnd))))

(defn make-native-lib-stub
  "Create the class needed for JNA direct mapping."
  [lib clj-name fn-descriptors & options]
  (let [defaults {:pkg (.replaceAll (str (ns-name *ns*)) "-" "_")
                  :name clj-name
                  :libname lib}
        opts (merge defaults (apply array-map options))
        {:keys [pkg name libname]} opts]
    (let [#^ClassVisitor cv (ClassWriter. 0)]
      (.visit cv Opcodes/V1_5 Opcodes/ACC_PUBLIC
              (.replaceAll (str pkg \/ name) "\\." "/")
              nil "java/lang/Object" (make-array String 0))
      ;; Class static block, must call Native.register here
      (doto #^MethodVisitor (.visitMethod cv Opcodes/ACC_STATIC
                                          "<clinit>" "()V" nil nil)
        (.visitCode)
        (.visitLdcInsn libname)
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

(defn parse-functions
  [fns user-types]
  (let [third (fn [coll] (first (next (next coll))))
        drop-until (fn [pred coll] (drop-while #(not (pred %)) coll))]
    (doall
     (for [fdef fns]
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
           ;; User must manually use the .getByteBuffer of the Pointer class.
           ;; constchar* is ok though since they are null terminated.
           (when (and rettype
                      (.endsWith (str rettype) "*")
                      (and (not= 'void* rettype)
                           (not= 'constchar* rettype)
                           (not= 'constwchar_t* rettype)
                           (not (contains? @user-types rettype))))
             (throw (Exception.
                     (str
                      "typed pointers (with the exception of"
                      " constchar*/constwchar_t*) are"
                      " not supported as return types: "
                      fdef))))
           {:name (str name)
            :rettype (delay (check-type (or rettype 'void) user-types))
            :argtypes (delay (vec (for [a argvec] (check-type a user-types))))
            :doc doc
            :cljname cljname}))))))

(defn make-function-stubs
  "Creates stub functions for linking against before the native
  library has been loaded."
  [lib]
  (for [fdef (:fns lib)]
    (let [m (if (:doc fdef)
              {:doc (:doc fdef)}
              {})
          ;; TODO: fix this again... breaks with user defined classes
          ;; m (if (not= (force (:rettype fdef)) 'void)
          ;;     (assoc m :tag
          ;;            (let [t (resolve-type (force (:rettype fdef)))]
          ;;              (if (map? t)
          ;;                (symbol (user-type-class t))
          ;;                t)))
          ;;     m)
          ;; m (assoc m :arglists (list 'quote (list (force (:argtypes fdef)))))
          n (:cljname fdef)
          args (argnames (force (:argtypes fdef)))
          msg (str "Must call (loadlib " (:lib lib) ") before this function")]
      (list 'def (with-meta n m)
            (list `fn (vec args) `(throw (Exception. ~msg)))))))
