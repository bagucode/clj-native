;;   Copyright (c) Markus Gustavsson. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;;; ***************************************************************************
;;;
;;; -----===== Structs =====-----
;;;
;;; ***************************************************************************

(ns clj-native.structs
  (:use [clj-native core])
  (:import [clojure.asm ClassVisitor MethodVisitor ClassWriter Opcodes]
           [java.util UUID]))

(defn make-native-struct
  "Creates jna classes for representing a C struct
  that may be passed by value or reference.
  Returns a vector with 3 items; the bytecode for
  the specified struct as well as bytecode for inner
  classes representing the ByValue and ByReference
  versions of the struct respectively."
  [struct-spec]
  (let [#^ClassVisitor cv (ClassWriter. 0)
        replace-dots (fn [#^String s] (.replaceAll s "\\." "/"))
        internal-name (replace-dots (:classname struct-spec))
        inner-name (fn [t] (str internal-name "$" t))
        inner (fn [t]
                (let [#^ClassVisitor cv (ClassWriter. 0)
                      iname (inner-name t)]
                  (.visit cv Opcodes/V1_5
                          (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC)
                          iname nil internal-name
                          (into-array
                           String [(str "com/sun/jna/Structure$" t)]))
                  (.visitOuterClass cv internal-name nil nil)
                  (doto #^MethodVisitor
                    (.visitMethod cv Opcodes/ACC_PUBLIC "<init>" "()V" nil nil)
                    (.visitCode)
                    (.visitVarInsn Opcodes/ALOAD 0)
                    (.visitMethodInsn
                     Opcodes/INVOKESPECIAL internal-name "<init>" "()V")
                    (.visitInsn Opcodes/RETURN)
                    (.visitMaxs 1 1)
                    (.visitEnd))
                  (doto #^MethodVisitor
                    (.visitMethod cv Opcodes/ACC_PUBLIC "<init>" "(I)V" nil nil)
                    (.visitCode)
                    (.visitVarInsn Opcodes/ALOAD 0)
                    (.visitMethodInsn
                     Opcodes/INVOKESPECIAL internal-name "<init>" "()V")
                    (.visitVarInsn Opcodes/ALOAD 0)
                    (.visitVarInsn Opcodes/ILOAD 1)
                    (.visitMethodInsn
                     Opcodes/INVOKEVIRTUAL internal-name "setAlignType" "(I)V")
                    (.visitInsn Opcodes/RETURN)
                    (.visitMaxs 2 2)
                    (.visitEnd))
                  (.visitEnd cv)
                  (.toByteArray cv)))]
    (.visit cv Opcodes/V1_5 Opcodes/ACC_PUBLIC internal-name
            nil "com/sun/jna/Structure" (make-array String 0))
    (.visitInnerClass cv (inner-name 'ByValue) internal-name
                      "ByValue" (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC))
    (.visitInnerClass cv (inner-name 'ByReference) internal-name
                      "ByReference"
                      (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC))
    (doto #^MethodVisitor
      (.visitMethod cv Opcodes/ACC_PUBLIC "<init>" "()V" nil nil)
      (.visitCode)
      (.visitVarInsn Opcodes/ALOAD 0)
      (.visitMethodInsn
       Opcodes/INVOKESPECIAL "com/sun/jna/Structure" "<init>" "()V")
      (.visitInsn Opcodes/RETURN)
      (.visitMaxs 1 1)
      (.visitEnd))
    (doseq [field (:fields struct-spec)]
      (let [{nm :name
             type :type} field]
        (.visitEnd (.visitField cv Opcodes/ACC_PUBLIC (name nm)
                                (descriptor (resolve-type type))
                                nil nil))))
    (.visitEnd cv)
    [(.toByteArray cv) (inner 'ByValue) (inner 'ByReference)]))

(defn make-struct-constructor-stubs
  "Creates stub functions for struct constructors."
  [lib]
  (let [ns (ns-name *ns*)
        msg (str "Must call loadlib-" (:lib lib) " before this function")]
    (for [sspec (:structs lib)]
      (let [valname (symbol (str (:name sspec) "-byval"))
            refname (symbol (str (:name sspec) "-byref"))]
        `(do
           (defn ~valname
             ~(str "Creates a new structure object of type\n  "
                   (:name sspec) " that can be passed by value\n "
                   "between jvm and native code.")
             ([] (throw (Exception. ~msg)))
             ([~'alignment] (throw (Exception. ~msg))))
           (defn ~refname
             ~(str "Creates a new structure object of type\n  "
                   (:name sspec) " that can be passed by reference\n "
                   "between jvm and native code.")
             ([] (throw (Exception. ~msg)))
             ([~'alignment] (throw (Exception. ~msg)))))))))

(defn make-struct-constructor
  "Creates code for creating a constructor
  function for a given struct."
  [sspec]
  (let [ns (ns-name *ns*)
        valname (symbol (str (:name sspec) "-byval"))
        refname (symbol (str (:name sspec) "-byref"))
        v (gensym)
        code (fn [name jname]
               `(eval
                 ~(list 'quote
                        `(let [~v (ns-resolve '~ns '~name)]
                           (.bindRoot
                            ~v
                            (fn
                              ([] (~(symbol (str (:classname sspec)
                                                 "$" jname "."))))
                              ([alignment#]
                                 (~(symbol (str (:classname sspec)
                                                "$" jname "."))
                                  alignment#))))))))]
    `(do
       ~(code valname 'ByValue)
       ~(code refname 'ByReference))))

(defn parse-structs
  [structs user-types]
  (doall
   (for [s structs]
     (do
       (when-not (symbol? (first s))
         (throw (Exception. (str "Malformed struct spec: " s
                                 " name must be a symbol."))))
       (when-not (even? (count (next s)))
         (throw (Exception. (str "Malformed struct spec: " s
                                 " uneven field declarations."))))
       (let [name (first s)
             classname (.replaceAll
                        (str (ns-name *ns*) \. "struct_" (UUID/randomUUID))
                        "-" "_")
             fields (apply array-map (next s))]
         (swap! user-types assoc name
                {:name name :classname classname :kind :struct})
         (let [ptrname (symbol (str name "*"))]
           (swap! user-types assoc ptrname
                  {:name ptrname :classname classname :kind :struct}))
         {:name name
          :classname classname
          :fields (for [f fields]
                    {:name (key f)
                     :type (delay (check-type (val f) user-types))})})))))

