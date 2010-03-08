;;   Copyright (c) Markus Gustavsson. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;;; ***************************************************************************
;;;
;;; -----===== Unions =====-----
;;;
;;; ***************************************************************************

(ns clj-native.unions
  (:use [clj-native core])
  (:import [clojure.asm ClassVisitor MethodVisitor ClassWriter Opcodes]
           [java.util UUID]))

;;; ye olde stuff

(defn make-native-union
  "Creates jna classes for representing a C union
  that may be passed by value or reference.
  Returns a vector with 3 items; the bytecode for
  the specified union as well as bytecode for inner
  classes representing the ByValue and ByReference
  versions of the union respectively."
  [union-spec]
  (let [#^ClassVisitor cv (ClassWriter. 0)
        replace-dots (fn [#^String s] (.replaceAll s "\\." "/"))
        internal-name (replace-dots (:classname union-spec))
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
            nil "com/sun/jna/Union" (make-array String 0))
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
       Opcodes/INVOKESPECIAL "com/sun/jna/Union" "<init>" "()V")
      (.visitInsn Opcodes/RETURN)
      (.visitMaxs 1 1)
      (.visitEnd))
    (doseq [field (:fields union-spec)]
      (let [{nm :name
             type :type} field]
        (.visitEnd (.visitField cv Opcodes/ACC_PUBLIC (name nm)
                                (descriptor (resolve-type type))
                                nil nil))))
    (.visitEnd cv)
    [(.toByteArray cv) (inner 'ByValue) (inner 'ByReference)]))

(defn make-union-stubs
  [ns lib]
  (for [uspec (:unions lib)]
    `(intern (or (find-ns ~ns) *ns*) '~(:name uspec))))

(defn make-union-constructors
  [ns uspec]
  (let [class (symbol (:classname uspec))
        valclass (symbol (str (:classname uspec) "$ByValue"))
        refclass (symbol (str (:classname uspec) "$ByReference"))
        code (fn [type]
               `(eval
                 ~(list 'quote
                        `(fn
                           ([] (~(symbol (str class "$" type "."))))
                           ([alignment#]
                              (~(symbol (str class "$" type "."))
                               alignment#))))))]
    `(intern (or (find-ns ~ns) *ns*) '~(:name uspec)
             ~{:type :union
               :class `(eval ~(list 'quote class))
               :valclass `(eval ~(list 'quote valclass))
               :refclass `(eval ~(list 'quote refclass))
               :byval (code 'ByValue)
               :byref (code 'ByReference)})))

(defn parse-unions
  [unions user-types]
  (doall
   (for [u unions]
     (do
       (when-not (symbol? (first u))
         (throw (Exception. (str "Malformed union spec: " u
                                 " name must be a symbol."))))
       (when-not (even? (count (next u)))
         (throw (Exception. (str "Malformed union spec: " u
                                 " uneven field declarations."))))
       (let [name (first u)
             classname (.replaceAll
                        (str (ns-name *ns*) \. "union_" (UUID/randomUUID))
                        "-" "_")
             fields (apply array-map (next u))]
         ;; Treat as structs when checking :kind. They work in the same way
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

