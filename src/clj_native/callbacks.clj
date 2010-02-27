;;   Copyright (c) Markus Gustavsson. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;;; ***************************************************************************
;;;
;;; -----===== Callbacks =====-----
;;;
;;; ***************************************************************************

(ns clj-native.callbacks
  (:use [clj-native core])
  (:import [clojure.asm ClassVisitor MethodVisitor ClassWriter Opcodes]
           [java.util UUID]))

(defn make-callback-interface
  "Creates a java interface for a C callback specification."
  [cbspec]
  (let [#^ClassVisitor cv (ClassWriter. 0)]
    (.visit cv Opcodes/V1_5
            (+ Opcodes/ACC_PUBLIC Opcodes/ACC_ABSTRACT Opcodes/ACC_INTERFACE)
            (.replaceAll (:classname cbspec) "\\." "/") nil "java/lang/Object"
            (into-array String ["com/sun/jna/Callback"]))
    (let [rettype (resolve-type (:rettype cbspec))
          argtypes (map resolve-type (:argtypes cbspec))
          opcodes (+ Opcodes/ACC_PUBLIC Opcodes/ACC_ABSTRACT)
          desc (str "(" (apply str (map descriptor argtypes))
                    ")" (descriptor rettype))]
      (doto #^MethodVisitor (.visitMethod cv (int opcodes) "invoke"
                                          desc nil nil)
            (.visitEnd)))
    (.visitEnd cv)
    (.toByteArray cv)))

(defn make-callback-constructor-stubs
  "Creates stub functions for callback constructors."
  [lib]
  (let [ns (ns-name *ns*)
        msg (str "Must call loadlib-" (:lib lib) " before this function")]
    (for [cbspec (:cbs lib)]
      (let [name (symbol (str "make-" (:name cbspec)))]
        `(defn ~name
           ~(str "Creates a new callback proxy object of type\n  "
                 (:name cbspec) " that will delegate to the supplied clojure\n"
                 "  function when it is called from native code.\n"
                 "  The clojure function must take " (count (force (:argtypes cbspec)))
                 " arguments\n  and return an object"
                 " of type " (let [t (resolve-type (force (:rettype cbspec)))]
                               (if (map? t)
                                 (:name t)
                                 t)))
           [~'f] (throw (Exception. ~msg)))))))

(defn make-callback-constructor
  "Creates code for creating a constructor
  function for a given callback."
  [cbspec]
  (let [args (take (count (force (:argtypes cbspec))) (repeatedly gensym))
        ns (ns-name *ns*)
        name (symbol (str "make-" (:name cbspec)))
        v (gensym)]
    `(eval
      ~(list 'quote
             `(let [~v (ns-resolve '~ns '~name)]
                (.bindRoot
                 ~v
                 (fn [~'f]
                   (proxy [~(symbol (:classname cbspec))] []
                     (~'invoke ~(vec args)
                               (~'f ~@args))))))))))

(defn parse-callbacks
  [cbs user-types]
  (doall
   (for [cb cbs]
     (let [name (first cb)
           argtypes (second cb)
           rettype (first (next (next cb)))
           except #(throw (Exception. (str "malformed callback spec: " cb)))
           classname (.replaceAll
                      (str (ns-name *ns*) \. "callback_" (UUID/randomUUID))
                      "-" "_")]
       (when-not name (except))
       (when-not (and argtypes (vector? argtypes)) (except))
       (swap! user-types assoc name
              {:name name :classname classname :kind :callback})
       {:name name
        :argtypes (delay (vec (for [a argtypes] (check-type a user-types))))
        :rettype (delay (check-type (or rettype 'void) user-types))
        :classname classname}))))
