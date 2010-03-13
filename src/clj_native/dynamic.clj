;;   Copyright (c) Markus Gustavsson. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;;; ***************************************************************************
;;;
;;; -----===== JNA Dynamic Mapping =====-----
;;;
;;; ***************************************************************************

(ns clj-native.dynamic
  (:import [com.sun.jna Function NativeLibrary Pointer]))

(defn #^Function get-function
  "Obtain a jna Function object representing a native
  function that follows the standard \"C\" calling convention."
  [#^String lib-name #^String fn-name]
  (Function/getFunction lib-name fn-name))

(defn bind-function
  "Binds a jna Function object to a clojure function."
  [#^Function jna-fn #^Class return-type]
  (if (= Void/TYPE return-type)
    (fn [& args] (.invoke jna-fn (into-array args)))
    (fn [& args] (.invoke jna-fn return-type (into-array args)))))

(defn #^Pointer get-global
  "Gets a pointer to a global variable in the specified library.
  Throws UnsatisfiedLinkError if the library or symbol could not be found."
  [#^String lib-name #^String variable-name]
  (let [lib #^NativeLibrary (NativeLibrary/getInstance lib-name)]
    (.getGlobalVariableAddress lib variable-name)))

(defmacro defcfn
  "Defines a Clojure function with a variable number of arguments
  in the current namespace that will delegate to a C function.
  If no clj-name is supplied the clojure function will have the
  same name as the C function.
  Syntax for fn-sym is library/function example: c/printf or m/sin."
  ([fn-sym return-type]
     `(defcfn ~fn-sym ~return-type nil))
  ([fn-sym return-type clj-name]
     (let [f (gensym "f")]
       `(let [~f (bind-function
                  (get-function (namespace '~fn-sym) (name '~fn-sym))
                  ~return-type)]
          ~(if clj-name
             `(intern ~'*ns* '~clj-name ~f)
             `(intern ~'*ns* (symbol (name '~fn-sym)) ~f))))))

(defmacro defcvar
  "Defines a Clojure Var in the current namespace
  that contains a JNA Pointer object that holds the
  address of a global C variable."
  ([variable-sym]
     `(defcvar ~variable-sym nil))
  ([variable-sym clj-name]
     (let [v (gensym "v")]
       `(let [~v (get-global (namespace '~variable-sym)
                             (name '~variable-sym))]
          ~(if clj-name
             `(intern ~'*ns* '~clj-name ~v)
             `(intern ~'*ns* (symbol (name '~variable-sym)) ~v))))))