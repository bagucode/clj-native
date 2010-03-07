;;   Copyright (c) Markus Gustavsson. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;;; ***************************************************************************
;;;
;;; -----===== JNA Direct Mapping =====-----
;;;
;;; ***************************************************************************

(ns clj-native.direct
  (:use [clj-native core functions callbacks structs])
  (:import [java.util UUID]
           [com.sun.jna Native Library]))

(defn parse-lib
  "Parses input to defclib and returns a library specification map"
  [lib & body]
  (when-not (symbol? lib)
    (throw (Exception. "lib must be a symbol"))) ;; overkill?
  (let [when-key (fn [k f ut]
                   (when-let [stuff (some #(when (= k (first %))
                                             (next %))
                                          body)]
                     (f stuff ut)))
        user-types (atom {})
        structs (when-key :structs parse-structs user-types)
        callbacks (when-key :callbacks parse-callbacks user-types)
        functions (when-key :functions parse-functions user-types)]
    {:lib lib
     :cbs callbacks
     :fns functions
     :structs structs
     :pkg (symbol (.replaceAll (str (ns-name *ns*)) "-" "_"))
     :classname (symbol (.replaceAll
                         (str "lib_" (UUID/randomUUID)) "-" "_"))}))

(defn loadlib-fn
  "Creates a function that will load a native library
  and replace all the mapped function stubs with real versions."
  [lib]
  (let [clsname (str (:pkg lib) \. (:classname lib))]
    `(fn []
       ;; Try to load the native library before creating and
       ;; loading the JNA class because we want to discover the
       ;; error of not finding the library file here rather than
       ;; in the static class initializer.
       (Native/loadLibrary ~(str (:lib lib)) Library)
        ;; Structs
       ~@(for [sspec (:structs lib)]
           `(let [[main# val# ref#] (make-native-struct
                                     '~(update-in
                                        sspec [:fields]
                                        #(doall
                                          (for [f %]
                                            (update-in f [:type] force)))))]
              (load-code ~(:classname sspec) main#)
              (load-code ~(str (:classname sspec) "$ByValue") val#)
              (load-code ~(str (:classname sspec) "$ByReference") ref#)
              ~(make-struct-constructors (list 'quote (ns-name *ns*)) sspec)))
       ;; Callback interfaces and constructors
       ~@(for [cbspec (:cbs lib)]
           `(do
              (load-code ~(:classname cbspec)
                         (make-callback-interface
                          '~(assoc cbspec
                              :rettype (force (:rettype cbspec))
                              :argtypes (force (:argtypes cbspec)))))
              ~(make-callback-constructor cbspec)))
       ;; Main glue class
       (load-code ~clsname
                  (make-native-lib-stub
                   ~(str (:lib lib))
                   '~(doall
                      (for [fdef (:fns lib)]
                        (-> (update-in fdef [:argtypes] force)
                            (update-in [:rettype] force))))
                   :name '~(:classname lib)
                   :pkg '~(:pkg lib)))
       ;; Rebinding of function var roots
       ;; TODO: move to own function like the others
       ~@(for [fdef (:fns lib)]
           (let [native (:name fdef)
                 name (:cljname fdef)
                 args (vec (argnames (force (:argtypes fdef))))
                 ns (ns-name *ns*)
                 v (gensym)]
             `(eval
               ~(list 'quote
                      (list 'let [v `(ns-resolve '~ns '~name)]
                            `(.bindRoot
                              ~v
                              ~(list `fn args
                                     (list* (symbol (str clsname \/ native))
                                            args)))))))))))

;;; ***************************************************************************
;;;
;;; -----===== Public Interface =====-----
;;;
;;; ***************************************************************************

;; TODO: new doc string
(defmacro defclib
  "Create C library bindings."
  [lib & body]
  (let [lib (apply parse-lib lib body)]
    `(do
       (def ~(:lib lib) {:loadfn ~(loadlib-fn lib)})
       ~@(make-struct-stubs (list 'quote (ns-name *ns*)) lib)
       ~@(make-callback-constructor-stubs lib)
       ~@(make-function-stubs lib))))

(defn loadlib
  "Loads a native library.
  DO NOT call this on the top level, should be called at runtime only."
  [libdef]
  ((:loadfn libdef)))


