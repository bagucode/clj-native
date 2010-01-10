(ns c-lib
  (:use [clj-native.core :only [defclib]]))

(defclib
  c_lib
  (:callbacks
   (add-cb [int int] int))
  (:functions
   (add [int int] int)
   (call_add_callback call-add-callback [fn int int] int)))

(defn main
  []
  (loadlib-c_lib)
  (let [callback (make-add-cb (fn [x y]
                                (println "in callback!")
                                (+ x y)))]
    (println "Result of add(10, 35):" (add 10 35))
    (println "Result of call_add_callback(callback, 10, 78):"
             (call-add-callback callback 10 78))))

