(ns c-lib
  (:use [clj-native.core :only [defclib]]))

(defclib
  c_lib
  ;; (:globals
  ;;  (globalInt int)
  ;;  (globalString constchar*))
  ;; (:unions
  ;;  (either :s1 struct1* :s2 struct2*))
  (:structs
   (struct1 :x int :y char :k float)
   ;; (struct2 :ll longlong :s1ByValue struct1)
   ;; (circle1 :c2 circle2*)
   ;; (circle2 :c1 circle1*)
   ;; (c-list :data void* :next c-list*)
   ) ;; can't really name it list
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
                                (+ x y)))
        s1val (struct1-byval)
        s1ref (struct1-byref)]
    (println "Result of add(10, 35):" (add 10 35))
    (println "Result of call_add_callback(callback, 10, 78):"
             (call-add-callback callback 10 78))))

(comment

(main)

)