;; IMPORTANT!
;; (System/setProperty "jna.library.path" "your/lib/path/here")
;; Must be done before calling loadlib*, preferrably on command line

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
   (struct2 :ll longlong :s1ByValue struct1)
   (circle1 :c2 circle2*)
   (circle2 :c1 circle1*)
   (c-list :data void* :next c-list*)) ;; can't really name it list
  (:callbacks
   (add-cb [int int] int))
  (:functions
   (add [int int] int)
   (call_add_callback call-add-callback [add-cb int int] int)
   (addOneToStructByReference [struct1*] struct1*)
   (addOneToStructByValue [struct1] struct1)
   (addOneToStructTwoByValue [struct2] struct2)
   (returnsConstantString [] constchar*)
   (returnsConstantWString [] constwchar_t*)))

(defn main
  []
  (loadlib-c_lib)
  (let [callback (make-add-cb (fn [x y]
                                (println "in callback!")
                                (+ x y)))
        s1val (struct1-byval)
        s1ref (struct1-byref)
        s2val (struct2-byval)]
    (println "Result of add(10, 35):" (add 10 35))
    (println "Result of call_add_callback(callback, 10, 78):"
             (call-add-callback callback 10 78))
    (println "Passing struct1 by value")
    (println s1val)
    (println (addOneToStructByValue s1val))
    (println s1val)
    (println "Passing struct1 by reference")
    (println s1ref)
    (println (addOneToStructByReference s1ref))
    (println s1ref)
    (println "Passing struct2 by value")
    (println s2val)
    (println (addOneToStructTwoByValue s2val))
    (println s2val)
    (println (returnsConstantString))
    (println (returnsConstantWString))))

(comment

(main)

)