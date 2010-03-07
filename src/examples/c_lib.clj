;; IMPORTANT!
;; (System/setProperty "jna.library.path" "your/lib/path/here")
;; Must be done before calling loadlib, preferrably on command line


(ns c-lib
  (:use [clj-native.direct :only [defclib loadlib]]
        [clj-native.structs :only [byref byval]]
        [clj-native.callbacks :only [callback]]))

(defclib
  c_lib
  (:structs
   (struct1 :x int :y char :k float)
   (struct2 :ll longlong :s1ByValue struct1)
   (circle1 :c2 circle2*)
   (circle2 :c1 circle1*)
   (c-list :data void* :next c-list*) ;; can't really name it list
   (packed :s1 short :s2 short))
  (:unions
   (splitint :theint int :packed packed))
  (:callbacks
   (add-cb [int int] int))
  (:functions
   (add [int int] int)
   (call_add_callback call-add-callback [add-cb int int] int)
   (addOneToStructByReference [struct1*] struct1*)
   (addOneToStructByValue [struct1] struct1)
   (addOneToStructTwoByValue [struct2] struct2)
   (returnsConstantString [] constchar*)
   (returnsConstantWString [] constwchar_t*)
   (addOneToUnionIntByValue [splitint] splitint)
   (addOneToUnionIntByReference [splitint*])))

  (defn main
    []
    (loadlib c_lib)
    (let [cb (callback add-cb (fn [x y]
                                (println "in callback!")
                                (+ x y)))
          s1val (byval struct1)
          s1ref (byref struct1)
          s2val (byval struct2)
          ;; set alignment to match the C code (packed)
          splitintval (byval splitint com.sun.jna.Structure/ALIGN_NONE)
          splitintref (byref splitint com.sun.jna.Structure/ALIGN_NONE)]
      (println "Result of add(10, 35):" (add 10 35))
      (println "Result of call_add_callback(cb, 10, 78):"
               (call-add-callback cb 10 78))
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
      (println (returnsConstantWString))
      (.setType splitintval Integer/TYPE)
      (set! (.theint splitintval) 66000)
      (.setType splitintref Integer/TYPE)
      (set! (.theint splitintref) 66000)
      (println "Passing splitint by value")
      (println (.theint splitintval))
      (let [ret (addOneToUnionIntByValue splitintval)
            _ (.setType ret Integer/TYPE)
            intval (. ret theint)
            _ (.readField ret "packed") ;; force read. Should this really be necessary?
            s1 (.s1 (.packed ret))
            s2 (.s2 (.packed ret))]
        (println (str "theint: " intval
                      " s1: " s1
                      ", s2: " s2)))
      (println "Passing splitint by reference")
      (println (.theint splitintref))
      (addOneToUnionIntByReference splitintref)
      (.readField splitintref "packed") ;; force read
      (println (str "theint: " (.theint splitintref)
                    " s1: " (.s1 (.packed splitintref))
                    ", s2: " (.s2 (.packed splitintref))))))

(comment
  (main)
  )
