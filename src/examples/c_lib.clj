;; IMPORTANT!
;; (System/setProperty "jna.library.path" "your/lib/path/here")
;; Must be done before calling loadlib, preferrably on command line


(ns examples.c-lib
  (:use [clj-native.direct :only [defclib loadlib typeof]]
        [clj-native.structs :only [byref byval]]
        [clj-native.callbacks :only [callback]]))

(defclib
  my-lib-name ;;  c_lib
  (:libname "c_lib")
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
   (add-cb [int int] int)
   (reply-callback [void* void* i32] void)
   (void-param-callback [void*] void))
  (:functions
   (add [int int] int)
   (call-add-callback call_add_callback [add-cb int int] int)
   (call-void-param-callback call_void_param_callback [void-param-callback void*] void)
   (get-ptr get_ptr [] void*)
   (call-reply-callback call_reply_callback [reply-callback void* char* i32] void)
   (count-bytes count_bytes [char*] long)
   (addOneToStructByReference [struct1*] struct1*)
   (addOneToStructByValue [struct1] struct1)
   (addOneToStructTwoByValue [struct2] struct2)
   (returnsConstantString [] constchar*)
   (returnsConstantWString [] constwchar_t*)
   (addOneToUnionIntByValue [splitint] splitint)
   (addOneToUnionIntByReference [splitint*])))

  (defn main
    []
    (loadlib my-lib-name)
    ;; (loadlib c_lib)
    (let [cb (callback add-cb (fn [x y]
                                (println "in callback!")
                                (+ x y)))
          rcb (callback reply-callback
                        (fn [ptr buf size]
                          (let [bb (.getByteBuffer buf 0 10000)
                                n-bytes (count-bytes bb)]
                            (println "in reply callback! size is " size n-bytes bb))))
          vcb (callback void-param-callback
                        (fn [vp]
                          (println "The pointer is" vp)))
          vptr (get-ptr)
          s1val (byval struct1)
          s1ref (byref struct1)
          s2val (byval struct2)
          ;; set alignment to match the C code (packed)
          splitintval (byval splitint com.sun.jna.Structure/ALIGN_NONE)
          splitintref (byref splitint com.sun.jna.Structure/ALIGN_NONE)
          countme (java.nio.ByteBuffer/allocate 10000)]
      (dotimes [_ 10000] (.put countme (byte 0)))
      (.rewind countme)
      (dotimes [_ 100] (.put countme (byte 1)))
      (.rewind countme)
      (println "count 100 bytes:" (count-bytes countme))
      (call-void-param-callback vcb vptr)
      (println "vptr is" vptr)
      (println "Result of add(10, 35):" (add 10 35))
      (println "Result of call_add_callback(cb, 10, 78):"
               (call-add-callback cb 10 78))
      (println "Result of call_add_callback(cb, 90, 78):"
               (call-add-callback cb 90 78))
      (println "Passing struct1 by value")
      (println s1val)
      (println (addOneToStructByValue s1val))
      (println s1val)
      (println "Passing struct1 by reference")
      (println s1ref)
      (println (addOneToStructByReference s1ref))
      (println s1ref)
      (println "testing reply callback")
      (println "Result of call_reply_callback():"
               (call-reply-callback rcb vptr countme 1))
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
            _ (.readField ret "packed") ;; force read. Should this really be necessary?
            ;; _ (.setType ret (typeof packed :val)) ;; I wish this forced a read
            s1 (.s1 (.packed ret))
            s2 (.s2 (.packed ret))]
        (println (str " s1: " s1
                      ", s2: " s2)))
      (println "Passing splitint by reference")
      (println (.theint splitintref))
      (addOneToUnionIntByReference splitintref)
      (.readField splitintref "packed") ;; force read
      ;; (.setType splitintref (typeof packed :val))
      (println (str " s1: " (.s1 (.packed splitintref))
                    ", s2: " (.s2 (.packed splitintref))))))

(comment
  (main)
  )
