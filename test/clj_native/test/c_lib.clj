(ns clj-native.test.c-lib
  (:use [clj-native.direct :only [defclib loadlib typeof]]
        [clj-native.structs :only [byref byval]]
        [clj-native.callbacks :only [callback]]
        [clojure.test]))

;; The code from src/examples/c_lib.clj has been morphed into a test
;; structure to get some coverage of a variety of issues.

;; ======================================================================
(defclib c_lib
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

;; Must be done before calling loadlib, preferrably on command line
(println "NOTE: Testing assumes a built src/examples/c_lib library")
(System/setProperty "jna.library.path" "./src/examples")
(loadlib c_lib)

;; ======================================================================
(deftest test-add
  (are [a b z] (= z (add a b))
       10 35 (+ 10 35)
       ))

(deftest test-countme
  (let [countme (java.nio.ByteBuffer/allocate 10000)
        _ (dotimes [_ 10000] (.put countme (byte 0)))
        _ (.rewind countme)
        _ (dotimes [_ 100] (.put countme (byte 1)))
        _ (.rewind countme)
        r (count-bytes countme)]
    (is (= r 100))))

(deftest test-callback1
  (let [cb (callback add-cb (fn [x y]
                              (println "in callback!")
                              (+ x y)))]
    (is (+ 10 78) (call-add-callback cb 10 78))
    (is (+ 90 78) (call-add-callback cb 90 78))))

(deftest test-pass-by-val
  (let [s1val (byval struct1)
        _ (is (= 0 (.x s1val)))
        _ (is (= 0 (.y s1val)))
        _ (is (= 0.0 (.k s1val)))
        s1valres (addOneToStructByValue s1val)
        _ (is (= 0 (.x s1val)))
        _ (is (= 0 (.y s1val)))
        _ (is (= 0.0 (.k s1val)))
        _ (is (= 1 (.x s1valres)))
        _ (is (= 1 (.y s1valres)))
        _ (is (= 1.0 (.k s1valres)))
        s1valres2 (addOneToStructByValue s1val)]
    (is (= 0 (.x s1val)))
    (is (= 0 (.y s1val)))
    (is (= 0.0 (.k s1val)))
    (is (= 1 (.x s1valres)))
    (is (= 1 (.y s1valres)))
    (is (= 1.0 (.k s1valres)))
    ))

(deftest test-pass-by-ref
  (let [s1ref (byref struct1)
        _ (is (= 0 (.x s1ref)))
        _ (is (= 0 (.y s1ref)))
        _ (is (= 0.0 (.k s1ref)))
        s1refres (addOneToStructByReference s1ref)
        _ (is (= 1 (.x s1ref)))
        _ (is (= 1 (.y s1ref)))
        _ (is (= 1.0 (.k s1ref)))
        _ (is (= 1 (.x s1refres)))
        _ (is (= 1 (.y s1refres)))
        _ (is (= 1.0 (.k s1refres)))
        s1refres2 (addOneToStructByReference s1ref)]
    (is (= 2 (.x s1ref)))
    (is (= 2 (.y s1ref)))
    (is (= 2.0 (.k s1ref)))
    (is (= 2 (.x s1refres2)))
    (is (= 2 (.y s1refres2)))
    (is (= 2.0 (.k s1refres2)))
    ))

(deftest test-pass-by-val2
  (let [s2val (byval struct2)
        s2valres (addOneToStructTwoByValue s2val)]
    (is (= 0 (.ll s2val)))
    (is (= 0 (.x (.s1ByValue s2val))))
    (is (= 0 (.y (.s1ByValue s2val))))
    (is (= 0.0 (.k (.s1ByValue s2val))))
    (is (= 1 (.ll s2valres)))
    (is (= 1 (.x (.s1ByValue s2valres))))
    (is (= 1 (.y (.s1ByValue s2valres))))
    (is (= 1.0 (.k (.s1ByValue s2valres))))
    ))

(deftest test-string
  (is (= "This string should be safe to read as const char*"
         (returnsConstantString)))
  )

;; FIXME!
(comment probably better to have tests pass...
(deftest test-wstring
  (is (= "This string should be safe to read as const wchar_t*"
         (returnsConstantWString)))
  )
)


;; ======================================================================
;; TODO -- move everything mentioned below into deftest routines
;; use the ; X at the end of the line to mark bits that now have tests
;; eventually, delete all this...
(comment                                                                ; X
  (defn main                                                            ; X
    []                                                                  ; X
    (loadlib my-lib-name)                                               ; X
    ;; (loadlib c_lib)                                                  ; X
    (let [cb (callback add-cb (fn [x y]                                 ; X
                                (println "in callback!")                ; X
                                (+ x y)))                               ; X
          rcb (callback reply-callback
                        (fn [ptr buf size]
                          (let [bb (.getByteBuffer buf 0 10000)
                                n-bytes (count-bytes bb)]
                            (println "in reply callback! size is " size n-bytes bb))))
          vcb (callback void-param-callback
                        (fn [vp]
                          (println "The pointer is" vp)))
          vptr (get-ptr)
          s1val (byval struct1)                                         ; X
          s1ref (byref struct1)                                         ; X
          s2val (byval struct2)                                         ; X
          ;; set alignment to match the C code (packed)
          splitintval (byval splitint com.sun.jna.Structure/ALIGN_NONE)
          splitintref (byref splitint com.sun.jna.Structure/ALIGN_NONE)
          countme (java.nio.ByteBuffer/allocate 10000)]                 ; X
      (dotimes [_ 10000] (.put countme (byte 0)))                       ; X
      (.rewind countme)                                                 ; X
      (dotimes [_ 100] (.put countme (byte 1)))                         ; X
      (.rewind countme)                                                 ; X
      (println "count 100 bytes:" (count-bytes countme))                ; X
      (call-void-param-callback vcb vptr)
      (println "vptr is" vptr)
      (println "Result of add(10, 35):" (add 10 35))                    ; X
      (println "Result of call_add_callback(cb, 10, 78):"               ; X
               (call-add-callback cb 10 78))                            ; X
      (println "Result of call_add_callback(cb, 90, 78):"               ; X
               (call-add-callback cb 90 78))                            ; X
      (println "Passing struct1 by value")                              ; X
      (println s1val)                                                   ; X
      (println (addOneToStructByValue s1val))                           ; X
      (println s1val)                                                   ; X
      (println "Passing struct1 by reference")                          ; X
      (println s1ref)                                                   ; X
      (println (addOneToStructByReference s1ref))                       ; X
      (println s1ref)                                                   ; X
      (println "testing reply callback")
      (println "Result of call_reply_callback():"
               (call-reply-callback rcb vptr countme 1))
      (println "Passing struct2 by value")                              ; X
      (println s2val)                                                   ; X
      (println (addOneToStructTwoByValue s2val))                        ; X
      (println s2val)                                                   ; X
      (println (returnsConstantString))                                 ; X
      (println (returnsConstantWString))                                ; X
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
  )

