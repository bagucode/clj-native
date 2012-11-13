(ns clj-native.test.c-lib
  (:use [clj-native.direct :only [defclib loadlib typeof]]
        [clj-native.structs :only [byref byval]]
        [clj-native.callbacks :only [callback]]
        [clojure.test]))

;; The code from src/examples/c_lib.clj has been morphed into a test
;; structure to get some coverage of a variety of issues.  There are a
;; few FIXME issues below to clean up.

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

;; WString is just a way to explicitly pass UTF-16 strings back and forth
;; between C and Java (wchar_t*)
(deftest test-wstring
  (is (= "This string should be safe to read as const wchar_t*"
         (.toString (returnsConstantWString)))))


(deftest test-void-param-callback
  (let [check (atom nil)
        vcb (callback void-param-callback
                      (fn [vp]
                        (reset! check vp)))
        vptr (get-ptr)]
    (call-void-param-callback vcb vptr)
    (is (= @check vptr))))

(deftest test-reply-callback
  (let [check (atom nil)
        rcb (callback reply-callback
                      (fn [ptr buf size]
                        (let [bb (.getByteBuffer buf 0 10000)
                              n-bytes (count-bytes bb)]
                          (reset! check n-bytes))))
        vptr (get-ptr)
        countme (java.nio.ByteBuffer/allocate 10000)
        _ (dotimes [_ 10000] (.put countme (byte 0)))
        _ (.rewind countme)
        _ (dotimes [_ 100] (.put countme (byte 1)))
        _ (.rewind countme)]
    (call-reply-callback rcb vptr countme 1)
    (is (= @check 100))))

(deftest test-splitint
  (let [splitintval (byval splitint com.sun.jna.Structure/ALIGN_NONE)
        splitintref (byref splitint com.sun.jna.Structure/ALIGN_NONE)]
    ;; Tell JNA which part of the union to use
    (.setType splitintval Integer/TYPE)
    ;; Set the integer part of the union to 66000. This puts the decimal
    ;; number 464 in the low 16 bits and the number 1 in the high 16 bits.
    (set! (.theint splitintval) 66000)
    (.setType splitintref Integer/TYPE)
    (set! (.theint splitintref) 66000)
    (is (= 66000 (.theint splitintval)))
    ;; Call a C function that adds one to the int part of the union
    (let [ret (addOneToUnionIntByValue splitintval)

          _ (.readField ret "packed") ;; force read. Should this really be necessary?
          ;; _ (.setType ret (typeof packed :val)) ;; I wish this forced a read

          ;; Read the union as a struct of two shorts.
          s1 (.s1 (.packed ret))
          s2 (.s2 (.packed ret))]
      ;; After the call to the C function, the first (low) short should
      ;; have the value 465 and the second (high) should have the value 1
      (is (= 465 s1))
      (is (= 1 s2)))
    ;; Same test as above but pass the union by reference
    (is (= 66000 (.theint splitintref)))
    (addOneToUnionIntByReference splitintref)
    (.readField splitintref "packed") ;; force read
    ;; (.setType splitintref (typeof packed :val))
    (is (= 465 (.s1 (.packed splitintref))))
    (is (= 1 (.s2 (.packed splitintref))))))
