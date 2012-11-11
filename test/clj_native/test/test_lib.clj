(ns clj-native.test.test-lib
  (:use [clj-native.direct :only [defclib loadlib typeof]]
        [clj-native.structs :only [byref byval]]
        [clj-native.callbacks :only [callback]]
        [clojure.test]))

(defclib test_lib
  (:libname "test_lib")
  (:structs
   (n-buf
    :n int
    :buf void*))
  (:functions
   (mul [int int] int)
   (and2 [byte byte] byte)
   (and3 [byte byte byte*] void)
   (and3_buf [byte byte byte* int n-buf*] void)))

(println "NOTE: Testing assumes a built test_lib library")
(System/setProperty "jna.library.path" "./test/clj_native/test")
(loadlib test_lib)

(deftest test-mul
  (are [a b z] (= z (mul a b))
       1 1 (* 1 1)
       100 -10 (* 100 -10)
       65535 10 (* 65535 10)
       10000000 10 (* 10000000 10)
       ))

(deftest test-and2
  (are [a b z] (= z (and2 a b))
       0 0 0
       0 1 0
       1 0 0
       1 1 1
       ))

(deftest test-and3
  (are [a b z] (= z (let [r (java.nio.ByteBuffer/allocate 1)]
                      (and3 a b r)
                      (.get r 0)))
       0 0 0
       0 1 0
       1 0 0
       1 1 1
       ))

;; This is trying to be a close match for
;; overtone/src/overtone/sc/machinery/server/native.clj:scsynth-get-buffer-data
(deftest test-and3-buf
  (are [a b z n] (= [(apply vector (range n)) n z]
                    (let [r (java.nio.ByteBuffer/allocate 1)
                          nbr (byref n-buf)]
                      (and3_buf a b r n nbr)
                      (vector
                       (apply vector (for [i (range n)] (.getInt (.buf nbr) (* 4 i))))
                       (.n nbr)
                       (.get r 0))))
       1 1 1 4
       0 0 0 3
       ))

