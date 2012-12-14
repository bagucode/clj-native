(ns clj-native.test.test-lib
  (:use [clj-native.direct :only [defclib loadlib typeof]]
        [clj-native.structs :only [byref byval]]
        [clj-native.callbacks :only [callback]]
        [clojure.test]))

;; added test cases to help resolve an issue that was plaguing the Overtone project.
;; See discussion here: https://groups.google.com/forum/?fromgroups=#!topic/overtone/wrlzi3dNhr0
;; Feel free to add new functions/tests as issues pop up.

(defclib test_lib
  (:libname "test_lib")
  (:structs
   (n-buf
    :n int
    :buf void*)
   (point
    :x int
    :y int
    :name constchar*))
  (:functions
   (mul [int int] int)
   (and2 [byte byte] byte)
   (and3 [byte byte byte*] void)
   (and3_buf [byte byte byte* int n-buf*] void)
   (static_point [int int] point*)))

(println "NOTE: Testing assumes a built test/clj_native/test/test_lib library")
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

;; This was motivated by the Pm_GetDeviceInfo function from Portmidi,
;; which has the signature:
;;
;; PMEXPORT const PmDeviceInfo* Pm_GetDeviceInfo( PmDeviceID id );
;;
;; The memory pointed to is owned by Portmidi and is not to be
;; freed. A static variable has this property, so is a similar
;; use-case.
(deftest test-point
  (are [x y x' y'] (let [thepoint (static_point x y)]
                     (and
                      (= x' (.x thepoint))
                      (= y' (.y thepoint))
                      (= "foo" (.name thepoint))))
       1 2 1 2
       -1 -4 -1 -4))

