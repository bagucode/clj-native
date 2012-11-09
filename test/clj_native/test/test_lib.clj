(ns clj-native.test.test-lib
  (:use [clj-native.direct :only [defclib loadlib typeof]]
        [clj-native.structs :only [byref byval]]
        [clj-native.callbacks :only [callback]]
        [clojure.test]))

(defclib test_lib
  (:libname "test_lib")
  (:functions
   (mul [int int] int)))

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
