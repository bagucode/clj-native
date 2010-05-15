(ns clj-native.utils
  (:refer-clojure :exclude [make-array])
  (:import [com.sun.jna Pointer Memory]
           [java.lang.ref ReferenceQueue PhantomReference]))

(defn ptr-array
  "Creates a pointer to an array of pointers in native
  memory that may be passed to functions taking a jna
  Pointer (void*) as an argument."
  [null-terminate? & pointers]
  (let [n (inc (count pointers))
        size (* Pointer/SIZE n)
        mem #^Pointer (Memory. size)
        ptrseq (if null-terminate?
                 (conj (vec pointers) (Pointer. 0))
                 pointers)]
    (.write mem (long 0) #^"[Lcom.sun.jna.Pointer;"
            (into-array Pointer ptrseq) (int 0) (int n))
    mem))

(defn make-phantom-queue
  []
  {:queue (ReferenceQueue.)
   :user-data (atom {})})

(defn make-phantom
  [target queue user-data]
  (let [phantom (PhantomReference. target (:queue queue))]
    (swap! (:user-data queue) assoc phantom user-data)
    phantom))

(defn remove-phantom-data
  [queue phantom]
  (let [data (@(:user-data queue) phantom)]
    (swap! (:user-data queue) dissoc phantom)
    data))

(defn next-phantom-data-from-queue
  [queue]
  (remove-phantom-data queue (.poll (:queue queue))))
