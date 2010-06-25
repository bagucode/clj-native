;;   Copyright (c) Markus Gustavsson. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clj-native.direct-util
  (:use [clojure.contrib.seq-utils :only [indexed]])
  (:import [clojure.asm Type]
           [java.nio ByteBuffer IntBuffer CharBuffer
            ShortBuffer LongBuffer FloatBuffer DoubleBuffer]
           [com.sun.jna NativeLong Pointer WString]))

;;; ***************************************************************************
;;;
;;; -----===== Code loading =====-----
;;;
;;; ***************************************************************************

(def #^{:private true :tag clojure.lang.DynamicClassLoader}
     loader (clojure.lang.DynamicClassLoader.))

(defn load-code
  "Loads bytecode (a class definition) using the root DynamicClassLoader"
  [#^String classname #^"[B" bytecode src]
  (.defineClass loader classname bytecode src))

;;; ***************************************************************************
;;;
;;; -----===== Types =====-----
;;;
;;; ***************************************************************************

(defn native-long
  "Returns the java type corresponding to
  the bitsize of a native long."
  []
  (condp = NativeLong/SIZE
    4 Integer/TYPE
    8 Long/TYPE))

(defn native-long-buffer
  "Returns the type of nio buffer appropriate
  to store arrays of native longs."
  []
  (condp = NativeLong/SIZE
    4 IntBuffer
    8 LongBuffer))

(def type-map
     {'char Byte/TYPE
      'wchar_t Character/TYPE
      'byte Byte/TYPE
      'short Short/TYPE
      'int Integer/TYPE
      'enum Integer/TYPE
      'BOOL Boolean/TYPE
      'bool Boolean/TYPE
      'size_t native-long
      'long native-long
      'longlong Long/TYPE
      '__int64 Long/TYPE
      'i8 Byte/TYPE
      'i16 Short/TYPE
      'i32 Integer/TYPE
      'i64 Long/TYPE
      'float Float/TYPE
      'double Double/TYPE
      'void Void/TYPE
      'void* Pointer
      'byte* ByteBuffer
      'char* ByteBuffer
      'constchar* String
      'wchar_t* CharBuffer
      'constwchar_t* WString
      'short* ShortBuffer
      'int* IntBuffer
      'long* native-long-buffer
      'size_t* native-long-buffer
      'longlong* LongBuffer
      '__int64* LongBuffer
      'i8* ByteBuffer
      'i16* ShortBuffer
      'i32* IntBuffer
      'i64* LongBuffer
      'float* FloatBuffer
      'double* DoubleBuffer})

(defn resolve-type
  "Get type from the type map or return input
  as is if it's a user defined type"
  [t]
  (or (type-map t) t))

(defn user-type-class
  [t]
  (cond
   ;; struct by reference
   (and (map? t) (= :struct (:kind t))
        (.endsWith (name (:name t)) "*"))
   (str (:classname t) "$ByReference")
   ;; struct by value
   (and (map? t) (= :struct (:kind t)))
   (str (:classname t) "$ByValue")
   ;; callback
   (map? t) (:classname t)))

(defn descriptor
  "Get internal name type descriptor for t"
  [t]
  (let [replace-dots (fn [#^String s] (.replaceAll s "\\." "/"))]
    (cond
     (class? t) (Type/getDescriptor t)
     (fn? t) (Type/getDescriptor (t))
     (map? t) (str "L" (replace-dots (user-type-class t)) ";"))))

(defn check-type
  ([t]
     (check-type t nil))
  ([t user-types]
     (let [resolved (if (type-map t)
                      t
                      (and user-types (get @user-types t)))]
       (when-not resolved
         (throw (Exception. (str "Unknown type: " t))))
       resolved)))

;;; ***************************************************************************
;;;
;;; -----===== Misc. =====-----
;;;
;;; ***************************************************************************

(defn argnames
  "Create unique names for a seq of argument types"
  [argtypes]
  (for [[i t] (indexed argtypes)]
    (symbol (str (if (map? t) (:name t) t) i))))

