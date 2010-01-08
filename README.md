# clj-native

Clojure library for generating JNA direct mappings

## Usage

  (use ['clj-native.core :only defclib])
  (import 'com.sun.jna.NativeLong)

  (defclib
    m
    (sin [double] double)
    (cos [double] double))

  (sin 1) ;; => 0.8414709848078965

  ;; Typed pointers are represented as nio buffers.
  ;; It's also possible to use java arrays with jna but I chose
  ;; nio buffers because they are faster (less copying) and
  ;; safer (direct buffers are not moved by GC so it's safe for
  ;; native code to hold on to their pointers).

  (defclib
    c
    (malloc [size_t] void*)
    (free [void*])
    (memset [byte* int size_t] void*))

  ;; At the moment NativeLongs have to be explicitly constructed.
  ;; I have a plan to get rid of that and other such type issues.

  (def mem (malloc (NativeLong. 100)))
  (def view (.getByteBuffer mem 0 100))
  (memset view 10 (NativeLong. 100))
  (.get view 20) ;; => 10
  (def int-view (.asIntBuffer view)) ;; 25 ints (100/4)
  (memset view 1 (NativeLong. 100))
  (.get int-view 20) ;; => 16843009 (four bytes, each with their lsb set)
  (free mem) ;; => nil
  (.get int-vew 0) ;; => 0

## Installation



## License

    Eclipse public license version 1.0. See epl-v10.html for details.
