# clj-native

Clojure library for generating JNA direct mappings.

From the doc of defclib:

    -------------------------
    clj-native.core/defclib
    ([lib & body])
    Macro
      Create C library bindings.
      lib is a symbol naming the native library to link eg. 'c' for
      linking against the standard C runtime.
      body is any number of function descriptions of the form:

      (name docstring? clojure-name? [argtypes*] returntype?)

      Where name must match the name of the C function. The only
      other required component is the vector containing the argument
      types, even if the function takes no arguments. If the type
      of the return value is left out, void is assumed.
      A clojure function mapped to a C function that returns void
      will always return nil.
      clojure-name can be used to give the function a different name
      within the clojure runtime, useful eg. for getting rid of annoying
      prefixes used by C libraries to ensure unique names.

      This macro will create clojure functions in the current
      namespace corresponding to the imported C functions. The first
      time these functions are called they will dynamically load
      the C library and then replace themselves with versions that
      simply call their C library namesakes.
      Because of this black magic self-replacement and the loading
      of the native library, it will probably break things horribly
      if any of the functions are called during compile time.
      This can also cause trouble if the functions are used as
      arguments to higher order functions since the replacement
      only modifies the function's var. That means that any reference
      directly to the function (such as a local in another function)
      will not be updated. It is therefore best not to use the
      functions as arguments to higher order functions until after
      they have been called at least once.
      You have been warned.

## Usage

    (use ['clj-native.core :only ['defclib]])

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

    (def mem (malloc 100))
    (def view (.getByteBuffer mem 0 100))
    (memset view 10 100)
    (.get view 20) ;; => 10
    (def int-view (.asIntBuffer view)) ;; 25 ints (100/4)
    (memset view 1 100)
    (.get int-view 20) ;; => 16843009 (four bytes, each with their lsb set)
    (free mem) ;; => nil
    (.get int-view 0) ;; => Undefined. Don't use freed memory ;)

## Caveats

Currenty it is not possible to use char\*\* (String[]), wchar_t\*\* (WString[]),
void\*\* (Pointer[]) or varargs (Object[]) as function arguments since JNA
direct mapping does not support this. If a need arises (I'm thinking mostly
of varargs since the others can be simulated in other ways) I might look at
adding support for non-direct mappings for functions that need these
special argument types.

## License

    Eclipse public license version 1.0. See epl-v10.html for details.
