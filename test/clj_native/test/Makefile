#!/usr/bin/env make

# only tested on macos so far

libtest_lib.dylib: test_lib.o
	gcc -dynamiclib -current_version 1.0  test_lib.o  -o libtest_lib.dylib
test_lib.o: test_lib.c
	gcc -c -fPIC test_lib.c
clean:
	rm -f libtest_lib.dylib test_lib.o
