#!/usr/bin/env make
#
# make clean
# make BUILD=debug
# -or-
# make BUILD=release
#

# FIXME only tested on macos so far

ifeq ($(BUILD),debug)   
# "Debug" build - no optimization, and debugging symbols
CFLAGS += -O0 -g
else
# "Release" build - optimization, and no debug symbols
CFLAGS += -O2 -s -DNDEBUG
endif

all: libtest_lib.dylib

test_lib.o: test_lib.c
	gcc $(CFLAGS) -c -fPIC test_lib.c

libtest_lib.dylib: test_lib.o
	gcc -dynamiclib -current_version 1.0  test_lib.o  -o libtest_lib.dylib

clean:
	rm -f libtest_lib.dylib test_lib.o
