/*
  #include <stdio.h>
*/

/* globals */
int globalInt = 10;
const char* globalString = "Hello Globals!";

/* structures and unions */
typedef struct struct1_struct {
  int x;
  char y; /* alignment issues? */
  float k;
} struct1;

typedef struct struct2_struct {
  long long ll;
  struct1 s1ByValue;
} struct2;

typedef union either_union {
  struct1* s1;
  struct2* s2;
} either;

/* functions and callbacks */
int add(int x, int y) {
  return x + y;
}

typedef int (*add_callback)(int, int);

int call_add_callback(add_callback cb, int x, int y) {
  return cb(x, y);
}


