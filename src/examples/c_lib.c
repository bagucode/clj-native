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

/* structures with circular references */
typedef struct circle2_struct circle2;

typedef struct circle1_struct {
  circle2* c2;
} circle1;

struct circle2_struct {
  circle1* c1;
};

/* structure with reference to own type */
typedef struct list_struct list;

struct list_struct {
  void* data;
  list* next;
};

/* functions and callbacks */
int add(int x, int y) {
  return x + y;
}

typedef int (*add_callback)(int, int);

int call_add_callback(add_callback cb, int x, int y) {
  return cb(x, y);
}

struct1 addOneToStructByValue(struct1 s1)
{
  struct1 ret;
  ret.x = s1.x + 1;
  ret.y = s1.y + 1;
  ret.k = s1.k + 1;
  return ret;
}

void addOneToStructByReference(struct1* s1)
{
  ++s1->x;
  ++s1->y;
  ++s1->k;
}
