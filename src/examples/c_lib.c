#ifdef _WIN32

#define EXPORT __declspec(dllexport)
#define CALLCONV __stdcall

#else

#define EXPORT
#define CALLCONV

#endif

/*
  #include <stdio.h>
*/
#include <wchar.h>

/* globals */
EXPORT int globalInt = 10;
EXPORT const char* globalString = "Hello Globals!";

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

#ifdef _WIN32
#pragma pack(push, 1)
typedef struct packed_struct {
  short s1;
  short s2;
} packed;
#pragma pack(pop)
#else // assumes gcc
typedef struct packed_struct {
  short s1;
  short s2;
} packed __attribute__ ((aligned (1)));
#endif

typedef union splitint_union {
  int the_int;
  packed p;
} splitint;

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
EXPORT int CALLCONV add(int x, int y) {
  return x + y;
}

typedef int (*add_callback)(int, int);

EXPORT int CALLCONV call_add_callback(add_callback cb, int x, int y) {
  return cb(x, y);
}

EXPORT struct2 CALLCONV addOneToStructTwoByValue(struct2 s2)
{
  struct2 ret;
  ret.ll = s2.ll + 1;
  ret.s1ByValue.x = s2.s1ByValue.x + 1;
  ret.s1ByValue.y = s2.s1ByValue.y + 1;
  ret.s1ByValue.k = s2.s1ByValue.k + 1;
  return ret;
}

EXPORT struct1 CALLCONV addOneToStructByValue(struct1 s1)
{
  struct1 ret;
  ret.x = s1.x + 1;
  ret.y = s1.y + 1;
  ret.k = s1.k + 1;
  return ret;
}

EXPORT void CALLCONV addOneToStructByReference(struct1* s1)
{
  ++s1->x;
  ++s1->y;
  ++s1->k;
}

EXPORT const char* CALLCONV returnsConstantString()
{
  return "This string should be safe to read as const char*";
}

EXPORT const wchar_t* CALLCONV returnsConstantWString()
{
  return L"This string should be safe to read as const wchar_t*";
}

EXPORT splitint CALLCONV addOneToUnionIntByValue(splitint s1)
{
  splitint ret;
  ret.the_int = s1.the_int + 1;
  return ret;
}

EXPORT void CALLCONV addOneToUnionIntByReference(splitint* s1)
{
  ++s1->the_int;
}

