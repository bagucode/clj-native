#include <stdlib.h>
#include <stdbool.h>

// simple test
int mul(int x, int y) {
    return x * y;
}

// boolean tests
bool and2(bool x, bool y) {
    return x & y;
}
void and3(bool x, bool y, bool *z) {
    *z = x & y;
}

// boolean test + allocate a buffer
// get closer to https://github.com/supercollider/supercollider/blob/master/server/scsynth/SC_World.cpp World_CopySndBuf
struct NBuf {
    int n;
    int *buf;
};
typedef struct NBuf NBuf;
void and3_buf(bool x, bool y, bool *z, int n, NBuf *pb) {
    // boolean test
    *z = x & y;
    // allocate buffer
    pb->n = n;
    pb->buf = (int *)malloc(n*sizeof(int));
    // initialize buffer
    int *p = pb->buf;
    int i;
    for(i = 0; i < n; i++) {
        *p++ = i;
    }
}
