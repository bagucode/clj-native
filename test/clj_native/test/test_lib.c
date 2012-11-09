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
