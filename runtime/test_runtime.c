#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void assert_int_equal(int64_t i, int64_t j) {
    if (i != j) {
        printf("Assertion failed: %ld != %ld\n", i, j);
    } else {
        printf("Assert succeeded: %ld == %ld\n", i, j);
    }
}
