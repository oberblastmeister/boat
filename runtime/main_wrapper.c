#include "runtime.h"
#include <stdio.h>

extern int64_t program(int64_t argc, int64_t *oargv);

/*
 * Convert the argv array into an Boat array of
 * type string[]
 * Invoke the Boat 'program' entry point after
 * initializing the global variables.
 * Prints the results of the Boat program call
 * to the terminal.
 */
int main(int argc, char *argv[]) {
    int64_t *oargv, i, result;

    oargv = boat_alloc_array(argc);

    /* Copy the string pointers to the correct places. */
    for (i = 0; i < argc; i++) {
        oargv[i + 1] = (int64_t)argv[i];
    }

    /* Call the initialization code. */
    result = program(argc, oargv);
    printf("%ld\n", result);
    return result;
}
