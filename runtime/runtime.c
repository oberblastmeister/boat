#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// llvmlite internal functions
void *ll_malloc(int64_t n, int64_t size) { return calloc(n, size); }

int64_t ll_strlen(int8_t *s) { return 0; }

int8_t *ll_strncopy(int8_t *dst, int8_t *src, int64_t i) {
    int64_t src_size = ll_strlen(src);
    int64_t dst_size = ll_strlen(dst);
    if (i >= dst_size)
        return dst;
    else
        return (int8_t *)strncpy((char *)dst + i, (char *)src, dst_size - i);
}

void ll_puts(int8_t *s) { puts((char *)s); }

int64_t ll_atol(int8_t *s) { return atol((char *)s); }

int64_t ll_ltoa(int64_t i, int8_t *dst) {
    int64_t size = ll_strlen(dst);
    return snprintf((char *)dst, size, "%ld", (long)i);
}


/* Boat Internal Functions --------------------------------------------------- */

// this just leaks memory for now
int64_t *boat_malloc(int64_t size) {
    return (int64_t *)calloc(size, sizeof(char));
}

int64_t *boat_alloc_array(int64_t size) {
    assert(size >= 0);
    int64_t *arr = (int64_t *)malloc(sizeof(int64_t) * (size + 1));
    arr[0] = size;
    return arr;
}

void boat_assert_not_null(int8_t *ptr) {
    if (ptr == NULL) {
        fprintf(stderr, "Attempted to dereference null pointer");
        exit(1);
    }
}

void boat_assert_array_length(int64_t *array, int64_t ind) {
    if (array == NULL) {
        fprintf(stderr, "Attempted to index null array");
        exit(1);
    } else if (*array <= ind) {
        fprintf(stderr, "Out of bounds index %ld for array length %ld",
                (long)ind, (long)*array);
        exit(1);
    }
}

/* Boat Builtin Functions ---------------------------------------------------- */

int64_t *array_of_string(char *str) {
    int64_t len, i, *arr;

    assert(NULL != str);

    len = strlen(str);
    assert(len >= 0);

    arr = (int64_t *)malloc(sizeof(int64_t) * (len + 1));
    arr[0] = len;
    for (i = 0; i < len; i++) {
        arr[i + 1] = (int64_t)str[i];
    }

    return arr;
}

char *string_of_array(int64_t *arr) {
    int64_t len, i;
    char *str;

    assert(NULL != arr);

    len = arr[0];
    assert(len >= 0);

    str = malloc(sizeof(char) * (len + 1));

    for (i = 0; i < len; i++) {
        str[i] = (char)arr[i + 1];
        assert(0 != str[i]);
    }
    str[len] = 0;

    return str;
}

int64_t length_of_string(char *str) {
    assert(NULL != str);
    return strlen(str);
}

char *string_of_int(int64_t i) {
    static char buf[128];
    static int len;
    len = sprintf(buf, "%ld", (long)i);
    char *str = (char *)malloc(sizeof(char) * (len + 1));
    memcpy(str, buf, len);
    str[len] = 0;
    return (char *)str;
}

char *string_cat(char *l, char *r) {
    size_t ll = strlen(l);
    size_t lr = strlen(r);
    char *new = (char *)malloc(sizeof(char) * (ll + lr + 1));
    memcpy(new, l, ll);
    memcpy(new + ll, r, lr);
    new[ll + lr] = 0;
    return new;
}

void print_string(char *str) {
    assert(NULL != str);
    printf("%s", str);
}

void print_int(int64_t i) { printf("%ld\n", (long)i); }

void print_bool(int64_t i) {
    if (i == 0) {
        printf("false");
    } else {
        printf("true");
    }
}

extern int64_t /* MANGLED_PROGRAM_NAME */(int64_t argc, int64_t *oargv);

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

    result = /* MANGLED_PROGRAM_NAME */(argc, oargv);
    return result;
}
