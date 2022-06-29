#ifndef RUNTIME
#define RUNTIME

#include <stdint.h>

int64_t *boat_malloc(int64_t size);
int64_t *boat_alloc_array(int64_t size);
void boat_assert_not_null(int8_t *ptr);
void boat_assert_array_length(int64_t *array, int64_t ind);
int64_t *array_of_string(char *str);
char *string_of_array(int64_t *arr);
int64_t length_of_string(char *str);
char *string_of_int(int64_t i);
char *string_cat(char *l, char *r);
void print_string(char *str);
void print_int(int64_t i);
void print_bool(int64_t i);

#endif
