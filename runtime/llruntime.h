#ifndef LL_RUNTIME
#define LL_RUNTIME

#include <stdint.h>

void *ll_malloc(int64_t n, int64_t size);
int64_t ll_strlen(int8_t *s);
int8_t *ll_strncopy(int8_t *dst, int8_t *src, int64_t i);
void ll_puts(int8_t *s);
int64_t ll_atol(int8_t *s);
int64_t ll_ltoa(int64_t i, int8_t *dst);

#endif
