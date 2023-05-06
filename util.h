#ifndef UTIL_H
#define UTIL_H 1

#include <ctype.h>
#include <stdbool.h>
#include <stddef.h>

#define ISSPACE(c) (c == ' ' || c == '\t' || c == '\n' || c == '\r')
#define ISSTARTIDENT(c) (isalpha(c) || c == '_' || c == '$')
#define ISIDENT(c) (isalnum(c) || c == '_' || c == '$')
#define ISODIGIT(c)                                                            \
    (c == '0' || c == '1' || c == '2' || c == '3' || c == '4' || c == '5'      \
        || c == '6' || c == '7')

#define ARRAY_SIZE(a) (sizeof(a) / sizeof(a[0]))

typedef unsigned int cc_string_key;

void cc_alloc_init(bool track);
void cc_alloc_deinit(void);
void* cc_malloc(size_t size);
void* cc_zalloc(size_t size);
void* cc_realloc(void* p, size_t size);
void cc_free(void* p);
const char* cc_strview(cc_string_key key);
cc_string_key cc_strndup(const char* s, size_t n);
cc_string_key cc_strdupcat(const char* s1, const char* s2);
cc_string_key cc_strdup(const char* s);
void cc_strfree(cc_string_key s);
#define cc_realloc_array(arr, n) cc_realloc(arr, sizeof(*(arr)) * (n))

#ifdef OCC_TRACE_TIME
/* TODO: Timers */
#endif

typedef unsigned char cc_alnum_char;

#endif
