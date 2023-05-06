/* util.c - C utility functions for upplementing a C90 libc. */
#include "util.h"
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef OCC_MEMSTATS
static struct cc_alloc_context {
    struct cc_alloc_ptr {
        void* p;
        size_t size;
        bool is_string;
    } * ptrs;
    size_t n_ptrs;
    bool active;
    bool is_string;

    size_t total_normal;
    size_t total_strings;
} alloc_ctx;
#endif

void cc_alloc_init(bool track)
{
#ifdef OCC_MEMSTATS
    alloc_ctx.active = track;
#endif
    atexit(cc_alloc_deinit);
}

void cc_alloc_deinit(void)
{
#ifdef OCC_MEMSTATS
    size_t total = 0;
    size_t total_string = 0;
    size_t n_strings = 0;
    size_t i;
    for (i = 0; i < alloc_ctx.n_ptrs; i++) {
        total += alloc_ctx.ptrs[i].size;
        if (alloc_ctx.ptrs[i].is_string) {
            total_string += alloc_ctx.ptrs[i].size;
            n_strings++;
        }
    }
    printf("Final used memory: %.2f KB (%u bytes accross %u objects)\n",
        (float)total / 1000.f, (unsigned int)total,
        (unsigned int)alloc_ctx.n_ptrs);
    printf("%.2f%% was used for strings: %.2f KB (%u bytes accross %u "
           "strings)\n",
        100.f * ((float)total_string / (float)total), total_string / 1000.f,
        (unsigned int)total_string, (unsigned int)n_strings);
    printf("Total: %.2f KB (%.2f KB for strings)\n",
        (float)alloc_ctx.total_normal / 1000.f,
        alloc_ctx.total_strings / 1000.f);

    for (i = 0; i < alloc_ctx.n_ptrs; i++)
        free(alloc_ctx.ptrs[i].p);
    free(alloc_ctx.ptrs);
    alloc_ctx.ptrs = NULL;
    alloc_ctx.n_ptrs = 0;
#endif
}

#ifdef OCC_MEMSTATS
static void cc_alloc_add(void* p, size_t size)
{
    if (!alloc_ctx.active)
        return;
    alloc_ctx.total_normal += size;
    alloc_ctx.ptrs = realloc(
        alloc_ctx.ptrs, sizeof(*alloc_ctx.ptrs) * (alloc_ctx.n_ptrs + 1));
    if (alloc_ctx.ptr == NULL)
        cc_abort(__FILE__, __LINE__);
    alloc_ctx.ptrs[alloc_ctx.n_ptrs].p = p;
    alloc_ctx.ptrs[alloc_ctx.n_ptrs].size = size;
    alloc_ctx.ptrs[alloc_ctx.n_ptrs].is_string = alloc_ctx.is_string;
    ++alloc_ctx.n_ptrs;
}

static void cc_alloc_remove(void* p)
{
    size_t i;
    if (!alloc_ctx.active)
        return;
    for (i = 0; i < alloc_ctx.n_ptrs; i++) {
        if (alloc_ctx.ptrs[i].p == p) {
            assert(alloc_ctx.ptrs[i].is_string == alloc_ctx.is_string);
            if (i + 1 < alloc_ctx.n_ptrs)
                memmove(&alloc_ctx.ptrs[i], &alloc_ctx.ptrs[i + 1],
                    sizeof(*alloc_ctx.ptrs) * (alloc_ctx.n_ptrs - i - 1));
            alloc_ctx.n_ptrs--;
            return;
        }
    }
}
#endif

void* cc_malloc(size_t size)
{
#ifdef OCC_MEMSTATS
    void* p = malloc(size);
    cc_alloc_add(p, size);
    if (p == NULL)
        cc_abort(__FILE__, __LINE__);
    return p;
#else
    return malloc(size);
#endif
}

void* cc_zalloc(size_t size)
{
#ifdef OCC_MEMSTATS
    void* p = malloc(size);
    cc_alloc_add(p, size);
    if (p == NULL)
        cc_abort(__FILE__, __LINE__);
    memset(p, 0, size);
    return p;
#else
    void *p = calloc(size, 1);
    if (p == NULL)
        cc_abort(__FILE__, __LINE__);
    return p;
#endif
}

void* cc_realloc(void* p, size_t size)
{
#ifdef OCC_MEMSTATS
    void* np;
    if (p == NULL) {
        np = cc_malloc(size);
        if (np == NULL)
            cc_abort(__FILE__, __LINE__);
        return np;
    }

    cc_alloc_remove(p);
    np = realloc(p, size);
    if (np == NULL)
        cc_abort(__FILE__, __LINE__);

    cc_alloc_add(np, size);
    return np;
#else
    void* np = realloc(p, size);
    if (np == NULL)
        cc_abort(__FILE__, __LINE__);
    return np;
#endif
}

void cc_free(void* p)
{
#ifdef OCC_MEMSTATS
    if (p == NULL)
        return;
    cc_alloc_remove(p);
#endif
    free(p);
}

static char* str_pool = NULL;
static size_t str_pool_size = 1;

const char* cc_strview(cc_string_key key)
{
    assert((size_t)key < str_pool_size);
    return &str_pool[key];
}

cc_string_key cc_strndup(const char* s, size_t n)
{
    size_t start = str_pool_size;
    size_t i;

    assert(s != NULL);
    n = n > strlen(s) ? strlen(s) : n; /* Limit to strlen */

    /* If the string is a nil string, we can use the properties of our
       string pool to our advantage, since the first two characters are
       always zero, for alignment reasons. */
    if (!n) {
        /* Second character in the array will always be a '\0' */
        return (cc_string_key)1;
    }

    for (i = 2; i < str_pool_size;) {
        size_t len = strlen(cc_strview(i));
        if (len == n && !memcmp(s, cc_strview(i), len))
            return (cc_string_key)i;
        i += strlen(cc_strview(i)) + 1;
    }

    /* String hasn't been added to the string pool yet! */
#ifdef OCC_MEMSTATS
    alloc_ctx.is_string = true;
    alloc_ctx.total_strings += n + 1;
#endif
    assert(str_pool_size == start);
    str_pool_size += n + 1;
    str_pool = cc_realloc(str_pool, str_pool_size);
    memcpy(&str_pool[start], s, n);
    str_pool[start + n] = '\0';
#ifdef OCC_MEMSTATS
    alloc_ctx.is_string = false;
#endif
    assert(start < USHRT_MAX);
    return (cc_string_key)start;
}

cc_string_key cc_strdup(const char* s) { return cc_strndup(s, strlen(s)); }

cc_string_key cc_strdupcat(const char* s1, const char* s2)
{
    size_t len[2] = { strlen(s1), strlen(s2) };
    char* s = malloc(len[0] + len[1] + 1);
    cc_string_key key;

    if (s == NULL)
        cc_abort(__FILE__, __LINE__);

    memcpy(s, s1, len[0]);
    memcpy(s + len[0], s2, len[1]);
    s[len[0] + len[1]] = '\0';
    key = cc_strdup(s);
    free(s);
    return key;
}

void cc_strfree(cc_string_key s)
{
    /* ... Nothing is done! */
}

void cc_abort_1(const char *filename, size_t line)
{
    fprintf(stderr, "Program aborted! :(\n");
    fprintf(stderr, "File: %s - Line: %u\n", filename,  (unsigned int) line);
#ifdef __WIN32__
    exit(EXIT_FAILURE); /* Do this so windows doesn't improperly detect rc */
#endif
}
