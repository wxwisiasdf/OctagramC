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
} g_alloc_ctx;
#endif

void cc_alloc_init(bool track)
{
#ifdef OCC_MEMSTATS
    g_alloc_ctx.active = track;
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
    for (i = 0; i < g_alloc_ctx.n_ptrs; i++) {
        total += g_alloc_ctx.ptrs[i].size;
        if (g_alloc_ctx.ptrs[i].is_string) {
            total_string += g_alloc_ctx.ptrs[i].size;
            n_strings++;
        }
    }
    printf("Final used memory: %.2f KB (%u bytes accross %u objects)\n",
        (float)total / 1000.f, (unsigned int)total,
        (unsigned int)g_alloc_ctx.n_ptrs);
    printf("%.2f%% was used for strings: %.2f KB (%u bytes accross %u "
           "strings)\n",
        100.f * ((float)total_string / (float)total), total_string / 1000.f,
        (unsigned int)total_string, (unsigned int)n_strings);
    printf("Total: %.2f KB (%.2f KB for strings)\n",
        (float)g_alloc_ctx.total_normal / 1000.f,
        g_alloc_ctx.total_strings / 1000.f);

    for (i = 0; i < g_alloc_ctx.n_ptrs; i++)
        free(g_alloc_ctx.ptrs[i].p);
    free(g_alloc_ctx.ptrs);
    g_alloc_ctx.ptrs = NULL;
    g_alloc_ctx.n_ptrs = 0;
#endif
}

#ifdef OCC_MEMSTATS
static void cc_alloc_add(void* p, size_t size)
{
    if (!g_alloc_ctx.active)
        return;
    g_alloc_ctx.total_normal += size;
    g_alloc_ctx.ptrs = realloc(
        g_alloc_ctx.ptrs, sizeof(*g_alloc_ctx.ptrs) * (g_alloc_ctx.n_ptrs + 1));
    g_alloc_ctx.ptrs[g_alloc_ctx.n_ptrs].p = p;
    g_alloc_ctx.ptrs[g_alloc_ctx.n_ptrs].size = size;
    g_alloc_ctx.ptrs[g_alloc_ctx.n_ptrs].is_string = g_alloc_ctx.is_string;
    ++g_alloc_ctx.n_ptrs;
}

static void cc_alloc_remove(void* p)
{
    size_t i;
    if (!g_alloc_ctx.active)
        return;
    for (i = 0; i < g_alloc_ctx.n_ptrs; i++) {
        if (g_alloc_ctx.ptrs[i].p == p) {
            assert(g_alloc_ctx.ptrs[i].is_string == g_alloc_ctx.is_string);
            if (i + 1 < g_alloc_ctx.n_ptrs)
                memmove(&g_alloc_ctx.ptrs[i], &g_alloc_ctx.ptrs[i + 1],
                    sizeof(*g_alloc_ctx.ptrs) * (g_alloc_ctx.n_ptrs - i - 1));
            g_alloc_ctx.n_ptrs--;
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
    if (p == NULL) {
        fprintf(stderr, "Out of memory");
        cc_alloc_deinit();
    }
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
    if (p == NULL) {
        fprintf(stderr, "Out of memory");
        cc_alloc_deinit();
    }
    memset(p, 0, size);
    return p;
#else
    return calloc(size, 1);
#endif
}

void* cc_realloc(void* p, size_t size)
{
#ifdef OCC_MEMSTATS
    void* np;
    if (p == NULL)
        return cc_malloc(size);
    cc_alloc_remove(p);
    np = realloc(p, size);
    cc_alloc_add(np, size);
    if (np == NULL) {
        fprintf(stderr, "Out of memory");
        cc_alloc_deinit();
    }
    return np;
#else
    return realloc(p, size);
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

#define STRING_HASH_SLOTS 1024
static cc_string_key str_hash_slots[STRING_HASH_SLOTS] = {0};
static char *str_pool = NULL;
static size_t str_pool_size = 1;

static unsigned short cc_hash_sdbm(const char *s, size_t n) {
    const unsigned char *us = (const unsigned char *)s;
    unsigned long hash = 0;
    while(n--)
        hash = *us++ + (hash << 6) + (hash << 16) - hash;
    return hash % STRING_HASH_SLOTS;
}

const char *cc_strview(cc_string_key key) {
    assert((size_t)key < str_pool_size);
    return &str_pool[key];
}

cc_string_key cc_strndup(const char* s, size_t n)
{
    unsigned short hash;
    assert(s != NULL);
    n = n > strlen(s) ? strlen(s) : n; /* Limit to strlen */
    hash = cc_hash_sdbm(s, n);

    if (str_hash_slots[hash] == 0) {
        /* String hasn't been added to the string pool yet! */
        size_t start = str_pool_size;
        str_pool_size += n + 1;
#ifdef OCC_MEMSTATS
        g_alloc_ctx.is_string = true;
        g_alloc_ctx.total_strings += n + 1;
#endif
        str_pool = cc_realloc(str_pool, str_pool_size);
#ifdef OCC_MEMSTATS
        g_alloc_ctx.is_string = false;
#endif
        memcpy(&str_pool[start], s, n);
        str_pool[start + n] = '\0';
        str_hash_slots[hash] = start;
        return start;
    }
    assert(!strncmp(&str_pool[str_hash_slots[hash]], s, n));
    return str_hash_slots[hash];
}

cc_string_key cc_strdup(const char* s)
{
    return cc_strndup(s, strlen(s));
}

cc_string_key cc_strdupcat(const char* s1, const char* s2)
{
    size_t len[2] = { strlen(s1), strlen(s2) };
    char *s = malloc(len[0] + len[1] + 1);
    cc_string_key key;
    memcpy(s, s1, len[0]);
    memcpy(s + len[0], s2, len[1]);
    s[len[0] + len[1]] = '\0';
    key = cc_strdup(s);
    free(s);
    return key;
}

void cc_strfree(cc_string_key s)
{
    if (!s)
        return;
    /* Do nothing... */
/*
#ifdef OCC_MEMSTATS
    g_alloc_ctx.is_string = true;
    cc_alloc_remove(s);
    g_alloc_ctx.is_string = false;
#endif
    free(s);
*/
}
