/* util.c - C utility functions for upplementing a C90 libc. */
#include "util.h"
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

void cc_alloc_init(bool track)
{
    g_alloc_ctx.active = track;
    atexit(cc_alloc_deinit);
}

void cc_alloc_deinit(void)
{
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
    printf("Final used memory: %.2f KB (%zu bytes accross %zu objects)\n",
        (float)total / 1000.f, total, g_alloc_ctx.n_ptrs);
    printf("%.2f%% was used for strings: %.2f KB (%zu bytes accross %zu "
           "strings)\n",
        100.f * ((float)total_string / (float)total), total_string / 1000.f,
        total_string, n_strings);
    printf("Total: %.2f KB (%.2f KB for strings)\n",
        (float)g_alloc_ctx.total_normal / 1000.f,
        g_alloc_ctx.total_strings / 1000.f);

    for (i = 0; i < g_alloc_ctx.n_ptrs; i++)
        free(g_alloc_ctx.ptrs[i].p);
    free(g_alloc_ctx.ptrs);
    g_alloc_ctx.ptrs = NULL;
    g_alloc_ctx.n_ptrs = 0;
}

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
    g_alloc_ctx.n_ptrs++;
}

static void cc_alloc_remove(void* p)
{
    size_t i;
    if (!g_alloc_ctx.active)
        return;
    for (i = 0; i < g_alloc_ctx.n_ptrs; i++) {
        if (g_alloc_ctx.ptrs[i].p == p) {
            memmove(&g_alloc_ctx.ptrs[i], &g_alloc_ctx.ptrs[i + 1],
                sizeof(*g_alloc_ctx.ptrs) * (g_alloc_ctx.n_ptrs - i - 1));
            g_alloc_ctx.n_ptrs--;
            return;
        }
    }
}

void* cc_malloc(size_t size)
{
    void* p = malloc(size);
    cc_alloc_add(p, size);
    if (p == NULL) {
        fprintf(stderr, "Out of memory");
        cc_alloc_deinit();
    }
    return p;
}

void* cc_zalloc(size_t size)
{
    void* p = malloc(size);
    cc_alloc_add(p, size);
    if (p == NULL) {
        fprintf(stderr, "Out of memory");
        cc_alloc_deinit();
    }
    memset(p, 0, size);
    return p;
}

void* cc_realloc(void* p, size_t size)
{
    if (p == NULL)
        return cc_malloc(size);

    cc_alloc_remove(p);
    void* np = realloc(p, size);
    cc_alloc_add(np, size);
    if (np == NULL) {
        fprintf(stderr, "Out of memory");
        cc_alloc_deinit();
    }
    return np;
}

void cc_free(void* p)
{
    if (p == NULL)
        return;
    cc_alloc_remove(p);
    free(p);
}

char* cc_strndup(const char* s, size_t n)
{
    assert(s != NULL);
    n = n > strlen(s) ? strlen(s) : n; /* Limit to strlen */

    g_alloc_ctx.is_string = true;
    g_alloc_ctx.total_strings += n + 1;
    char* ns = cc_malloc(n + 1);
    g_alloc_ctx.is_string = false;

    memcpy(ns, s, n);
    ns[n] = '\0';
    return ns;
}

char* cc_strdup(const char* s)
{
    assert(s != NULL);
    size_t len = strlen(s);

    g_alloc_ctx.is_string = true;
    g_alloc_ctx.total_strings += len + 1;
    char* ns = cc_malloc(len + 1);
    g_alloc_ctx.is_string = false;

    memcpy(ns, s, len);
    ns[len] = '\0';
    return ns;
}
