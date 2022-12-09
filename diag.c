/* diag.c - Diagnostic and error reporting facilities */
#include "diag.h"
#include "ast.h"
#include "context.h"
#include "lexer.h"
#include "parser.h"
#include "util.h"
#include <assert.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

static void cc_diag_print_diag(cc_context* ctx, cc_diag_info info,
    const char* severity, const char* fmt, va_list args)
{
    fprintf(stderr, "%s: %s:%zu: ", severity, info.filename, info.line);
    vfprintf(stderr, fmt, args);

    FILE* fp = fopen(info.filename, "rt");
    if (fp != NULL) {
        char tmpbuf[80];
        size_t line = 0;
        while (fgets(tmpbuf, sizeof(tmpbuf), fp) != NULL
            && line != info.line - 1) {
            size_t len = strlen(tmpbuf);
            if (tmpbuf[len - 1] == '\n')
                line++;
        }
        fprintf(stderr, "\n%s", tmpbuf);
        for (size_t i = 0; i < info.column; i++)
            fputc(' ', stderr);
        fprintf(stderr, "^\n");
        fclose(fp);
    } else {
        fprintf(stderr, "\n<unable to open file>");
    }

    abort();
}

/* Diagnostics */
static void cc_diag_common(
    cc_context* ctx, const char* severity, const char* fmt, va_list args)
{
    if (ctx->stage == STAGE_PARSER) {
        const cc_lexer_token* tok = &ctx->tokens[ctx->c_token];
        cc_diag_print_diag(ctx, tok->info, severity, fmt, args);
    } else if (ctx->stage == STAGE_LEXER) {
        if (ctx->n_diag_infos) {
            cc_diag_print_diag(ctx, ctx->diag_infos[ctx->n_diag_infos - 1],
                severity, fmt, args);
        } else {
            fprintf(stderr, "<lexer>\n");
        }
    } else if (ctx->stage == STAGE_AST) {
        if (ctx->diag_node != NULL) {
            cc_diag_print_diag(ctx, ctx->diag_node->info, severity, fmt, args);
        } else {
            fprintf(stderr, "<ast>\n");
        }
    }
}

static int err_cnt = 0;
void cc_diag_error(cc_context* ctx, const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    cc_diag_common(ctx, "error", fmt, args);
    va_end(args);
    err_cnt++;

    abort();
#if 0
    if (err_cnt > 10)
        exit(EXIT_FAILURE);
#endif
}

void cc_diag_warning(cc_context* ctx, const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    cc_diag_common(ctx, "warning", fmt, args);
    va_end(args);
}

void cc_diag_add_info(cc_context* ctx, cc_diag_info info)
{
    assert(info.filename != NULL);
    ctx->diag_infos = cc_realloc(
        ctx->diag_infos, sizeof(cc_diag_info) * (ctx->n_diag_infos + 1));
    ctx->diag_infos[ctx->n_diag_infos++] = info;
}

void cc_diag_update_current(cc_context* ctx, cc_diag_info new_info)
{
    if (ctx->n_diag_infos)
        ctx->diag_infos[ctx->n_diag_infos - 1] = new_info;
}

void cc_diag_increment_linenum(cc_context* ctx)
{
    if (ctx->n_diag_infos)
        ctx->diag_infos[ctx->n_diag_infos - 1].line++;
}

void cc_diag_return_to_file(cc_context* ctx, cc_diag_info new_info)
{
    for (size_t i = 0; i < ctx->n_diag_infos; i++) {
        cc_diag_info* info = &ctx->diag_infos[i];
        if (!strcmp(info->filename, new_info.filename)) {
            info->line = new_info.line;
            cc_free(new_info.filename); /* Discard new_info's filename */
            i++;
            /* Cutoff includes after returning to this one */
            for (size_t j = i; j < ctx->n_diag_infos; j++) {
                cc_diag_info* info = &ctx->diag_infos[j];
                cc_free(info->filename);
            }
            ctx->n_diag_infos = i;
            return;
        }
    }
    cc_diag_add_info(ctx, new_info);
}
