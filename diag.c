/* diag.c - Diagnostic and error reporting facilities */
#include "diag.h"
#include "ast.h"
#include "context.h"
#include "lexer.h"
#include "parser.h"
#include "ssa.h"
#include "util.h"
#include <assert.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#ifdef ANSI_COLOUR
#undef ANSI_COLOUR
#define ANSI_COLOUR(f) "\x1B[" #f "m"
#else
#define ANSI_COLOUR(f)
#endif

static void cc_diag_print_diag(
    cc_diag_info info, const char* severity, const char* fmt, va_list args)
{
    FILE* fp;
    fprintf(stderr,
        "%s: " ANSI_COLOUR(96) "%s" ANSI_COLOUR(0) ":%u: ", severity,
        cc_strview(info.filename), info.line);
    vfprintf(stderr, fmt, args);

    fp = fopen(cc_strview(info.filename), "r");
    if (fp != NULL) {
        char tmpbuf[80];
        unsigned short line = 0;
        size_t i;

        while (fgets(tmpbuf, sizeof(tmpbuf), fp) != NULL
            && line != info.line - 1) {
            size_t len = strlen(tmpbuf);
            if (tmpbuf[len - 1] == '\n')
                line++;
        }
        fprintf(stderr, "\n%s", tmpbuf);
        for (i = 0; i < info.column; i++)
            fputc(' ', stderr);
        fprintf(stderr, "^\n");
        fclose(fp);
    } else {
        fprintf(stderr, "\n<unable to open file>");
    }
}

/* Diagnostics */
static void cc_diag_common(
    cc_context* ctx, const char* severity, const char* fmt, va_list args)
{
    if (ctx->stage == STAGE_PARSER) {
        const cc_lexer_token* tok = &ctx->tokens[ctx->c_token];
        cc_diag_print_diag(tok->info, severity, fmt, args);
    } else if (ctx->stage == STAGE_LEXER) {
        if (ctx->n_tokens > 0) {
            cc_diag_print_diag(
                ctx->tokens[ctx->n_tokens - 1].info, severity, fmt, args);
        } else {
            fprintf(stderr, "<lexer>\n");
        }
    } else if (ctx->stage == STAGE_AST) {
        if (ctx->diag_node != NULL) {
            cc_diag_print_diag(ctx->diag_node->info, severity, fmt, args);
        } else {
            fprintf(stderr, "<ast>\n");
        }
    } else if (ctx->stage == STAGE_SSA) {
        if (ctx->ssa_current_tok != NULL) {
            cc_diag_print_diag(ctx->ssa_current_tok->info, severity, fmt, args);
        } else {
            fprintf(stderr, "<ssa>\n");
        }
    }
}

void cc_diag_error(cc_context* ctx, const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    cc_diag_common(ctx, ANSI_COLOUR(31) "error" ANSI_COLOUR(0), fmt, args);
    va_end(args);
    ctx->error_cnt++;

    if (ctx->error_cnt > 3)
        cc_abort(__FILE__, __LINE__);
}

void cc_diag_warning(cc_context* ctx, const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    cc_diag_common(ctx, ANSI_COLOUR(93) "warning" ANSI_COLOUR(0), fmt, args);
    va_end(args);
}

void cc_diag_add_info(cc_context* ctx, cc_diag_info info)
{
    assert(info.filename);
    ctx->diag_infos = cc_realloc_array(ctx->diag_infos, ctx->n_diag_infos + 1);
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

static void cc_diag_cutoff(cc_context* ctx, size_t offset) {
    size_t i;
    assert(offset < ctx->n_diag_infos);
    /* Cutoff includes after returning to this one */
    for (i = offset; i < ctx->n_diag_infos; ++i) {
        cc_diag_info* info = &ctx->diag_infos[i];
        cc_strfree(info->filename);
    }
}

void cc_diag_return_to_file(cc_context* ctx, cc_diag_info new_info)
{
    size_t i;
    for (i = 0; i < ctx->n_diag_infos; i++) {
        cc_diag_info* info = &ctx->diag_infos[i];
        if (info->filename == new_info.filename) {
            info->line = new_info.line;
            cc_strfree(new_info.filename); /* Discard new_info's filename */
            ++i;
            cc_diag_cutoff(ctx, i);
            ctx->n_diag_infos = i;
            return;
        }
    }
    cc_diag_add_info(ctx, new_info);
}

void cc_diag_copy(cc_diag_info* dest, const cc_diag_info* src)
{
    dest->filename = src->filename;
    dest->line = src->line;
    dest->column = src->column;
}
