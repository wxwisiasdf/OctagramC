/* lexer.c - C lexer and tokenizer. */
#include "lexer.h"
#include "context.h"
#include "diag.h"
#include "util.h"
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#define LEXER_TOKEN_LIST_1(x, v) v
static const char* lexer_token_match[] = { LEXER_TOKEN_LIST };
#undef LEXER_TOKEN_LIST_1
#undef LEXER_TOKEN_LIST

static char* cc_lex_get_logical_line(cc_context* ctx)
{
    size_t total_len = 0;
    char* p = NULL;
    char tmpbuf[80];
    while (fgets(tmpbuf, sizeof(tmpbuf), ctx->fp)) {
        size_t len = strlen(tmpbuf);
        total_len += len;
        p = cc_realloc(p, total_len + 1);
        memcpy(p, tmpbuf, len + 1);
        cc_diag_increment_linenum(ctx);

        /* Logical lines can only continue after ecaping the newline */
        if (len > 1 && tmpbuf[len - 2] == '\\' && tmpbuf[len - 1] == '\n')
            continue;
        else if (len > 0 && tmpbuf[len - 1] == '\\')
            continue;
        break;
    }
    return p;
}

void cc_lex_print_token(const cc_lexer_token* tok)
{
    switch (tok->type) {
    case LEXER_TOKEN_IDENT:
        printf("%s", tok->data);
        return;
    case LEXER_TOKEN_NUMBER:
        printf("%s", tok->data);
        return;
    case LEXER_TOKEN_CHAR_LITERAL:
        printf("\'%s\'", tok->data);
        return;
    case LEXER_TOKEN_STRING_LITERAL:
        printf("\"%s\"", tok->data);
        return;
    default:
        break;
    }
    printf("%s", lexer_token_match[tok->type]);
}

/* Match a token in the current cptr, return the matched token_type
   and advances the cptr of the state */
static enum cc_lexer_token_type cc_lex_match_token(cc_context* ctx)
{
    size_t i;
    for (i = 0; i < ARRAY_SIZE(lexer_token_match); i++) {
        const char* m = lexer_token_match[i];
        if (m == NULL)
            continue;

        if (!strncmp(ctx->cptr, m, strlen(m))) {
            if (ISSTARTIDENT(m[0]) && ISIDENT(ctx->cptr[strlen(m)]))
                continue;
            ctx->cptr += strlen(m);
            return (enum cc_lexer_token_type)i;
        }
    }
    return LEXER_TOKEN_NONE;
}

static const char* cc_lex_literal(cc_context* ctx, const char* p)
{
    const char* s = p++;
    int base = 10;

    if (*s == '0') {
        switch (*p) {
        case 'x':
            base = 16;
            break;
        case 'o':
            base = 8;
            break;
        case 'b':
            break;
        /* Non-standard */
        case 'h':
            base = 16;
            break;
        case 'd':
            base = 10;
            break;
        default:
            return p++;
        }
        p++;
    }

    switch (base) {
    case 16:
        while (isxdigit(*p))
            p++;
        break;
    case 10:
        while (isdigit(*p))
            p++;

        if (*p == '.')
            p++;

        while (isdigit(*p))
            p++;
        break;
    case 8:
        while (ISODIGIT(*p))
            p++;
        break;
    case 2:
        while (*p == '0' || *p == '1')
            p++;
        break;
    default:
        cc_diag_error(ctx, "Invalid numeric literal base %i", base);
        return NULL;
    }
    return p;
}

static const char* cc_lex_string(cc_context* ctx, const char* p, const char** s)
{
    char ch = *p++; /* Skip the opening quotes */
    *s = p; /* Take sample *after* the quotes */
    while (*p != '\0' && *p != ch) {
        if (*p == '\\')
            p++;
        p++;
    }
    return p;
}

cc_lexer_token* cc_lex_token_peek(cc_context* ctx, int offset)
{
    if ((int)ctx->c_token + offset >= (int)ctx->n_tokens)
        return NULL;
    return &ctx->tokens[(int)ctx->c_token + offset];
}

cc_lexer_token* cc_lex_token_consume(cc_context* ctx)
{
    if (ctx->c_token >= ctx->n_tokens)
        return NULL;
    return &ctx->tokens[ctx->c_token++];
}

const cc_lexer_token* cc_lex_skip_until(
    cc_context* ctx, enum cc_lexer_token_type type)
{
    const cc_lexer_token* ctok;
    while ((ctok = cc_lex_token_consume(ctx)) != NULL)
        if (ctok->type == type)
            return ctok;
    return NULL;
}

static void cc_lex_line(cc_context* ctx, const char* line)
{
    ctx->cptr = ctx->cbuf = line;
    while (*ctx->cptr != '\0') {
        cc_lexer_token tok = { 0 };

        /* Skip space */
        while (ISSPACE(*ctx->cptr))
            ctx->cptr++;
        if (*ctx->cptr == '\0')
            break;

        /* Special handling for literals of the form:
           .<numbers><suffix> */
        if (ctx->cptr[0] == '.' && isdigit(ctx->cptr[1]))
            goto handle_literal;

        tok.type = cc_lex_match_token(ctx); /* Match a token */
        if (tok.type == LEXER_TOKEN_NONE) {
            /* Special handling for the idents, literals, etc */
            if (isdigit(*ctx->cptr)) { /* Numbers */
                const char* s;
            handle_literal:
                s = ctx->cptr;
                ctx->cptr = cc_lex_literal(ctx, ctx->cptr);
                tok.data = cc_strndup(s, (ptrdiff_t)ctx->cptr - (ptrdiff_t)s);
                tok.type = LEXER_TOKEN_NUMBER;
            } else if (ISSTARTIDENT(*ctx->cptr)) { /* Identifiers */
                const char* s = ctx->cptr++;
                while (ISIDENT(*ctx->cptr))
                    ctx->cptr++;
                tok.data = cc_strndup(s, (ptrdiff_t)ctx->cptr - (ptrdiff_t)s);
                tok.type = LEXER_TOKEN_IDENT;
            } else if (*ctx->cptr == '\"' || *ctx->cptr == '\'') {
                const char* s;
                char ch = *ctx->cptr;
                ctx->cptr = cc_lex_string(ctx, ctx->cptr, &s);
                tok.data = cc_strndup(s, (ptrdiff_t)ctx->cptr - (ptrdiff_t)s);
                tok.type = ch == '\'' ? LEXER_TOKEN_CHAR_LITERAL
                                      : LEXER_TOKEN_STRING_LITERAL;
                ctx->cptr++; /* Skip closing quotes */
            } else {
                cc_diag_error(ctx, "Unrecognized token");
                ctx->cptr++;
                exit(EXIT_FAILURE);
                continue;
            }
        }

        tok.info = (cc_diag_info) {
            .filename = cc_strdup(ctx->n_diag_infos
                    ? ctx->diag_infos[ctx->n_diag_infos - 1].filename
                    : "<unknown>"),
            .column = (size_t)((ptrdiff_t)ctx->cptr - (ptrdiff_t)ctx->cbuf),
            .line = ctx->n_diag_infos
                ? ctx->diag_infos[ctx->n_diag_infos - 1].line
                : 1,
        };
        ctx->tokens = cc_realloc_array(ctx->tokens, ctx->n_tokens + 1);
        ctx->tokens[ctx->n_tokens++] = tok;
    }
}

int cc_lex_top(cc_context* ctx)
{
    char* line = NULL;
    ctx->stage = STAGE_LEXER;
    while ((line = cc_lex_get_logical_line(ctx)) != NULL) {
        cc_lex_line(ctx, line);
        cc_free(line);
    }
    return 0;
}

static void cc_lex_destroy_token(cc_lexer_token* tok, bool managed)
{
    switch (tok->type) {
    case LEXER_TOKEN_IDENT:
    case LEXER_TOKEN_NUMBER:
    case LEXER_TOKEN_CHAR_LITERAL:
    case LEXER_TOKEN_STRING_LITERAL:
        cc_free(tok->data);
        break;
    default:
        break;
    }
    if (managed)
        cc_free(tok);
}

void cc_lex_deinit(cc_context* ctx)
{
    size_t i;
    for (i = 0; i < ctx->n_tokens; i++)
        cc_lex_destroy_token(&ctx->tokens[i], false);
    cc_free(ctx->tokens);
    ctx->n_tokens = 0;
    ctx->tokens = NULL;
    if (ctx->fp != stdin)
        fclose(ctx->fp);
}
