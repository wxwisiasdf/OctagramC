/* lexer.c - C lexer and tokenizer. */
#include "lexer.h"
#include "context.h"
#include "diag.h"
#include "util.h"
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#define LEXER_TOKEN_LIST_1(x, v) v
static const char* lexer_token_match[NUM_LEXER_TOKENS] = { LEXER_TOKEN_LIST };
#undef LEXER_TOKEN_LIST_1
#undef LEXER_TOKEN_LIST

static void cc_lex_destroy_token(cc_lexer_token* tok, bool managed)
{
    switch (tok->type) {
    case LEXER_TOKEN_IDENT:
    case LEXER_TOKEN_CHAR_LITERAL:
    case LEXER_TOKEN_STRING_LITERAL:
        cc_strfree(tok->data.text);
        break;
    default:
        break;
    }
    if (managed)
        cc_free(tok);
}

static char* cc_lex_get_logical_line(cc_context* ctx, char **buf, size_t *total)
{
    size_t end = 0;
    char *p = *buf;
    p[0] = '\0';
    while (fgets(&p[end], *total - end, ctx->fp) != NULL) {
        size_t len = strlen(&p[end]);
        if (!len)
            goto expand_buffer;

        end += len;
        /* Logical lines can only continue after ecaping the newline */
        if (len > 0 && p[end - 1] == '\n') {
            p[end - 1] = '\0';
            cc_diag_increment_linenum(ctx);
            break;
        } else if ((len > 1 && p[end - 2] == '\\' && p[end - 1] == '\n')
        || (len > 0 && p[end - 1] == '\\'))
            cc_diag_increment_linenum(ctx);
        
        if (end >= *total) {
expand_buffer:
            *total = end + 1024;
            p = *buf = cc_realloc(*buf, *total + 1);
        }
    }
    return p[0] == '\0' ? NULL : p;
}

void cc_lex_print_token(const cc_lexer_token* tok)
{
    switch (tok->type) {
    case LEXER_TOKEN_IDENT:
        printf("i(%s)", cc_strview(tok->data.text));
        return;
    case LEXER_TOKEN_CHAR_LITERAL:
        printf("\'%s\'", cc_strview(tok->data.text));
        return;
    case LEXER_TOKEN_STRING_LITERAL:
        printf("\"%s\"", cc_strview(tok->data.text));
        return;
    case LEXER_TOKEN_NUMBER:
        if (tok->data.num.is_float)
            printf("n(%lf,%c,%c)", tok->data.num.value.d,
                tok->data.num.suffix[0], tok->data.num.suffix[1]);
        else
            printf("n(%lu,%c,%c)", tok->data.num.value.ul,
                tok->data.num.suffix[0], tok->data.num.suffix[1]);
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

static const char* cc_lex_literal(
    cc_context* ctx, cc_lexer_token* tok, const char* p)
{
    unsigned char base = 10;
    const char* start = p;

    tok->data.num.is_float = false;
    if (*p == '0') {
        ++p;
        if (ISODIGIT(*p)) {
            base = 8;
            start = p;
            goto octal_num;
        }
        switch (*p) {
        case '.':
            base = 10;
            start = p;
            goto after_frac;
        case 'x': /* Hexadecimal */
        case 'h': /* Non-standard hex */
            base = 16;
            ++p;
            break;
        case 'o': /* Octal */
            base = 8;
            ++p;
            break;
        case 'b': /* Binary */
            base = 2;
            ++p;
            break;
        case 'd': /* Non-standard decimal */
            base = 10;
            ++p;
            break;
        default:
            return p;
        }
    }

    start = p;
    switch (base) {
    case 16:
        for (; isxdigit(*p); ++p)
            ;
        break;
    case 10:
        for (; isdigit(*p); ++p)
            ;
    after_frac:
        if (*p == '.') {
            tok->data.num.is_float = true;
            ++p;
        }
        for (; isdigit(*p); ++p)
            ;
        break;
    case 8:
    octal_num:
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

    if (tok->data.num.is_float)
        tok->data.num.value.d = strtod(start, NULL);
    else
        tok->data.num.value.ul = strtoul(start, NULL, base);

    /* Literal specifiers */
    tok->data.num.suffix[0] = tok->data.num.suffix[1] = tok->data.num.suffix[2]
        = '\0';
    if (isalpha(*p)) {
        tok->data.num.suffix[0] = *p;
        ++p;
        if (isalpha(*p)) {
            tok->data.num.suffix[1] = *p;
            ++p;
            if (isalpha(*p)) {
                tok->data.num.suffix[2] = *p;
                ++p;
            }
        }
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

cc_lexer_token* cc_lex_token_unconsume(cc_context* ctx)
{
    if (ctx->c_token == 0)
        return NULL;
    return &ctx->tokens[--ctx->c_token];
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

static bool cc_parse_preprocessor(cc_context* ctx)
{
    const cc_lexer_token* ctok;
    if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
        && ctok->type == LEXER_TOKEN_HASHTAG) {
        cc_lex_token_consume(ctx);
        if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && ctok->type == LEXER_TOKEN_IDENT
            && !strcmp(cc_strview(ctok->data.text), "line")) {
            cc_lex_token_consume(ctx);
            if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
                && ctok->type == LEXER_TOKEN_NUMBER) {
                unsigned long int n_lines = ctok->data.num.value.ul;
                assert(ctok->data.num.is_float == false);
                cc_lex_token_consume(ctx);

                if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
                    && ctok->type == LEXER_TOKEN_STRING_LITERAL) {
                    const char* filename = cc_strview(ctok->data.text);
                    unsigned char flags = 0;
                    cc_lex_token_consume(ctx);

                    while ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
                        && ctok->type == LEXER_TOKEN_NUMBER) {
                        flags |= 1 << (unsigned char)ctok->data.num.value.ul;
                        assert(ctok->data.num.is_float == false);
                        cc_lex_token_consume(ctx);
                    }

                    if ((flags & (1 << 1)) != 0) { /* New file */
                        cc_diag_info info = { 0 };
                        info.filename = cc_strdup(filename);
                        info.line = n_lines;
                        cc_diag_add_info(ctx, info);
                    } else if ((flags & (1 << 2)) != 0) { /* Return to file */
                        cc_diag_info info = { 0 };
                        info.filename = cc_strdup(filename);
                        info.line = n_lines;
                        cc_diag_add_info(ctx, info);
                    }
                }
            }
        }
        return true;
    }
    return false;
}

static void cc_lex_line(cc_context* ctx, const char* line)
{
    bool is_preproc = false;
    size_t tokens_before_line = ctx->n_tokens;

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
        if (tok.type == LEXER_TOKEN_HASHTAG) {
            is_preproc = true;
        } else if (tok.type == LEXER_TOKEN_NONE) {
            /* Special handling for the idents, literals, etc */
            if (isdigit(*ctx->cptr)) { /* Numbers */
                const char* s;
            handle_literal:
                s = ctx->cptr;
                ctx->cptr = cc_lex_literal(ctx, &tok, ctx->cptr);
                tok.type = LEXER_TOKEN_NUMBER;
            } else if (ISSTARTIDENT(*ctx->cptr)) { /* Identifiers */
                const char* s = ctx->cptr++;
                for (; ISIDENT(*ctx->cptr); ++ctx->cptr)
                    /* ... */;
                tok.data.text
                    = cc_strndup(s, (ptrdiff_t)ctx->cptr - (ptrdiff_t)s);
                tok.type = LEXER_TOKEN_IDENT;
            } else if (*ctx->cptr == '\"' || *ctx->cptr == '\'') {
                cc_lexer_token* prev_tok = &ctx->tokens[ctx->n_tokens - 1];
                const char* s;
                char ch = *ctx->cptr;
                ctx->cptr = cc_lex_string(ctx, ctx->cptr, &s);
                tok.type = ch == '\'' ? LEXER_TOKEN_CHAR_LITERAL
                                      : LEXER_TOKEN_STRING_LITERAL;
                tok.data.text
                    = cc_strndup(s, (ptrdiff_t)ctx->cptr - (ptrdiff_t)s);
                if (ctx->n_tokens > 0 && prev_tok->type == tok.type) {
                    prev_tok->data.text
                        = cc_strdupcat(cc_strview(prev_tok->data.text),
                            cc_strview(tok.data.text));
                    cc_strfree(prev_tok->data.text);
                    cc_strfree(tok.data.text);
                    ctx->cptr++; /* Skip closing quotes */
                    continue; /* Next token, do not add to tokenlist! */
                }
                ctx->cptr++; /* Skip closing quotes */
            } else {
                cc_diag_error(ctx, "Unrecognized token");
                ctx->cptr++;
                exit(EXIT_FAILURE);
                continue;
            }
        }

        tok.info.filename = ctx->n_diag_infos
                ? ctx->diag_infos[ctx->n_diag_infos - 1].filename
                : cc_strdup("<unknown>");
        tok.info.column = (size_t)((ptrdiff_t)ctx->cptr - (ptrdiff_t)ctx->cbuf);
        tok.info.line = ctx->n_diag_infos
            ? ctx->diag_infos[ctx->n_diag_infos - 1].line
            : 1;
        ctx->tokens = cc_realloc_array(ctx->tokens, ctx->n_tokens + 1);
        ctx->tokens[ctx->n_tokens++] = tok;
    }

    if (is_preproc) {
        size_t i;
        /* Run a minimal parsing of the preprocessor, and discard the tokens
           from the preprocessor line. */
        cc_parse_preprocessor(ctx);
        for (i = tokens_before_line + 1; i < ctx->n_tokens; ++i)
            cc_lex_destroy_token(&ctx->tokens[i], false);
        ctx->n_tokens = tokens_before_line;
    }
}

int cc_lex_top(cc_context* ctx)
{
    size_t total = 1024;
    char* line = cc_malloc(total + 1);
    ctx->stage = STAGE_LEXER;
    /* To avoid many relocations, reuse the same buffer over and over
       and expand it as needed. */
    while (cc_lex_get_logical_line(ctx, &line, &total) != NULL)
        cc_lex_line(ctx, line);
    cc_free(line);
    ctx->c_token = 0;
    return 0;
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
