#ifndef C_PARSE
#define C_PARSE 1

#include "ast.h"
#include "context.h"
#include <stdbool.h>
#include <stddef.h>

#define CC_PARSE_EXPECT(ctx, ctok, _type, msg)                                 \
    do {                                                                       \
        if (((ctok) = cc_lex_token_peek(ctx, 0)) == NULL                       \
            || (ctok)->type != _type) {                                        \
            cc_diag_error(ctx, msg);                                   \
            cc_lex_token_consume(ctx);                                         \
            goto error_handle;                                                 \
        }                                                                      \
        cc_lex_token_consume(ctx);                                             \
    } while (0)

int cc_parse_top(cc_context* ctx);

#endif
