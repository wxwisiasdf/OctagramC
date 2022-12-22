#ifndef C_PARSE
#define C_PARSE 1

#include "ast.h"
#include "context.h"
#include <stdbool.h>
#include <stddef.h>

#define CC_PARSE_EXPECT(ctx, ctok, _type, ...)                                 \
    do {                                                                       \
        if (((ctok) = cc_lex_token_peek(ctx, 0)) == NULL                       \
            || (ctok)->type != _type) {                                        \
            cc_diag_error(ctx, __VA_ARGS__);                                   \
            cc_lex_token_consume(ctx);                                         \
            goto error_handle;                                                 \
        }                                                                      \
        cc_lex_token_consume(ctx);                                             \
    } while (0)

bool cc_parse_expression(cc_context* ctx, cc_ast_node* node);
bool cc_parse_unary_expression(cc_context* ctx, cc_ast_node* node);
bool cc_parse_constant_expression(
    cc_context* ctx, cc_ast_node* node, cc_ast_literal* r);
bool cc_parse_assignment_expression(
    cc_context* ctx, cc_ast_node* node, cc_ast_variable* var);
int cc_parse_top(cc_context* ctx);

#endif
