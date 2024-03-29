#ifndef CEVAL_H
#define CEVAL_H 1

#include "ast.h"
#include "context.h"
#include <stdbool.h>
#include <stddef.h>

unsigned short cc_ceval_literal_to_ushort(
    cc_context* ctx, const cc_ast_literal* literal);
cc_ast_literal cc_ceval_eval(cc_context* ctx, cc_ast_node* node);
bool cc_ceval_constant_expression(
    cc_context* ctx, cc_ast_node** node, cc_ast_literal* literal);
bool cc_ceval_deduce_type_1(
    cc_context* ctx, const cc_ast_node* node, cc_ast_type* type, bool as_func);
bool cc_ceval_deduce_type(
    cc_context* ctx, const cc_ast_node* node, cc_ast_type* type);
bool cc_ceval_is_const(cc_context* ctx, const cc_ast_node* node);
cc_ast_type cc_ceval_get_string_type(cc_context* ctx);

#endif
