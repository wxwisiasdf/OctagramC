#ifndef PAREXPR_H
#define PAREXPR_H 1

#include "ast.h"
#include "context.h"
#include "parser.h"
#include <stdbool.h>

bool cc_parse_expression(cc_context* ctx, cc_ast_node* node);
bool cc_parse_unary_expression(cc_context* ctx, cc_ast_node* node);
bool cc_parse_constant_expression(
    cc_context* ctx, cc_ast_node* node, cc_ast_literal* r);
bool cc_parse_assignment_expression(
    cc_context* ctx, cc_ast_node* node, cc_ast_variable* var);

#endif
