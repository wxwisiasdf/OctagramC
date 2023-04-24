#ifndef OPTZR_H
#define OPTZR_H 1

#include "ast.h"
#include "context.h"
#include <stdbool.h>

bool cc_optimizer_is_empty_block(const cc_ast_node* node);
void cc_optimizer_merge_block(
    cc_context* ctx, cc_ast_node** pnode, bool managed);
void cc_optimizer_expr_condense(
    cc_context* ctx, cc_ast_node** pnode, bool managed);
int cc_optimizer_top(cc_context* ctx);

#endif
