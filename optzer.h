#ifndef OPTZR_H
#define OPTZR_H 1

#include "ast.h"
#include "context.h"
#include <stdbool.h>

void cc_optimizer_expr_condense(cc_ast_node** pnode, _Bool managed);
int cc_optimizer_top(cc_context* ctx);

#endif
