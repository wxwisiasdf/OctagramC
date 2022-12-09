#ifndef OPTZR_H
#define OPTZR_H 1

typedef struct cc_context cc_context;
typedef struct cc_ast_node cc_ast_node;
void cc_optimizer_expr_condense(cc_ast_node** pnode, _Bool managed);
int cc_optimizer_top(cc_context* ctx);

#endif
