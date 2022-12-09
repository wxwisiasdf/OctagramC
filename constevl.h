#ifndef CEVAL_H
#define CEVAL_H 1

#include <stdbool.h>
#include <stddef.h>

typedef struct cc_context cc_context;
typedef struct cc_ast_node cc_ast_node;
typedef struct cc_ast_type cc_ast_type;
_Bool cc_ceval_constant_expression(cc_context* ctx, cc_ast_node* node);
_Bool cc_ceval_deduce_type(
    cc_context* ctx, const cc_ast_node* node, cc_ast_type* type);
size_t cc_ceval_get_field_offset(
    cc_context* ctx, const cc_ast_type* type, const cc_ast_node* node);

#endif
