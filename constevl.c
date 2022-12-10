/* constevl.c - C static constexpr evaluation engine */
#include "constevl.h"
#include "ast.h"
#include "context.h"
#include "optzer.h"
#include <assert.h>

/* Evaluate a constant expression, modifying the resulting AST node
   and (if it could be evaluated) reduced into a single literal node. */
_Bool cc_ceval_constant_expression(cc_context* ctx, cc_ast_node* node)
{
    cc_optimizer_expr_condense(&node, true); /* Condense and eliminate
                                                dead code */
    return true;
}

/* Deduces a type from a node, the type written into type is a non-owning
   view of the real type, hence use cc_ast_copy_type to safely obtain a
   owning version if desired. */
_Bool cc_ceval_deduce_type(
    cc_context* ctx, const cc_ast_node* node, cc_ast_type* type)
{
    switch (node->type) {
    case AST_NODE_VARIABLE: {
        cc_ast_variable* var = cc_ast_find_variable(node->data.var.name, node);
        *type = var->type;
    } return true;
    case AST_NODE_STRING_LITERAL:
        type->n_cv_qual = 1;
        type->cv_qual[0].is_const = true;
        type->mode = TYPE_MODE_CHAR;
        return true;
    case AST_NODE_LITERAL:
        /* TODO: Literals with suffixes like zu, u, ul, etc */
        type->mode = TYPE_MODE_INT;
        return true;
    case AST_NODE_BINOP:
        /* Assignments promote to their lvalue type */
        if (node->data.binop.op == AST_BINOP_ASSIGN)
            return cc_ceval_deduce_type(ctx, node->data.binop.left, type);
        /*cc_ceval_deduce_type(ctx, node->data.binop.right, type);*/
        return cc_ceval_deduce_type(ctx, node->data.binop.left, type);
    case AST_NODE_UNOP:
        if (node->data.unop.op == AST_UNOP_CAST) {
            *type = node->data.unop.cast;
            return true;
        } else if (node->data.unop.op == AST_UNOP_DEREF) {
            if (!cc_ceval_deduce_type(ctx, node->data.unop.child, type))
                return false;
            /* Dereference type */
            if (type->n_cv_qual == 0) {
                cc_diag_error(ctx, "Dereferencing non-pointer type");
                goto error_handle;
            }
            type->n_cv_qual--;
            return true;
        } else if (node->data.unop.op == AST_UNOP_REF) {
            if (!cc_ceval_deduce_type(ctx, node->data.unop.child, type))
                return false;
            /* Reference type */
            type->cv_qual[type->n_cv_qual++] = type->cv_qual[0];
            if (type->n_cv_qual >= MAX_CV_QUALIFIERS) {
                cc_diag_error(ctx, "Deduced type exceeds pointer depth size");
                goto error_handle;
            }
            return true;
        }
        return cc_ceval_deduce_type(ctx, node->data.unop.child, type);
    case AST_NODE_BLOCK:
        if (!node->data.block.n_children)
            return true;
        assert(node->data.block.n_children == 1); /* We can't really deduce
                                                     the type of a big block! */
        return cc_ceval_deduce_type(ctx, &node->data.block.children[0], type);
    default:
        assert(0);
        break;
    }
error_handle:
    return false;
}

/* Obtain the offset of the field in an structure of type type, where the
   dot node is appearing on node. */
size_t cc_ceval_get_field_offset(
    cc_context* ctx, const cc_ast_type* type, const cc_ast_node* node)
{
    return 0;
}
