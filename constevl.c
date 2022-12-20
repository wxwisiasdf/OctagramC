/* constevl.c - C static constexpr evaluation engine */
#include "constevl.h"
#include "ast.h"
#include "context.h"
#include "optzer.h"
#include "util.h"
#include <assert.h>
#include <string.h>

typedef struct cc_ceval_io {
    const char* name; /* View of name, non-owning */
    cc_ast_literal literal;
} cc_ceval_io;

static cc_ast_literal cc_ceval_eval_1(
    cc_context* ctx, cc_ast_node* node, cc_ceval_io** list, size_t* n_list)
{
    if (node == NULL)
        goto error_handle;

    cc_ast_literal lhs, rhs, child;
    switch (node->type) {
    case AST_NODE_VARIABLE: {
        for (size_t i = 0; i < *n_list; i++)
            if (!strcmp((*list)[i].name, node->data.var.name))
                return (*list)[i].literal;
        const cc_ast_variable* var
            = cc_ast_find_variable(node->data.var.name, node);
        if (var != NULL && var->type.storage == AST_STORAGE_CONSTEXPR
            && var->initializer != NULL)
            return cc_ceval_eval_1(ctx, var->initializer, list, n_list);
        cc_diag_warning(ctx, "Unable to constexpr variable, defaulting to 0");
    } break;
    case AST_NODE_BINOP:
        lhs = cc_ceval_eval_1(ctx, node->data.binop.left, list, n_list);
        rhs = cc_ceval_eval_1(ctx, node->data.binop.right, list, n_list);
        switch (node->data.binop.op) {
#define CEVAL_CONDITIONAL(type, op)                                            \
    case type: {                                                               \
        if (lhs.is_float && rhs.is_float)                                      \
            return (cc_ast_literal) { .is_signed = false,                      \
                .is_float = false,                                             \
                .value.d = lhs.value.d op rhs.value.d };                       \
        else if (!lhs.is_float && rhs.is_float)                                \
            return (cc_ast_literal) { .is_signed = false,                      \
                .is_float = false,                                             \
                .value.u = lhs.value.u op rhs.value.d };                       \
        else if (lhs.is_float && !rhs.is_float)                                \
            return (cc_ast_literal) { .is_signed = false,                      \
                .is_float = false,                                             \
                .value.u = lhs.value.d op rhs.value.u };                       \
        else if (!lhs.is_float && !rhs.is_float)                               \
            return (cc_ast_literal) { .is_signed = false,                      \
                .is_float = false,                                             \
                .value.u = lhs.value.u op rhs.value.u };                       \
    } break
            CEVAL_CONDITIONAL(AST_BINOP_ADD, +);
            CEVAL_CONDITIONAL(AST_BINOP_MINUS, -);
            CEVAL_CONDITIONAL(AST_BINOP_MUL, *);
            CEVAL_CONDITIONAL(AST_BINOP_DIV, /);
            CEVAL_CONDITIONAL(AST_BINOP_GT, >);
            CEVAL_CONDITIONAL(AST_BINOP_GTE, >=);
            CEVAL_CONDITIONAL(AST_BINOP_LT, <);
            CEVAL_CONDITIONAL(AST_BINOP_LTE, <=);
            CEVAL_CONDITIONAL(AST_BINOP_COND_EQ, ==);
            CEVAL_CONDITIONAL(AST_BINOP_COND_NEQ, !=);
            CEVAL_CONDITIONAL(AST_BINOP_AND, &&);
            CEVAL_CONDITIONAL(AST_BINOP_OR, ||);
#undef CEVAL_CONDITIONAL
        case AST_BINOP_MOD:
            if ((lhs.is_float && rhs.is_float)
                || (!lhs.is_float && rhs.is_float)
                || (lhs.is_float && !rhs.is_float))
                cc_diag_error(ctx, "Modulous on float literal");
            return (cc_ast_literal) { .is_signed = lhs.is_signed,
                .is_float = false,
                .value.u = lhs.value.u % rhs.value.u };
        default:
            cc_diag_error(ctx, "Unrecognized binop %i", node->data.binop.op);
            break;
        }
        break;
    case AST_NODE_LITERAL:
        return node->data.literal;
    case AST_NODE_CALL: {
        assert(node->data.call.call_expr->type == AST_NODE_VARIABLE);
        cc_ast_variable* var = cc_ast_find_variable(
            node->data.call.call_expr->data.var.name, node);
        assert(var != NULL && var->type.mode == AST_TYPE_MODE_FUNCTION);

        assert(var->type.data.func.n_params == node->data.call.n_params);
        for (size_t i = 0; i < var->type.data.func.n_params; i++) {
            *list = cc_realloc_array(*list, *n_list + 1);
            (*list)[(*n_list)++] = (cc_ceval_io) { .name
                = var->type.data.func.params[i].name,
                .literal = cc_ceval_eval(ctx, &node->data.call.params[i]) };
        }
        return cc_ceval_eval_1(ctx, var->body, list, n_list);
    }
    case AST_NODE_RETURN:
        return cc_ceval_eval_1(ctx, node->data.return_expr, list, n_list);
    case AST_NODE_UNOP:
        child = cc_ceval_eval_1(ctx, node->data.unop.child, list, n_list);
        switch (node->data.unop.op) {
        case AST_UNOP_CAST:
            switch (node->data.unop.cast.mode) {
            case AST_TYPE_MODE_CHAR:
                if (node->data.unop.cast.is_signed)
                    return (cc_ast_literal) { .is_float = false,
                        .is_signed = child.is_signed,
                        .value.s
                        = (signed char)(child.is_float ? child.value.d
                                                       : child.value.s) };
                return (cc_ast_literal) { .is_float = false,
                    .is_signed = child.is_signed,
                    .value.s
                    = (unsigned char)(child.is_float ? child.value.d
                                                     : child.value.u) };
            case AST_TYPE_MODE_SHORT:
                if (node->data.unop.cast.is_signed)
                    return (cc_ast_literal) { .is_float = false,
                        .is_signed = child.is_signed,
                        .value.s
                        = (signed short)(child.is_float ? child.value.d
                                                        : child.value.s) };
                return (cc_ast_literal) { .is_float = false,
                    .is_signed = child.is_signed,
                    .value.s
                    = (unsigned short)(child.is_float ? child.value.d
                                                      : child.value.u) };
            case AST_TYPE_MODE_INT:
                if (node->data.unop.cast.is_signed)
                    return (cc_ast_literal) { .is_float = false,
                        .is_signed = child.is_signed,
                        .value.s
                        = (signed int)(child.is_float ? child.value.d
                                                      : child.value.s) };
                return (cc_ast_literal) { .is_float = false,
                    .is_signed = child.is_signed,
                    .value.s = (unsigned int)(child.is_float ? child.value.d
                                                             : child.value.u) };
            case AST_TYPE_MODE_LONG:
                if (node->data.unop.cast.is_signed)
                    return (cc_ast_literal) { .is_float = false,
                        .is_signed = child.is_signed,
                        .value.s
                        = (signed long)(child.is_float ? child.value.d
                                                       : child.value.s) };
                return (cc_ast_literal) { .is_float = false,
                    .is_signed = child.is_signed,
                    .value.s
                    = (unsigned long)(child.is_float ? child.value.d
                                                     : child.value.u) };
            case AST_TYPE_MODE_FLOAT:
                return (cc_ast_literal) { .is_float = true,
                    .is_signed = child.is_signed,
                    .value.d = (float)(child.is_float
                            ? child.value.d
                            : (child.is_signed ? child.value.s
                                               : (float)child.value.u)) };
            case AST_TYPE_MODE_DOUBLE:
                return (cc_ast_literal) { .is_float = true,
                    .is_signed = child.is_signed,
                    .value.d = (double)(child.is_float
                            ? child.value.d
                            : (child.is_signed ? child.value.s
                                               : (double)child.value.u)) };
            default:
                cc_ast_print(node);
                cc_diag_error(
                    ctx, "Unknown unop %i for consteval", node->data.unop.op);
                break;
            }
            break;
        default:
            break;
        }
        cc_ast_print(node);
        cc_diag_error(ctx, "Unknown node %i for consteval", node->type);
        break;
    default:
        cc_ast_print(node);
        cc_diag_error(ctx, "Unknown node %i for consteval", node->type);
        break;
    }

error_handle:
    return (cc_ast_literal) {
        .is_signed = false,
        .is_float = false,
        .value.u = 0,
    };
}

cc_ast_literal cc_ceval_eval(cc_context* ctx, cc_ast_node* node)
{
    cc_ceval_io* list = NULL;
    size_t n_list = 0;
    cc_ast_literal literal = cc_ceval_eval_1(ctx, node, &list, &n_list);
    cc_free(list);
    return literal;
}

/* Evaluate a constant expression, modifying the resulting AST node
   and (if it could be evaluated) reduced into a single literal node. */
bool cc_ceval_constant_expression(
    cc_context* ctx, cc_ast_node** node, cc_ast_literal* literal)
{
    cc_optimizer_expr_condense(ctx, node, true); /* Condense and eliminate
                                                dead code */
    *literal = cc_ceval_eval(ctx, *node);
    return true;
}

/* Deduces a type from a node, the type written into type is a non-owning
   view of the real type, hence use cc_ast_copy_type to safely obtain a
   owning version if desired. */
bool cc_ceval_deduce_type(
    cc_context* ctx, const cc_ast_node* node, cc_ast_type* type)
{
    switch (node->type) {
    case AST_NODE_VARIABLE: {
        cc_ast_variable* var = cc_ast_find_variable(node->data.var.name, node);
        *type = var->type;
    }
        return true;
    case AST_NODE_STRING_LITERAL:
        type->n_cv_qual = 1;
        type->cv_qual[0].is_const = true;
        type->mode = AST_TYPE_MODE_CHAR;
        return true;
    case AST_NODE_LITERAL:
        /* TODO: Literals with suffixes like zu, u, ul, etc */
        type->mode = AST_TYPE_MODE_INT;
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

bool cc_ceval_is_const(cc_context* ctx, const cc_ast_node* node)
{
    switch (node->type) {
    case AST_NODE_LITERAL:
        return true;
    case AST_NODE_VARIABLE:
        const cc_ast_variable* var
            = cc_ast_find_variable(node->data.var.name, node);
        if (var->type.storage == AST_STORAGE_CONSTEXPR)
            return true;
        return false;
    default:
        return false;
    }
    return false;
}
