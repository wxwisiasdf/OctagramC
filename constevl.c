/* constevl.c - C static constexpr evaluation engine */
#include "constevl.h"
#include "ast.h"
#include "context.h"
#include "optzer.h"
#include "util.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>

unsigned short cc_ceval_literal_to_ushort(
    cc_context* ctx, const cc_ast_literal* literal)
{
    if (literal->is_float && literal->value.d < 1.f) {
        cc_diag_error(
            ctx, "Float value must be above 0, not %f", literal->value.d);
        return 0;
    }
    if (literal->is_signed && literal->value.s <= 0) {
        cc_diag_error(
            ctx, "Integer value must be above 0, not %u", literal->value.s);
        return 0;
    }

    if (literal->is_float)
        return (unsigned short)literal->value.d;
    else if (literal->is_signed)
        return (unsigned short)literal->value.s;
    return literal->value.u;
}

typedef struct cc_ceval_io {
    const char* name; /* View of name, non-owning */
    cc_ast_literal literal;
} cc_ceval_io;

static cc_ast_literal cc_ceval_eval_cast_unop(cc_context* ctx,
    cc_ast_node* node, cc_ceval_io** list, size_t* n_list,
    const cc_ast_literal* child)
{
    const cc_ast_type* cast = &node->data.unop.cast;
    switch (cast->mode) {
    case AST_TYPE_MODE_CHAR:
        if (cast->data.num.is_signed)
            return (cc_ast_literal) { .is_float = false,
                .is_signed = child->is_signed,
                .value.s = (signed char)(child->is_float ? child->value.d
                                                        : child->value.s) };
        return (cc_ast_literal) { .is_float = false,
            .is_signed = child->is_signed,
            .value.s
            = (unsigned char)(child->is_float ? child->value.d : child->value.u) };
    case AST_TYPE_MODE_SHORT:
        if (cast->data.num.is_signed)
            return (cc_ast_literal) { .is_float = false,
                .is_signed = child->is_signed,
                .value.s = (signed short)(child->is_float ? child->value.d
                                                         : child->value.s) };
        return (cc_ast_literal) { .is_float = false,
            .is_signed = child->is_signed,
            .value.s = (unsigned short)(child->is_float ? child->value.d
                                                       : child->value.u) };
    case AST_TYPE_MODE_INT:
        if (cast->data.num.is_signed)
            return (cc_ast_literal) { .is_float = false,
                .is_signed = child->is_signed,
                .value.s = (signed int)(child->is_float ? child->value.d
                                                       : child->value.s) };
        return (cc_ast_literal) { .is_float = false,
            .is_signed = child->is_signed,
            .value.s
            = (unsigned int)(child->is_float ? child->value.d : child->value.u) };
    case AST_TYPE_MODE_LONG:
        if (cast->data.num.is_signed)
            return (cc_ast_literal) { .is_float = false,
                .is_signed = child->is_signed,
                .value.s = (signed long)(child->is_float ? child->value.d
                                                        : child->value.s) };
        return (cc_ast_literal) { .is_float = false,
            .is_signed = child->is_signed,
            .value.s
            = (unsigned long)(child->is_float ? child->value.d : child->value.u) };
    case AST_TYPE_MODE_FLOAT:
        return (cc_ast_literal) {
            .is_float = true,
            .is_signed = child->is_signed,
            .value.d = (float)(child->is_float
                    ? child->value.d
                    : (child->is_signed ? child->value.s : (float)child->value.u))
        };
    case AST_TYPE_MODE_DOUBLE:
        return (cc_ast_literal) {
            .is_float = true,
            .is_signed = child->is_signed,
            .value.d = (double)(child->is_float
                    ? child->value.d
                    : (child->is_signed ? child->value.s : (double)child->value.u))
        };
    default:
        cc_ast_print(node);
        cc_diag_error(ctx, "Unknown unop %i for consteval", node->data.unop.op);
        break;
    }
error_handle:
    return (cc_ast_literal) {
        .is_signed = false,
        .is_float = false,
        .value.u = 0,
    };
}

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
#define CEVAL_OPERATOR(type, op)                                               \
    case type: {                                                               \
        if (lhs.is_float && rhs.is_float) {                                    \
            return (cc_ast_literal) { .is_signed = false,                      \
                .is_float = true,                                              \
                .value.d = lhs.value.d op rhs.value.d };                       \
        } else if (!lhs.is_float && rhs.is_float) {                            \
            if (lhs.is_signed)                                                 \
                return (cc_ast_literal) { .is_signed = true,                   \
                    .is_float = false,                                         \
                    .value.s = lhs.value.s op rhs.value.d };                   \
            return (cc_ast_literal) { .is_signed = false,                      \
                .is_float = false,                                             \
                .value.u = lhs.value.u op rhs.value.d };                       \
        } else if (lhs.is_float && !rhs.is_float) {                            \
            if (rhs.is_signed)                                                 \
                return (cc_ast_literal) { .is_signed = true,                   \
                    .is_float = true,                                          \
                    .value.d = lhs.value.d op rhs.value.s };                   \
            return (cc_ast_literal) { .is_signed = false,                      \
                .is_float = true,                                              \
                .value.d = lhs.value.d op rhs.value.u };                       \
        } else if (!lhs.is_float && !rhs.is_float) {                           \
            if (lhs.is_signed && rhs.is_signed)                                \
                return (cc_ast_literal) { .is_signed = true,                   \
                    .is_float = false,                                         \
                    .value.s = lhs.value.s op rhs.value.s };                   \
            else if (lhs.is_signed && !rhs.is_signed)                          \
                return (cc_ast_literal) { .is_signed = true,                   \
                    .is_float = false,                                         \
                    .value.s = lhs.value.s op(long signed int) rhs.value.u };  \
            else if (!lhs.is_signed && rhs.is_signed)                          \
                return (cc_ast_literal) { .is_signed = true,                   \
                    .is_float = false,                                         \
                    .value.s = (long signed int)lhs.value.u op rhs.value.s };  \
            return (cc_ast_literal) { .is_signed = false,                      \
                .is_float = false,                                             \
                .value.u = lhs.value.u op rhs.value.u };                       \
        }                                                                      \
    } break
            CEVAL_OPERATOR(AST_BINOP_ADD, +);
            CEVAL_OPERATOR(AST_BINOP_SUB, -);
            CEVAL_OPERATOR(AST_BINOP_MUL, *);
            CEVAL_OPERATOR(AST_BINOP_DIV, /);
            CEVAL_OPERATOR(AST_BINOP_GT, >);
            CEVAL_OPERATOR(AST_BINOP_GTE, >=);
            CEVAL_OPERATOR(AST_BINOP_LT, <);
            CEVAL_OPERATOR(AST_BINOP_LTE, <=);
            CEVAL_OPERATOR(AST_BINOP_COND_EQ, ==);
            CEVAL_OPERATOR(AST_BINOP_COND_NEQ, !=);
            CEVAL_OPERATOR(AST_BINOP_AND, &&);
            CEVAL_OPERATOR(AST_BINOP_OR, ||);
#undef CEVAL_OPERATOR
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
            return cc_ceval_eval_cast_unop(ctx, node, list, n_list, &child);
        default:
            break;
        }
        cc_ast_print(node);
        cc_diag_error(ctx, "Unknown unary node %i for consteval", node->type);
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

static unsigned int cc_ceval_get_rank(const cc_ast_type* type)
{
    switch (type->mode) {
    case AST_TYPE_MODE_LONG:
    case AST_TYPE_MODE_FLOAT:
        return type->data.num.is_longer ? 9 : 8;
    case AST_TYPE_MODE_ENUM:
    case AST_TYPE_MODE_INT:
        return 7;
    case AST_TYPE_MODE_SHORT:
        return 6;
    case AST_TYPE_MODE_CHAR:
        return 5;
    case AST_TYPE_MODE_BOOL:
        return 4;
    default:
        abort();
    }
    return 0;
}

static void cc_ceval_promote_type(cc_ast_type* dest, const cc_ast_type* src)
{
    if (dest->mode == AST_TYPE_MODE_NONE) {
        *dest = *src;
        return;
    }

    if (src->mode == AST_TYPE_MODE_FUNCTION || src->mode == AST_TYPE_MODE_STRUCT
        || src->mode == AST_TYPE_MODE_UNION) {
        *dest = *src;
        return;
    }

    unsigned int dest_rank = cc_ceval_get_rank(dest);
    unsigned int src_rank = cc_ceval_get_rank(src);
    if (src_rank >= dest_rank) {
        *dest = *src;
        return;
    }
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
        cc_ceval_promote_type(type, &var->type);
    }
        return true;
    case AST_NODE_CALL: {
        cc_ast_type tmp_type = { 0 };
        if (!cc_ceval_deduce_type(ctx, node->data.call.call_expr, &tmp_type))
            return false;
        assert(tmp_type.mode == AST_TYPE_MODE_FUNCTION
            && tmp_type.data.func.return_type != NULL);
        *type = *tmp_type.data.func.return_type;
    }
        return true;
    case AST_NODE_STRING_LITERAL:
        *type = cc_ceval_get_string_type(ctx);
        return true;
    case AST_NODE_LITERAL:
        /* TODO: Literals with suffixes like zu, u, ul, etc */
        type->mode = AST_TYPE_MODE_INT;
        type->data.num.is_signed = ctx->is_default_signed;
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
        abort();
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
    case AST_NODE_VARIABLE: {
        const cc_ast_variable* var
            = cc_ast_find_variable(node->data.var.name, node);
        if (var->type.storage == AST_STORAGE_CONSTEXPR)
            return true;
    } return false;
    default:
        return false;
    }
    return false;
}

cc_ast_type cc_ceval_get_string_type(cc_context* ctx)
{
    cc_ast_type type = { 0 };
    type.n_cv_qual = 1;
    type.cv_qual[0].is_const = true;
    type.mode = AST_TYPE_MODE_CHAR;
    return type;
}
