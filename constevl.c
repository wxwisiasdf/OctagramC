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
    cc_string_key name; /* View of name, non-owning */
    cc_ast_literal literal;
} cc_ceval_io;

static cc_ast_literal cc_ceval_eval_cast_unop(
    cc_context* ctx, cc_ast_node* node, const cc_ast_literal* child)
{
    const cc_ast_type* cast = &node->data.unop.cast;
    cc_ast_literal literal = { 0 };
    cc_ast_literal literal_zero = { 0 };
    switch (cast->mode) {
    case AST_TYPE_MODE_BOOL:
        literal.is_float = false;
        literal.is_signed = false;
        literal.value.u
            = (child->is_float ? child->value.d != 0.f : child->value.s != 0);
        return literal;
    case AST_TYPE_MODE_CHAR:
        if (cast->data.num.is_signed) {
            literal.is_float = false;
            literal.is_signed = child->is_signed;
            literal.value.s = (signed char)(child->is_float ? child->value.d
                                                            : child->value.s);
            return literal;
        }
        literal.is_float = false;
        literal.is_signed = child->is_signed;
        literal.value.s = (unsigned char)(child->is_float ? child->value.d
                                                          : child->value.u);
        return literal;
    case AST_TYPE_MODE_SHORT:
        if (cast->data.num.is_signed) {
            literal.is_float = false, literal.is_signed = child->is_signed;
            literal.value.s = (signed short)(child->is_float ? child->value.d
                                                             : child->value.s);
            return literal;
        }
        literal.is_float = false, literal.is_signed = child->is_signed;
        literal.value.s = (unsigned short)(child->is_float ? child->value.d
                                                           : child->value.u);
        return literal;
    case AST_TYPE_MODE_INT:
        if (cast->data.num.is_signed) {
            literal.is_float = false;
            literal.is_signed = child->is_signed;
            literal.value.s = (signed int)(child->is_float ? child->value.d
                                                           : child->value.s);
            return literal;
        }
        literal.is_float = false;
        literal.is_signed = child->is_signed;
        literal.value.s
            = (unsigned int)(child->is_float ? child->value.d : child->value.u);
        return literal;
    case AST_TYPE_MODE_LONG:
        if (cast->data.num.is_signed) {
            literal.is_float = false, literal.is_signed = child->is_signed,
            literal.value.s = (signed long)(child->is_float ? child->value.d
                                                            : child->value.s);
            return literal;
        }
        literal.is_float = false;
        literal.is_signed = child->is_signed;
        literal.value.s = (unsigned long)(child->is_float ? child->value.d
                                                          : child->value.u);
        return literal;
    case AST_TYPE_MODE_FLOAT:
        literal.is_float = true;
        literal.is_signed = child->is_signed;
        literal.value.d = (float)(child->is_float
                ? child->value.d
                : (child->is_signed ? child->value.s : (float)child->value.u));
        return literal;
    case AST_TYPE_MODE_DOUBLE:
        literal.is_float = true;
        literal.is_signed = child->is_signed;
        literal.value.d = (double)(child->is_float
                ? child->value.d
                : (child->is_signed ? child->value.s : (double)child->value.u));
        return literal;
    default:
        cc_ast_print(node);
        cc_diag_error(ctx, "Unknown unop %i for consteval", node->data.unop.op);
        break;
    }
error_handle:
    return literal_zero;
}

static cc_ast_literal cc_ceval_eval_1(
    cc_context* ctx, cc_ast_node* node, cc_ceval_io** list, size_t* n_list)
{
    cc_ast_literal lhs, rhs, child;
    cc_ast_literal literal_zero = { 0 };
    if (node == NULL)
        goto error_handle;

    switch (node->type) {
    case AST_NODE_VARIABLE: {
        const cc_ast_variable* var;
        size_t i;
        for (i = 0; i < *n_list; i++)
            if ((*list)[i].name == node->data.var.name)
                return (*list)[i].literal;
        var = cc_ast_find_variable(
            cc_get_cfunc_name(ctx), node->data.var.name, node);
        if ((var->storage & AST_STORAGE_CONSTEXPR) == 0) {
            cc_diag_warning(ctx,
                "Non-constexpr variable '%s' used in constexpr",
                cc_strview(var->name));
            goto error_handle;
        }
        if (var != NULL && var->initializer != NULL)
            return cc_ceval_eval_1(ctx, var->initializer, list, n_list);
        cc_diag_warning(ctx, "Unable to constexpr variable, defaulting to 0");
    } break;
    case AST_NODE_BINOP:
        lhs = cc_ceval_eval_1(ctx, node->data.binop.left, list, n_list);
        rhs = cc_ceval_eval_1(ctx, node->data.binop.right, list, n_list);
        switch (node->data.binop.op) {
#define CEVAL_OPERATOR(type, op)                                               \
    case type: {                                                               \
        cc_ast_literal literal = { 0 };                                        \
        if (lhs.is_float && rhs.is_float) {                                    \
            literal.is_signed = false;                                         \
            literal.is_float = true;                                           \
            literal.value.d = lhs.value.d op rhs.value.d;                      \
            return literal;                                                    \
        } else if (!lhs.is_float && rhs.is_float) {                            \
            if (lhs.is_signed) {                                               \
                literal.is_signed = true;                                      \
                literal.is_float = false;                                      \
                literal.value.s = lhs.value.s op rhs.value.d;                  \
                return literal;                                                \
            }                                                                  \
            literal.is_signed = false;                                         \
            literal.is_float = false;                                          \
            literal.value.u = lhs.value.u op rhs.value.d;                      \
            return literal;                                                    \
        } else if (lhs.is_float && !rhs.is_float) {                            \
            if (rhs.is_signed) {                                               \
                literal.is_signed = true;                                      \
                literal.is_float = true;                                       \
                literal.value.d = lhs.value.d op rhs.value.s;                  \
                return literal;                                                \
            }                                                                  \
            literal.is_signed = false;                                         \
            literal.is_float = true;                                           \
            literal.value.d = lhs.value.d op rhs.value.u;                      \
            return literal;                                                    \
        } else if (!lhs.is_float && !rhs.is_float) {                           \
            if (lhs.is_signed && rhs.is_signed) {                              \
                literal.is_signed = true;                                      \
                literal.is_float = false;                                      \
                literal.value.s = lhs.value.s op rhs.value.s;                  \
                return literal;                                                \
            } else if (lhs.is_signed && !rhs.is_signed) {                      \
                literal.is_signed = true;                                      \
                literal.is_float = false;                                      \
                literal.value.s = lhs.value.s op(long signed int) rhs.value.u; \
                return literal;                                                \
            } else if (!lhs.is_signed && rhs.is_signed) {                      \
                literal.is_signed = true;                                      \
                literal.is_float = false;                                      \
                literal.value.s = (long signed int)lhs.value.u op rhs.value.s; \
                return literal;                                                \
            }                                                                  \
            literal.is_signed = false;                                         \
            literal.is_float = false;                                          \
            literal.value.u = lhs.value.u op rhs.value.u;                      \
            return literal;                                                    \
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
#define CEVAL_OPERATOR(type, op)                                               \
    case type: {                                                               \
        cc_ast_literal literal = { 0 };                                        \
        if ((lhs.is_float && rhs.is_float) || (!lhs.is_float && rhs.is_float)  \
            || (lhs.is_float && !rhs.is_float))                                \
            cc_diag_error(ctx, "Modulous on float literal");                   \
        literal.is_signed = lhs.is_signed;                                     \
        literal.is_float = false,                                              \
        literal.value.u = lhs.value.u op rhs.value.u;                          \
        return literal;                                                        \
    } break
            CEVAL_OPERATOR(AST_BINOP_MOD, %);
            CEVAL_OPERATOR(AST_BINOP_LSHIFT, <<);
            CEVAL_OPERATOR(AST_BINOP_RSHIFT, >>);
#undef CEVAL_OPERATOR
        default:
            cc_diag_error(ctx, "Unrecognized binop %i", node->data.binop.op);
            break;
        }
        break;
    case AST_NODE_LITERAL:
        return node->data.literal;
    case AST_NODE_CALL: {
        cc_ast_variable* var;
        size_t i;

        assert(node->data.call.call_expr->type == AST_NODE_VARIABLE);
        var = cc_ast_find_variable(cc_get_cfunc_name(ctx),
            node->data.call.call_expr->data.var.name, node);
        assert(var != NULL && var->type.mode == AST_TYPE_MODE_FUNCTION);

        assert(var->type.data.func.n_params == node->data.call.n_params);
        for (i = 0; i < var->type.data.func.n_params; i++) {
            cc_ceval_io ceval_io = { 0 };
            ceval_io.name = var->type.data.func.params[i].name;
            ceval_io.literal = cc_ceval_eval(ctx, &node->data.call.params[i]);

            *list = cc_realloc_array(*list, *n_list + 1);
            (*list)[(*n_list)++] = ceval_io;
        }
        return cc_ceval_eval_1(ctx, var->body, list, n_list);
    }
    case AST_NODE_RETURN:
        return cc_ceval_eval_1(ctx, node->data.return_expr, list, n_list);
    case AST_NODE_UNOP:
        child = cc_ceval_eval_1(ctx, node->data.unop.child, list, n_list);
        switch (node->data.unop.op) {
        case AST_UNOP_CAST:
            return cc_ceval_eval_cast_unop(ctx, node, &child);
        default:
            break;
        }
        cc_ast_print(node);
        cc_diag_error(ctx, "Unknown unary node %i for consteval", node->type);
        break;
    case AST_NODE_BLOCK:
        /* Evaluate the last relevant operation, so:
            return 1, 2;
        Is evaluated as: (return 2;) */
        if (node->data.block.n_children == 0) {
            cc_diag_error(ctx, "Empty block for consteval");
            goto error_handle;
        }
        return cc_ceval_eval_1(ctx,
            &node->data.block.children[node->data.block.n_children - 1], list,
            n_list);
    case AST_NODE_IF:
        child = cc_ceval_eval_1(ctx, node->data.if_expr.cond, list, n_list);
        if (child.is_float
                ? child.value.d != 0.f
                : (child.is_signed ? child.value.s != 0 : child.value.u != 0))
            return cc_ceval_eval_1(ctx, node->data.if_expr.block, list, n_list);
        return cc_ceval_eval_1(ctx, node->data.if_expr.tail_else, list, n_list);
    default:
        cc_ast_print(node);
        cc_diag_error(ctx, "Unknown node %i for consteval", node->type);
        break;
    }
error_handle:
    return literal_zero;
}

cc_ast_literal cc_ceval_eval(cc_context* ctx, cc_ast_node* node)
{
    cc_ceval_io* list = NULL;
    size_t n_list = 0;
    cc_ast_literal literal = cc_ceval_eval_1(ctx, node, &list, &n_list);
    const cc_ast_variable* old_fn = ctx->ast_current_func;
    cc_free(list);
    ctx->ast_current_func = old_fn;
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
        cc_abort(__FILE__, __LINE__);
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

    if (cc_ceval_get_rank(src) >= cc_ceval_get_rank(dest))
        *dest = *src;
}

/* Deduces a type from a node, the type written into type is a non-owning
   view of the real type, hence use cc_ast_copy_type to safely obtain a
   owning version if desired.
   The as_func parameter will return the type of the call function instead
   of it's returning value. */
bool cc_ceval_deduce_type_1(
    cc_context* ctx, const cc_ast_node* node, cc_ast_type* type, bool as_func)
{
    switch (node->type) {
    case AST_NODE_VARIABLE: {
        cc_ast_variable* var = cc_ast_find_variable(
            cc_get_cfunc_name(ctx), node->data.var.name, node);
        cc_ceval_promote_type(type, &var->type);

        if (type->mode == AST_TYPE_MODE_FUNCTION
            || type->mode == AST_TYPE_MODE_STRUCT
            || type->mode == AST_TYPE_MODE_UNION)
            assert(type->data.shared != NULL);
        return true;
    }
    case AST_NODE_CALL: {
        cc_ast_type tmp_type = { 0 };
        if (!cc_ceval_deduce_type_1(
                ctx, node->data.call.call_expr, &tmp_type, as_func))
            return false;
        assert(tmp_type.mode == AST_TYPE_MODE_FUNCTION
            && tmp_type.data.func.return_type != NULL);
        *type = as_func ? tmp_type : *tmp_type.data.func.return_type;
        return true;
    }
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
            return cc_ceval_deduce_type_1(
                ctx, node->data.binop.left, type, as_func);
        /*cc_ceval_deduce_type_1(ctx, node->data.binop.right, type, as_func);*/
        return cc_ceval_deduce_type_1(
            ctx, node->data.binop.left, type, as_func);
    case AST_NODE_FIELD_ACCESS: {
        cc_ast_variable* field_var;
        cc_ast_type vtype = { 0 };
        if (!cc_ceval_deduce_type_1(
                ctx, node->data.field_access.left, &vtype, as_func)) {
            cc_diag_error(ctx, "Can't deduce type of left struct");
            goto error_handle;
        }
        if ((field_var = cc_ast_get_field_of(
                 &vtype, node->data.field_access.field_name))
            == NULL) {
            cc_diag_error(ctx, "Unable to obtain field '%s'",
                node->data.field_access.field_name);
            goto error_handle;
        }
        *type = field_var->type;
        return true;
    }
    case AST_NODE_UNOP:
        if (node->data.unop.op == AST_UNOP_CAST) {
            *type = node->data.unop.cast;
            return true;
        } else if (node->data.unop.op == AST_UNOP_DEREF) {
            if (!cc_ceval_deduce_type_1(
                    ctx, node->data.unop.child, type, as_func))
                return false;
            /* Dereference type */
            if (type->n_cv_qual == 0) {
                cc_diag_error(ctx, "Dereferencing non-pointer type");
                goto error_handle;
            }
            type->n_cv_qual--;
            return true;
        } else if (node->data.unop.op == AST_UNOP_REF) {
            if (!cc_ceval_deduce_type_1(
                    ctx, node->data.unop.child, type, as_func))
                return false;
            /* Reference type */
            type->cv_qual[type->n_cv_qual++] = type->cv_qual[0];
            if (type->n_cv_qual >= MAX_CV_QUALIFIERS) {
                cc_diag_error(ctx, "Deduced type exceeds pointer depth size");
                goto error_handle;
            }
            return true;
        }
        return cc_ceval_deduce_type_1(
            ctx, node->data.unop.child, type, as_func);
    case AST_NODE_BLOCK:
        if (!node->data.block.n_children)
            return true;
        return cc_ceval_deduce_type_1(ctx,
            &node->data.block.children[node->data.block.n_children - 1], type,
            as_func);
    case AST_NODE_IF:
        /* Both elements "returned" by the if expression should be of the
           same type. So we just have to test the block itself.
           The condition is just for determining what value will be choosen
           from the branching paths, the type of the value itself is what
           we wish to obtain */
        if (node->data.if_expr.block != NULL)
            return cc_ceval_deduce_type_1(
                ctx, node->data.if_expr.block, type, as_func);
        else if (node->data.if_expr.tail_else != NULL)
            return cc_ceval_deduce_type_1(
                ctx, node->data.if_expr.tail_else, type, as_func);
        else
            cc_abort(__FILE__, __LINE__);
        break;
    case AST_NODE_MIRROR:
        return cc_ceval_deduce_type_1(ctx, node->data.mirror_expr, type, as_func);
    default:
        cc_abort(__FILE__, __LINE__);
    }
error_handle:
    return false;
}

bool cc_ceval_deduce_type(
    cc_context* ctx, const cc_ast_node* node, cc_ast_type* type)
{
    return cc_ceval_deduce_type_1(ctx, node, type, false);
}

static bool cc_ceval_var_is_const(cc_context* ctx, const cc_ast_variable* var)
{
    assert(var != NULL);
    return (var->storage & AST_STORAGE_CONSTEXPR) != 0;
}

bool cc_ceval_is_const(cc_context* ctx, const cc_ast_node* node)
{
    switch (node->type) {
    case AST_NODE_LITERAL:
        return true;
    case AST_NODE_VARIABLE: {
        const cc_ast_variable* var = cc_ast_find_variable(
            cc_get_cfunc_name(ctx), node->data.var.name, node);
        return cc_ceval_var_is_const(ctx, var);
    }
    case AST_NODE_BINOP:
        return cc_ceval_is_const(ctx, node->data.binop.left)
            && cc_ceval_is_const(ctx, node->data.binop.right);
    case AST_NODE_CALL: {
        const cc_ast_variable* var;
        /* Save old current function... */
        const cc_ast_variable* old_fn = ctx->ast_current_func;
        bool r; /* Return value*/
        size_t i;

        assert(node->data.call.call_expr->type == AST_NODE_VARIABLE);
        var = cc_ast_find_variable(cc_get_cfunc_name(ctx),
            node->data.call.call_expr->data.var.name, node);
        assert(var != NULL && var->type.mode == AST_TYPE_MODE_FUNCTION);
        /* Function may have side effects... */
        if (var->body == NULL)
            return false;

        ctx->ast_current_func = var;
        r = cc_ceval_is_const(ctx, var->body);
        ctx->ast_current_func = old_fn;
        /* Check call parameters being const too */
        for (i = 0; i < node->data.call.n_params && r; ++i)
            r = r && cc_ceval_is_const(ctx, &node->data.call.params[i]);
        return r;
    }
    case AST_NODE_RETURN:
        return cc_ceval_is_const(ctx, node->data.return_expr);
    case AST_NODE_UNOP:
        return cc_ceval_is_const(ctx, node->data.unop.child);
    case AST_NODE_BLOCK:
        if (node->data.block.n_children == 0)
            return true;
        return cc_ceval_is_const(
            ctx, &node->data.block.children[node->data.block.n_children - 1]);
    case AST_NODE_IF:
        return cc_ceval_is_const(ctx, node->data.if_expr.tail_else)
            && cc_ceval_is_const(ctx, node->data.if_expr.block);
    case AST_NODE_FIELD_ACCESS: {
        cc_ast_type su_type = { 0 };
        cc_ast_variable* var;
        if (!cc_ceval_deduce_type(ctx, node->data.field_access.left, &su_type))
            return false;
        var = cc_ast_get_field_of(&su_type, node->data.field_access.field_name);
        return cc_ceval_var_is_const(ctx, var)
            && cc_ceval_is_const(ctx, node->data.field_access.left);
    }
    case AST_NODE_MIRROR:
        return cc_ceval_is_const(ctx, node->data.mirror_expr);
    default:
        cc_abort(__FILE__, __LINE__);
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
