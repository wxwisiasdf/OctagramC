#include "ssa.h"
#include "ast.h"
#include "constevl.h"
#include "util.h"
#include <assert.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>

static void cc_ssa_print_param(const cc_ssa_param* param)
{
    printf("%s%u ", param->is_signed ? "i" : "u", param->size * 8);

    switch (param->type) {
    case SSA_PARAM_CONSTANT:
        if (param->data.constant.is_negative)
            printf("-");
        if (param->data.constant.is_float)
            printf("%Lf", (long double)param->data.constant.value.d);
        else
            printf("%lu", param->data.constant.value.u);
        break;
    case SSA_PARAM_STRING_LITERAL:
        printf("string_%u_\"%s\"", param->data.string.tmpid,
            cc_strview(param->data.string.literal));
        break;
    case SSA_PARAM_VARIABLE:
        printf("var_%s", cc_strview(param->data.var_name));
        break;
    case SSA_PARAM_TMPVAR:
        printf("tmp_%i", param->data.tmpid);
        break;
    case SSA_PARAM_NONE:
        printf("none");
        break;
    case SSA_PARAM_LABEL:
        printf("L_%u", param->data.label_id);
        break;
    default:
        cc_abort(__FILE__, __LINE__);
    }
}

static void cc_ssa_print_token_unop(const cc_ssa_token* tok, const char* name)
{
    cc_ssa_print_param(&tok->data.unop.left);
    printf(" = %s ", name);
    cc_ssa_print_param(&tok->data.unop.right);
}

static void cc_ssa_print_token_binop(const cc_ssa_token* tok, const char* name)
{
    printf(
        "x%u tmp_%u = ", tok->data.binop.size * 8, tok->data.binop.left_tmpid);
    cc_ssa_print_param(&tok->data.binop.right);
    printf(" %s ", name);
    cc_ssa_print_param(&tok->data.binop.extra);
}

static void cc_ssa_print_token(const cc_ssa_token* tok)
{
    if (tok->type == SSA_TOKEN_LABEL) {
        printf("L_%u:\n", tok->data.label_id);
        return;
    }

    printf("\t");
    switch (tok->type) {
    case SSA_TOKEN_ADD:
        cc_ssa_print_token_binop(tok, "add");
        break;
    case SSA_TOKEN_ALLOCA:
        cc_ssa_print_param(&tok->data.alloca.left);
        printf(" = alloca size ");
        cc_ssa_print_param(&tok->data.alloca.size);
        printf(", align ");
        cc_ssa_print_param(&tok->data.alloca.align);
        break;
    case SSA_TOKEN_AND:
        cc_ssa_print_token_binop(tok, "and");
        break;
    case SSA_TOKEN_BRANCH:
        printf("branch ");
        cc_ssa_print_param(&tok->data.branch.eval);
        printf(", onTrue ");
        cc_ssa_print_param(&tok->data.branch.t_branch);
        printf(", onFalse ");
        cc_ssa_print_param(&tok->data.branch.f_branch);
        break;
    case SSA_TOKEN_CALL: {
        size_t i;
        cc_ssa_print_param(&tok->data.call.left);
        printf(" = call ");
        cc_ssa_print_param(&tok->data.call.right);
        printf("(");
        for (i = 0; i < tok->data.call.n_params; i++) {
            cc_ssa_print_param(&tok->data.call.params[i]);
            printf(", ");
        }
        printf(")");
    } break;
    case SSA_TOKEN_COMPARE:
        cc_ssa_print_token_binop(tok, "cmp");
        break;
    case SSA_TOKEN_DIV:
        cc_ssa_print_token_binop(tok, "div");
        break;
    case SSA_TOKEN_GET_ELEMENT:
        cc_ssa_print_token_binop(tok, "get_element");
        break;
    case SSA_TOKEN_SET_ELEMENT:
        cc_ssa_print_token_binop(tok, "set_element");
        break;
    case SSA_TOKEN_STORE_FROM:
        cc_ssa_print_token_unop(tok, "store_from");
        break;
    case SSA_TOKEN_ASSIGN:
        printf("u%u tmp_%u = ", (unsigned int)tok->data.load.size * 8,
            (unsigned int)tok->data.load.val_tmpid);
        cc_ssa_print_param(&tok->data.load.addr);
        break;
    case SSA_TOKEN_LOAD_FROM:
        printf("u%u tmp_%u = load_from ", (unsigned int)tok->data.load.size * 8,
            (unsigned int)tok->data.load.val_tmpid);
        cc_ssa_print_param(&tok->data.load.addr);
        break;
    case SSA_TOKEN_LSHIFT:
        cc_ssa_print_token_binop(tok, "lshift");
        break;
    case SSA_TOKEN_MUL:
        cc_ssa_print_token_binop(tok, "mul");
        break;
    case SSA_TOKEN_NOT:
        cc_ssa_print_token_binop(tok, "not");
        break;
    case SSA_TOKEN_OR:
        cc_ssa_print_token_binop(tok, "or");
        break;
    case SSA_TOKEN_PHI:
        cc_ssa_print_token_binop(tok, "phi");
        break;
    case SSA_TOKEN_MOD:
        cc_ssa_print_token_binop(tok, "mod");
        break;
    case SSA_TOKEN_RET:
        printf("ret ");
        cc_ssa_print_param(&tok->data.retval);
        break;
    case SSA_TOKEN_RSHIFT:
        cc_ssa_print_token_binop(tok, "rshift");
        break;
    case SSA_TOKEN_SUB:
        cc_ssa_print_token_binop(tok, "sub");
        break;
    case SSA_TOKEN_XOR:
        cc_ssa_print_token_binop(tok, "xor");
        break;
    case SSA_TOKEN_GT:
        cc_ssa_print_token_binop(tok, "gt");
        break;
    case SSA_TOKEN_GTE:
        cc_ssa_print_token_binop(tok, "gte");
        break;
    case SSA_TOKEN_LT:
        cc_ssa_print_token_binop(tok, "lt");
        break;
    case SSA_TOKEN_LTE:
        cc_ssa_print_token_binop(tok, "lte");
        break;
    case SSA_TOKEN_EQ:
        cc_ssa_print_token_binop(tok, "eq");
        break;
    case SSA_TOKEN_NEQ:
        cc_ssa_print_token_binop(tok, "neq");
        break;
    case SSA_TOKEN_DROP:
        printf("drop tmp_%u", (unsigned int)tok->data.dropped_tmpid);
        break;
    case SSA_TOKEN_JUMP:
        printf("jump ");
        cc_ssa_print_param(&tok->data.jump_target);
        break;
    default:
        cc_abort(__FILE__, __LINE__);
    }
    printf("\n");
}

static void cc_ssa_print_func(const cc_ssa_func* func)
{
    size_t i;
    printf("fn %s {\n", cc_strview(func->ast_var->name));
    for (i = 0; i < func->n_tokens; i++)
        cc_ssa_print_token(&func->tokens[i]);
    printf("}\n");
}

static void cc_ssa_print(const cc_context* ctx)
{
    size_t i;
    for (i = 0; i < ctx->n_ssa_funcs; i++)
        cc_ssa_print_func(&ctx->ssa_funcs[i]);
}

static enum cc_ssa_storage cc_ssa_ast_storage_to_ssa(enum cc_ast_storage v)
{
    enum cc_ssa_storage o = 0;
    if ((v & AST_STORAGE_STATIC) != 0) {
        o |= SSA_STORAGE_INTERNAL;
    } else {
        o |= (v & AST_STORAGE_EXTERN) != 0 ? SSA_STORAGE_EXTERN : 0;
        o |= (v & AST_STORAGE_GLOBAL) != 0 ? SSA_STORAGE_GLOBAL : 0;
    }
    o |= (v & AST_STORAGE_AUTO) != 0 ? SSA_STORAGE_STACK : 0;
    o |= (v & AST_STORAGE_INLINE) != 0 ? SSA_STORAGE_INLINE : 0;
    o |= (v & AST_STORAGE_THREAD_LOCAL) != 0 ? SSA_STORAGE_THREAD_LOCAL : 0;
    return o;
}

static cc_ssa_param cc_ssa_literal_to_param(const cc_ast_literal* literal)
{
    cc_ssa_param tmp_param = { 0 };
    cc_ssa_constant tmp_const = { 0 };
    tmp_param.type = SSA_PARAM_CONSTANT;
    tmp_param.size = 4;
    if (literal->is_float) {
        tmp_const.is_float = true;
        tmp_const.value.d = literal->value.d;
        tmp_param.data.constant = tmp_const;
        return tmp_param;
    }
    if (literal->is_signed) {
        tmp_const.is_float = false;
        tmp_const.is_negative = literal->value.s < 0;
#define ABS(x) (x > 0) ? (x) : -(x)
        tmp_const.value.u = (unsigned long)ABS(literal->value.s);
#undef ABS
        tmp_param.data.constant = tmp_const;
        return tmp_param;
    }
    tmp_const.is_float = false;
    tmp_const.is_negative = false;
    tmp_const.value.u = literal->value.u;
    tmp_param.data.constant = tmp_const;
    return tmp_param;
}

/* Obtain the SSA parameter for the given variable - we always use a pointer
   to point to the variable - then we can materialize said pointers
   into something else after SSA, but for now they will be materialized
   into pointers. */
static cc_ssa_param cc_ssa_variable_to_param(
    cc_context* ctx, const cc_ast_variable* var)
{
    cc_ssa_param tmp = { 0 };
    tmp.type = SSA_PARAM_VARIABLE;
    tmp.storage = cc_ssa_ast_storage_to_ssa(var->storage);
    tmp.data.var_name = var->name;
    tmp.is_signed = false;
    tmp.size = ctx->get_sizeof(ctx, &var->type);
    return tmp;
}

static cc_ssa_param cc_ssa_label_param(cc_context* ctx, unsigned short label_id)
{
    cc_ssa_param tmp = { 0 };
    tmp.type = SSA_PARAM_LABEL;
    tmp.storage = SSA_STORAGE_STACK;
    tmp.data.label_id = label_id;
    tmp.is_signed = false;
    tmp.size = 4;
    return tmp;
}

static unsigned short cc_ssa_get_unique_tmpid(cc_context* ctx)
{
    assert(ctx->ssa_tmpid + 1 < USHRT_MAX);
    return ++ctx->ssa_tmpid;
}

/* Temporary variable parameter */
cc_ssa_param cc_ssa_tempvar_param_1(
    cc_context* ctx, bool is_signed, unsigned short size)
{
    cc_ssa_param tmp = { 0 };
    tmp.type = SSA_PARAM_TMPVAR;
    tmp.storage = SSA_STORAGE_STACK;
    tmp.data.tmpid = cc_ssa_get_unique_tmpid(ctx);
    tmp.is_signed = is_signed;
    tmp.size = size;
    return tmp;
}

cc_ssa_param cc_ssa_tempvar_param(
    cc_context* ctx, const struct cc_ast_type* base_type)
{
    /* Kludge for arrays to work properly... */
    if (base_type->cv_qual[base_type->n_cv_qual].is_array) {
        cc_ast_type tmp_type = *base_type;
        ++tmp_type.n_cv_qual;
        return cc_ssa_tempvar_param_1(ctx, base_type->data.num.is_signed,
            ctx->get_sizeof(ctx, &tmp_type));
    }
    return cc_ssa_tempvar_param_1(
        ctx, base_type->data.num.is_signed, ctx->get_sizeof(ctx, base_type));
}

static void cc_ssa_push_token(
    cc_context* ctx, cc_ssa_func* func, cc_ssa_token tok)
{
    assert(tok.type != SSA_TOKEN_NONE);
    if (func->n_tokens + 1 >= func->n_alloc_tokens) {
        func->n_alloc_tokens
            = func->n_tokens + ctx->alloc_reserve_factor / sizeof(cc_ssa_token);
        func->tokens = cc_realloc_array(func->tokens, func->n_alloc_tokens);
    }
    func->tokens[func->n_tokens++] = tok;
    ctx->ssa_current_tok = &func->tokens[func->n_tokens - 1];
}

static void cc_ssa_from_ast(
    cc_context* ctx, const cc_ast_node* node, cc_ssa_param param);

static void cc_ssa_process_binop(
    cc_context* ctx, const cc_ast_node* node, cc_ssa_param param)
{
    cc_ssa_token tok = { 0 };
    cc_ast_type lhs_type = { 0 };
    cc_ast_type rhs_type = { 0 };
    cc_ssa_param lhs_param;
    cc_ssa_param rhs_param;

    if (!cc_ceval_deduce_type(ctx, node->data.binop.left, &lhs_type))
        cc_abort(__FILE__, __LINE__);
    lhs_param = cc_ssa_tempvar_param(ctx, &lhs_type);
    if (!cc_ceval_deduce_type(ctx, node->data.binop.right, &rhs_type))
        cc_abort(__FILE__, __LINE__);
    rhs_param = cc_ssa_tempvar_param(ctx, &rhs_type);

    if (node->data.binop.op == AST_BINOP_COND_AND) {
        /* C uses short-circuit conditionals, if the first part of the AND
           is not true, the second term ISN'T evaluated! */
        /* So we will compose a control flow like this:
           
            tmp1 = left-side
            branch tmp1, onTrue L1, onFalse L2
        L1
            tmp2 = right-side
            branch tmp2, onTrue L3, onFalse L2
        L3
            __occ_cond = store_from 0
            jump L4
        L2
            __occ_cond = store_from 1
        L4
            param = load_from __occ_cond */
        cc_ssa_from_ast(ctx, node->data.binop.left, lhs_param);
        cc_ssa_from_ast(ctx, node->data.binop.right, rhs_param);
        tok.type = SSA_TOKEN_ADD;
        tok.data.binop.left_tmpid = param.data.tmpid;
        tok.data.binop.size = param.size;
        tok.data.binop.right = lhs_param;
        tok.data.binop.extra = rhs_param;
    } else if (node->data.binop.op == AST_BINOP_COND_OR) {
        /* C uses short-circuit conditionals, if the first part of the OR
           is not true we will evaluate the second one, if it was true
           then we don't need to evaluate the second one */
        /* So we will compose a control flow like this:
           
            tmp1 = left-side
            branch tmp1, onTrue L2, onFalse L1
        L1
            tmp2 = right-side
            branch tmp2, onTrue L2, onFalse L3
        L3
            __occ_cond = store_from 0
            jump L4
        L2
            __occ_cond = store_from 1
        L4
            param = load_from __occ_cond */
        cc_ssa_from_ast(ctx, node->data.binop.left, lhs_param);
        cc_ssa_from_ast(ctx, node->data.binop.right, rhs_param);
        tok.type = SSA_TOKEN_ADD;
        tok.data.binop.left_tmpid = param.data.tmpid;
        tok.data.binop.size = param.size;
        tok.data.binop.right = lhs_param;
        tok.data.binop.extra = rhs_param;
    } else if (node->data.binop.op == AST_BINOP_ADD
        && node->data.binop.op == AST_BINOP_SUB) {
        unsigned int lhs_psize = 0;
        unsigned int rhs_psize = 0;
        cc_ssa_param parith_param;
        cc_ssa_param tmp_param;

        /* Pointer with pointer arithmethic is illegal */
        if (lhs_type.n_cv_qual > 0 && rhs_type.n_cv_qual > 0)
            cc_abort(__FILE__, __LINE__);
        else {
            /* Obtain sizes from types iff they are pointers (to properly perform
            pointer arithmethic). */
            if (lhs_type.n_cv_qual > 0) {
                --lhs_type.n_cv_qual;
                lhs_psize = ctx->get_sizeof(ctx, &lhs_type);
                ++lhs_type.n_cv_qual;
            }
            if (rhs_type.n_cv_qual > 0) {
                --rhs_type.n_cv_qual;
                rhs_psize = ctx->get_sizeof(ctx, &rhs_type);
                ++rhs_type.n_cv_qual;
            }
        }

        if (!lhs_psize && !rhs_psize)
            goto non_pointer;

        tmp_param
            = cc_ssa_tempvar_param(ctx, lhs_psize ? &lhs_type : &rhs_type);

        cc_ssa_from_ast(
            ctx, node->data.binop.left, lhs_psize ? tmp_param : lhs_param);
        cc_ssa_from_ast(
            ctx, node->data.binop.right, lhs_psize ? rhs_param : tmp_param);

        memset(&tok, 0, sizeof(tok));
        tok.type = SSA_TOKEN_ASSIGN;
        if (lhs_psize) {
            assert(lhs_param.type == SSA_PARAM_TMPVAR);
            tok.data.load.val_tmpid = lhs_param.data.tmpid;
            tok.data.load.size = lhs_param.size;
        } else {
            assert(rhs_param.type == SSA_PARAM_TMPVAR);
            tok.data.load.val_tmpid = rhs_param.data.tmpid;
            tok.data.load.size = rhs_param.size;
        }
        tok.data.load.addr = tmp_param;
        cc_diag_copy(&tok.info, &node->info);
        cc_ssa_push_token(ctx, ctx->ssa_current_func, tok);
        memset(&tok, 0, sizeof(tok));
#define HANDLE_POINTER_ARITH()                                                 \
    do {                                                                       \
        cc_ast_literal sizeof_lit;                                             \
        cc_ssa_param tmplit_parm;                                              \
        sizeof_lit.is_float = false;                                           \
        sizeof_lit.is_signed = false;                                          \
        sizeof_lit.value.u = lhs_psize ? lhs_psize : rhs_psize;                \
        tmplit_parm = cc_ssa_literal_to_param(&sizeof_lit);                    \
        parith_param                                                           \
            = cc_ssa_tempvar_param(ctx, lhs_psize ? &lhs_type : &rhs_type);    \
        /* Assignment is an unop here */                                       \
        tok.type = SSA_TOKEN_MUL;                                              \
        tok.data.binop.left_tmpid = parith_param.data.tmpid;                   \
        tok.data.binop.size = parith_param.size;                               \
        tok.data.binop.right = lhs_psize ? rhs_param : lhs_param;              \
        tok.data.binop.extra = tmplit_parm;                                    \
        cc_diag_copy(&tok.info, &node->info);                                  \
        cc_ssa_push_token(ctx, ctx->ssa_current_func, tok);                    \
        memset(&tok, 0, sizeof(tok));                                          \
    } while (0)
        if (node->data.binop.op == AST_BINOP_ADD) {
            HANDLE_POINTER_ARITH();
            tok.type = SSA_TOKEN_ADD;
        } else if (node->data.binop.op == AST_BINOP_SUB) {
            HANDLE_POINTER_ARITH();
            tok.type = SSA_TOKEN_SUB;
        } else {
            cc_abort(__FILE__, __LINE__);
        }
#undef HANDLE_POINTER_ARITH
        assert(parith_param.type != SSA_PARAM_NONE);

        /* Pointer arithmethic with other pointers isn't allowed */
        assert((lhs_psize != 0 && rhs_psize == 0)
            || (lhs_psize == 0 && rhs_psize != 0)
            || (lhs_psize == 0 && rhs_psize == 0));

        tok.data.binop.left_tmpid = param.data.tmpid;
        tok.data.binop.size = param.size;
        tok.data.binop.right = lhs_psize == 0 ? lhs_param : parith_param;
        tok.data.binop.extra = rhs_psize == 0 ? rhs_param : parith_param;
    } else {
    non_pointer:
        if (node->data.binop.op == AST_BINOP_ASSIGN) {
            bool old_v = ctx->assign_lhs;
            ctx->assign_lhs = true;
            cc_ssa_from_ast(ctx, node->data.binop.left, lhs_param);
            ctx->assign_lhs = false;
            cc_ssa_from_ast(ctx, node->data.binop.right, rhs_param);
            ctx->assign_lhs = old_v;
        } else {
            cc_ssa_from_ast(ctx, node->data.binop.left, lhs_param);
            cc_ssa_from_ast(ctx, node->data.binop.right, rhs_param);
        }

        switch (node->data.binop.op) {
        case AST_BINOP_ADD:
            tok.type = SSA_TOKEN_ADD;
            break;
        case AST_BINOP_SUB:
            tok.type = SSA_TOKEN_SUB;
            break;
        case AST_BINOP_MUL:
            tok.type = SSA_TOKEN_MUL;
            break;
        case AST_BINOP_DIV:
            tok.type = SSA_TOKEN_DIV;
            break;
        case AST_BINOP_MOD:
            tok.type = SSA_TOKEN_MOD;
            break;
        case AST_BINOP_ASSIGN: {
            /* Generate a sequence of tokens so we store the data onto lhs */
            /* Assignment is an unop here */
            memset(&tok, 0, sizeof(tok));
            tok.type = SSA_TOKEN_STORE_FROM;
            tok.data.unop.left = lhs_param;
            tok.data.unop.right = rhs_param;
            cc_diag_copy(&tok.info, &node->info);
            cc_ssa_push_token(ctx, ctx->ssa_current_func, tok);
            /* Return result of assignment */
            if (param.type != SSA_PARAM_NONE) {
                assert(param.type == SSA_PARAM_TMPVAR);
                memset(&tok, 0, sizeof(tok));
                tok.type = SSA_TOKEN_ASSIGN;
                tok.data.load.val_tmpid = param.data.tmpid;
                tok.data.load.addr = rhs_param;
                cc_diag_copy(&tok.info, &node->info);
                cc_ssa_push_token(ctx, ctx->ssa_current_func, tok);
            }
            return;
        }
        case AST_BINOP_GT:
            tok.type = SSA_TOKEN_GT;
            break;
        case AST_BINOP_GTE:
            tok.type = SSA_TOKEN_GTE;
            break;
        case AST_BINOP_LT:
            tok.type = SSA_TOKEN_LT;
            break;
        case AST_BINOP_LTE:
            tok.type = SSA_TOKEN_LTE;
            break;
        case AST_BINOP_COND_EQ:
            tok.type = SSA_TOKEN_EQ;
            break;
        case AST_BINOP_COND_NEQ:
            tok.type = SSA_TOKEN_NEQ;
            break;
        case AST_BINOP_LSHIFT:
            tok.type = SSA_TOKEN_LSHIFT;
            break;
        case AST_BINOP_RSHIFT:
            tok.type = SSA_TOKEN_RSHIFT;
            break;
        case AST_BINOP_AND:
            tok.type = SSA_TOKEN_AND;
            break;
        case AST_BINOP_OR:
            tok.type = SSA_TOKEN_OR;
            break;
        case AST_BINOP_XOR:
            tok.type = SSA_TOKEN_XOR;
            break;
        default:
            cc_abort(__FILE__, __LINE__);
        }
        tok.data.binop.left_tmpid = param.data.tmpid;
        tok.data.binop.size = param.size;
        tok.data.binop.right = lhs_param;
        tok.data.binop.extra = rhs_param;
    }
    cc_diag_copy(&tok.info, &node->info);
    cc_ssa_push_token(ctx, ctx->ssa_current_func, tok);
}

static void cc_ssa_add_dummy_ret(cc_context* ctx, cc_ssa_func* func)
{
    /* Push a dummy return */
    cc_ssa_token tok = { 0 };
    tok.type = SSA_TOKEN_RET;
    cc_ssa_push_token(ctx, func, tok);
}

/* Helper function for cc_ssa_process_block_1 */
static void cc_ssa_process_block_2(cc_context* ctx, const cc_ast_node* node,
    cc_ssa_param param, const cc_ast_variable* var)
{
    bool is_comp = false; /* Is composite? (i.e multidimensional VLA) */
    size_t i;

    /* Global variables are handled by a ctor function! */
    assert(ctx->ssa_current_func != NULL);
    /* Non-VLA, non-array, so it's simple to handle */
    /*if (var->type.n_cv_qual == 0) {
        cc_ast_literal literal = { 0 };
        literal.is_float = literal.is_signed = false;
        literal.value.u = ctx->get_sizeof(ctx, &var->type);
        tok.data.alloca.size = cc_ssa_literal_to_param(&literal);
        cc_ssa_push_token(ctx, ctx->ssa_current_func, tok);
    } else {*/

    for (i = 0; i <= var->type.n_cv_qual; ++i)
        if (var->type.cv_qual[i].is_vla)
            is_comp = true;
    /* Composite multidimensionals require more effort to produce
        and more tokens are emitted */
    if (is_comp) {
        for (i = 0; i <= var->type.n_cv_qual; ++i) {
            /* TODO: Composite multidimensional VLA arrays support */
        }
    } else {
        cc_ssa_token tok = { 0 };
        cc_ast_literal literal = { 0 };
        cc_ast_type tmp_type = var->type;

        literal.is_float = literal.is_signed = false;

        /* Obtain the size of the primitive conforming the array. */
        assert(tmp_type.n_cv_qual > 0);
        literal.value.u = ctx->get_sizeof(ctx, &tmp_type);

        for (i = 0; i <= var->type.n_cv_qual; ++i) {
            if (!var->type.cv_qual[i].is_array)
                break;
            assert(var->type.cv_qual[i].is_vla == false);
            assert(var->type.cv_qual[i].array.size > 0);
            literal.value.u *= (unsigned int)var->type.cv_qual[i].array.size;
        }

        tok.type = SSA_TOKEN_ALLOCA;
        tok.data.alloca.left = cc_ssa_variable_to_param(ctx, var);
        tok.data.alloca.size = cc_ssa_literal_to_param(&literal);
        cc_diag_copy(&tok.info, &node->info);
        cc_ssa_push_token(ctx, ctx->ssa_current_func, tok);
    }
}
/* Helper function for cc_ssa_process_block */
static void cc_ssa_process_block_1(cc_context* ctx, const cc_ast_node* node,
    cc_ssa_param param, const cc_ast_variable* var)
{
    /* typedefs are a c-construct language that is irrelevant for codegen */
    if ((var->storage & AST_STORAGE_TYPEDEF) != 0)
        return;

    if (var->type.n_cv_qual > 0) {
        cc_ssa_process_block_2(ctx, node, param, var);
    } else if (var->type.mode == AST_TYPE_MODE_FUNCTION && var->body != NULL) {
        cc_ssa_func func = { 0 };
        cc_ssa_func* old_current_ssa_func = ctx->ssa_current_func;
        cc_ssa_param none_param = { 0 };
        bool old_func_has_return = ctx->func_has_return;

        func.ast_var = var;
        /* Enter into a new function contextee */
        ctx->ssa_current_func = &func;
        ctx->func_has_return = false;
        cc_ssa_from_ast(ctx, var->body, none_param);
        if (!ctx->func_has_return) {
            if (var->type.data.func.return_type->mode != AST_TYPE_MODE_VOID) {
                cc_diag_error(
                    ctx, "Function that returns non-void doesn't return");
            } else {
                cc_ssa_add_dummy_ret(ctx, ctx->ssa_current_func);
            }
        }
        ctx->func_has_return = old_func_has_return;
        ctx->ssa_current_func = old_current_ssa_func;

        /* Shrink array of tokens for this function */
        func.tokens = cc_realloc_array(func.tokens, func.n_tokens + 1);

        ctx->ssa_funcs = cc_realloc_array(ctx->ssa_funcs, ctx->n_ssa_funcs + 1);
        ctx->ssa_funcs[ctx->n_ssa_funcs++] = func;
    } else if(ctx->ssa_current_func != NULL) {
        enum cc_ssa_storage storage = cc_ssa_ast_storage_to_ssa(var->storage);
        if((storage & SSA_STORAGE_STACK) != 0) {
            cc_ssa_token tok = { 0 };
            cc_ast_literal literal = { 0 };
            literal.is_float = literal.is_signed = false;
            literal.value.u = ctx->get_sizeof(ctx, &var->type);
            tok.type = SSA_TOKEN_ALLOCA;
            tok.data.alloca.left = cc_ssa_variable_to_param(ctx, var);
            tok.data.alloca.size = cc_ssa_literal_to_param(&literal);
            cc_diag_copy(&tok.info, &node->info);
            cc_ssa_push_token(ctx, ctx->ssa_current_func, tok);
        }
    }
}

static void cc_ssa_process_block(
    cc_context* ctx, const cc_ast_node* node, cc_ssa_param param)
{
    size_t i;
    for (i = 0; i < node->data.block.n_vars; i++)
        cc_ssa_process_block_1(ctx, node, param, &node->data.block.vars[i]);
    for (i = 0; i < node->data.block.n_children; i++)
        cc_ssa_from_ast(ctx, &node->data.block.children[i], param);
}

static void cc_ssa_process_call(
    cc_context* ctx, const cc_ast_node* node, cc_ssa_param param)
{
    cc_ssa_token tok = { 0 };
    cc_ast_type call_retval_type = { 0 };
    cc_ssa_param call_retval_param;
    size_t i;

    if (!cc_ceval_deduce_type(
            ctx, node->data.call.call_expr, &call_retval_type))
        cc_abort(__FILE__, __LINE__);
    call_retval_param = cc_ssa_tempvar_param(ctx, &call_retval_type);
    cc_ssa_from_ast(ctx, node->data.call.call_expr, call_retval_param);

    tok.type = SSA_TOKEN_CALL;
    tok.data.call.left = param;
    tok.data.call.right = call_retval_param;
    for (i = 0; i < node->data.call.n_params; ++i) {
        cc_ast_type call_arg_type = { 0 };
        cc_ssa_param call_arg_param;
        if (!cc_ceval_deduce_type(
                ctx, &node->data.call.params[i], &call_arg_type))
            cc_abort(__FILE__, __LINE__);
        call_arg_param = cc_ssa_tempvar_param(ctx, &call_arg_type);
        cc_ssa_from_ast(ctx, &node->data.call.params[i], call_arg_param);

        tok.data.call.params = cc_realloc_array(
            tok.data.call.params, tok.data.call.n_params + 1);
        tok.data.call.params[tok.data.call.n_params++] = call_arg_param;
    }
    cc_diag_copy(&tok.info, &node->info);
    cc_ssa_push_token(ctx, ctx->ssa_current_func, tok);
}

static void cc_ssa_process_switch(
    cc_context* ctx, const cc_ast_node* node, cc_ssa_param param)
{
    cc_ast_type control_type = { 0 };
    cc_ssa_param control_param;
    if (!cc_ceval_deduce_type(
            ctx, node->data.switch_expr.control, &control_type))
        cc_abort(__FILE__, __LINE__);
    control_param = cc_ssa_tempvar_param(ctx, &control_type);
    cc_ssa_from_ast(ctx, node->data.switch_expr.control, control_param);
    cc_ssa_from_ast(ctx, node->data.switch_expr.block, param);
}

static void cc_ssa_process_if(
    cc_context* ctx, const cc_ast_node* node, cc_ssa_param param)
{
    cc_ast_type cond_type = { 0 };
    cc_ssa_param cond_param;

    /* Conditions will always be evaluated, for example when calling a
       function inside an if statment, we have to call it no matter what. */
    if (!cc_ceval_deduce_type(ctx, node->data.if_expr.cond, &cond_type))
        cc_abort(__FILE__, __LINE__);
    cond_param = cc_ssa_tempvar_param(ctx, &cond_type);
    cc_ssa_from_ast(ctx, node->data.if_expr.cond, cond_param);

    /* Skip effect-less if statment blocks */
    if (node->data.if_expr.block == NULL
        && node->data.if_expr.tail_else == NULL)
        return;
    else if (node->data.if_expr.block != NULL
        || node->data.if_expr.tail_else != NULL) {
        cc_ssa_token tok = { 0 };
        cc_ssa_token if_label_tok = { 0 };
        cc_ssa_token else_label_tok = { 0 };

        /* Both conditionals are present wherein... */
        if_label_tok.type = SSA_TOKEN_LABEL;
        if_label_tok.data.label_id = node->data.if_expr.block == NULL
            ? cc_ast_alloc_label_id(ctx)
            : node->data.if_expr.block->label_id;
        cc_diag_copy(&if_label_tok.info, &node->info);

        else_label_tok.type = SSA_TOKEN_LABEL;
        else_label_tok.data.label_id = node->data.if_expr.tail_else == NULL
            ? cc_ast_alloc_label_id(ctx)
            : node->data.if_expr.tail_else->label_id;
        cc_diag_copy(&else_label_tok.info, &node->info);

        /* Tail else is null, we will have to synthetize a label after this if */
        tok.type = SSA_TOKEN_BRANCH;
        tok.data.branch.eval = cond_param;
        tok.data.branch.t_branch
            = cc_ssa_label_param(ctx, if_label_tok.data.label_id);
        tok.data.branch.f_branch
            = cc_ssa_label_param(ctx, else_label_tok.data.label_id);
        cc_diag_copy(&tok.info, &node->info);
        cc_ssa_push_token(ctx, ctx->ssa_current_func, tok);

        cc_ssa_push_token(ctx, ctx->ssa_current_func, if_label_tok);
        cc_ssa_from_ast(ctx, node->data.if_expr.block, param);

        cc_ssa_push_token(ctx, ctx->ssa_current_func, else_label_tok);
        cc_ssa_from_ast(ctx, node->data.if_expr.tail_else, param);
    }
}

static void cc_ssa_process_jump(
    cc_context* ctx, const cc_ast_node* node, cc_ssa_param param)
{
    cc_ssa_token tok = { 0 };
    tok.type = SSA_TOKEN_JUMP;
    tok.data.jump_target = cc_ssa_label_param(ctx, node->data.jump_label_id);
    cc_diag_copy(&tok.info, &node->info);
    cc_ssa_push_token(ctx, ctx->ssa_current_func, tok);
}

static void cc_ssa_process_literal(
    cc_context* ctx, const cc_ast_node* node, cc_ssa_param param)
{
    cc_ssa_token tok = { 0 };
    assert(param.type == SSA_PARAM_TMPVAR);

    tok.type = SSA_TOKEN_ASSIGN;
    tok.data.load.val_tmpid = param.data.tmpid;
    tok.data.load.addr = cc_ssa_literal_to_param(&node->data.literal);
    cc_diag_copy(&tok.info, &node->info);
    cc_ssa_push_token(ctx, ctx->ssa_current_func, tok);
}

static void cc_ssa_process_return(
    cc_context* ctx, const cc_ast_node* node, cc_ssa_param param)
{
    const cc_ast_variable* var = ctx->ssa_current_func->ast_var;
    cc_ssa_token tok = { 0 };
    cc_ssa_param retval
        = cc_ssa_tempvar_param(ctx, var->type.data.func.return_type);

    assert(var->type.mode == AST_TYPE_MODE_FUNCTION);
    cc_ssa_from_ast(ctx, node->data.return_expr, retval);

    tok.type = SSA_TOKEN_RET;
    tok.data.retval = retval;
    cc_diag_copy(&tok.info, &node->info);
    cc_ssa_push_token(ctx, ctx->ssa_current_func, tok);
}

static void cc_ssa_process_string_literal(
    cc_context* ctx, const cc_ast_node* node, cc_ssa_param param)
{
    cc_ssa_token tok = { 0 };
    /* A const char* */
    cc_ast_type string_type = cc_ceval_get_string_type(ctx);
    cc_ssa_param literal_param;
    assert(node->type == AST_NODE_STRING_LITERAL);
    if (param.type != SSA_PARAM_NONE) {
        literal_param.type = SSA_PARAM_STRING_LITERAL;
        literal_param.size = ctx->get_sizeof(ctx, &string_type);
        literal_param.is_signed = false;
        literal_param.storage = SSA_STORAGE_STACK;
        literal_param.data.string.tmpid = cc_ssa_get_unique_tmpid(ctx);
        literal_param.data.string.literal = node->data.string_literal;

        assert(param.type == SSA_PARAM_TMPVAR);
        tok.type = SSA_TOKEN_ASSIGN;
        tok.data.load.val_tmpid = param.data.tmpid;
        tok.data.load.addr = literal_param;
        cc_diag_copy(&tok.info, &node->info);
        cc_ssa_push_token(ctx, ctx->ssa_current_func, tok);
    }
}

/* Post/prefix Increment/Decrement */
static void cc_ssa_process_unop_ppid(cc_context* ctx, const cc_ast_node* node,
    cc_ssa_param param, const cc_ast_type* child_type, bool is_inc, bool is_pre)
{
    cc_ssa_param one_param;
    cc_ssa_param child_param;
    cc_ast_literal one_literal = { 0 };
    cc_ssa_token tok = { 0 };

    one_literal.is_signed = one_literal.is_float = false;
    one_literal.value.u = 1;
    one_param = cc_ssa_literal_to_param(&one_literal);

    child_param = cc_ssa_tempvar_param(ctx, child_type);
    cc_ssa_from_ast(ctx, node->data.unop.child, child_param);
    if (is_pre) {
        /* First, add/sub a one from the children, then store it onto the
           higher parameter, and then store the value of the parameter
           onto the children. */
        tok.type = is_inc ? SSA_TOKEN_ADD : SSA_TOKEN_SUB;
        tok.data.binop.left_tmpid = param.data.tmpid;
        tok.data.binop.size = param.size;
        tok.data.binop.right = child_param;
        tok.data.binop.extra = one_param;
        cc_diag_copy(&tok.info, &node->info);
        cc_ssa_push_token(ctx, ctx->ssa_current_func, tok);

        memset(&tok, 0, sizeof(tok));
        tok.type = SSA_TOKEN_STORE_FROM;
        tok.data.unop.left = child_param;
        tok.data.unop.right = param;
        cc_diag_copy(&tok.info, &node->info);
        cc_ssa_push_token(ctx, ctx->ssa_current_func, tok);
    } else {
        /* First obtain the value from the children parameter and store it
           onto the parameter. */
        cc_ssa_param tmp_param = cc_ssa_tempvar_param(ctx, child_type);

        if (param.type != SSA_PARAM_NONE) {
            assert(param.type == SSA_PARAM_TMPVAR);
            tok.type = SSA_TOKEN_LOAD_FROM;
            tok.data.load.val_tmpid = param.data.tmpid;
            tok.data.load.size = param.size;
            tok.data.load.addr = child_param;
            cc_diag_copy(&tok.info, &node->info);
            cc_ssa_push_token(ctx, ctx->ssa_current_func, tok);

            memset(&tok, 0, sizeof(tok));
            tok.type = is_inc ? SSA_TOKEN_ADD : SSA_TOKEN_SUB;
            tok.data.binop.left_tmpid = param.data.tmpid;
            tok.data.binop.size = param.size;
            tok.data.binop.right = child_param;
            tok.data.binop.extra = one_param;
            cc_diag_copy(&tok.info, &node->info);
            cc_ssa_push_token(ctx, ctx->ssa_current_func, tok);

            memset(&tok, 0, sizeof(tok));
            tok.type = SSA_TOKEN_STORE_FROM;
            tok.data.unop.left = child_param;
            tok.data.unop.right = tmp_param;
            cc_diag_copy(&tok.info, &node->info);
            cc_ssa_push_token(ctx, ctx->ssa_current_func, tok);
        }
    }
}

static void cc_ssa_process_unop(
    cc_context* ctx, const cc_ast_node* node, cc_ssa_param param)
{
    cc_ssa_token tok = { 0 };
    cc_ast_type child_type = { 0 };
    if (!cc_ceval_deduce_type(ctx, node->data.unop.child, &child_type))
        cc_abort(__FILE__, __LINE__);

    switch (node->data.unop.op) {
    case AST_UNOP_CAST: {
        cc_ssa_param child_param = cc_ssa_tempvar_param(ctx, &child_type);
        cc_ssa_from_ast(ctx, node->data.unop.child, child_param);
        if (param.type != SSA_PARAM_NONE) {
            assert(param.type == SSA_PARAM_TMPVAR);
            tok.type = SSA_TOKEN_ASSIGN;
            tok.data.load.val_tmpid = param.data.tmpid;
            tok.data.load.addr = child_param;
            cc_diag_copy(&tok.info, &node->info);
            cc_ssa_push_token(ctx, ctx->ssa_current_func, tok);
        }
    } break;
    case AST_UNOP_DEREF: {
        cc_ssa_param child_param = cc_ssa_tempvar_param(ctx, &child_type);
        cc_ssa_from_ast(ctx, node->data.unop.child, child_param);
        if (param.type != SSA_PARAM_NONE) {
            assert(param.type == SSA_PARAM_TMPVAR);
            tok.type = SSA_TOKEN_LOAD_FROM;
            tok.data.load.val_tmpid = param.data.tmpid;
            tok.data.load.size = param.size;
            tok.data.load.addr = child_param;
            cc_diag_copy(&tok.info, &node->info);
            cc_ssa_push_token(ctx, ctx->ssa_current_func, tok);
        }
    } break;
    case AST_UNOP_POSTINC:
    case AST_UNOP_POSTDEC:
    case AST_UNOP_PREINC:
    case AST_UNOP_PREDEC: {
        bool is_inc = node->data.unop.op == AST_UNOP_POSTINC
            || node->data.unop.op == AST_UNOP_PREINC;
        bool is_pre = node->data.unop.op == AST_UNOP_PREINC
            || node->data.unop.op == AST_UNOP_PREDEC;
        cc_ssa_process_unop_ppid(ctx, node, param, &child_type, is_inc, is_pre);
    } break;
    case AST_UNOP_NOT:
    case AST_UNOP_REF:
    case AST_UNOP_COND_NOT: {
        /* TODO: Refs */
        cc_ssa_param child_param = cc_ssa_tempvar_param(ctx, &child_type);
        cc_ssa_from_ast(ctx, node->data.unop.child, child_param);

        assert(param.type == SSA_PARAM_TMPVAR);
        tok.type = SSA_TOKEN_LOAD_FROM;
        tok.data.load.val_tmpid = param.data.tmpid;
        tok.data.load.size = param.size;
        tok.data.load.addr = child_param;
        cc_diag_copy(&tok.info, &node->info);
        cc_ssa_push_token(ctx, ctx->ssa_current_func, tok);
    } break;
    default:
        cc_abort(__FILE__, __LINE__);
    }
}

static void cc_ssa_process_field_access(
    cc_context* ctx, const cc_ast_node* node, cc_ssa_param param)
{
    cc_ast_type left_type = { 0 };
    cc_ssa_param left_param = { 0 };
    cc_ast_variable* field_var;
    size_t field_offset;
    if (!cc_ceval_deduce_type(ctx, node->data.field_access.left, &left_type))
        cc_abort(__FILE__, __LINE__);
    left_param = cc_ssa_tempvar_param(ctx, &left_type);
    cc_ssa_from_ast(ctx, node->data.field_access.left, left_param);

    field_var
        = cc_ast_get_field_of(&left_type, node->data.field_access.field_name);
    assert(field_var != NULL);
    field_offset = ctx->get_offsetof(
        ctx, &left_type, node->data.field_access.field_name);
    if (field_offset) {
        cc_ssa_token tok = { 0 };
        cc_ssa_param size_param = { 0 };
        cc_ast_literal size_literal = { 0 };

        size_literal.is_float = size_literal.is_signed = false;
        size_literal.value.u = (unsigned int)field_offset;
        size_param = cc_ssa_literal_to_param(&size_literal);

        assert(param.type == SSA_PARAM_TMPVAR);
        tok.type = SSA_TOKEN_ADD;
        tok.data.binop.left_tmpid = param.data.tmpid;
        tok.data.binop.size = param.size;
        tok.data.binop.right = left_param;
        tok.data.binop.extra = size_param;
        cc_diag_copy(&tok.info, &node->info);
        cc_ssa_push_token(ctx, ctx->ssa_current_func, tok);
    }
}

static void cc_ssa_process_variable(
    cc_context* ctx, const cc_ast_node* node, cc_ssa_param param)
{
    cc_ssa_token tok = { 0 };
    const cc_ast_variable* var = cc_ast_find_variable(
        ctx->ssa_current_func->ast_var->name, node->data.var.name, node);

    assert(var != NULL);
    assert(param.type == SSA_PARAM_TMPVAR);

    if (ctx->assign_lhs) {
        tok.type = SSA_TOKEN_ASSIGN;
        tok.data.load.val_tmpid = param.data.tmpid;
        tok.data.load.size = param.size;
        tok.data.load.addr = cc_ssa_variable_to_param(ctx, var);
        cc_diag_copy(&tok.info, &node->info);
        cc_ssa_push_token(ctx, ctx->ssa_current_func, tok);
        return;
    }

    if (var->type.cv_qual[var->type.n_cv_qual].is_array) {
        /* Same variable, with an extra indirection so we obtain a pointer
           to this variable instead of the entire array. */
        cc_ast_variable tmp_var = *var;
        ++tmp_var.type.n_cv_qual;

        tok.type = SSA_TOKEN_ASSIGN;
        tok.data.load.val_tmpid = param.data.tmpid;
        tok.data.load.size = param.size;
        tok.data.load.addr = cc_ssa_variable_to_param(ctx, &tmp_var);
        cc_diag_copy(&tok.info, &node->info);
        cc_ssa_push_token(ctx, ctx->ssa_current_func, tok);
    } else if (!var->type.n_cv_qual
        && var->type.mode == AST_TYPE_MODE_FUNCTION) {
        /* var func_name would take the address of the function, and functions are
           in the compiler's eyes, a label to a section of code, so naturally we're
           not going to generate a pointer to every function. */
        tok.type = SSA_TOKEN_ASSIGN;
        tok.data.load.val_tmpid = param.data.tmpid;
        tok.data.load.size = param.size;
        tok.data.load.addr = cc_ssa_variable_to_param(ctx, var);
        cc_diag_copy(&tok.info, &node->info);
        cc_ssa_push_token(ctx, ctx->ssa_current_func, tok);
    } else {
        tok.type = SSA_TOKEN_LOAD_FROM;
        tok.data.load.val_tmpid = param.data.tmpid;
        tok.data.load.size = param.size;
        tok.data.load.addr = cc_ssa_variable_to_param(ctx, var);
        cc_diag_copy(&tok.info, &node->info);
        cc_ssa_push_token(ctx, ctx->ssa_current_func, tok);
    }
}

static void cc_ssa_process_label(cc_context* ctx, const cc_ast_node* node)
{
    cc_ssa_token tok = { 0 };
    tok.type = SSA_TOKEN_LABEL;
    tok.data.label_id = node->label_id;
    cc_diag_copy(&tok.info, &node->info);
    cc_ssa_push_token(ctx, ctx->ssa_current_func, tok);
}

/* For expressions of the type (lhs) = (rhs) */
static void cc_ssa_from_ast(
    cc_context* ctx, const cc_ast_node* node, cc_ssa_param param)
{
    if (node == NULL)
        return;

    if (!ctx->mirror_mode && node->ref_count > 0)
        cc_ssa_process_label(ctx, node);

    switch (node->type) {
    case AST_NODE_BINOP:
        cc_ssa_process_binop(ctx, node, param);
        break;
    case AST_NODE_BLOCK:
        cc_ssa_process_block(ctx, node, param);
        break;
    case AST_NODE_CALL:
        cc_ssa_process_call(ctx, node, param);
        break;
    case AST_NODE_SWITCH:
        cc_ssa_process_switch(ctx, node, param);
        break;
    case AST_NODE_IF:
        cc_ssa_process_if(ctx, node, param);
        break;
    case AST_NODE_JUMP:
        cc_ssa_process_jump(ctx, node, param);
        break;
    case AST_NODE_LITERAL:
        cc_ssa_process_literal(ctx, node, param);
        break;
    case AST_NODE_RETURN:
        ctx->func_has_return = true;
        cc_ssa_process_return(ctx, node, param);
        break;
    case AST_NODE_STRING_LITERAL:
        cc_ssa_process_string_literal(ctx, node, param);
        break;
    case AST_NODE_UNOP:
        cc_ssa_process_unop(ctx, node, param);
        break;
    case AST_NODE_VARIABLE:
        cc_ssa_process_variable(ctx, node, param);
        break;
    case AST_NODE_FIELD_ACCESS:
        cc_ssa_process_field_access(ctx, node, param);
        break;
    case AST_NODE_MIRROR: {
        bool old_v = ctx->mirror_mode;
        ctx->mirror_mode = true;
        cc_ssa_from_ast(ctx, node->data.mirror_expr, param);
        ctx->mirror_mode = old_v;
    } break;
    default:
        cc_abort(__FILE__, __LINE__);
    }
}

/* Helper function for cc_ssa_tmpassign_func_1 */
static void cc_ssa_tmpassign_param(cc_ssa_param* restrict param,
    unsigned short tmpid, const cc_ssa_param* restrict new_colour)
{
    if (param->type == SSA_PARAM_TMPVAR && param->data.tmpid == tmpid)
        *param = *new_colour;
}
/* Helper function for cc_ssa_tmpassign_func */
static void cc_ssa_tmpasign_func_1(char* restrict visited,
    cc_ssa_func* restrict func, unsigned short tmpid,
    const cc_ssa_param* restrict new_colour, size_t offset)
{
    size_t i;
    for (i = offset; i < func->n_tokens; ++i) {
        cc_ssa_token* tok = &func->tokens[i];
        switch (tok->type) {
        case SSA_TOKEN_STORE_FROM:
            cc_ssa_tmpassign_param(&tok->data.unop.left, tmpid, new_colour);
            cc_ssa_tmpassign_param(&tok->data.unop.right, tmpid, new_colour);
            break;
        case SSA_TOKEN_ASSIGN:
        case SSA_TOKEN_LOAD_FROM:
            if (tok->data.load.val_tmpid == tmpid
                && new_colour->type == SSA_PARAM_TMPVAR) {
                tok->data.load.val_tmpid = new_colour->data.tmpid;
                tok->data.load.size = new_colour->size;
            }
            cc_ssa_tmpassign_param(&tok->data.load.addr, tmpid, new_colour);
            break;
        case SSA_TOKEN_ADD:
        case SSA_TOKEN_SUB:
        case SSA_TOKEN_AND:
        case SSA_TOKEN_COMPARE:
        case SSA_TOKEN_DIV:
        case SSA_TOKEN_MUL:
        case SSA_TOKEN_OR:
        case SSA_TOKEN_XOR:
        case SSA_TOKEN_GT:
        case SSA_TOKEN_GTE:
        case SSA_TOKEN_LT:
        case SSA_TOKEN_LTE:
        case SSA_TOKEN_EQ:
        case SSA_TOKEN_NEQ:
        case SSA_TOKEN_LSHIFT:
        case SSA_TOKEN_RSHIFT:
        case SSA_TOKEN_MOD:
            if (tok->data.binop.left_tmpid == tmpid
                && new_colour->type == SSA_PARAM_TMPVAR) {
                tok->data.binop.left_tmpid = new_colour->data.tmpid;
                tok->data.binop.size = new_colour->size;
            }
            cc_ssa_tmpassign_param(&tok->data.binop.right, tmpid, new_colour);
            cc_ssa_tmpassign_param(&tok->data.binop.extra, tmpid, new_colour);
            break;
        case SSA_TOKEN_CALL: {
            size_t j;
            cc_ssa_tmpassign_param(&tok->data.call.left, tmpid, new_colour);
            cc_ssa_tmpassign_param(&tok->data.call.right, tmpid, new_colour);
            for (j = 0; j < tok->data.call.n_params; ++j)
                cc_ssa_tmpassign_param(
                    &tok->data.call.params[j], tmpid, new_colour);
        } break;
        case SSA_TOKEN_ALLOCA:
            cc_ssa_tmpassign_param(&tok->data.alloca.left, tmpid, new_colour);
            cc_ssa_tmpassign_param(&tok->data.alloca.size, tmpid, new_colour);
            cc_ssa_tmpassign_param(&tok->data.alloca.align, tmpid, new_colour);
            break;
        case SSA_TOKEN_JUMP:
            cc_ssa_tmpassign_param(&tok->data.jump_target, tmpid, new_colour);
            break;
        case SSA_TOKEN_BRANCH:
            cc_ssa_tmpassign_param(&tok->data.branch.eval, tmpid, new_colour);
            cc_ssa_tmpassign_param(
                &tok->data.branch.t_branch, tmpid, new_colour);
            cc_ssa_tmpassign_param(
                &tok->data.branch.f_branch, tmpid, new_colour);
            break;
        case SSA_TOKEN_RET:
            cc_ssa_tmpassign_param(&tok->data.retval, tmpid, new_colour);
            break;
        case SSA_TOKEN_LABEL:
            /* No operation */
            break;
        default:
            cc_abort(__FILE__, __LINE__);
        }
    }
}
/* Helper function for cc_ssa_tmpassign_func */
static void cc_ssa_tmpasign_func_2(char* restrict visited,
    cc_ssa_func* restrict func, unsigned short tmpid,
    const cc_ssa_token* binop_tok, size_t offset)
{
    size_t i;
    for (i = offset; i < func->n_tokens; ++i) {
        cc_ssa_token* tok = &func->tokens[i];
        switch (tok->type) {
        case SSA_TOKEN_ADD:
        case SSA_TOKEN_SUB:
        case SSA_TOKEN_AND:
        case SSA_TOKEN_COMPARE:
        case SSA_TOKEN_DIV:
        case SSA_TOKEN_MUL:
        case SSA_TOKEN_OR:
        case SSA_TOKEN_XOR:
        case SSA_TOKEN_GT:
        case SSA_TOKEN_GTE:
        case SSA_TOKEN_LT:
        case SSA_TOKEN_LTE:
        case SSA_TOKEN_EQ:
        case SSA_TOKEN_NEQ:
        case SSA_TOKEN_LSHIFT:
        case SSA_TOKEN_RSHIFT:
        case SSA_TOKEN_MOD:
            if (cc_ssa_is_param_same(
                    &binop_tok->data.binop.right, &tok->data.binop.right)
                && cc_ssa_is_param_same(
                    &binop_tok->data.binop.extra, &tok->data.binop.extra)) {
                cc_ssa_param tmp_param = { 0 };
                tmp_param.type = SSA_PARAM_TMPVAR;
                tmp_param.data.tmpid = binop_tok->data.binop.left_tmpid;
                tmp_param.size = binop_tok->data.binop.size;

                /* Same result, so replace it with the first temporal that
                   obtained this result (and keep reusing said tempoworal) */
                cc_ssa_tmpasign_func_1(visited, func,
                    tok->data.binop.left_tmpid, &tmp_param, i + 1);

                /* Remove this instance of computation */
                memmove(&func->tokens[i], &func->tokens[i + 1],
                    sizeof(cc_ssa_token) * (func->n_tokens - i - 1));
                func->n_tokens--;
                i--;
            }
            break;
        default:
            break;
        }
    }
}
/* Temporal assignment elimination

   "Paint" an existing temporal variable into another parameter, for example
   the following SSA:
   
   i32 tmp_0 = i32 var a
   i32 tmp_1 = i32 0
   i32 tmp_0 = i32 tmp_0 + i32 tmp_1
   
   Colouring allows us to replace them as:
   i32 var a = i32 var a
   i32 0 = i32 0
   i32 var a = i32 var a + i32 0 */
static void cc_ssa_tmpassign_func(
    char* restrict visited, cc_ssa_func* restrict func)
{
    size_t i;
    for (i = 0; i < func->n_tokens; ++i) {
        cc_ssa_token* vtok = &func->tokens[i];
        unsigned short tmpid = cc_ssa_get_lhs_tmpid(vtok);
        if (tmpid == 0)
            continue;

        if ((visited[tmpid / CHAR_BIT]) & (1 << (tmpid % CHAR_BIT)))
            continue;
        visited[tmpid / CHAR_BIT] |= 1 << (tmpid % CHAR_BIT);

        switch (vtok->type) {
        case SSA_TOKEN_ASSIGN:
            cc_ssa_tmpasign_func_1(
                visited, func, tmpid, &vtok->data.load.addr, i + 1);

            /* Remove the assignment token as we've tmpassigned every instance
            of this temporal away. */
            memmove(&func->tokens[i], &func->tokens[i + 1],
                sizeof(cc_ssa_token) * (func->n_tokens - i - 1));
            func->n_tokens--;
            i--;
            break;
        case SSA_TOKEN_LOAD_FROM:
            break;
        case SSA_TOKEN_ADD:
        case SSA_TOKEN_SUB:
        case SSA_TOKEN_AND:
        case SSA_TOKEN_COMPARE:
        case SSA_TOKEN_DIV:
        case SSA_TOKEN_MUL:
        case SSA_TOKEN_OR:
        case SSA_TOKEN_XOR:
        case SSA_TOKEN_GT:
        case SSA_TOKEN_GTE:
        case SSA_TOKEN_LT:
        case SSA_TOKEN_LTE:
        case SSA_TOKEN_EQ:
        case SSA_TOKEN_NEQ:
        case SSA_TOKEN_LSHIFT:
        case SSA_TOKEN_RSHIFT:
        case SSA_TOKEN_MOD: {
            cc_ssa_param tmp_param = { 0 };
            cc_ssa_tmpasign_func_2(visited, func, tmpid, vtok, i + 1);
            tmp_param.type = SSA_PARAM_TMPVAR;
            tmp_param.data.tmpid = vtok->data.binop.left_tmpid;
            tmp_param.size = vtok->data.binop.size;
            cc_ssa_tmpasign_func_1(visited, func, tmpid, &tmp_param, i + 1);
        } break;
        default:
            break;
        }
    }
}

bool cc_ssa_is_param_same(
    const cc_ssa_param* restrict p1, const cc_ssa_param* restrict p2)
{
    if (p1->type != p2->type)
        return false;
    assert(p1->type == p2->type);
    switch (p1->type) {
    case SSA_PARAM_VARIABLE:
        return p1->data.var_name == p2->data.var_name;
    case SSA_PARAM_LABEL:
        return p1->data.label_id == p2->data.label_id;
    case SSA_PARAM_TMPVAR:
        return p1->data.tmpid == p2->data.tmpid;
    case SSA_PARAM_CONSTANT:
        return p1->data.constant.is_float == p2->data.constant.is_float
            && p1->data.constant.is_negative == p2->data.constant.is_negative
            && (p1->data.constant.is_float
                    ? p1->data.constant.value.u == p2->data.constant.value.u
                    : p1->data.constant.value.d == p2->data.constant.value.d);
    default:
        return false; /*p1->size == p2->size;*/
    }
}

/* Removes redundant assignments - or assignments which would otherwise
   have no effect, it also merges adjacent labels */
static void cc_ssa_remove_assign_func(cc_ssa_func* func)
{
    size_t i;
    /* Remove assignments */
    for (i = 0; i < func->n_tokens; i++) {
        cc_ssa_token* tok = &func->tokens[i];
        bool erase = false;

        switch (tok->type) {
        case SSA_TOKEN_STORE_FROM:
            erase = cc_ssa_is_param_same(
                &tok->data.unop.left, &tok->data.unop.right);
            /* Assignments **can** only be into temporals! */
            if (tok->data.unop.left.type == SSA_PARAM_NONE)
                erase = true;
            if (tok->data.unop.left.is_volatile
                || tok->data.unop.right.is_volatile)
                erase = false;
            break;
        case SSA_TOKEN_ASSIGN:
        case SSA_TOKEN_LOAD_FROM:
            erase = tok->data.load.addr.type == SSA_PARAM_TMPVAR
                && tok->data.load.addr.data.tmpid == tok->data.load.val_tmpid;
            if (tok->data.load.addr.is_volatile)
                erase = false;
            break;
        case SSA_TOKEN_BRANCH: {
            erase = cc_ssa_is_param_same(
                &tok->data.branch.t_branch, &tok->data.branch.f_branch);
            /* Remove read without side effect */
            if (tok->data.branch.eval.type == SSA_PARAM_NONE
                || tok->data.branch.t_branch.type == SSA_PARAM_NONE
                || tok->data.branch.f_branch.type == SSA_PARAM_NONE)
                erase = true;
            if (tok->data.branch.eval.is_volatile
                || tok->data.branch.t_branch.is_volatile
                || tok->data.branch.f_branch.is_volatile)
                erase = false;
        } break;
        default:
            break;
        }

        if (erase) {
            memmove(&func->tokens[i], &func->tokens[i + 1],
                sizeof(cc_ssa_token) * (func->n_tokens - i - 1));
            func->n_tokens--;
            i--;
        }
    }
}

/* Helper for cc_ssa_remove_loadstore_func, for removing redundant loads */
static void cc_ssa_remove_loadstore_func_1(char* restrict visited,
    cc_ssa_func* restrict func, const cc_ssa_token* restrict ld_tok,
    size_t offset)
{
    size_t i;
    cc_ssa_param tmp_param = { 0 };
    assert(ld_tok->type == SSA_TOKEN_LOAD_FROM);

    /* Construct a fake parameter for replacing this temporal within
       other blocks! */
    tmp_param.type = SSA_PARAM_TMPVAR;
    tmp_param.size = ld_tok->data.load.size;
    tmp_param.data.tmpid = ld_tok->data.load.val_tmpid;
    for (i = offset; i < func->n_tokens; i++) {
        cc_ssa_token* tok = &func->tokens[i];
        switch (tok->type) {
        case SSA_TOKEN_STORE_FROM:
            if (cc_ssa_is_param_same(
                    &ld_tok->data.unop.right, &tok->data.unop.left))
                return;
            break;
        case SSA_TOKEN_LOAD_FROM:
            if (cc_ssa_is_param_same(
                    &ld_tok->data.load.addr, &tok->data.load.addr)) {
                /* Coalesce temporal loads into a single temporal if possible... */
                cc_ssa_tmpasign_func_1(
                    visited, func, tok->data.load.val_tmpid, &tmp_param, i + 1);

                /* Remove this block (it's redundant since the temporal was removed) */
                memmove(&func->tokens[i], &func->tokens[i + 1],
                    sizeof(cc_ssa_token) * (func->n_tokens - i - 1));
                func->n_tokens--;
                i--;
            }
            break;
        default:
            break;
        }
    }
}
/* Removes redundant loads and stores. */
static void cc_ssa_remove_loadstore_func(
    char* restrict visited, size_t visited_len, cc_ssa_func* restrict func)
{
    size_t i;
    for (i = 0; i < func->n_tokens; i++) {
        cc_ssa_token* tok = &func->tokens[i];
        switch (tok->type) {
        case SSA_TOKEN_STORE_FROM:
            break;
        case SSA_TOKEN_LOAD_FROM:
            memset(visited, 0, visited_len);
            cc_ssa_remove_loadstore_func_1(visited, func, tok, i + 1);
            break;
        default:
            break;
        }
    }
}

/* Helper function for cc_ssa_labmerge_func_1 */
static void cc_ssa_labmerge_param(
    cc_ssa_param* param, unsigned short label_id, unsigned short new_label_id)
{
    assert(label_id != new_label_id);
    if (param->type == SSA_PARAM_LABEL && param->data.label_id == label_id)
        param->data.label_id = new_label_id;
}
/* Helper function for cc_ssa_labmerge_func */
static void cc_ssa_labmerge_func_1(
    cc_ssa_func* func, unsigned short label_id, unsigned short new_label_id)
{
    size_t i;
    assert(label_id != new_label_id);
    for (i = 0; i < func->n_tokens; ++i) {
        cc_ssa_token* tok = &func->tokens[i];
        switch (tok->type) {
        case SSA_TOKEN_STORE_FROM:
            cc_ssa_labmerge_param(&tok->data.unop.left, label_id, new_label_id);
            cc_ssa_labmerge_param(
                &tok->data.unop.right, label_id, new_label_id);
            break;
        case SSA_TOKEN_ASSIGN:
        case SSA_TOKEN_LOAD_FROM:
            cc_ssa_labmerge_param(&tok->data.load.addr, label_id, new_label_id);
            break;
        case SSA_TOKEN_ADD:
        case SSA_TOKEN_SUB:
        case SSA_TOKEN_AND:
        case SSA_TOKEN_COMPARE:
        case SSA_TOKEN_DIV:
        case SSA_TOKEN_MUL:
        case SSA_TOKEN_OR:
        case SSA_TOKEN_XOR:
        case SSA_TOKEN_GT:
        case SSA_TOKEN_GTE:
        case SSA_TOKEN_LT:
        case SSA_TOKEN_LTE:
        case SSA_TOKEN_EQ:
        case SSA_TOKEN_NEQ:
        case SSA_TOKEN_LSHIFT:
        case SSA_TOKEN_RSHIFT:
        case SSA_TOKEN_MOD:
            cc_ssa_labmerge_param(
                &tok->data.binop.right, label_id, new_label_id);
            cc_ssa_labmerge_param(
                &tok->data.binop.extra, label_id, new_label_id);
            break;
        case SSA_TOKEN_CALL: {
            size_t j;
            cc_ssa_labmerge_param(&tok->data.call.left, label_id, new_label_id);
            cc_ssa_labmerge_param(
                &tok->data.call.right, label_id, new_label_id);
            for (j = 0; j < tok->data.call.n_params; ++j)
                cc_ssa_labmerge_param(
                    &tok->data.call.params[j], label_id, new_label_id);
        } break;
        case SSA_TOKEN_ALLOCA:
            cc_ssa_labmerge_param(
                &tok->data.alloca.left, label_id, new_label_id);
            cc_ssa_labmerge_param(
                &tok->data.alloca.size, label_id, new_label_id);
            cc_ssa_labmerge_param(
                &tok->data.alloca.align, label_id, new_label_id);
            break;
        case SSA_TOKEN_BRANCH:
            cc_ssa_labmerge_param(
                &tok->data.branch.eval, label_id, new_label_id);
            cc_ssa_labmerge_param(
                &tok->data.branch.t_branch, label_id, new_label_id);
            cc_ssa_labmerge_param(
                &tok->data.branch.f_branch, label_id, new_label_id);
            break;
        case SSA_TOKEN_RET:
            cc_ssa_labmerge_param(&tok->data.retval, label_id, new_label_id);
            break;
        case SSA_TOKEN_JUMP:
            cc_ssa_labmerge_param(&tok->data.retval, label_id, new_label_id);
            break;
        case SSA_TOKEN_LABEL:
            /* No operation */
            break;
        default:
            cc_abort(__FILE__, __LINE__);
        }
    }
}
/* Merges adjacent labels so we can remove redundant branching */
static void cc_ssa_labmerge_func(cc_ssa_func* func)
{
    size_t i;
    for (i = 0; i < func->n_tokens; i++) {
        cc_ssa_token* tok = &func->tokens[i];
        if (tok->type == SSA_TOKEN_LABEL) {
            cc_ssa_token* ltok = &func->tokens[i + 1];
            /* Merge adjacent labels */
            if (i + 1 < func->n_tokens && ltok->type == SSA_TOKEN_LABEL) {
                cc_ssa_labmerge_func_1(
                    func, tok->data.label_id, ltok->data.label_id);
                memmove(&func->tokens[i], &func->tokens[i + 1],
                    sizeof(cc_ssa_token) * (func->n_tokens - i - 1));
                --func->n_tokens;
                --i;
            }
        }
    }
}

/* Helper function for cc_ssa_is_livetmp_func */
static bool cc_ssa_is_livetmp_param(cc_ssa_param param, unsigned int tmpid)
{
    return param.type == SSA_PARAM_TMPVAR && param.data.tmpid == tmpid;
}
/* Checks if a temporal with id tmpid is live within a given token */
static bool cc_ssa_is_livetmp_token(const cc_ssa_token* tok, unsigned int tmpid)
{
    switch (tok->type) {
    case SSA_TOKEN_STORE_FROM:
        return cc_ssa_is_livetmp_param(tok->data.unop.left, tmpid)
            || cc_ssa_is_livetmp_param(tok->data.unop.right, tmpid);
    case SSA_TOKEN_ASSIGN:
    case SSA_TOKEN_LOAD_FROM:
        return tok->data.load.val_tmpid == tmpid
            || cc_ssa_is_livetmp_param(tok->data.load.addr, tmpid);
    case SSA_TOKEN_ADD:
    case SSA_TOKEN_SUB:
    case SSA_TOKEN_AND:
    case SSA_TOKEN_COMPARE:
    case SSA_TOKEN_DIV:
    case SSA_TOKEN_MUL:
    case SSA_TOKEN_OR:
    case SSA_TOKEN_XOR:
    case SSA_TOKEN_GT:
    case SSA_TOKEN_GTE:
    case SSA_TOKEN_LT:
    case SSA_TOKEN_LTE:
    case SSA_TOKEN_EQ:
    case SSA_TOKEN_NEQ:
    case SSA_TOKEN_LSHIFT:
    case SSA_TOKEN_RSHIFT:
    case SSA_TOKEN_MOD:
        return tok->data.binop.left_tmpid == tmpid
            || cc_ssa_is_livetmp_param(tok->data.binop.right, tmpid)
            || cc_ssa_is_livetmp_param(tok->data.binop.extra, tmpid);
    case SSA_TOKEN_CALL: {
        size_t i;
        bool b = false;
        b = cc_ssa_is_livetmp_param(tok->data.call.left, tmpid) ? true : b;
        b = cc_ssa_is_livetmp_param(tok->data.call.right, tmpid) ? true : b;
        for (i = 0; i < tok->data.call.n_params; i++)
            b = cc_ssa_is_livetmp_param(tok->data.call.params[i], tmpid) ? true
                                                                         : b;
        return b;
    }
    case SSA_TOKEN_ALLOCA:
        return cc_ssa_is_livetmp_param(tok->data.alloca.left, tmpid)
            || cc_ssa_is_livetmp_param(tok->data.alloca.size, tmpid)
            || cc_ssa_is_livetmp_param(tok->data.alloca.align, tmpid);
    case SSA_TOKEN_JUMP:
        return cc_ssa_is_livetmp_param(tok->data.jump_target, tmpid);
    case SSA_TOKEN_BRANCH:
        return cc_ssa_is_livetmp_param(tok->data.branch.eval, tmpid)
            || cc_ssa_is_livetmp_param(tok->data.branch.t_branch, tmpid)
            || cc_ssa_is_livetmp_param(tok->data.branch.f_branch, tmpid);
    case SSA_TOKEN_RET:
        return cc_ssa_is_livetmp_param(tok->data.retval, tmpid);
    case SSA_TOKEN_LABEL:
    case SSA_TOKEN_DROP:
        /* Codegen aids - ignored */
        return false;
    default:
        cc_abort(__FILE__, __LINE__);
    }
    return false;
}
/* Obtains the location where the temporal was last live. */
static size_t cc_ssa_get_livetmp_location(cc_ssa_func* func, unsigned int tmpid)
{
    size_t last = 0;
    size_t i;
    for (i = 0; i < func->n_tokens; ++i)
        if (cc_ssa_is_livetmp_token(&func->tokens[i], tmpid))
            last = i;
    return last;
}
/* Adds livetmp markers to a function - those are just like normal
   tokens, except they serve as codegen aid rather than codegen instructions. */
static void cc_ssa_livetmp_func(char* visited, cc_ssa_func* func)
{
    size_t i;
    for (i = 0; i < func->n_tokens; ++i) {
        const cc_ssa_token* tok = &func->tokens[i];
        unsigned short tmpid = cc_ssa_get_lhs_tmpid(tok);
        if (tmpid > 0) {
            cc_ssa_token drop_tok = { 0 };
            size_t loc;
            if ((visited[tmpid / CHAR_BIT]) & (1 << (tmpid % CHAR_BIT)))
                continue;
            visited[tmpid / CHAR_BIT] |= 1 << (tmpid % CHAR_BIT);

            loc = cc_ssa_get_livetmp_location(func, tmpid) + 1;

            /* TODO: This code may write past the function tokens... */
            func->tokens = cc_realloc_array(func->tokens, func->n_tokens + 2);
            tok = &func->tokens[i];
            ++func->n_tokens;
            if (loc + 1 < func->n_tokens)
                /* Insert "DROP" token to aid codegen that the temporal is
                    now dead and should be dropped. */
                memmove(&func->tokens[loc + 1], &func->tokens[loc],
                    sizeof(cc_ssa_token) * (func->n_tokens - loc));

            drop_tok.type = SSA_TOKEN_DROP;
            drop_tok.data.dropped_tmpid = tmpid;
            func->tokens[loc] = drop_tok;
        }
    }
}

/* Obtain the LHS parameters of a token. */
unsigned short cc_ssa_get_lhs_tmpid(const cc_ssa_token* tok)
{
    switch (tok->type) {
    case SSA_TOKEN_STORE_FROM:
        return tok->data.unop.left.type == SSA_PARAM_TMPVAR
            ? tok->data.unop.left.data.tmpid
            : 0;
    case SSA_TOKEN_ASSIGN:
    case SSA_TOKEN_LOAD_FROM:
        return tok->data.load.val_tmpid;
    case SSA_TOKEN_ADD:
    case SSA_TOKEN_SUB:
    case SSA_TOKEN_AND:
    case SSA_TOKEN_COMPARE:
    case SSA_TOKEN_DIV:
    case SSA_TOKEN_MUL:
    case SSA_TOKEN_OR:
    case SSA_TOKEN_XOR:
    case SSA_TOKEN_GT:
    case SSA_TOKEN_GTE:
    case SSA_TOKEN_LT:
    case SSA_TOKEN_LTE:
    case SSA_TOKEN_EQ:
    case SSA_TOKEN_NEQ:
    case SSA_TOKEN_LSHIFT:
    case SSA_TOKEN_RSHIFT:
    case SSA_TOKEN_MOD:
        return tok->data.binop.left_tmpid;
    case SSA_TOKEN_CALL:
        return tok->data.call.left.type == SSA_PARAM_TMPVAR
            ? tok->data.call.left.data.tmpid
            : 0;
    case SSA_TOKEN_ALLOCA:
        return tok->data.alloca.left.type == SSA_PARAM_TMPVAR
            ? tok->data.alloca.left.data.tmpid
            : 0;
    case SSA_TOKEN_BRANCH:
    case SSA_TOKEN_JUMP:
    case SSA_TOKEN_RET:
    case SSA_TOKEN_LABEL:
    case SSA_TOKEN_DROP:
        /* No-operation or codegen aids - ignored */
        return 0;
    default:
        cc_abort(__FILE__, __LINE__);
    }
    return 0;
}

static void cc_ssa_colour_func(const cc_context* ctx, cc_ssa_func* func)
{
    /* Make a bit array for each visited node... */
    size_t visited_len = ((ctx->ssa_tmpid + 1) / CHAR_BIT) + 1;
    char* visited = cc_malloc(visited_len);

    if (ctx->print_ssa) {
        printf("###############################################\n");
        printf("=>vanilla\n");
        cc_ssa_print_func(func);
    }

    /* None of these passes should ever add any new temporals, they shall
       only remove temporals. */
    memset(visited, 0, visited_len);
    cc_ssa_tmpassign_func(visited, func);
    if (ctx->print_ssa) {
        printf("=>tmpassign\n");
        cc_ssa_print_func(func);
    }

    memset(visited, 0, visited_len);
    cc_ssa_remove_assign_func(func);
    if (ctx->print_ssa) {
        printf("=>remove_assign\n");
        cc_ssa_print_func(func);
    }

    memset(visited, 0, visited_len);
    cc_ssa_remove_loadstore_func(visited, visited_len, func);
    if (ctx->print_ssa) {
        printf("=>remove_loadstore\n");
        cc_ssa_print_func(func);
    }

    memset(visited, 0, visited_len);
    cc_ssa_tmpassign_func(visited, func);
    if (ctx->print_ssa) {
        printf("=>tmpassign\n");
        cc_ssa_print_func(func);
    }

    memset(visited, 0, visited_len);
    cc_ssa_remove_assign_func(func);
    if (ctx->print_ssa) {
        printf("=>remove_assign\n");
        cc_ssa_print_func(func);
    }

    memset(visited, 0, visited_len);
    cc_ssa_labmerge_func(func);
    if (ctx->print_ssa) {
        printf("=>labmerge\n");
        cc_ssa_print_func(func);
    }

    memset(visited, 0, visited_len);
    cc_ssa_livetmp_func(visited, func);
    if (ctx->print_ssa) {
        printf("=>livetmp\n");
        cc_ssa_print_func(func);
    }

    cc_free(visited);
}

static void cc_ssa_colour(cc_context* ctx)
{
    size_t i;
    for (i = 0; i < ctx->n_ssa_funcs; i++)
        cc_ssa_colour_func(ctx, &ctx->ssa_funcs[i]);
}

static cc_ast_variable cc_ssa_create_func_var(const char* name)
{
    cc_ast_variable var = { 0 };
    var.name = cc_strdup(name);
    var.body = NULL;
    var.type.mode = AST_TYPE_MODE_FUNCTION;
    var.storage = AST_STORAGE_STATIC;
    return var;
}

void cc_ssa_top(cc_context* ctx)
{
    /* TODO: Do not use statics, use something else or shove it into the
       context! */
    static cc_ssa_func static_ctor_func = { 0 };
    static cc_ast_variable static_ctor_var = { 0 };
    cc_ssa_param none_param = { 0 };

    ctx->assign_lhs = false; /* Start at right-hand-side */

    static_ctor_var = cc_ssa_create_func_var("_occ_ctor");
    static_ctor_func.ast_var = &static_ctor_var;
    ctx->ssa_current_func = &static_ctor_func;
    cc_ssa_from_ast(ctx, ctx->root, none_param);
    /* Add the ctor function to the list of functions */
    ctx->ssa_funcs = cc_realloc_array(ctx->ssa_funcs, ctx->n_ssa_funcs + 1);
    ctx->ssa_funcs[ctx->n_ssa_funcs++] = static_ctor_func;
    ctx->ssa_current_func = NULL;

    if (ctx->print_ssa)
        cc_ssa_print(ctx);

    cc_ssa_colour(ctx);

    if (ctx->print_ssa)
        cc_ssa_print(ctx);
}
