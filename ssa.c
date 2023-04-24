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
            printf("%lf", param->data.constant.value.d);
        else
            printf("%lu", param->data.constant.value.u);
        break;
    case SSA_PARAM_STRING_LITERAL:
        printf("string_%u_\"%s\"", param->data.string.tmpid,
            param->data.string.literal);
        break;
    case SSA_PARAM_VARIABLE:
        printf("var_%s", param->data.var_name);
        break;
    case SSA_PARAM_TMPVAR:
        printf("tmp_%i", param->data.tmpid);
        break;
    case SSA_PARAM_NONE:
        printf("none");
        break;
    case SSA_PARAM_RETVAL:
        printf("retval");
        break;
    case SSA_PARAM_LABEL:
        printf("L_%u", param->data.label_id);
        break;
    default:
        abort();
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
    cc_ssa_print_param(&tok->data.binop.left);
    printf(" = ");
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
    case SSA_TOKEN_ASSIGN:
        cc_ssa_print_param(&tok->data.unop.left);
        printf(" = ");
        cc_ssa_print_param(&tok->data.unop.right);
        break;
    case SSA_TOKEN_STORE_AT:
        cc_ssa_print_token_unop(tok, "store_at");
        break;
    case SSA_TOKEN_LOAD_FROM:
        cc_ssa_print_token_unop(tok, "load_from");
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
    case SSA_TOKEN_REM:
        cc_ssa_print_token_binop(tok, "rem");
        break;
    case SSA_TOKEN_RET:
        printf("ret");
        break;
    case SSA_TOKEN_RSHIFT:
        cc_ssa_print_token_binop(tok, "rshift");
        break;
    case SSA_TOKEN_SIGN_EXT:
        cc_ssa_print_token_unop(tok, "sign_ext");
        break;
    case SSA_TOKEN_SUB:
        cc_ssa_print_token_binop(tok, "sub");
        break;
    case SSA_TOKEN_XOR:
        cc_ssa_print_token_binop(tok, "xor");
        break;
    case SSA_TOKEN_ZERO_EXT:
        cc_ssa_print_token_unop(tok, "zero_ext");
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
    default:
        abort();
    }
    printf("\n");
}

static void cc_ssa_print_func(const cc_ssa_func* func)
{
    size_t i;
    printf("fn %s {\n", func->ast_var->name);
    for (i = 0; i < func->n_tokens; i++)
        cc_ssa_print_token(&func->tokens[i]);
    printf("}\n");
}

static void cc_ssa_print(cc_context* ctx)
{
    size_t i;
    for (i = 0; i < ctx->n_ssa_funcs; i++)
        cc_ssa_print_func(&ctx->ssa_funcs[i]);
}

static enum cc_ssa_storage cc_ssa_ast_storage_to_ssa(enum cc_ast_storage v)
{
    return (enum cc_ssa_storage)v;
}

static cc_ssa_param cc_ssa_literal_to_param(const cc_ast_literal* literal)
{
    if (literal->is_float) {
        return (cc_ssa_param) { .type = SSA_PARAM_CONSTANT,
            .size = 4,
            .data.constant = (cc_ssa_constant) {
                .is_float = true, .value.d = literal->value.d } };
    }
    if (literal->is_signed) {
        return (cc_ssa_param) { .type = SSA_PARAM_CONSTANT,
            .size = 4,
            .data.constant = (cc_ssa_constant) { .is_float = false,
                .is_negative = literal->value.s < 0,
#define ABS(x) (x > 0) ? (x) : -(x)
                .value.u = (unsigned long)ABS(literal->value.s)
#undef ABS
            } };
    }
    return (cc_ssa_param) { .type = SSA_PARAM_CONSTANT,
        .size = 4,
        .data.constant = (cc_ssa_constant) { .is_float = false,
            .is_negative = false,
            .value.u = literal->value.u } };
}

/* Obtain the SSA parameter for the given variable - we always use a pointer
   to point to the variable - then we can materialize said pointers
   into something else after SSA, but for now they will be materialized
   into pointers. */
static cc_ssa_param cc_ssa_variable_to_param(
    cc_context* ctx, const cc_ast_variable* var)
{
    cc_ast_type tmp_type = var->type;
    tmp_type.n_cv_qual++;
    assert(tmp_type.n_cv_qual <= MAX_CV_QUALIFIERS);
    return (cc_ssa_param) {
        .type = SSA_PARAM_VARIABLE,
        .storage = cc_ssa_ast_storage_to_ssa(var->type.storage),
        .data = {
            .var_name = cc_strdup(var->name),
        },
        .is_signed = false,
        .size = ctx->get_sizeof(ctx, &tmp_type),
        .version = 0,
    };
}

/* Obtain the SSA parameter for the return value */
static cc_ssa_param cc_ssa_retval_param(
    cc_context* ctx, const cc_ast_type* ret_type)
{
    return (cc_ssa_param) {
        .type = SSA_PARAM_RETVAL,
        .storage = SSA_STORAGE_AUTO,
        .data.var_name = NULL,
        .is_signed = ret_type->data.num.is_signed,
        .size = ctx->get_sizeof(ctx, ret_type),
        .version = 0,
    };
}

static cc_ssa_param cc_ssa_label_param(cc_context* ctx, unsigned short label_id)
{
    return (cc_ssa_param) {
        .type = SSA_PARAM_LABEL,
        .storage = SSA_STORAGE_AUTO,
        .data.label_id = label_id,
        .is_signed = false,
        .size = 4,
        .version = 0,
    };
}

static unsigned short cc_ssa_get_unique_tmpid(cc_context* ctx)
{
    assert(ctx->tmpid + 1 < USHRT_MAX);
    return ctx->tmpid++;
}

/* Temporary variable parameter */
cc_ssa_param cc_ssa_tempvar_param_1(
    cc_context* ctx, bool is_signed, unsigned short size)
{
    return (cc_ssa_param) {
        .type = SSA_PARAM_TMPVAR,
        .storage = SSA_STORAGE_AUTO,
        .data = {
            .tmpid = ctx->tmpid++,
        },
        .is_signed = is_signed,
        .size = size,
        .version = 0,
    };
}

cc_ssa_param cc_ssa_tempvar_param(
    cc_context* ctx, const struct cc_ast_type* base_type)
{
    return cc_ssa_tempvar_param_1(
        ctx, base_type->data.num.is_signed, ctx->get_sizeof(ctx, base_type));
}

static void cc_ssa_push_token(
    cc_context* ctx, cc_ssa_func* func, cc_ssa_token tok)
{
    func->tokens = cc_realloc_array(func->tokens, func->n_tokens + 1);
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
    cc_ssa_param lhs_param;
    cc_ast_type rhs_type = { 0 };
    cc_ssa_param rhs_param;
    cc_ssa_param parith_param;
    unsigned int lhs_psize = 0;
    unsigned int rhs_psize = 0;

    if (!cc_ceval_deduce_type(ctx, node->data.binop.left, &lhs_type))
        abort();
    lhs_param = cc_ssa_tempvar_param(ctx, &lhs_type);
    if (!cc_ceval_deduce_type(ctx, node->data.binop.right, &rhs_type))
        abort();
    rhs_param = cc_ssa_tempvar_param(ctx, &rhs_type);

    cc_ssa_from_ast(ctx, node->data.binop.left, lhs_param);
    cc_ssa_from_ast(ctx, node->data.binop.right, rhs_param);

    /* Pointer with pointer arithmethic is illegal */
    if(lhs_type.n_cv_qual > 0 && rhs_type.n_cv_qual > 0)
        abort();

    /* Obtain sizes from types iff they are pointers (to properly perform
       pointer arithmethic). */
    --lhs_type.n_cv_qual;
    lhs_psize = ctx->get_sizeof(ctx, &lhs_type);
    --rhs_type.n_cv_qual;
    rhs_psize = ctx->get_sizeof(ctx, &rhs_type);

    switch (node->data.binop.op) {
#define HANDLE_POINTER_ARITH() \
    if(lhs_psize || rhs_psize) { \
        cc_ast_literal sizeof_lit; \
        cc_ssa_param tmp_param; \
        sizeof_lit.is_float = false; \
        sizeof_lit.is_signed = false; \
        sizeof_lit.value.u = lhs_psize ? lhs_psize : rhs_psize; \
        tmp_param = cc_ssa_literal_to_param(&sizeof_lit); \
        parith_param = cc_ssa_tempvar_param( \
            ctx, lhs_psize ? &lhs_type : &rhs_type); \
        /* Assignment is an unop here */ \
        tok.type = SSA_TOKEN_MUL; \
        tok.data.binop.left = parith_param; \
        tok.data.binop.right = lhs_psize ? lhs_param : rhs_param; \
        tok.data.binop.extra = tmp_param; \
        tok.info = node->info; \
        tok.info.filename = cc_strdup(node->info.filename); \
        cc_ssa_push_token(ctx, ctx->ssa_current_func, tok); \
        memset(&tok, 0, sizeof(tok)); \
    }
    case AST_BINOP_ADD:
HANDLE_POINTER_ARITH()
        tok.type = SSA_TOKEN_ADD;
        break;
    case AST_BINOP_SUB:
HANDLE_POINTER_ARITH()
        tok.type = SSA_TOKEN_SUB;
        break;
#undef HANDLE_POINTER_ARITH
    case AST_BINOP_MUL:
        tok.type = SSA_TOKEN_MUL;
        break;
    case AST_BINOP_DIV:
        tok.type = SSA_TOKEN_DIV;
        break;
    case AST_BINOP_ASSIGN:
        /* Assignment is an unop here */
        tok.type = SSA_TOKEN_ASSIGN;
        tok.data.unop.left = lhs_param;
        tok.data.unop.right = rhs_param;
        tok.info = node->info;
        tok.info.filename = cc_strdup(node->info.filename);
        cc_ssa_push_token(ctx, ctx->ssa_current_func, tok);

        memset(&tok, 0, sizeof(tok));
        tok.type = SSA_TOKEN_ASSIGN;
        tok.data.unop.left = param;
        tok.data.unop.right = lhs_param;
        tok.info = node->info;
        tok.info.filename = cc_strdup(node->info.filename);
        cc_ssa_push_token(ctx, ctx->ssa_current_func, tok);
        return;
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
    default:
        abort();
    }
    tok.data.binop.left = param;
    tok.data.binop.right = lhs_psize ? parith_param : lhs_param;
    tok.data.binop.extra = rhs_psize ? parith_param : rhs_param;
    tok.info = node->info;
    tok.info.filename = cc_strdup(node->info.filename);
    cc_ssa_push_token(ctx, ctx->ssa_current_func, tok);
}

static void cc_ssa_add_dummy_ret(cc_context* ctx, cc_ssa_func* func)
{
    /* Push a dummy return */
    cc_ssa_token tok = { 0 };
    tok.type = SSA_TOKEN_RET;
    cc_ssa_push_token(ctx, func, tok);
}

static void cc_ssa_process_block(
    cc_context* ctx, const cc_ast_node* node, cc_ssa_param param)
{
    cc_ssa_token tok = { 0 };
    size_t i;
    for (i = 0; i < node->data.block.n_vars; i++) {
        const cc_ast_variable* var = &node->data.block.vars[i];
        if (var->type.mode == AST_TYPE_MODE_FUNCTION && var->body != NULL) {
            cc_ssa_func func = { 0 };
            func.ast_var = var;

            cc_ssa_param none_param = { 0 };

            /* Enter into a new function contextee */
            cc_ssa_func* old_current_ssa_func = ctx->ssa_current_func;
            ctx->ssa_current_func = &func;
            bool old_func_has_return = ctx->func_has_return;
            ctx->func_has_return = false;
            cc_ssa_from_ast(ctx, var->body, none_param);
            if (!ctx->func_has_return) {
                if (var->type.data.func.return_type->mode
                    != AST_TYPE_MODE_VOID) {
                    cc_diag_error(
                        ctx, "Function that returns non-void doesn't return");
                } else {
                    cc_ssa_add_dummy_ret(ctx, ctx->ssa_current_func);
                }
            }

            ctx->func_has_return = old_func_has_return;
            ctx->ssa_current_func = old_current_ssa_func;

            ctx->ssa_funcs
                = cc_realloc_array(ctx->ssa_funcs, ctx->n_ssa_funcs + 1);
            ctx->ssa_funcs[ctx->n_ssa_funcs++] = func;
        } else if (var->type.mode != AST_TYPE_MODE_FUNCTION) {
            if (ctx->ssa_current_func != NULL) {
                tok.type = SSA_TOKEN_ALLOCA;
                tok.data.alloca.left = cc_ssa_variable_to_param(ctx, var);
                cc_ast_literal literal = (cc_ast_literal) { .is_float = false,
                    .is_signed = false,
                    .value.u = ctx->get_sizeof(ctx, &var->type) };
                tok.data.alloca.size = cc_ssa_literal_to_param(&literal);
                tok.info = node->info;
                tok.info.filename = cc_strdup(node->info.filename);
                cc_ssa_push_token(ctx, ctx->ssa_current_func, tok);
            } else {
                enum cc_ast_storage storage
                    = var->type.storage & ~(AST_STORAGE_THREAD_LOCAL);
                assert(storage == AST_STORAGE_EXTERN
                    || storage == AST_STORAGE_GLOBAL
                    || storage == AST_STORAGE_CONSTEXPR);
            }
        }
    }
    for (i = 0; i < node->data.block.n_children; i++)
        cc_ssa_from_ast(ctx, &node->data.block.children[i], param);
}

static void cc_ssa_process_call(
    cc_context* ctx, const cc_ast_node* node, cc_ssa_param param)
{
    cc_ssa_token tok = { 0 };
    cc_ast_type call_retval_type = { 0 };
    size_t i;

    if (!cc_ceval_deduce_type(
            ctx, node->data.call.call_expr, &call_retval_type))
        abort();
    cc_ssa_param call_retval_param
        = cc_ssa_tempvar_param(ctx, &call_retval_type);
    cc_ssa_from_ast(ctx, node->data.call.call_expr, call_retval_param);

    tok.type = SSA_TOKEN_CALL;
    tok.data.call.left = param;
    tok.data.call.right = call_retval_param;
    for (i = 0; i < node->data.call.n_params; i++) {
        cc_ast_type call_arg_type = { 0 };
        if (!cc_ceval_deduce_type(
                ctx, node->data.call.call_expr, &call_arg_type))
            abort();
        cc_ssa_param call_arg_param = cc_ssa_tempvar_param(ctx, &call_arg_type);
        cc_ssa_from_ast(ctx, &node->data.call.params[i], call_arg_param);

        tok.data.call.params = cc_realloc_array(
            tok.data.call.params, tok.data.call.n_params + 1);
        tok.data.call.params[tok.data.call.n_params++] = call_arg_param;
    }
    tok.info = node->info;
    tok.info.filename = cc_strdup(node->info.filename);
    cc_ssa_push_token(ctx, ctx->ssa_current_func, tok);
}

static void cc_ssa_process_switch(
    cc_context* ctx, const cc_ast_node* node, cc_ssa_param param)
{
    cc_ast_type control_type = { 0 };
    if (!cc_ceval_deduce_type(
            ctx, node->data.switch_expr.control, &control_type))
        abort();
    cc_ssa_param control_param = cc_ssa_tempvar_param(ctx, &control_type);
    cc_ssa_from_ast(ctx, node->data.switch_expr.control, control_param);
    cc_ssa_from_ast(ctx, node->data.switch_expr.block, param);
}

static void cc_ssa_process_if(
    cc_context* ctx, const cc_ast_node* node, cc_ssa_param param)
{
    cc_ast_type cond_type = { 0 };
    if (!cc_ceval_deduce_type(ctx, node->data.if_expr.cond, &cond_type))
        abort();
    cc_ssa_param cond_param = cc_ssa_tempvar_param(ctx, &cond_type);
    cc_ssa_from_ast(ctx, node->data.if_expr.cond, cond_param);

    /* Skip effect-less if statment blocks (condition needs to be evaluated anyways) */
    if (node->data.if_expr.block == NULL
        && node->data.if_expr.tail_else == NULL)
        return;

    cc_ssa_token tok = { 0 };
    tok.type = SSA_TOKEN_BRANCH;
    tok.data.branch.eval = cond_param;
    if (node->data.if_expr.tail_else != NULL) {
        tok.data.branch.t_branch
            = cc_ssa_label_param(ctx, node->data.if_expr.block->label_id);
        tok.data.branch.f_branch
            = cc_ssa_label_param(ctx, node->data.if_expr.tail_else->label_id);
        tok.info = node->info;
        tok.info.filename = cc_strdup(node->info.filename);
        cc_ssa_push_token(ctx, ctx->ssa_current_func, tok);
        cc_ssa_from_ast(ctx, node->data.if_expr.block, param);
        cc_ssa_from_ast(ctx, node->data.if_expr.tail_else, param);
    } else {
        cc_ssa_token label_tok = { 0 };
        label_tok.type = SSA_TOKEN_LABEL;
        label_tok.data.label_id = cc_ast_alloc_label_id(ctx);
        /* Tail else is null, we will have to synthetize a label after this if */
        tok.data.branch.t_branch
            = cc_ssa_label_param(ctx, node->data.if_expr.block->label_id);
        tok.data.branch.f_branch
            = cc_ssa_label_param(ctx, label_tok.data.label_id);
        tok.info = node->info;
        tok.info.filename = cc_strdup(node->info.filename);
        cc_ssa_push_token(ctx, ctx->ssa_current_func, tok);
        cc_ssa_from_ast(ctx, node->data.if_expr.block, param);
        label_tok.info = node->info;
        label_tok.info.filename = cc_strdup(node->info.filename);
        cc_ssa_push_token(ctx, ctx->ssa_current_func, label_tok);
    }
}

static void cc_ssa_process_jump(
    cc_context* ctx, const cc_ast_node* node, cc_ssa_param param)
{
    cc_ssa_token tok = { 0 };
    tok.type = SSA_TOKEN_BRANCH;
    cc_ast_literal literal = (cc_ast_literal) {
        .is_float = false,
        .is_signed = false,
        .value.u = 0,
    };
    tok.data.branch.eval = cc_ssa_literal_to_param(&literal);
    tok.data.branch.t_branch
        = cc_ssa_label_param(ctx, node->data.jump_label_id);
    tok.data.branch.f_branch
        = cc_ssa_label_param(ctx, node->data.jump_label_id);
    tok.info = node->info;
    tok.info.filename = cc_strdup(node->info.filename);
    cc_ssa_push_token(ctx, ctx->ssa_current_func, tok);
}

static void cc_ssa_process_literal(
    cc_context* ctx, const cc_ast_node* node, cc_ssa_param param)
{
    cc_ssa_token tok = { 0 };
    cc_ssa_param literal_param = cc_ssa_literal_to_param(&node->data.literal);
    tok.type = SSA_TOKEN_ASSIGN;
    tok.data.unop.left = param;
    tok.data.unop.right = literal_param;
    tok.info = node->info;
    tok.info.filename = tok.info.filename == NULL ? NULL : cc_strdup(node->info.filename);
    cc_ssa_push_token(ctx, ctx->ssa_current_func, tok);
}

static void cc_ssa_process_return(
    cc_context* ctx, const cc_ast_node* node, cc_ssa_param param)
{
    const cc_ast_variable* var = ctx->ssa_current_func->ast_var;
    assert(var->type.mode == AST_TYPE_MODE_FUNCTION);
    cc_ssa_from_ast(ctx, node->data.return_expr,
        cc_ssa_retval_param(ctx, var->type.data.func.return_type));

    cc_ssa_token tok = { 0 };
    tok.type = SSA_TOKEN_RET;
    tok.info = node->info;
    tok.info.filename = cc_strdup(node->info.filename);
    cc_ssa_push_token(ctx, ctx->ssa_current_func, tok);
}

static void cc_ssa_process_string_literal(
    cc_context* ctx, const cc_ast_node* node, cc_ssa_param param)
{
    assert(node->type == AST_NODE_STRING_LITERAL);
    cc_ssa_token tok = { 0 };

    /* const char* */
    cc_ast_type string_type = cc_ceval_get_string_type(ctx);
    cc_ssa_param literal_param = (cc_ssa_param) {
        .type = SSA_PARAM_STRING_LITERAL,
        .size = ctx->get_sizeof(ctx, &string_type),
        .is_signed = false,
        .storage = SSA_STORAGE_AUTO,
        .version = 0,
        .data.string.tmpid = cc_ssa_get_unique_tmpid(ctx),
        .data.string.literal = node->data.string_literal,
    };
    tok.type = SSA_TOKEN_ASSIGN;
    tok.data.unop.left = param;
    tok.data.unop.right = literal_param;
    tok.info = node->info;
    tok.info.filename = cc_strdup(node->info.filename);
    cc_ssa_push_token(ctx, ctx->ssa_current_func, tok);
}

static void cc_ssa_process_unop(
    cc_context* ctx, const cc_ast_node* node, cc_ssa_param param)
{
    cc_ssa_token tok = { 0 };
    cc_ast_type child_type = { 0 };
    if (!cc_ceval_deduce_type(ctx, node->data.unop.child, &child_type))
        abort();
    cc_ssa_param child_param = { 0 };

    switch (node->data.unop.op) {
    case AST_UNOP_CAST:
        tok.type = SSA_TOKEN_ZERO_EXT;
        break;
    case AST_UNOP_DEREF:
        child_param = cc_ssa_tempvar_param(ctx, &child_type);
        cc_ssa_from_ast(ctx, node->data.unop.child, child_param);
        tok.type = SSA_TOKEN_LOAD_FROM;
        tok.data.unop.left = param;
        tok.data.unop.right = child_param;
        tok.info = node->info;
        tok.info.filename = cc_strdup(node->info.filename);
        cc_ssa_push_token(ctx, ctx->ssa_current_func, tok);
        return;
    default:
        abort();
    }

    child_param = cc_ssa_tempvar_param(ctx, &child_type);
    cc_ssa_from_ast(ctx, node->data.unop.child, child_param);
    tok.data.unop.left = param;
    tok.data.unop.right = child_param;
    tok.info = node->info;
    tok.info.filename = cc_strdup(node->info.filename);
    cc_ssa_push_token(ctx, ctx->ssa_current_func, tok);
}

static void cc_ssa_process_variable(
    cc_context* ctx, const cc_ast_node* node, cc_ssa_param param)
{
    cc_ssa_token tok = { 0 };
    const cc_ast_variable* var
        = cc_ast_find_variable(node->data.var.name, node);
    cc_ssa_param var_param = cc_ssa_variable_to_param(ctx, var);

    tok.type = SSA_TOKEN_ASSIGN;
    tok.data.unop.left = param;
    tok.data.unop.right = var_param;
    tok.info = node->info;
    tok.info.filename = cc_strdup(node->info.filename);
    cc_ssa_push_token(ctx, ctx->ssa_current_func, tok);
}

static void cc_ssa_process_label(cc_context* ctx, const cc_ast_node* node)
{
    cc_ssa_token tok = { 0 };
    tok.type = SSA_TOKEN_LABEL;
    tok.data.label_id = node->label_id;
    tok.info = node->info;
    tok.info.filename = cc_strdup(node->info.filename);
    cc_ssa_push_token(ctx, ctx->ssa_current_func, tok);
}

/* For expressions of the type (lhs) = (rhs) */
static void cc_ssa_from_ast(
    cc_context* ctx, const cc_ast_node* node, cc_ssa_param param)
{
    if (node == NULL)
        return;

    if (node->ref_count > 0)
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
    default:
        abort();
    }
}

static void cc_ssa_tmpassign_param(
    cc_ssa_param* param, unsigned short tmpid, cc_ssa_param new_colour)
{
    if (param->type == SSA_PARAM_TMPVAR && param->data.tmpid == tmpid)
        *param = new_colour;
}

/* Helper function for cc_ssa_tmpassign_func */
static void cc_ssa_tmpassign_binop(
    unsigned short tmpid, cc_ssa_param new_colour, cc_ssa_token* tok)
{
    cc_ssa_tmpassign_param(&tok->data.binop.left, tmpid, new_colour);
    cc_ssa_tmpassign_param(&tok->data.binop.right, tmpid, new_colour);
    cc_ssa_tmpassign_param(&tok->data.binop.extra, tmpid, new_colour);
}

/* Helper function for cc_ssa_tmpassign_func */
static void cc_ssa_tmpassign_unop(
    unsigned short tmpid, cc_ssa_param new_colour, cc_ssa_token* tok)
{
    cc_ssa_tmpassign_param(&tok->data.unop.left, tmpid, new_colour);
    cc_ssa_tmpassign_param(&tok->data.unop.right, tmpid, new_colour);
}

static void cc_ssa_tmpassign_call(
    unsigned short tmpid, cc_ssa_param new_colour, cc_ssa_token* tok)
{
    size_t i;
    cc_ssa_tmpassign_param(&tok->data.call.left, tmpid, new_colour);
    cc_ssa_tmpassign_param(&tok->data.call.right, tmpid, new_colour);
    for (i = 0; i < tok->data.call.n_params; i++)
        cc_ssa_tmpassign_param(&tok->data.call.params[i], tmpid, new_colour);
}

/* Temporal assignment elimination */
/* "Paint" an existing temporal variable into another parameter, for example
   the following SSA:
   
   i32 tmp_0 = i32 var a
   i32 tmp_1 = i32 0
   i32 tmp_0 = i32 tmp_0 + i32 tmp_1
   
   Colouring allows us to replace them as:
   i32 var a = i32 var a
   i32 0 = i32 0
   i32 var a = i32 var a + i32 0 */
static void cc_ssa_tmpassign_func(const cc_ssa_func* func)
{
    size_t i;
    for (i = 0; i < func->n_tokens; i++) {
        cc_ssa_token* vtok = &func->tokens[i];
        unsigned short tmpid;
        size_t j;

        if (vtok->type != SSA_TOKEN_ASSIGN
            || vtok->data.unop.left.type != SSA_PARAM_TMPVAR)
            continue;

        tmpid = vtok->data.unop.left.data.tmpid;
        for (j = 0; j < func->n_tokens; j++) {
            cc_ssa_token* tok = &func->tokens[j];
            switch (tok->type) {
            case SSA_TOKEN_ASSIGN:
            case SSA_TOKEN_ZERO_EXT:
            case SSA_TOKEN_SIGN_EXT:
                cc_ssa_tmpassign_unop(tmpid, vtok->data.unop.right, tok);
                break;
            case SSA_TOKEN_ADD:
            case SSA_TOKEN_SUB:
            case SSA_TOKEN_AND:
            case SSA_TOKEN_BRANCH:
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
                cc_ssa_tmpassign_binop(tmpid, vtok->data.unop.right, tok);
                break;
            case SSA_TOKEN_CALL:
                cc_ssa_tmpassign_call(tmpid, vtok->data.unop.right, tok);
                break;
            case SSA_TOKEN_RET:
            case SSA_TOKEN_LABEL:
            case SSA_TOKEN_ALLOCA:
                /* No operation */
                break;
            case SSA_TOKEN_LOAD_FROM:
            case SSA_TOKEN_STORE_AT:
                break;
            default:
                abort();
            }
        }
    }
}

bool cc_ssa_is_param_same(
    const cc_ssa_param* restrict p1, const cc_ssa_param* restrict p2)
{
    if (p1->type != p2->type)
        return false;
    if (p1->type == SSA_PARAM_VARIABLE && p2->type == SSA_PARAM_VARIABLE)
        return strcmp(p1->data.var_name, p2->data.var_name) == 0;
    return p1->size == p2->size;
}

static void cc_ssa_remove_assign_func(cc_ssa_func* func)
{
    size_t i;
    for (i = 0; i < func->n_tokens; i++) {
        cc_ssa_token* tok = &func->tokens[i];
        bool erase = false;

        switch (tok->type) {
        case SSA_TOKEN_ASSIGN:
            erase = cc_ssa_is_param_same(
                &tok->data.unop.left, &tok->data.unop.right);
            /* Remove read without side effect */
            if (tok->data.unop.left.type == SSA_PARAM_NONE)
                erase = true;

            if (tok->data.unop.left.is_volatile
                || tok->data.unop.right.is_volatile)
                erase = false;
            break;
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

static void cc_ssa_colour_func(cc_ssa_func* func)
{
    cc_ssa_tmpassign_func(func);
    cc_ssa_remove_assign_func(func);
}

static void cc_ssa_colour(cc_context* ctx)
{
    size_t i;
    for (i = 0; i < ctx->n_ssa_funcs; i++)
        cc_ssa_colour_func(&ctx->ssa_funcs[i]);
}

static cc_ast_variable cc_ssa_create_func_var(const char* name)
{
    cc_ast_variable var = { 0 };
    var.type.mode = AST_TYPE_MODE_FUNCTION;
    var.name = cc_strdup(name);
    return var;
}

void cc_ssa_top(cc_context* ctx)
{
    assert(SSA_STORAGE_AUTO == SSA_STORAGE_AUTO);
    assert(SSA_STORAGE_EXTERN == SSA_STORAGE_EXTERN);
    assert(SSA_STORAGE_STATIC == SSA_STORAGE_STATIC);
    assert(SSA_STORAGE_REGISTER == SSA_STORAGE_REGISTER);
    assert(SSA_STORAGE_CONSTEXPR == SSA_STORAGE_CONSTEXPR);
    assert(SSA_STORAGE_GLOBAL == SSA_STORAGE_GLOBAL);
    assert(SSA_STORAGE_THREAD_LOCAL == SSA_STORAGE_THREAD_LOCAL);
    assert(SSA_STORAGE_INLINE == SSA_STORAGE_INLINE);

    cc_ssa_func static_ctor_func = { 0 };
    cc_ast_variable static_ctor_var = cc_ssa_create_func_var("__gdtor");
    static_ctor_func.ast_var = &static_ctor_var;
    ctx->ssa_current_func = &static_ctor_func;
    cc_ssa_param none_param = { 0 };
    cc_ssa_from_ast(ctx, ctx->root, none_param);
    cc_ast_destroy_var(&static_ctor_var, false);

    cc_ssa_print(ctx);

    cc_ssa_colour(ctx);
    cc_ssa_print(ctx);
}
