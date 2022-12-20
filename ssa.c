#include "ssa.h"
#include "ast.h"
#include "constevl.h"
#include "util.h"
#include <assert.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>

static enum cc_ssa_storage cc_ssa_ast_storage_to_ssa(enum cc_ast_storage v)
{
    return (enum cc_ssa_storage)v;
}

static cc_ssa_param cc_ssa_literal_to_param(const cc_ast_literal* literal)
{
    if (literal->is_float) {
        return (cc_ssa_param) { .type = SSA_PARAM_CONSTANT,
            .data.constant = (cc_ssa_constant) {
                .is_float = true, .value.d = literal->value.d } };
    }
    if (literal->is_signed) {
        return (cc_ssa_param) { .type = SSA_PARAM_CONSTANT,
            .data.constant = (cc_ssa_constant) { .is_float = false,
                .is_negative = literal->value.s > 0,
                .value.u = abs(literal->value.s) } };
    }
    return (cc_ssa_param) { .type = SSA_PARAM_CONSTANT,
        .data.constant = (cc_ssa_constant) { .is_float = false,
            .is_negative = false,
            .value.u = literal->value.u } };
}

/* Obtain the SSA parameter for the given variable */
static cc_ssa_param cc_ssa_variable_to_param(
    cc_context* ctx, const cc_ast_variable* var)
{
    assert(var->type.mode != AST_TYPE_MODE_FUNCTION);
    return (cc_ssa_param) {
        .type = SSA_PARAM_VARIABLE,
        .storage = cc_ssa_ast_storage_to_ssa(var->type.storage),
        .data = {
            .var_name = cc_strdup(var->name),
        },
        .is_signed = false,
        .size = ctx->get_sizeof(ctx, &var->type),
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
        .data = { 0 },
        .is_signed = false,
        .size = ctx->get_sizeof(ctx, ret_type),
        .version = 0,
    };
}

static unsigned short cc_ssa_get_unique_tmpid(cc_context* ctx)
{
    assert(ctx->tmpid + 1 < USHRT_MAX);
    return ctx->tmpid++;
}

/* Temporary variable parameter */
static cc_ssa_param cc_ssa_tempvar_param(
    cc_context* ctx, const cc_ast_type* base_type)
{
    return (cc_ssa_param) {
        .type = SSA_PARAM_TMPVAR,
        .storage = SSA_STORAGE_AUTO,
        .data = {
            .tmpid = ctx->tmpid++,
        },
        .is_signed = false,
        .size = ctx->get_sizeof(ctx, base_type),
        .version = 0,
    };
}

static void cc_ssa_push_token(cc_ssa_func* func, cc_ssa_token tok)
{
    func->tokens = cc_realloc_array(func->tokens, func->n_tokens + 1);
    func->tokens[func->n_tokens++] = tok;
}

/* For expressions of the type (lhs) = (rhs) */
static void cc_ssa_from_ast(
    cc_context* ctx, const cc_ast_node* node, cc_ssa_param param)
{
    if (node == NULL)
        return;

    cc_ssa_token tok = { 0 };
    switch (node->type) {
    case AST_NODE_BINOP:
        cc_ast_type lhs_type = { 0 };
        if (!cc_ceval_deduce_type(ctx, node->data.binop.left, &lhs_type))
            abort();
        cc_ssa_param lhs_param = cc_ssa_tempvar_param(ctx, &lhs_type);

        cc_ast_type rhs_type = { 0 };
        if (!cc_ceval_deduce_type(ctx, node->data.binop.right, &rhs_type))
            abort();
        cc_ssa_param rhs_param = cc_ssa_tempvar_param(ctx, &rhs_type);

        cc_ssa_from_ast(ctx, node->data.binop.left, lhs_param);
        cc_ssa_from_ast(ctx, node->data.binop.right, rhs_param);

        switch (node->data.binop.op) {
        case AST_BINOP_ADD:
            tok.type = SSA_TOKEN_ADD;
            break;
        case AST_BINOP_MUL:
            tok.type = SSA_TOKEN_MUL;
            break;
        case AST_BINOP_ASSIGN:
            tok.type = SSA_TOKEN_STORE;
            break;
        default:
            abort();
        }
        tok.left = param;
        tok.right = lhs_param;
        tok.data.extra = rhs_param;
        cc_ssa_push_token(ctx->ssa_current_func, tok);
        break;
    case AST_NODE_BLOCK:
        for (size_t i = 0; i < node->data.block.n_vars; i++) {
            const cc_ast_variable* var = &node->data.block.vars[i];
            if (var->type.mode == AST_TYPE_MODE_FUNCTION && var->body != NULL) {
                cc_ssa_func func = { 0 };
                func.ast_var = var;

                cc_ssa_param none_param = { 0 };
                cc_ssa_func* old_current_ssa_func = ctx->ssa_current_func;
                ctx->ssa_current_func = &func;
                cc_ssa_from_ast(ctx, var->body, none_param);
                ctx->ssa_current_func = old_current_ssa_func;

                ctx->ssa_funcs
                    = cc_realloc_array(ctx->ssa_funcs, ctx->n_ssa_funcs + 1);
                ctx->ssa_funcs[ctx->n_ssa_funcs++] = func;
            } else if (var->type.mode != AST_TYPE_MODE_FUNCTION) {
                if (ctx->ssa_current_func != NULL) {
                    tok.type = SSA_TOKEN_ALLOCA;
                    tok.left = cc_ssa_variable_to_param(ctx, var);
                    cc_ast_literal literal
                        = (cc_ast_literal) { .is_float = false,
                              .is_signed = false,
                              .value.u = ctx->get_sizeof(ctx, &var->type) };
                    tok.right = cc_ssa_literal_to_param(&literal);
                    cc_ssa_push_token(ctx->ssa_current_func, tok);
                } else {
                    enum cc_ast_storage storage
                        = var->type.storage & ~(AST_STORAGE_THREAD_LOCAL);
                    assert(storage == AST_STORAGE_EXTERN
                        || storage == AST_STORAGE_GLOBAL
                        || storage == AST_STORAGE_CONSTEXPR);
                }
            }
        }
        for (size_t i = 0; i < node->data.block.n_children; i++)
            cc_ssa_from_ast(ctx, &node->data.block.children[i], param);
        break;
    case AST_NODE_CALL:
        cc_ssa_from_ast(ctx, node->data.call.call_expr, param);
        for (size_t i = 0; i < node->data.call.n_params; i++)
            cc_ssa_from_ast(ctx, &node->data.call.params[i], param);
        break;
    case AST_NODE_SWITCH:
        cc_ssa_from_ast(ctx, node->data.switch_expr.control, param);
        cc_ssa_from_ast(ctx, node->data.switch_expr.block, param);
        break;
    case AST_NODE_IF:
        cc_ssa_from_ast(ctx, node->data.if_expr.cond, param);
        cc_ssa_from_ast(ctx, node->data.if_expr.block, param);
        cc_ssa_from_ast(ctx, node->data.if_expr.tail_else, param);
        break;
    case AST_NODE_JUMP:
        break;
    case AST_NODE_LITERAL:
        break;
    case AST_NODE_RETURN: {
        const cc_ast_variable* var = ctx->ssa_current_func->ast_var;
        assert(var->type.mode == AST_TYPE_MODE_FUNCTION);
        cc_ssa_from_ast(ctx, node->data.return_expr,
            cc_ssa_retval_param(ctx, var->type.data.func.return_type));
    } break;
    case AST_NODE_STRING_LITERAL:
        break;
    case AST_NODE_UNOP:
        cc_ssa_from_ast(ctx, node->data.unop.child, param);
        break;
    case AST_NODE_VARIABLE: {
        const cc_ast_variable* var
            = cc_ast_find_variable(node->data.var.name, node);
    } break;
    default:
        abort();
    }
}

static void cc_ssa_print_param(const cc_ssa_param* param)
{
    switch (param->type) {
    case SSA_PARAM_CONSTANT:
        printf("const ");
        if (param->data.constant.is_negative)
            printf("-");
        if (param->data.constant.is_float)
            printf("%lf", param->data.constant.value.d);
        else
            printf("%u", param->data.constant.value.u);
        break;
    case SSA_PARAM_STRING_LITERAL:
        printf("string \"%s\"", param->data.string_literal);
        break;
    case SSA_PARAM_VARIABLE:
        printf("var %s", param->data.var_name);
        break;
    case SSA_PARAM_TMPVAR:
        printf("t%i", param->data.tmpid);
        break;
    case SSA_PARAM_NONE:
        printf("none");
        break;
    case SSA_PARAM_RETVAL:
        printf("%%retval");
        break;
    default:
        abort();
    }
}

static void cc_ssa_print_token_unop(const cc_ssa_token* tok, const char* name)
{
    cc_ssa_print_param(&tok->left);
    printf(" = %s ", name);
    cc_ssa_print_param(&tok->right);
}

static void cc_ssa_print_token_binop(const cc_ssa_token* tok, const char* name)
{
    cc_ssa_print_param(&tok->left);
    printf(" = ");
    cc_ssa_print_param(&tok->right);
    printf(" %s ", name);
    cc_ssa_print_param(&tok->data.extra);
}

static void cc_ssa_print_token(const cc_ssa_token* tok)
{
    printf("\t");
    switch (tok->type) {
    case SSA_TOKEN_ADD:
        cc_ssa_print_token_binop(tok, "add");
        break;
    case SSA_TOKEN_ALLOCA:
        cc_ssa_print_token_unop(tok, "alloca");
        break;
    case SSA_TOKEN_AND:
        cc_ssa_print_token_binop(tok, "and");
        break;
    case SSA_TOKEN_BRANCH:
        cc_ssa_print_token_unop(tok, "branch");
        break;
    case SSA_TOKEN_CALL:
        printf("call");
        break;
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
    case SSA_TOKEN_LABEL:
        cc_ssa_print_token_unop(tok, "label");
        break;
    case SSA_TOKEN_LOAD:
        cc_ssa_print_token_unop(tok, "load");
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
        cc_ssa_print_token_unop(tok, "ret");
        break;
    case SSA_TOKEN_RSHIFT:
        cc_ssa_print_token_binop(tok, "rshift");
        break;
    case SSA_TOKEN_SIGN_EXT:
        cc_ssa_print_token_unop(tok, "sign_ext");
        break;
    case SSA_TOKEN_STORE:
        cc_ssa_print_token_unop(tok, "store");
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
    default:
        abort();
    }
    printf("\n");
}

static void cc_ssa_print_func(const cc_ssa_func* func)
{
    printf("fn %s {\n", func->ast_var->name);
    for (size_t i = 0; i < func->n_tokens; i++)
        cc_ssa_print_token(&func->tokens[i]);
    printf("}\n");
}

static void cc_ssa_print(cc_context* ctx)
{
    for (size_t i = 0; i < ctx->n_ssa_funcs; i++)
        cc_ssa_print_func(&ctx->ssa_funcs[i]);
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
    ctx->ssa_current_func = NULL;

    cc_ssa_param none_param = { 0 };
    cc_ssa_from_ast(ctx, ctx->root, none_param);

    cc_ssa_print(ctx);
}
