/* parexp.c - C expression parser. */
#include "parexpr.h"
#include "ast.h"
#include "constevl.h"
#include "context.h"
#include "diag.h"
#include "lexer.h"
#include "optzer.h"
#include "parser.h"
#include "partyp.h"
#include "util.h"
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static bool cc_parse_primary_expression(cc_context* ctx, cc_ast_node* node);
static bool cc_parse_conditional_expression(cc_context* ctx, cc_ast_node* node);
static bool cc_parse_cast_expression(cc_context* ctx, cc_ast_node* node);

bool cc_parse_constant_expression(
    cc_context* ctx, cc_ast_node* node, cc_ast_literal* r)
{
    /* Loosely parent-linked to node, however not actually aded to node
       as a children since this is a virtual node to serve as an
       intermediare and facilitate execution. */
    cc_ast_node* const_expr = cc_ast_create_block(ctx, node);
    /* Parse like a normal expression */
    cc_parse_conditional_expression(ctx, const_expr);
    if (!cc_ceval_constant_expression(ctx, &const_expr, r)) {
        cc_diag_error(ctx, "Unable to evaluate constant expression");
        if (const_expr != NULL)
            cc_ast_destroy_node(const_expr, true);
        return false;
    }
    if (const_expr != NULL)
        cc_ast_destroy_node(const_expr, true);
    return true;
}

/* Only parses the "cast part" of the cast expression.
   diag should be set to false to silently fail instead of emitting a
   diagnostic. */
static bool cc_parse_cast_expression_1(
    cc_context* ctx, cc_ast_node* node, bool diag)
{
    const cc_lexer_token* ctok;
    if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
    && (ctok->type == LEXER_TOKEN_LPAREN)) { /* (cast) */
        cc_ast_node *cast_node;
        cc_lex_token_consume(ctx);
        cast_node = cc_ast_create_unop_expr(ctx, node, AST_UNOP_CAST);
        if (!cc_parse_type_name(ctx, cast_node, &cast_node->data.unop.cast)) {
            if(diag) {
                cc_diag_error(ctx, "Cast expected a typename");
                CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RPAREN, "Expected ')'");
            } else {
                cc_lex_token_unconsume(ctx);
            }
            return false;
        }
        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RPAREN, "Expected ')'");
#if 0
        /* Compound literal */
        if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && ctok->type == LEXER_TOKEN_LBRACE) {
            /* Compound literals are a hidden variable */
            cc_ast_variable var = { 0 };
            /* TODO: Generate compound literal names */
            var.name = cc_strdup("__cnd_1");
            cc_ast_copy_type(&var.type, &cast_node->data.unop.cast);
            cc_parse_declarator_braced_initializer(ctx, node, &var);
            cc_ast_add_block_variable(node, &var);
            return true;
        }
#endif
        if (!cc_parse_cast_expression(ctx, cast_node->data.unop.child)) {
            cc_diag_error(ctx, "Expected an expression after cast");
            goto error_handle;
        }
        cc_ast_add_block_node(node, cast_node);
        return true;
    }
error_handle:
    return false;
}

static bool cc_parse_cast_expression(cc_context* ctx, cc_ast_node* node)
{
    const cc_lexer_token* ctok;
    if (cc_parse_unary_expression(ctx, node))
        return true;
    else
        return cc_parse_cast_expression_1(ctx, node, true);
error_handle:
    return false;
}

#define CC_PARSER_OPERATOR_FN(fn_name, lw_fn_name, tok_cond, binop_cond)       \
    static bool fn_name(cc_context* ctx, cc_ast_node* node)                    \
    {                                                                          \
        cc_ast_node* block_node = cc_ast_create_block(ctx, node);              \
        if (lw_fn_name(ctx, block_node)) {                                     \
            const cc_lexer_token* ctok;                                        \
            if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL                     \
                && (tok_cond)) {                                               \
                cc_ast_node* binop_node                                        \
                    = cc_ast_create_binop_expr(ctx, node, binop_cond);         \
                cc_lex_token_consume(ctx);                                     \
                block_node->parent = binop_node->data.binop.left;              \
                cc_ast_add_block_node(                                         \
                    binop_node->data.binop.left, block_node);                  \
                cc_optimizer_merge_block(ctx, &binop_node->data.binop.left,    \
                    true);                                                     \
                fn_name(ctx, binop_node->data.binop.right);                    \
                cc_optimizer_merge_block(ctx, &binop_node->data.binop.right,   \
                    true);                                                     \
                block_node = binop_node;                                       \
            }                                                                  \
            cc_optimizer_merge_block(ctx, &block_node, true);                  \
            if(block_node != NULL)                                             \
                cc_ast_add_block_node(node, block_node);                       \
            return true;                                                       \
        }                                                                      \
        cc_ast_destroy_node(block_node, true);                                 \
        return false;                                                          \
    }

CC_PARSER_OPERATOR_FN(cc_parse_multiplicative_expression,
    cc_parse_cast_expression,
    ctok->type == LEXER_TOKEN_ASTERISK || ctok->type == LEXER_TOKEN_MOD
    || ctok->type == LEXER_TOKEN_DIV,
    ctok->type == LEXER_TOKEN_ASTERISK ? AST_BINOP_MUL :
        ctok->type == LEXER_TOKEN_MOD ? AST_BINOP_MOD : AST_BINOP_DIV)
CC_PARSER_OPERATOR_FN(cc_parse_additive_expression,
    cc_parse_multiplicative_expression,
    ctok->type == LEXER_TOKEN_PLUS || ctok->type == LEXER_TOKEN_MINUS,
    ctok->type == LEXER_TOKEN_PLUS ? AST_BINOP_ADD : AST_BINOP_SUB)
CC_PARSER_OPERATOR_FN(cc_parse_shift_expression,
    cc_parse_additive_expression,
    ctok->type == LEXER_TOKEN_LSHIFT || ctok->type == LEXER_TOKEN_RSHIFT,
    ctok->type == LEXER_TOKEN_LSHIFT ? AST_BINOP_LSHIFT : AST_BINOP_RSHIFT)
CC_PARSER_OPERATOR_FN(cc_parse_relational_expression,
    cc_parse_shift_expression,
    ctok->type == LEXER_TOKEN_GT || ctok->type == LEXER_TOKEN_GTE
    || ctok->type == LEXER_TOKEN_LT || ctok->type == LEXER_TOKEN_LTE,
    ctok->type == LEXER_TOKEN_GT ? AST_BINOP_GT : ctok->type == LEXER_TOKEN_GTE
    ? AST_BINOP_GTE : ctok->type == LEXER_TOKEN_LT ? AST_BINOP_LT
    : AST_BINOP_LTE)
CC_PARSER_OPERATOR_FN(cc_parse_equality_expression,
    cc_parse_relational_expression,
    ctok->type == LEXER_TOKEN_COND_EQ || ctok->type == LEXER_TOKEN_COND_NEQ,
    ctok->type == LEXER_TOKEN_COND_EQ ? AST_BINOP_COND_EQ : AST_BINOP_COND_NEQ)
CC_PARSER_OPERATOR_FN(cc_parse_and_expression, cc_parse_equality_expression,
    ctok->type == LEXER_TOKEN_AMPERSAND, AST_BINOP_AND)
CC_PARSER_OPERATOR_FN(cc_parse_exclusive_or_expression, cc_parse_and_expression,
    ctok->type == LEXER_TOKEN_XOR, AST_BINOP_XOR)
CC_PARSER_OPERATOR_FN(cc_parse_inclusive_or_expression,
    cc_parse_exclusive_or_expression, ctok->type == LEXER_TOKEN_OR,
    AST_BINOP_OR)
CC_PARSER_OPERATOR_FN(cc_parse_logical_and_expression,
    cc_parse_inclusive_or_expression, ctok->type == LEXER_TOKEN_LOGICAL_AND,
    AST_BINOP_COND_AND)
CC_PARSER_OPERATOR_FN(cc_parse_logical_or_expression,
    cc_parse_logical_and_expression, ctok->type == LEXER_TOKEN_LOGICAL_OR,
    AST_BINOP_COND_OR)
#undef CC_PARSER_OPERATOR_FN

static bool cc_parse_conditional_expression(cc_context* ctx, cc_ast_node* node)
{
    cc_ast_node* block_node = cc_ast_create_block(ctx, node);
    if (cc_parse_logical_or_expression(ctx, block_node)) {
        const cc_lexer_token* ctok;
        if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && ctok->type == LEXER_TOKEN_TERNARY) {
            cc_lex_token_consume(ctx);
            /* If statment converter, a ternary of the form:
                <expr-1> ? <expr-2> : <expr-3>
               is converted into:
                if (<expr-1>) <expr-2> else <expr-3> */
            cc_ast_node* if_expr = cc_ast_create_if_expr(ctx, node);
            block_node->parent = if_expr->data.if_expr.cond;
            cc_ast_add_block_node(if_expr->data.if_expr.cond, block_node);
            cc_parse_expression(ctx, if_expr->data.if_expr.block);
            CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_COLON, "Expected ':'");
            cc_parse_conditional_expression(
                ctx, if_expr->data.if_expr.tail_else);
            block_node = if_expr;
        }
        if (block_node != node)
            cc_ast_add_block_node(node, block_node);
        return true;
    }
error_handle:
    if (block_node != node)
        cc_ast_destroy_node(block_node, true);
    return false;
}

static bool cc_parse_assignment_operator(
    cc_context* ctx, enum cc_ast_binop_type* type)
{
    const cc_lexer_token* ctok;
    if ((ctok = cc_lex_token_peek(ctx, 0)) == NULL)
        return false;
    /* Assignment operator */
    switch (ctok->type) {
    case LEXER_TOKEN_ASSIGN:
        *type = AST_BINOP_ASSIGN;
        break;
    case LEXER_TOKEN_ASSIGN_AND:
        *type = AST_BINOP_AND;
        break;
    case LEXER_TOKEN_ASSIGN_DIV:
        *type = AST_BINOP_DIV;
        break;
    case LEXER_TOKEN_ASSIGN_MINUS:
        *type = AST_BINOP_SUB;
        break;
    case LEXER_TOKEN_ASSIGN_MOD:
        *type = AST_BINOP_MOD;
        break;
    case LEXER_TOKEN_ASSIGN_MUL:
        *type = AST_BINOP_MUL;
        break;
    case LEXER_TOKEN_ASSIGN_OR:
        *type = AST_BINOP_OR;
        break;
    case LEXER_TOKEN_ASSIGN_PLUS:
        *type = AST_BINOP_ADD;
        break;
    case LEXER_TOKEN_ASSIGN_XOR:
        *type = AST_BINOP_XOR;
        break;
    case LEXER_TOKEN_ASSIGN_LSHIFT:
        *type = AST_BINOP_LSHIFT;
        break;
    case LEXER_TOKEN_ASSIGN_RSHIFT:
        *type = AST_BINOP_RSHIFT;
        break;
    default:
        *type = AST_BINOP_NONE;
        return false;
    }
    cc_lex_token_consume(ctx);
    return true;
}

/* Parse an assignment expression of the form
   <lhs-expr> <assignment-operand> <rhs-expr>

   LHS can be specified with the lhs argument, in such a case a unary
   function won't do shit.
   
   var should be specified for initializer expressions - otherwise leave
   empty. */
bool cc_parse_assignment_expression(
    cc_context* ctx, cc_ast_node* node, cc_ast_variable* var)
{
    const cc_lexer_token* ctok;
    enum cc_ast_binop_type binop_type;
    cc_ast_node* lhs_paren_node = NULL;
    cc_ast_node* assign_node = cc_ast_create_binop_expr(
        ctx, lhs_paren_node != NULL ? lhs_paren_node : node, AST_BINOP_NONE);
    if (var == NULL) {
        if (!cc_parse_conditional_expression(
                ctx, assign_node->data.binop.left)) {
            cc_ast_destroy_node(assign_node, true);
            return false;
        }
    } else {
        cc_ast_node* var_node
            = cc_ast_create_var_ref(ctx, assign_node->data.binop.left, var);
        cc_ast_add_block_node(assign_node->data.binop.left, var_node);
    }
    /* Match the assignment operator... */
    if(cc_parse_assignment_operator(ctx, &binop_type)) {
        /* Expand assignment <lhs> += <rhs> into <lhs> = <lhs> + <rhs> */
        assert(binop_type != AST_BINOP_NONE);
        if (binop_type != AST_BINOP_ASSIGN) {
            cc_ast_node* binop_node = cc_ast_create_binop_expr(
                ctx, assign_node->data.binop.right, binop_type);
            assign_node->data.binop.op = AST_BINOP_ASSIGN;
            cc_ast_copy_node(ctx,
                binop_node->data.binop.left, assign_node->data.binop.left);
            cc_parse_assignment_expression(
                ctx, binop_node->data.binop.right, NULL);
            cc_ast_add_block_node(assign_node->data.binop.right, binop_node);
        } else {
            assign_node->data.binop.op = binop_type;
            /* Field name follows, thankfully for us (and our sanity), we are
            able to use a direct identifier :D */
            if (binop_type == AST_BINOP_DOT || binop_type == AST_BINOP_ARROW) {
                CC_PARSE_EXPECT(
                    ctx, ctok, LEXER_TOKEN_IDENT, "Field-name expected");
                cc_ast_add_block_node(assign_node->data.binop.right,
                    cc_ast_create_field_ref(
                        ctx, assign_node->data.binop.right, ctok->data));
            } else
                cc_parse_assignment_expression(
                    ctx, assign_node->data.binop.right, NULL);
        }
    }
    cc_ast_add_block_node(node, assign_node);
    return true;
error_handle:
    cc_ast_destroy_node(assign_node, true);
    return false;
}

static bool cc_parse_unary_call(cc_context* ctx, cc_ast_node* node,
    cc_ast_node* call_expr, cc_ast_node** expr_result)
{
    const cc_lexer_token* ctok;
    cc_ast_node* call_node;
    cc_ast_node* virtual_node;

    /* Collect arguments (they can be optional) */
    call_node = cc_ast_create_call(ctx, node);
    call_expr->parent = call_node->data.call.call_expr;
    /* Left side node (simple ident) */
    cc_ast_add_block_node(call_node->data.call.call_expr, call_expr);

    virtual_node = cc_ast_create_block(ctx, call_node);
    while (cc_parse_assignment_expression(ctx, virtual_node, NULL)) {
        cc_ast_add_call_param(call_node, virtual_node);
        virtual_node = cc_ast_create_block(ctx, call_node);
        if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && ctok->type == LEXER_TOKEN_COMMA) {
            cc_lex_token_consume(ctx);
            continue;
        }
        break;
    }
    cc_ast_destroy_node(virtual_node, true);
    *expr_result = call_node;
    return true;
}

static bool cc_parse_unary_sizeof_or_alignof(cc_context* ctx, cc_ast_node* node)
{
    const cc_lexer_token* ctok;
    cc_ast_type virtual_type = { 0 };
    cc_ast_node* virtual_node;
    bool deduce_required = true;
    bool do_alignof;
    bool old_v;
    bool old_szv;

    if ((ctok = cc_lex_token_peek(ctx, 0)) == NULL
        || (ctok->type != LEXER_TOKEN_sizeof
            && ctok->type != LEXER_TOKEN_alignof))
        return false;

    do_alignof = ctok->type == LEXER_TOKEN_alignof;
    cc_lex_token_consume(ctx);

    /* This block is just a virtual block and will be destroyed
        once we finish evaluating our alignof/sizeof. */
    virtual_node = cc_ast_create_block(ctx, node);
    old_v = ctx->declaration_ident_optional;
    old_szv = ctx->parsing_sizeof;
    ctx->declaration_ident_optional = true;
    ctx->parsing_sizeof = true;

    /* Parenthesis following means we can evaluate and obtain the size
        of a concise expression, otherwise we have to stick with another
        unary expression. */
    ctx->sizeof_type = &virtual_type;
    if (cc_parse_unary_expression(ctx, virtual_node)) {
        /* Will deduce type later... */
        deduce_required = true;
    } else if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
        && ctok->type == LEXER_TOKEN_LPAREN) {
        cc_ast_variable virtual_var = { 0 };
        cc_lex_token_consume(ctx);
        if (!cc_parse_type_name(ctx, virtual_node, &virtual_var.type)) {
            cc_diag_error(ctx,
                "Expected unary expression after alignof/sizeof");
            ctx->declaration_ident_optional = old_v;
            cc_ast_destroy_node(virtual_node, true);
            goto error_handle;
        }
        /* Type can be deduced from variable alone */
        assert(virtual_var.type.mode != AST_TYPE_MODE_NONE);
        cc_ast_copy_type(&virtual_type, &virtual_var.type);
        deduce_required = false;
        cc_ast_destroy_var(&virtual_var, false);
        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RPAREN, "Expected ')'");
    } else {
        ctx->declaration_ident_optional = true;
        if (!cc_parse_unary_expression(ctx, virtual_node)) {
            cc_diag_error(
                ctx, "Expected unary expression after alignof/sizeof");
            ctx->declaration_ident_optional = old_v;
            cc_ast_destroy_node(virtual_node, true);
            goto error_handle;
        }
    }
    ctx->declaration_ident_optional = old_v;
    ctx->parsing_sizeof = old_szv;
    if (deduce_required) {
        if (!cc_ceval_deduce_type(ctx, virtual_node, &virtual_type)) {
            cc_diag_error(ctx, "Unable to deduce abstract type");
            cc_ast_destroy_node(virtual_node, true);
            goto error_handle;
        }
    }
    cc_ast_destroy_node(virtual_node, true);

    if(virtual_type.cv_qual[virtual_type.n_cv_qual].is_array) {
        if (virtual_type.cv_qual[virtual_type.n_cv_qual].is_vla) {
            /* VLA type uses a node that needs to be computed at runtime! */
            const cc_ast_node* array_size_expr
                = virtual_type.cv_qual[virtual_type.n_cv_qual].array.size_expr;
            assert(array_size_expr != NULL);
            /* TODO: We should have a "create block by type" function
            so we don't crash when we do not receive a block node! */
            cc_ast_node* expr_node = cc_ast_create_block(ctx, node);
            cc_ast_copy_node(ctx, expr_node, array_size_expr);
            expr_node->parent = node;
            cc_ast_add_block_node(node, expr_node);
        } else {
            /* Non-VLA array uses constant size literal node */
            unsigned int r = do_alignof
                ? ctx->get_alignof(ctx, &virtual_type)
                : virtual_type.cv_qual[virtual_type.n_cv_qual].array.size;
            cc_ast_literal tmp_literal = { 0 };
            tmp_literal.is_float = tmp_literal.is_signed = false;
            tmp_literal.value.u = r;
            cc_ast_add_block_node(node,
                cc_ast_create_literal(ctx, node, tmp_literal));
        }
    } else {
        unsigned int r = do_alignof ? ctx->get_alignof(ctx, &virtual_type)
                    : ctx->get_sizeof(ctx, &virtual_type);
        cc_ast_literal tmp_literal = { 0 };
        cc_ast_node* literal_node;
        tmp_literal.is_float = tmp_literal.is_signed = false;
        tmp_literal.value.u = r;
        cc_ast_add_block_node(node,
            cc_ast_create_literal(ctx, node, tmp_literal));
    }
    return true;
error_handle:
    ctx->declaration_ident_optional = old_v;
    ctx->parsing_sizeof = old_szv;
    return false;
}

/* Produce and verify diagnostics for calls to functions this is it's own
   function because signatures for libc functions are also part of our
   diagnostics. */
static void cc_lint_call_node(
    cc_context* ctx, cc_ast_node* node, cc_ast_type type)
{
    size_t i;
    assert(node->type == AST_NODE_CALL);
    if (type.mode != AST_TYPE_MODE_FUNCTION) {
        if (type.n_cv_qual > 0) {
            cc_diag_error(
                ctx, "Calling non-function pointer, consider casting");
            goto error_handle;
        }
        cc_diag_error(ctx, "Calling non-function type");
        goto error_handle;
    }

    if (node->data.call.n_params > type.data.func.n_params
        && !type.data.func.variadic) {
        cc_diag_error(ctx,
            "Too many parameters on call to non-variadic function ("
            "expected %u)", (unsigned int)type.data.func.n_params);
        goto error_handle;
    } else if (
        node->data.call.n_params < type.data.func.n_params) {
        cc_diag_error(ctx, "Too few parameters on call to %s function ("
            "expected %u)",
            type.data.func.variadic ? "variadic" : "non-variadic",
            (unsigned int)type.data.func.n_params);
        goto error_handle;
    }

    /* Only evaluate the "shared number count of parameters", that is
      calling a variadic function without a parameter(s) on what would
      be the ellipsis, is completely valid; In other words:

      printf("hi %i", 10); - Is valid
      printf(); - Is not valid
      printf("hi"); - Is valid */
    for (i = 0; i < node->data.call.n_params && i < type.data.func.n_params;
        ++i) {
        cc_ast_type call_param_type = { 0 };
        if (cc_ceval_deduce_type(ctx, &node->data.call.params[i],
                &call_param_type)) {
            cc_ast_variable* param = &type.data.func.params[i];
            /* Expect pointer, but obtained non-pointer */
            if (param->type.n_cv_qual > 0
                && call_param_type.n_cv_qual == 0) {
                cc_diag_warning(ctx, "Passing argument to %i '%s'; implicitly "
                    "converting to a pointer",
                    i + 1,
                    param->type.name == NULL ? "<anonymous>"
                                             : param->type.name);
            } else if (param->type.n_cv_qual != call_param_type.n_cv_qual)
                cc_diag_warning(ctx, "Difference in pointer depth in "
                    "argument %i", i + 1);
        }
    }
error_handle:
    ;
}

/* Parses the postfix operator part of the postfix expression. The parent
   rerouting argument tells if this function did rearrange the expr_node
   in such a way that expr_node->parent is no longer node, but rather
   another node. This is needed for increments and decrements for example. */
static bool cc_parse_postfix_operator(cc_context* ctx, cc_ast_node* node,
    cc_ast_node* expr_node, bool *parent_rerouted)
{
    /* With postfix increment we will do a:
        <unop <op> <expr-node>> */
    const cc_lexer_token* ctok;
    *parent_rerouted = false;
    if ((ctok = cc_lex_token_peek(ctx, 0)) == NULL)
        return false;

    switch (ctok->type) {
    case LEXER_TOKEN_INCREMENT:
    case LEXER_TOKEN_DECREMENT: { /* Postfix ++/-- */
        cc_lex_token_consume(ctx); /* <unop <postinc> <expr>> */
        cc_ast_node* pi_node = cc_ast_create_unop_expr(ctx, node,
            ctok->type == LEXER_TOKEN_INCREMENT ? AST_UNOP_POSTINC
                                                : AST_UNOP_POSTDEC);
        expr_node->parent = pi_node->data.unop.child;
        *parent_rerouted = true;
        cc_ast_add_block_node(pi_node->data.unop.child, expr_node);
        cc_ast_add_block_node(node, pi_node);
    }
        return true;
    /* Array accessor <expr>[<expr>] syntax */
    case LEXER_TOKEN_LBRACKET: {
        /* Obtain the sizeof first and foremost! */
        cc_ast_type vtype = { 0 };
        cc_lex_token_consume(ctx);

        /* Temporarily chage parenting to master node so variable and type
           lookup can be done */
        expr_node->parent = node;
        if (expr_node->type == AST_NODE_BLOCK
            && !expr_node->data.block.n_children) {
            if (!cc_ceval_deduce_type(ctx, node, &vtype)) {
                cc_diag_error(ctx, "Unable to deduce type");
                goto error_handle;
            }
        } else {
            if (!cc_ceval_deduce_type(ctx, expr_node, &vtype)) {
                cc_diag_error(ctx, "Unable to deduce type");
                goto error_handle;
            }
        }

        if (!vtype.n_cv_qual) {
            cc_diag_error(ctx, "Accessed array type is non-pointer");
            cc_ast_node* tmp_node = cc_ast_create_block(ctx, node);
            cc_parse_expression(ctx, tmp_node);
            CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RBRACKET, "Expected ']'");
            cc_ast_destroy_node(tmp_node, true);
            goto error_handle;
        }
        vtype.n_cv_qual--;

        /* Create an expression of the form:
            <unop deref <binop <array> + index>>*/
        cc_ast_node* arr_deref_node
            = cc_ast_create_unop_expr(ctx, node, AST_UNOP_DEREF);
        cc_ast_node* arr_index_node = cc_ast_create_binop_expr(
            ctx, arr_deref_node->data.unop.child, AST_BINOP_ADD);
        expr_node->parent = arr_index_node->data.binop.left;
        *parent_rerouted = true;
        cc_ast_add_block_node(arr_index_node->data.binop.left, expr_node);
        cc_parse_expression(ctx, arr_index_node->data.binop.right);

        /* Wrap all of that and we obtain the address we wish! */
        cc_ast_add_block_node(arr_deref_node->data.unop.child, arr_index_node);
        cc_optimizer_expr_condense(ctx, &arr_deref_node, true);
        if (arr_deref_node == NULL) {
            cc_diag_error(ctx, "Empty array access");
            CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RBRACKET, "Expected ']'");
            goto error_handle;
        }
        cc_ast_add_block_node(node, arr_deref_node);

        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RBRACKET, "Expected ']'");
    }
        return true;
    case LEXER_TOKEN_DOT:
    case LEXER_TOKEN_ARROW: {
        cc_ast_node* accessor_node;
        cc_ast_node* var_node;
        cc_ast_type type = { 0 };

        cc_lex_token_consume(ctx);
        accessor_node = cc_ast_create_binop_expr(ctx, node,
            ctok->type == LEXER_TOKEN_DOT ? AST_BINOP_DOT : AST_BINOP_ARROW);
        expr_node->parent = accessor_node->data.binop.left;
        *parent_rerouted = true;
        cc_ast_add_block_node(accessor_node->data.binop.left, expr_node);
        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_IDENT, "Expected identifier");
        var_node = cc_ast_create_field_ref(
            ctx, accessor_node->data.binop.right, ctok->data);
        cc_ast_add_block_node(accessor_node->data.binop.right, var_node);
        /* Condense the single field */
        cc_optimizer_expr_condense(ctx, &accessor_node->data.binop.right, true);

        cc_ast_add_block_node(node, accessor_node);

        cc_ceval_deduce_type(ctx, expr_node, &type);
        if (cc_ast_get_field_of(&type, var_node->data.var.name) == NULL)
            cc_diag_error(ctx, "Accessing field '%s' not part of type '%s'",
                var_node->data.var.name, type.name);
    }
        return true;
    case LEXER_TOKEN_LPAREN: {
        cc_ast_node* result_node = NULL;
        cc_ast_type type = { 0 };
        cc_lex_token_consume(ctx);

        cc_parse_unary_call(ctx, node, expr_node, &result_node);
        *parent_rerouted = true;
        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RPAREN, "Expected ')'");

        result_node->parent = node;
        cc_ast_add_block_node(node, result_node);

        /* Do linting for parameters (for example when passing an integer as
           as pointer) - warning about those is not required if I recall
           correctly, however everyone nowadays expects a good linter. */
        if (cc_ceval_deduce_type(ctx, result_node, &type)) {
            cc_lint_call_node(ctx, result_node, type);
        } else {
            cc_diag_error(
                ctx, "Unable to deduce type for runtime-evaluated call");
            goto error_handle;
        }
    } return true;
    default:
        break;
    }
error_handle:
    return false;
}

static bool cc_parse_postfix_expression(cc_context* ctx, cc_ast_node* node)
{
    const cc_lexer_token* ctok;
    cc_ast_node* expr_node;
    bool parent_rerouted;
    if ((ctok = cc_lex_token_peek(ctx, 0)) == NULL)
        return false;

    expr_node = cc_ast_create_block(ctx, node);
    if(cc_parse_primary_expression(ctx, expr_node)) {
        /* Optional operator after primary expression uwu */
        cc_parse_postfix_operator(ctx, node, expr_node, &parent_rerouted);
    } else if(cc_parse_postfix_operator(ctx, node, expr_node, &parent_rerouted)) {

    } else
        goto error_handle;
    if(!parent_rerouted) {
        cc_ast_add_block_node(node, expr_node);
    }
    return true;
error_handle:
    if (expr_node != NULL)
        cc_ast_destroy_node(expr_node, true);
    return false;
}

/* TODO: Handle operator ordering and post/prefix inc/dec appropriately */
bool cc_parse_unary_expression(cc_context* ctx, cc_ast_node* node)
{
    const cc_lexer_token* ctok;
    if (cc_parse_postfix_expression(ctx, node))
        return true;
    else if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL) {
        cc_ast_node *binop_node = NULL;
        cc_ast_node *literal_node = NULL;
        cc_ast_node *unop_node = NULL;
        switch (ctok->type) {
        case LEXER_TOKEN_PLUS: /* Prefix +*/
            cc_lex_token_consume(ctx);
            binop_node = cc_ast_create_binop_expr(ctx, node, AST_BINOP_ADD);
            literal_node = cc_ast_create_literal_from_str(
                ctx, binop_node->data.binop.left, "0");
            break;
        case LEXER_TOKEN_MINUS: /* Prefix - */
            cc_lex_token_consume(ctx);
            binop_node = cc_ast_create_binop_expr(ctx, node, AST_BINOP_SUB);
            literal_node = cc_ast_create_literal_from_str(
                ctx, binop_node->data.binop.left, "0");
            break;
        case LEXER_TOKEN_INCREMENT: /* Prefix ++ */
            cc_lex_token_consume(ctx);
            unop_node = cc_ast_create_unop_expr(ctx, node, AST_UNOP_PREINC);
            break;
        case LEXER_TOKEN_DECREMENT: /* Prefix -- */
            cc_lex_token_consume(ctx);
            unop_node = cc_ast_create_unop_expr(ctx, node, AST_UNOP_PREDEC);
            break;
        case LEXER_TOKEN_AMPERSAND: /* Reference */
            cc_lex_token_consume(ctx);
            unop_node = cc_ast_create_unop_expr(ctx, node, AST_UNOP_REF);
            break;
        case LEXER_TOKEN_ASTERISK: /* Dereference */
            cc_lex_token_consume(ctx);
            unop_node = cc_ast_create_unop_expr(ctx, node, AST_UNOP_DEREF);
            break;
        case LEXER_TOKEN_COND_NOT: /* Conditional not */
            cc_lex_token_consume(ctx);
            unop_node = cc_ast_create_unop_expr(ctx, node, AST_UNOP_COND_NOT);
            break;
        case LEXER_TOKEN_NOT: /* Bitwise not */
            cc_lex_token_consume(ctx);
            unop_node = cc_ast_create_unop_expr(ctx, node, AST_UNOP_NOT);
            break;
        default:
            if (cc_parse_unary_sizeof_or_alignof(ctx, node))
                return true;
            break;
        }

        assert(!(binop_node != NULL && unop_node != NULL));
        if (binop_node != NULL) {
            cc_ast_add_block_node(binop_node->data.binop.left, literal_node);
            if (!cc_parse_unary_expression(ctx, binop_node->data.binop.right)) {
                cc_diag_error(ctx, "Expected an unary expression after binop");
                goto error_handle;
            }
            cc_ast_add_block_node(node, binop_node);
            return true;
        } else if (unop_node != NULL) {
            if (!cc_parse_unary_expression(ctx, unop_node->data.unop.child)) {
                cc_diag_error(ctx, "Expected an unary expression after unary");
                goto error_handle;
            }
            cc_ast_add_block_node(node, unop_node);
            return true;
        }
    }
error_handle:
    return false;
}

static bool cc_parse_primary_expression(cc_context* ctx, cc_ast_node* node)
{
    const cc_lexer_token* ctok;
    cc_ast_node* expr_node = NULL;

    if ((ctok = cc_lex_token_peek(ctx, 0)) == NULL)
        return false;
    switch (ctok->type) {
    case LEXER_TOKEN_NUMBER:
        cc_lex_token_consume(ctx);
        expr_node = cc_ast_create_literal_from_str(ctx, node, ctok->data);
        break;
    case LEXER_TOKEN_CHAR_LITERAL:
        cc_lex_token_consume(ctx);
        expr_node = cc_ast_create_literal_from_str(ctx, node, ctok->data);
        break;
    case LEXER_TOKEN_IDENT: {
        const cc_ast_variable* var = cc_ast_find_variable(
            cc_get_cfunc_name(ctx), ctok->data, node);
        cc_lex_token_consume(ctx);
        if (var == NULL) {
            cc_diag_error(ctx, "Couldn't find variable '%s'", ctok->data);
            goto error_handle;
        }
        expr_node = cc_ast_create_var_ref(ctx, node, var);
    } break;
    case LEXER_TOKEN_STRING_LITERAL:
        cc_lex_token_consume(ctx);
        expr_node = cc_ast_create_string_literal(ctx, node, ctok->data);
        break;
    case LEXER_TOKEN___func__:
        cc_lex_token_consume(ctx);
        if (ctx->ast_current_func == NULL)
            cc_diag_warning(ctx, "__func__ used outside of a function");
        expr_node = cc_ast_create_string_literal(ctx, node,
            cc_get_cfunc_name(ctx) != NULL ? cc_get_cfunc_name(ctx) : "");
        break;
    case LEXER_TOKEN_LPAREN: {
        cc_ast_type tmp_type = {0};

        if (ctx->parsing_sizeof) {
            cc_lex_token_consume(ctx);
            if (cc_parse_type_name(ctx, node, ctx->sizeof_type)) {
                CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RPAREN, "Expected ')'");
                return true;
            }
        } else {
            /* Is this a cast? (may be a cast!) */
            if (cc_parse_cast_expression_1(ctx, node, false))
                return true;
            cc_lex_token_consume(ctx);
        }

        expr_node = cc_ast_create_block(ctx, node);
        if (!cc_parse_expression(ctx, expr_node)) {
            cc_diag_error(
                ctx, "Malformed expression within parenthesis");
            CC_PARSE_EXPECT(
                ctx, ctok, LEXER_TOKEN_RPAREN, "Expected ')'");
            goto error_handle;
        }
        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RPAREN, "Expected ')'");
    } break;
    default:
        return false;
    }
    cc_ast_add_block_node(node, expr_node);
    return true;
error_handle:
    cc_ast_destroy_node(expr_node, true);
    return false;
}

bool cc_parse_expression(cc_context* ctx, cc_ast_node* node)
{
    while (cc_parse_assignment_expression(ctx, node, NULL)) {
        const cc_lexer_token* ctok;
        if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && ctok->type == LEXER_TOKEN_COMMA) {
            cc_lex_token_consume(ctx);
            continue;
        }
        return true;
    }
error_handle:
    return false;
}
