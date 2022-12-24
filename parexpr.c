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

bool cc_parse_constant_expression(
    cc_context* ctx, cc_ast_node* node, cc_ast_literal* r)
{
    /* Loosely parent-linked to node, however not actually aded to node
       as a children since this is a virtual node to serve as an
       intermediare and facilitate execution. */
    cc_ast_node* const_expr = cc_ast_create_block(ctx, node);
    /* Parse like a normal expression */
    cc_parse_expression(ctx, const_expr);
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

static bool cc_parse_multiplicative_expression(
    cc_context* ctx, cc_ast_node* node)
{
    cc_ast_node* block_node = cc_ast_create_block(ctx, node);
    if (cc_parse_unary_expression(ctx, block_node)) {
        const cc_lexer_token* ctok;
        if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && (ctok->type == LEXER_TOKEN_ASTERISK
                || ctok->type == LEXER_TOKEN_MOD
                || ctok->type == LEXER_TOKEN_DIV)) {
            cc_ast_node* binop_node
                = cc_ast_create_binop_expr(ctx, node, AST_BINOP_NONE);
            cc_lex_token_consume(ctx);
            block_node->parent = binop_node->data.binop.left;
            cc_ast_add_block_node(binop_node->data.binop.left, block_node);
            switch (ctok->type) {
            case LEXER_TOKEN_ASTERISK:
                binop_node->data.binop.op = AST_BINOP_MUL;
                break;
            case LEXER_TOKEN_MOD:
                binop_node->data.binop.op = AST_BINOP_MOD;
                break;
            case LEXER_TOKEN_DIV:
                binop_node->data.binop.op = AST_BINOP_DIV;
                break;
            default:
                break;
            }
            cc_parse_multiplicative_expression(
                ctx, binop_node->data.binop.right);
            cc_ast_add_block_node(node, binop_node);
        } else {
            cc_ast_add_block_node(node, block_node);
        }
        return true;
    }
    return false;
}

static bool cc_parse_additive_expression(cc_context* ctx, cc_ast_node* node)
{
    cc_ast_node* block_node = cc_ast_create_block(ctx, node);
    if (cc_parse_multiplicative_expression(ctx, block_node)) {
        const cc_lexer_token* ctok;
        if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && (ctok->type == LEXER_TOKEN_PLUS
                || ctok->type == LEXER_TOKEN_MINUS)) {
            cc_ast_node* binop_node
                = cc_ast_create_binop_expr(ctx, node, AST_BINOP_NONE);
            cc_lex_token_consume(ctx);
            block_node->parent = binop_node->data.binop.left;
            cc_ast_add_block_node(binop_node->data.binop.left, block_node);
            switch (ctok->type) {
            case LEXER_TOKEN_PLUS:
                binop_node->data.binop.op = AST_BINOP_ADD;
                break;
            case LEXER_TOKEN_MINUS:
                binop_node->data.binop.op = AST_BINOP_SUB;
                break;
            default:
                break;
            }
            cc_parse_additive_expression(ctx, binop_node->data.binop.right);
            cc_ast_add_block_node(node, binop_node);
        } else {
            cc_ast_add_block_node(node, block_node);
        }
        return true;
    }
    return false;
}

static bool cc_parse_shift_expression(cc_context* ctx, cc_ast_node* node)
{
    cc_ast_node* block_node = cc_ast_create_block(ctx, node);
    if (cc_parse_additive_expression(ctx, block_node)) {
        const cc_lexer_token* ctok;
        if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && (ctok->type == LEXER_TOKEN_LSHIFT
                || ctok->type == LEXER_TOKEN_RSHIFT)) {
            cc_ast_node* binop_node
                = cc_ast_create_binop_expr(ctx, node, AST_BINOP_NONE);
            cc_lex_token_consume(ctx);
            block_node->parent = binop_node->data.binop.left;
            cc_ast_add_block_node(binop_node->data.binop.left, block_node);
            switch (ctok->type) {
            case LEXER_TOKEN_LSHIFT:
                binop_node->data.binop.op = AST_BINOP_LSHIFT;
                break;
            case LEXER_TOKEN_RSHIFT:
                binop_node->data.binop.op = AST_BINOP_RSHIFT;
                break;
            default:
                break;
            }
            cc_parse_shift_expression(ctx, binop_node->data.binop.right);
            cc_ast_add_block_node(node, binop_node);
        } else {
            cc_ast_add_block_node(node, block_node);
        }
        return true;
    }
    return false;
}

static bool cc_parse_relational_expression(cc_context* ctx, cc_ast_node* node)
{
    cc_ast_node* block_node = cc_ast_create_block(ctx, node);
    if (cc_parse_shift_expression(ctx, block_node)) {
        const cc_lexer_token* ctok;
        if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && (ctok->type == LEXER_TOKEN_GT || ctok->type == LEXER_TOKEN_GTE
                || ctok->type == LEXER_TOKEN_LT
                || ctok->type == LEXER_TOKEN_LTE)) {
            cc_ast_node* binop_node
                = cc_ast_create_binop_expr(ctx, node, AST_BINOP_NONE);
            cc_lex_token_consume(ctx);
            block_node->parent = binop_node->data.binop.left;
            cc_ast_add_block_node(binop_node->data.binop.left, block_node);
            switch (ctok->type) {
            case LEXER_TOKEN_GT:
                binop_node->data.binop.op = AST_BINOP_GT;
                break;
            case LEXER_TOKEN_GTE:
                binop_node->data.binop.op = AST_BINOP_GTE;
                break;
            case LEXER_TOKEN_LT:
                binop_node->data.binop.op = AST_BINOP_LT;
                break;
            case LEXER_TOKEN_LTE:
                binop_node->data.binop.op = AST_BINOP_LTE;
                break;
            default:
                break;
            }
            cc_parse_relational_expression(ctx, binop_node->data.binop.right);
            cc_ast_add_block_node(node, binop_node);
        } else {
            cc_ast_add_block_node(node, block_node);
        }
        return true;
    }
    return false;
}

static bool cc_parse_equality_expression(cc_context* ctx, cc_ast_node* node)
{
    cc_ast_node* block_node = cc_ast_create_block(ctx, node);
    if (cc_parse_relational_expression(ctx, block_node)) {
        const cc_lexer_token* ctok;
        if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && (ctok->type == LEXER_TOKEN_COND_EQ
                || ctok->type == LEXER_TOKEN_COND_NEQ)) {
            cc_ast_node* binop_node
                = cc_ast_create_binop_expr(ctx, node, AST_BINOP_NONE);
            cc_lex_token_consume(ctx);
            block_node->parent = binop_node->data.binop.left;
            cc_ast_add_block_node(binop_node->data.binop.left, block_node);
            switch (ctok->type) {
            case LEXER_TOKEN_COND_EQ:
                binop_node->data.binop.op = AST_BINOP_COND_EQ;
                break;
            case LEXER_TOKEN_COND_NEQ:
                binop_node->data.binop.op = AST_BINOP_COND_NEQ;
                break;
            default:
                break;
            }
            cc_parse_equality_expression(ctx, binop_node->data.binop.right);
            cc_ast_add_block_node(node, binop_node);
        } else {
            cc_ast_add_block_node(node, block_node);
        }
        return true;
    }
    return false;
}

#define CC_PARSER_OPERATOR_FN(fn_name, lw_fn_name, tok_type, binop_type)       \
    static bool fn_name(cc_context* ctx, cc_ast_node* node)                    \
    {                                                                          \
        cc_ast_node* block_node = cc_optimizer_is_empty_block(node)            \
            ? node                                                             \
            : cc_ast_create_block(ctx, node);                                  \
        if (lw_fn_name(ctx, block_node)) {                                     \
            const cc_lexer_token* ctok;                                        \
            if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL                     \
                && ctok->type == (tok_type)) {                                 \
                cc_ast_node* binop_node                                        \
                    = cc_ast_create_binop_expr(ctx, node, binop_type);         \
                cc_lex_token_consume(ctx);                                     \
                if (block_node == node)                                        \
                    block_node = cc_ast_create_block(ctx, node);               \
                block_node->parent = binop_node->data.binop.left;              \
                cc_ast_add_block_node(                                         \
                    binop_node->data.binop.left, block_node);                  \
                fn_name(ctx, binop_node->data.binop.right);                    \
                cc_ast_add_block_node(node, binop_node);                       \
            } else {                                                           \
                if (block_node != node)                                        \
                    cc_ast_add_block_node(node, block_node);                   \
            }                                                                  \
            return true;                                                       \
        }                                                                      \
        return false;                                                          \
    }
CC_PARSER_OPERATOR_FN(cc_parse_and_expression, cc_parse_equality_expression,
    LEXER_TOKEN_AMPERSAND, AST_BINOP_AND)
CC_PARSER_OPERATOR_FN(cc_parse_exclusive_or_expression, cc_parse_and_expression,
    LEXER_TOKEN_XOR, AST_BINOP_XOR)
CC_PARSER_OPERATOR_FN(cc_parse_inclusive_or_expression,
    cc_parse_exclusive_or_expression, LEXER_TOKEN_OR, AST_BINOP_OR)
CC_PARSER_OPERATOR_FN(cc_parse_logical_and_expression,
    cc_parse_inclusive_or_expression, LEXER_TOKEN_LOGICAL_AND,
    AST_BINOP_COND_AND)
CC_PARSER_OPERATOR_FN(cc_parse_logical_or_expression,
    cc_parse_logical_and_expression, LEXER_TOKEN_LOGICAL_OR, AST_BINOP_COND_OR)
#undef CC_PARSER_OPERATOR_FN

static bool cc_parse_conditional_expression(cc_context* ctx, cc_ast_node* node)
{
    cc_ast_node* expr = cc_ast_create_block(ctx, node);
    if (cc_parse_logical_or_expression(ctx, expr)) {
        const cc_lexer_token* ctok;
        if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && ctok->type == LEXER_TOKEN_TERNARY) {
            cc_lex_token_consume(ctx);

            /* If statment converter, a ternary of the form:
                <expr-1> ? <expr-2> : <expr-3>
               is converted into:
                if (<expr-1>) <expr-2> else <expr-3> */
            cc_ast_node* if_expr = cc_ast_create_if_expr(ctx, node);
            expr->parent = if_expr->data.if_expr.cond;
            cc_ast_add_block_node(if_expr->data.if_expr.cond, expr);

            cc_parse_expression(ctx, if_expr->data.if_expr.block);
            CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_COLON, "Expected ':'");
            cc_parse_conditional_expression(
                ctx, if_expr->data.if_expr.tail_else);

            cc_ast_add_block_node(node, if_expr);
        } else { /* Not a ternary, just add the expr directly */
            cc_ast_add_block_node(node, expr);
        }
        return true;
    }
error_handle:
    cc_ast_destroy_node(expr, true);
    return false;
}

/* Parse an assignment expression of the form
   <lhs-expr> <assignment-operand> <rhs-expr>
   
   LHS can be specified with the lhs argument, in such a case a unary
   function won't do shit */
bool cc_parse_assignment_expression(
    cc_context* ctx, cc_ast_node* node, cc_ast_variable* var)
{
    const cc_lexer_token* ctok;
    cc_ast_node* assign_node
        = cc_ast_create_binop_expr(ctx, node, AST_BINOP_NONE);

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

    /* TODO: Respect operator precedence */
    if ((ctok = cc_lex_token_peek(ctx, 0)) == NULL)
        goto finish_true;
    /* Assignment operator */
    enum cc_ast_binop_type binop_type = AST_BINOP_NONE;
    bool expand_expr = false;
    switch (ctok->type) {
    case LEXER_TOKEN_ASSIGN:
        binop_type = AST_BINOP_ASSIGN;
        break;
    case LEXER_TOKEN_ASSIGN_AND:
        binop_type = AST_BINOP_AND;
        expand_expr = true;
        break;
    case LEXER_TOKEN_ASSIGN_DIV:
        binop_type = AST_BINOP_DIV;
        expand_expr = true;
        break;
    case LEXER_TOKEN_ASSIGN_MINUS:
        binop_type = AST_BINOP_SUB;
        expand_expr = true;
        break;
    case LEXER_TOKEN_ASSIGN_MOD:
        binop_type = AST_BINOP_MOD;
        expand_expr = true;
        break;
    case LEXER_TOKEN_ASSIGN_MUL:
        binop_type = AST_BINOP_MUL;
        expand_expr = true;
        break;
    case LEXER_TOKEN_ASSIGN_OR:
        binop_type = AST_BINOP_OR;
        expand_expr = true;
        break;
    case LEXER_TOKEN_ASSIGN_PLUS:
        binop_type = AST_BINOP_ADD;
        expand_expr = true;
        break;
    case LEXER_TOKEN_ASSIGN_XOR:
        binop_type = AST_BINOP_XOR;
        expand_expr = true;
        break;
    case LEXER_TOKEN_ASSIGN_LSHIFT:
        binop_type = AST_BINOP_LSHIFT;
        expand_expr = true;
        break;
    case LEXER_TOKEN_ASSIGN_RSHIFT:
        binop_type = AST_BINOP_RSHIFT;
        expand_expr = true;
        break;
    default:
        goto finish_true; /* Skip assignment and just finish */
    }
    cc_lex_token_consume(ctx);

    /* Expand assignment <lhs> += <rhs> into <lhs> = <lhs> + <rhs> */
    /*cc_ast_create_assign_expr();*/
    assert(binop_type != AST_BINOP_NONE);
    if (expand_expr) {
        cc_ast_node* binop_node = cc_ast_create_binop_expr(
            ctx, assign_node->data.binop.right, binop_type);
        assign_node->data.binop.op = AST_BINOP_ASSIGN;
        cc_ast_copy_node(
            binop_node->data.binop.left, assign_node->data.binop.left);
        cc_parse_assignment_expression(ctx, binop_node->data.binop.right, NULL);
        cc_ast_add_block_node(assign_node->data.binop.right, binop_node);
    } else {
        assign_node->data.binop.op = binop_type;
        /* Field name follows, thankfully for us (and our sanity), we are able
           to use a direct identifier :D */
        if (binop_type == AST_BINOP_DOT || binop_type == AST_BINOP_ARROW) {
            CC_PARSE_EXPECT(
                ctx, ctok, LEXER_TOKEN_IDENT, "Field-name expected");
            cc_ast_add_block_node(assign_node->data.binop.right,
                cc_ast_create_field_ref(
                    ctx, assign_node->data.binop.right, ctok->data));
        } else {
            cc_parse_assignment_expression(
                ctx, assign_node->data.binop.right, NULL);
        }
    }
finish_true:
    cc_ast_add_block_node(node, assign_node);
    return true;
error_handle:
    cc_ast_destroy_node(assign_node, true);
    return false;
}

static bool cc_parse_unary_call(cc_context* ctx, cc_ast_node* node,
    const cc_ast_variable* var, cc_ast_node** expr_result)
{
    const cc_lexer_token* ctok;
    if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
        && ctok->type == LEXER_TOKEN_LPAREN) {
        cc_lex_token_consume(ctx);

        /* Collect arguments (they can be optional) */
        cc_ast_node* call_node = cc_ast_create_call(ctx, node);
        cc_ast_node* left_node
            = cc_ast_create_var_ref(ctx, call_node->data.call.call_expr, var);

        /* Left side node (simple ident) */
        cc_ast_add_block_node(call_node->data.call.call_expr, left_node);

        cc_ast_node* virtual_node = cc_ast_create_block(ctx, call_node);
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

        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RPAREN, "Expected ')'");
        *expr_result = call_node;

        if (var->type.mode != AST_TYPE_MODE_FUNCTION)
            cc_diag_error(ctx, "Calling non-function variable");
        return true;
    error_handle:
        cc_ast_destroy_node(call_node, true);
        cc_ast_destroy_node(virtual_node, true);
        return false;
    }
    return false;
}

static bool cc_parse_unary_sizeof_or_alignof(cc_context* ctx, cc_ast_node* node)
{
    const cc_lexer_token* ctok;
    if ((ctok = cc_lex_token_peek(ctx, 0)) == NULL
        || (ctok->type != LEXER_TOKEN_sizeof
            && ctok->type != LEXER_TOKEN_alignof))
        return false;

    bool do_alignof = ctok->type == LEXER_TOKEN_alignof;
    cc_lex_token_consume(ctx);

    /* This block is just a virtual block and will be destroyed
        once we finish evaluating our alignof/sizeof. */
    cc_ast_node* virtual_node = cc_ast_create_block(ctx, node);
    bool old_v = ctx->declaration_ident_optional;
    ctx->declaration_ident_optional = true;
    bool deduce_required = true;

    /* Parenthesis following means we can evaluate and obtain the size
        of a concise expression, otherwise we have to stick with another
        unary expression. */
    cc_ast_type virtual_type = { 0 };
    virtual_type.data.num.is_signed = ctx->is_default_signed;
    if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
        && ctok->type == LEXER_TOKEN_LPAREN) {
        cc_lex_token_consume(ctx);
        cc_ast_variable virtual_var = { 0 };
        if (!cc_parse_declarator(ctx, virtual_node, &virtual_var)) {
            cc_diag_error(
                ctx, "Expected unary expression after alignof/sizeof");
            ctx->declaration_ident_optional = old_v;
            cc_ast_destroy_node(virtual_node, true);
            goto error_handle;
        }
        /* Type can be deduced from variable alone */
        if (virtual_var.type.mode != AST_TYPE_MODE_NONE) {
            cc_ast_copy_type(&virtual_type, &virtual_var.type);
            deduce_required = false;
        }
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
    if (deduce_required) {
        if (!cc_ceval_deduce_type(ctx, virtual_node, &virtual_type)) {
            cc_diag_error(ctx, "Unable to deduce abstract type");
            cc_ast_destroy_node(virtual_node, true);
            goto error_handle;
        }
    }
    cc_ast_destroy_node(virtual_node, true);

    unsigned int r = do_alignof ? ctx->get_alignof(ctx, &virtual_type)
                                : ctx->get_sizeof(ctx, &virtual_type);

    cc_ast_node* literal_node = cc_ast_create_literal(ctx, node,
        (cc_ast_literal) {
            .is_float = false, .is_signed = false, .value.u = r });
    cc_ast_add_block_node(node, literal_node);
    return true;
error_handle:
    return false;
}

static bool cc_parse_postfix_operator(cc_context* ctx, cc_ast_node* node,
    cc_ast_node* expr_node, bool* parent_rerouted)
{
    /* With postfix increment we will do a:
        <unop <op> <expr-node>> */
    const cc_lexer_token* ctok;
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
        cc_lex_token_consume(ctx);

        /* Obtain the sizeof first and foremost! */
        cc_ast_type vtype = { 0 };
        if (!cc_ceval_deduce_type(ctx, expr_node, &vtype)) {
            cc_diag_error(ctx, "Unable to deduce type");
            goto error_handle;
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
            <unop deref <binop <array> + <binop <sizeof-elem * <index>>*/
        cc_ast_node* arr_deref_node
            = cc_ast_create_unop_expr(ctx, node, AST_UNOP_DEREF);
        cc_ast_node* arr_index_node = cc_ast_create_binop_expr(
            ctx, arr_deref_node->data.unop.child, AST_BINOP_ADD);
        expr_node->parent = arr_index_node->data.binop.left;
        *parent_rerouted = true;
        cc_ast_add_block_node(arr_index_node->data.binop.left, expr_node);

        /* Now right side of the addition expression, the multiplication
            part... this is fun! */
        cc_ast_node* arr_index_mul_node = cc_ast_create_binop_expr(
            ctx, arr_index_node->data.binop.right, AST_BINOP_MUL);
        cc_parse_expression(ctx, arr_index_mul_node->data.binop.left);

        cc_ast_node* arr_sizeof_node
            = cc_ast_create_literal(ctx, arr_index_mul_node->data.binop.right,
                (cc_ast_literal) { .is_float = false,
                    .is_signed = false,
                    .value.u = ctx->get_sizeof(ctx, &vtype) });
        cc_ast_add_block_node(
            arr_index_mul_node->data.binop.right, arr_sizeof_node);

        cc_ast_add_block_node(
            arr_index_node->data.binop.right, arr_index_mul_node);

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
        cc_lex_token_consume(ctx);
        cc_ast_node* accessor_node = cc_ast_create_binop_expr(ctx, node,
            ctok->type == LEXER_TOKEN_DOT ? AST_BINOP_DOT : AST_BINOP_ARROW);
        expr_node->parent = accessor_node->data.binop.left;
        *parent_rerouted = true;
        cc_ast_add_block_node(accessor_node->data.binop.left, expr_node);
        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_IDENT, "Expected identifier");
        cc_ast_node* var_node = cc_ast_create_field_ref(
            ctx, accessor_node->data.binop.right, ctok->data);
        cc_ast_add_block_node(accessor_node->data.binop.right, var_node);
        cc_ast_add_block_node(node, accessor_node);

        cc_ast_type type = { 0 };
        cc_ceval_deduce_type(ctx, expr_node, &type);
        if (!cc_ast_is_field_of(&type, var_node->data.var.name))
            cc_diag_error(ctx, "Accessing field '%s' not part of type '%s'",
                var_node->data.var.name, type.name);
    }
        return true;
    default:
        break;
    }
error_handle:
    return false;
}

static bool cc_parse_postfix_expression(cc_context* ctx, cc_ast_node* node)
{
    const cc_lexer_token* ctok;
    if ((ctok = cc_lex_token_peek(ctx, 0)) == NULL)
        return false;

    bool matched_any = false;
    bool parent_rerouted = false;
    cc_ast_node* expr_node = NULL;
    switch (ctok->type) {
    case LEXER_TOKEN_NUMBER:
        cc_lex_token_consume(ctx);
        expr_node = cc_ast_create_literal_from_str(ctx, node, ctok->data);
        matched_any = true;
        goto finish_expr_setup;
    case LEXER_TOKEN_CHAR_LITERAL:
        cc_lex_token_consume(ctx);
        expr_node = cc_ast_create_literal_from_str(ctx, node, ctok->data);
        matched_any = true;
        goto finish_expr_setup;
    case LEXER_TOKEN_IDENT: {
        const cc_ast_variable* var = cc_ast_find_variable(ctok->data, node);
        cc_lex_token_consume(ctx);
        if (var == NULL) {
            cc_diag_error(ctx, "Couldn't find variable '%s'", ctok->data);
            goto error_handle;
        }

        if (cc_parse_unary_call(ctx, node, var, &expr_node)) {
            /* Parsed the call.. */
        } else { /* Not a call, just a variable reference */
            if (var->type.mode == AST_TYPE_MODE_FUNCTION)
                cc_diag_error(
                    ctx, "Expected call for function '%s()'", var->name);
            expr_node = cc_ast_create_var_ref(ctx, node, var);
        }
        matched_any = true;
    } break;
    case LEXER_TOKEN_STRING_LITERAL:
        cc_lex_token_consume(ctx);
        expr_node = cc_ast_create_string_literal(ctx, node, ctok->data);
        matched_any = true;
        break;
    case LEXER_TOKEN___func__:
        cc_lex_token_consume(ctx);
        if(ctx->ast_current_func == NULL)
            cc_diag_warning(ctx, "__func__ used outside of a function");
        expr_node = cc_ast_create_string_literal(ctx, node,
            ctx->ast_current_func != NULL ? ctx->ast_current_func->name : "");
        matched_any = true;
        break;
    default:
        expr_node = cc_ast_create_block(ctx, node);
        break;
    }
    matched_any
        = cc_parse_postfix_operator(ctx, node, expr_node, &parent_rerouted)
        || matched_any;
finish_expr_setup:
    if (!parent_rerouted) {
        expr_node->parent = node;
        cc_ast_add_block_node(node, expr_node);
    }
    return matched_any;
error_handle:
    if (expr_node != NULL)
        cc_ast_destroy_node(expr_node, true);
    return false;
}

/* TODO: Handle operator ordering and post/prefix inc/dec appropriately */
bool cc_parse_unary_expression(cc_context* ctx, cc_ast_node* node)
{
    const cc_lexer_token* ctok;
    if (cc_parse_unary_sizeof_or_alignof(ctx, node))
        return true;

    if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL) {
        cc_ast_node *binop_node = NULL, *literal_node = NULL, *unop_node = NULL;
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
        case LEXER_TOKEN_LPAREN: /* (cast) */
            cc_lex_token_consume(ctx);
            unop_node = cc_ast_create_unop_expr(ctx, node, AST_UNOP_CAST);
            if (cc_parse_declaration_specifier(
                    ctx, unop_node, &unop_node->data.unop.cast)) {
                CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RPAREN, "Expected ')'");
            } else { /* (unary-expresion) */
                cc_ast_destroy_node(unop_node, true);
                cc_ast_node* block_node = cc_ast_create_block(ctx, node);
                if (!cc_parse_expression(ctx, block_node)) {
                    cc_diag_error(
                        ctx, "Malformed expression within parenthesis");
                    CC_PARSE_EXPECT(
                        ctx, ctok, LEXER_TOKEN_RPAREN, "Expected ')'");
                    goto error_handle;
                }
                CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RPAREN, "Expected ')'");

                /* Optional unary expression after parenthesis */
                cc_parse_unary_expression(ctx, block_node);
                cc_ast_add_block_node(node, block_node);
                return true;
            }
            break;
        default:
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

    bool has_match = false;
    while (cc_parse_postfix_expression(ctx, node))
        has_match = true;
    return has_match;
error_handle:
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
    return false;
}
