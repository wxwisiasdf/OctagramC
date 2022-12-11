/* parser.c - C parser an AST generator. */
#include "parser.h"
#include "ast.h"
#include "backend.h"
#include "constevl.h"
#include "context.h"
#include "diag.h"
#include "lexer.h"
#include "util.h"
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define CC_PARSE_EXPECT(ctx, ctok, _type, ...)                                 \
    do {                                                                       \
        if (((ctok) = cc_lex_token_peek(ctx, 0)) == NULL                       \
            || (ctok)->type != _type) {                                        \
            cc_diag_error(ctx, __VA_ARGS__);                                   \
            cc_lex_token_consume(ctx);                                         \
            goto error_handle;                                                 \
        }                                                                      \
        cc_lex_token_consume(ctx);                                             \
    } while (0)

static bool cc_parse_declarator(
    cc_context* ctx, cc_ast_node* node, cc_ast_variable* var);
static bool cc_parse_expression(cc_context* ctx, cc_ast_node* node);
static bool cc_parse_constant_expression(
    cc_context* ctx, cc_ast_node* node, cc_ast_literal* r);
static bool cc_parse_compund_statment(cc_context* ctx, cc_ast_node* node);
static bool cc_parse_declaration_specifier(
    cc_context* ctx, cc_ast_node* node, cc_ast_type* type);

static bool cc_parse_struct_or_union_specifier(
    cc_context* ctx, cc_ast_node* node, cc_ast_type* type)
{
    const cc_lexer_token* ctok = cc_lex_token_peek(ctx, 0);
    if (ctok == NULL)
        return false;

    switch (ctok->type) {
    case LEXER_TOKEN_struct:
        cc_lex_token_consume(ctx);
        type->mode = AST_TYPE_MODE_STRUCT;
        break;
    case LEXER_TOKEN_union:
        cc_lex_token_consume(ctx);
        type->mode = AST_TYPE_MODE_UNION;
        break;
    default:
        assert(0);
        break;
    }

    /* TODO: Attributes */
    if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
        && ctok->type == LEXER_TOKEN_IDENT) {
        cc_lex_token_consume(ctx);
        type->name = cc_strdup(ctok->data);
    }

    /* Struct/Union without brace means declaration of a variable OR forward
       declaration... */
    if ((ctok = cc_lex_token_peek(ctx, 0)) == NULL
        || ctok->type != LEXER_TOKEN_LBRACE) {
        cc_ast_type* s_or_u_type = cc_ast_find_type(type->name, node);
        if (s_or_u_type != NULL) { /* We're using an already existing type? */
            if (type->name != NULL) {
                cc_free(type->name);
                type->name = NULL;
            }
            cc_ast_copy_type(type, s_or_u_type);
        } else { /* Not an existing type, forward declaration... */
            /* ... */
        }
        return true;
    }

    CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_LBRACE, "Expected '{'");

    /* TODO: Attributes */
    cc_ast_variable virtual_member = { 0 };
    if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
        && ctok->type == LEXER_TOKEN_RBRACE)
        goto empty_memberlist;

    while (cc_parse_declarator(ctx, node, &virtual_member)) {
        if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && ctok->type == LEXER_TOKEN_COLON) {
            cc_lex_token_consume(ctx);
            virtual_member.type.mode = AST_TYPE_MODE_BITINT;
            virtual_member.type.bitint_bits = 0;

            /* Constexpression follows, evaluate it nicely :) */
            cc_ast_literal literal = { 0 };
            cc_parse_constant_expression(ctx, node, &literal);
            if (literal.is_signed && literal.value.s < 0) {
                cc_diag_error(ctx, "Number of bits can't be negative");
                goto error_handle;
            }
            virtual_member.type.bitint_bits = literal.value.u;
        }
        cc_ast_add_type_member(type, &virtual_member);

        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_SEMICOLON, "Expected ';'");
        memset(&virtual_member, 0, sizeof(virtual_member));

        if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && ctok->type == LEXER_TOKEN_RBRACE)
            break;
    }

empty_memberlist:
    CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RBRACE, "Expected '}'");

    /* We will add structs that are not anonymous into the current defined
       types list so we can use them later as required. */
    if (type->name != NULL) /* Non-anonymous struct */
        cc_ast_add_block_type(node, type);
    return true;
error_handle:
    return false;
}

static bool cc_parse_enum_specifier(cc_context* ctx, cc_ast_type* type)
{
    printf("unimplemented cc_parse_enum_specifier\n");
    return false;
}

static bool cc_parse_function_specifier(cc_context* ctx, cc_ast_type* type)
{
    const cc_lexer_token* ctok = cc_lex_token_peek(ctx, 0);
    if (ctok == NULL)
        return false;
    switch (ctok->type) {
    case LEXER_TOKEN_inline:
        type->storage |= AST_STORAGE_INLINE;
        break;
    case LEXER_TOKEN__Noreturn:
        type->data.func.no_return = true;
        break;
    default:
        return false;
    }
    cc_lex_token_consume(ctx);
    return true;
}

static bool cc_parse_type_qualifier(cc_context* ctx, cc_ast_type* type)
{
    const cc_lexer_token* ctok = cc_lex_token_peek(ctx, 0);
    if (ctok == NULL)
        return false;
    switch (ctok->type) {
    case LEXER_TOKEN_const:
        type->cv_qual[type->n_cv_qual].is_const = true;
        break;
    case LEXER_TOKEN_volatile:
        type->cv_qual[type->n_cv_qual].is_volatile = true;
        break;
    case LEXER_TOKEN_restrict:
        type->cv_qual[type->n_cv_qual].is_restrict = true;
        break;
    case LEXER_TOKEN__Atomic:
        type->cv_qual[type->n_cv_qual].is_atomic = true;
        break;
    default:
        return false;
    }
    cc_lex_token_consume(ctx);
    return true;
}

static bool cc_parse_typedef_name(
    cc_context* ctx, cc_ast_node* node, cc_ast_type* type)
{
    const cc_lexer_token* ctok = cc_lex_token_peek(ctx, 0);
    if (ctok == NULL)
        return false;

    if (ctok->type == LEXER_TOKEN_IDENT) {
        const cc_ast_type* tpdef = NULL;
        if ((tpdef = cc_ast_find_typedef(ctok->data, node)) == NULL)
            return false;
        cc_lex_token_consume(ctx);
        cc_ast_copy_type(type, tpdef);
        return true;
    }
    return false;
}

static bool cc_parse_typeof_specifier(
    cc_context* ctx, cc_ast_node* node, cc_ast_type* type)
{
    const cc_lexer_token* ctok = cc_lex_token_peek(ctx, 0);
    if (ctok == NULL)
        return false;

    if (ctok->type == LEXER_TOKEN_typeof
        || ctok->type == LEXER_TOKEN_typeof_unqual) {
        bool unqual = false;
        if (ctok->type == LEXER_TOKEN_typeof_unqual)
            unqual = true;
        cc_lex_token_consume(ctx);

        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_LPAREN, "Expected '('");
        /* typeof ( <expr> ) */
        cc_ast_node* typeof_expr = cc_ast_create_block(ctx, node);
        cc_parse_expression(ctx, typeof_expr);
        if (!cc_ceval_deduce_type(ctx, typeof_expr, type)) {
            cc_diag_error(ctx, "Unable to deduce type for expression");
            goto error_handle;
        }
        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RPAREN, "Expected ')'");

        /* Unqualify if typeof_unqual is used */
        for (size_t i = 0; unqual && i < type->n_cv_qual; i++)
            type->cv_qual[i] = (cc_ast_type_cv) { .is_atomic = false,
                .is_const = false,
                .is_restrict = false,
                .is_volatile = false };
        return true;
    }
error_handle:
    return false;
}

static bool cc_parse_constant_expression(
    cc_context* ctx, cc_ast_node* node, cc_ast_literal* r)
{
    cc_ast_node* const_expr = cc_ast_create_block(ctx, node);
    cc_parse_expression(ctx, const_expr); /* Parse like a normal expression */
    if (!cc_ceval_constant_expression(ctx, const_expr, r)) {
        cc_diag_error(ctx, "Unable to evaluate static expression");
        return false;
    }
    return true;
}

static bool cc_parse_type_specifier(
    cc_context* ctx, cc_ast_node* node, cc_ast_type* type)
{
    const cc_lexer_token* ctok = ctok = cc_lex_token_peek(ctx, 0);
    if (ctok == NULL)
        return false;
    type->mode = AST_TYPE_MODE_INT;
    switch (ctok->type) {
    case LEXER_TOKEN_void:
        type->mode = AST_TYPE_MODE_VOID;
        break;
    case LEXER_TOKEN_char:
        type->mode = AST_TYPE_MODE_CHAR;
        break;
    case LEXER_TOKEN_short:
        type->mode = AST_TYPE_MODE_SHORT;
        break;
    case LEXER_TOKEN_int:
        type->mode = AST_TYPE_MODE_INT;
        break;
    case LEXER_TOKEN_long:
        type->mode = AST_TYPE_MODE_LONG;
        break;
    case LEXER_TOKEN_float:
        type->mode = AST_TYPE_MODE_FLOAT;
        break;
    case LEXER_TOKEN_double:
        type->mode = AST_TYPE_MODE_DOUBLE;
        break;
    case LEXER_TOKEN_signed:
        type->is_signed = true;
        break;
    case LEXER_TOKEN_unsigned:
        type->is_signed = false;
        break;
    case LEXER_TOKEN__BitInt: {
        type->mode = AST_TYPE_MODE_BITINT;
        ctok = cc_lex_token_consume(ctx);
        ctok = cc_lex_token_consume(ctx);

        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_LPAREN, "Expected '('");
        cc_ast_literal literal = { 0 };
        cc_parse_constant_expression(ctx, node, &literal);
        type->bitint_bits = literal.value.u;
        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RPAREN, "Expected ')'");
    } break;
    case LEXER_TOKEN__Bool:
        type->mode = AST_TYPE_MODE_BOOL;
        break;
    case LEXER_TOKEN__Complex:
        type->mode = AST_TYPE_MODE__COMPLEX;
        break;
    case LEXER_TOKEN__Decimal32:
        type->mode = AST_TYPE_MODE__DECIMAL32;
        break;
    case LEXER_TOKEN__Decimal64:
        type->mode = AST_TYPE_MODE__DECIMAL64;
        break;
    case LEXER_TOKEN__Decimal128:
        type->mode = AST_TYPE_MODE__DECIMAL128;
        break;
    case LEXER_TOKEN_struct:
    case LEXER_TOKEN_union:
        return cc_parse_struct_or_union_specifier(ctx, node, type);
    case LEXER_TOKEN_enum:
        cc_lex_token_consume(ctx);
        return cc_parse_enum_specifier(ctx, type);
    default:
        return cc_parse_typedef_name(ctx, node, type)
            || cc_parse_typeof_specifier(ctx, node, type);
    }
    cc_lex_token_consume(ctx);
    return true;
error_handle:
    return false;
}

static bool cc_parse_storage_class_specifier(cc_context* ctx, cc_ast_type* type)
{
    const cc_lexer_token* ctok = ctok = cc_lex_token_peek(ctx, 0);
    if (ctok == NULL)
        return false;

    switch (ctok->type) {
    case LEXER_TOKEN_extern:
        type->storage = AST_STORAGE_EXTERN;
        break;
    case LEXER_TOKEN_static:
        type->storage = AST_STORAGE_STATIC;
        break;
    case LEXER_TOKEN_register:
        type->storage = AST_STORAGE_REGISTER;
        break;
    case LEXER_TOKEN_thread_local:
        type->storage = AST_STORAGE_THREAD_LOCAL;
        break;
    case LEXER_TOKEN_constexpr:
        type->storage = AST_STORAGE_CONSTEXPR;
        break;
    case LEXER_TOKEN_typedef:
        ctx->is_parsing_typedef = true;
        break;
    default:
        return false;
    }
    cc_lex_token_consume(ctx);
    return true;
}

static bool cc_parse_unary_expression(cc_context* ctx, cc_ast_node* node);

static bool cc_parse_multiplicative_expression(
    cc_context* ctx, cc_ast_node* node)
{
    const cc_lexer_token* ctok;
    cc_ast_node* block_node = cc_ast_create_block(ctx, node);
    if (cc_parse_unary_expression(ctx, block_node)) {
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
    const cc_lexer_token* ctok;
    cc_ast_node* block_node = cc_ast_create_block(ctx, node);
    if (cc_parse_multiplicative_expression(ctx, block_node)) {
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
                binop_node->data.binop.op = AST_BINOP_PLUS;
                break;
            case LEXER_TOKEN_MINUS:
                binop_node->data.binop.op = AST_BINOP_MINUS;
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
    const cc_lexer_token* ctok;
    cc_ast_node* block_node = cc_ast_create_block(ctx, node);
    if (cc_parse_additive_expression(ctx, block_node)) {
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
    const cc_lexer_token* ctok;
    cc_ast_node* block_node = cc_ast_create_block(ctx, node);
    if (cc_parse_shift_expression(ctx, block_node)) {
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
    const cc_lexer_token* ctok;
    cc_ast_node* block_node = cc_ast_create_block(ctx, node);
    if (cc_parse_relational_expression(ctx, block_node)) {
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

static bool cc_parse_and_expression(cc_context* ctx, cc_ast_node* node)
{
    const cc_lexer_token* ctok;
    cc_ast_node* block_node = cc_ast_create_block(ctx, node);
    if (cc_parse_equality_expression(ctx, block_node)) {
        if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && ctok->type == LEXER_TOKEN_AMPERSAND) {
            cc_ast_node* binop_node
                = cc_ast_create_binop_expr(ctx, node, AST_BINOP_AND);
            cc_lex_token_consume(ctx);
            block_node->parent = binop_node->data.binop.left;
            cc_ast_add_block_node(binop_node->data.binop.left, block_node);
            cc_parse_and_expression(ctx, binop_node->data.binop.right);
            cc_ast_add_block_node(node, binop_node);
        } else {
            cc_ast_add_block_node(node, block_node);
        }
        return true;
    }
    return false;
}

static bool cc_parse_exclusive_or_expression(cc_context* ctx, cc_ast_node* node)
{
    const cc_lexer_token* ctok;
    cc_ast_node* block_node = cc_ast_create_block(ctx, node);
    if (cc_parse_and_expression(ctx, block_node)) {
        if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && ctok->type == LEXER_TOKEN_XOR) {
            cc_ast_node* binop_node
                = cc_ast_create_binop_expr(ctx, node, AST_BINOP_XOR);
            cc_lex_token_consume(ctx);
            block_node->parent = binop_node->data.binop.left;
            cc_ast_add_block_node(binop_node->data.binop.left, block_node);
            cc_parse_exclusive_or_expression(ctx, binop_node->data.binop.right);
            cc_ast_add_block_node(node, binop_node);
        } else {
            cc_ast_add_block_node(node, block_node);
        }
        return true;
    }
    return false;
}

static bool cc_parse_inclusive_or_expression(cc_context* ctx, cc_ast_node* node)
{
    const cc_lexer_token* ctok;
    cc_ast_node* block_node = cc_ast_create_block(ctx, node);
    if (cc_parse_exclusive_or_expression(ctx, block_node)) {
        if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && ctok->type == LEXER_TOKEN_OR) {
            cc_ast_node* binop_node
                = cc_ast_create_binop_expr(ctx, node, AST_BINOP_OR);
            cc_lex_token_consume(ctx);
            block_node->parent = binop_node->data.binop.left;
            cc_ast_add_block_node(binop_node->data.binop.left, block_node);
            cc_parse_inclusive_or_expression(ctx, binop_node->data.binop.right);
            cc_ast_add_block_node(node, binop_node);
        } else {
            cc_ast_add_block_node(node, block_node);
        }
        return true;
    }
    return false;
}

static bool cc_parse_logical_and_expression(cc_context* ctx, cc_ast_node* node)
{
    const cc_lexer_token* ctok;
    cc_ast_node* block_node = cc_ast_create_block(ctx, node);
    if (cc_parse_inclusive_or_expression(ctx, block_node)) {
        if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && ctok->type == LEXER_TOKEN_LOGICAL_AND) {
            cc_ast_node* binop_node
                = cc_ast_create_binop_expr(ctx, node, AST_BINOP_COND_AND);
            cc_lex_token_consume(ctx);
            block_node->parent = binop_node->data.binop.left;
            cc_ast_add_block_node(binop_node->data.binop.left, block_node);
            cc_parse_logical_and_expression(ctx, binop_node->data.binop.right);
            cc_ast_add_block_node(node, binop_node);
        } else {
            cc_ast_add_block_node(node, block_node);
        }
        return true;
    }
    return false;
}

static bool cc_parse_logical_or_expression(cc_context* ctx, cc_ast_node* node)
{
    const cc_lexer_token* ctok;
    cc_ast_node* block_node = cc_ast_create_block(ctx, node);
    if (cc_parse_logical_and_expression(ctx, block_node)) {
        if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && ctok->type == LEXER_TOKEN_LOGICAL_OR) {
            cc_ast_node* binop_node
                = cc_ast_create_binop_expr(ctx, node, AST_BINOP_COND_OR);
            cc_lex_token_consume(ctx);
            block_node->parent = binop_node->data.binop.left;
            cc_ast_add_block_node(binop_node->data.binop.left, block_node);
            cc_parse_logical_or_expression(ctx, binop_node->data.binop.right);
            cc_ast_add_block_node(node, binop_node);
        } else {
            cc_ast_add_block_node(node, block_node);
        }
        return true;
    }
    return false;
}

static bool cc_parse_conditional_expression(cc_context* ctx, cc_ast_node* node)
{
    const cc_lexer_token* ctok;
    cc_ast_node* expr = cc_ast_create_block(ctx, node);
    if (cc_parse_logical_or_expression(ctx, expr)) {
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
static bool cc_parse_assignment_expression(
    cc_context* ctx, cc_ast_node* node, cc_ast_node* lhs)
{
    const cc_lexer_token* ctok;
    cc_ast_node* assign_node
        = cc_ast_create_binop_expr(ctx, node, AST_BINOP_NONE);

    if (lhs == NULL) {
        if (!cc_parse_conditional_expression(
                ctx, assign_node->data.binop.left)) {
            cc_ast_destroy_node(assign_node, true);
            return false;
        }
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
        binop_type = AST_BINOP_MINUS;
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
        binop_type = AST_BINOP_PLUS;
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
        cc_ast_node* virtual_node = cc_ast_create_block(ctx, call_node);
        cc_ast_node* left_node
            = cc_ast_create_var_ref(ctx, call_node->data.call.call_expr, var);

        /* Left side node (simple ident) */
        cc_ast_add_block_node(call_node->data.call.call_expr, left_node);

        while (cc_parse_assignment_expression(ctx, virtual_node, NULL)) {
            cc_ast_add_call_param(call_node, virtual_node);
            cc_free(virtual_node);

            virtual_node = cc_ast_create_block(ctx, call_node);
            if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
                && ctok->type == LEXER_TOKEN_COMMA) {
                cc_lex_token_consume(ctx);
                continue;
            }
            cc_ast_destroy_node(virtual_node, true);
            break;
        }

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

static bool cc_parse_unary_sizeof(cc_context* ctx, cc_ast_node* node)
{
    const cc_lexer_token* ctok;
    if ((ctok = cc_lex_token_peek(ctx, 0)) == NULL
        || ctok->type != LEXER_TOKEN_sizeof)
        return false;

    cc_lex_token_consume(ctx);

    /* This block is just a virtual block and will be destroyed
        once we finish evaluating our sizeof. */
    cc_ast_node* virtual_node = cc_ast_create_block(ctx, node);
    bool old_v = ctx->declaration_ident_optional;
    ctx->declaration_ident_optional = true;
    bool deduce_required = true;

    /* Parenthesis following means we can evaluate and obtain the size
        of a concise expression, otherwise we have to stick with another
        unary expression. */
    cc_ast_type virtual_type = { 0 };
    if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
        && ctok->type == LEXER_TOKEN_LPAREN) {
        cc_lex_token_consume(ctx);
        cc_ast_variable virtual_var = { 0 };
        if (!cc_parse_declarator(ctx, virtual_node, &virtual_var)) {
            cc_diag_error(ctx, "Expected unary expression after sizeof");
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
            cc_diag_error(ctx, "Expected unary expression after sizeof");
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

    unsigned int obj_sizeof = ctx->backend_data->get_sizeof(ctx, &virtual_type);
    /* TODO: Better way to convert our numbers into strings */
    static char numbuf[80];
    snprintf(numbuf, sizeof(numbuf), "%u", obj_sizeof);
    cc_ast_node* literal_node = cc_ast_create_literal(ctx, node, numbuf);
    cc_ast_add_block_node(node, literal_node);
    return true;
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
        expr_node = cc_ast_create_literal(ctx, node, ctok->data);
        matched_any = true;
        goto finish_expr_setup;
    case LEXER_TOKEN_CHAR_LITERAL:
        cc_lex_token_consume(ctx);
        expr_node = cc_ast_create_literal(ctx, node, ctok->data);
        matched_any = true;
        goto finish_expr_setup;
    case LEXER_TOKEN_IDENT: {
        const cc_ast_variable* var = cc_ast_find_variable(ctok->data, node);
        if (var == NULL) {
            cc_diag_error(ctx, "Couldn't find variable '%s'", ctok->data);
            goto error_handle;
        }
        cc_lex_token_consume(ctx);

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
    default:
        expr_node = cc_ast_create_block(ctx, node);
        break;
    }

    /* With postfix increment we will do a:
        <unop <op> <expr-node>> */
    if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL) {
        switch (ctok->type) {
        case LEXER_TOKEN_INCREMENT:
        case LEXER_TOKEN_DECREMENT: { /* Postfix ++/-- */
            cc_lex_token_consume(ctx); /* <unop <postinc> <expr>> */
            cc_ast_node* pi_node = cc_ast_create_unop_expr(ctx, node,
                ctok->type == LEXER_TOKEN_INCREMENT ? AST_UNOP_POSTINC
                                                    : AST_UNOP_POSTDEC);
            expr_node->parent = pi_node->data.unop.child;
            cc_ast_add_block_node(pi_node->data.unop.child, expr_node);
            cc_ast_add_block_node(node, pi_node);
            parent_rerouted = true;
            matched_any = true;
        } break;
        /* Array accessor <expr>[<expr>] syntax */
        case LEXER_TOKEN_LBRACKET: {
            cc_lex_token_consume(ctx);

            /* Create an expression of the form:
                <unop deref <binop <array> + <index>>*/
            cc_ast_node* arr_deref_node
                = cc_ast_create_unop_expr(ctx, node, AST_UNOP_DEREF);
            cc_ast_node* arr_index_node = cc_ast_create_binop_expr(
                ctx, arr_deref_node->data.unop.child, AST_BINOP_PLUS);
            expr_node->parent = arr_index_node->data.binop.left;
            cc_ast_add_block_node(arr_index_node->data.binop.left, expr_node);
            cc_parse_expression(ctx, arr_index_node->data.binop.right);
            cc_ast_add_block_node(
                arr_deref_node->data.unop.child, arr_index_node);
            cc_ast_add_block_node(node, arr_deref_node);
            CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RBRACKET, "Expected ']'");
            parent_rerouted = true;
            matched_any = true;
        } break;
        case LEXER_TOKEN_DOT:
        case LEXER_TOKEN_ARROW: {
            cc_lex_token_consume(ctx);
            cc_ast_node* accessor_node = cc_ast_create_binop_expr(ctx, node,
                ctok->type == LEXER_TOKEN_DOT ? AST_BINOP_DOT
                                              : AST_BINOP_ARROW);
            expr_node->parent = accessor_node->data.binop.left;
            cc_ast_add_block_node(accessor_node->data.binop.left, expr_node);
            CC_PARSE_EXPECT(
                ctx, ctok, LEXER_TOKEN_IDENT, "Expected identifier");
            cc_ast_node* var_node = cc_ast_create_field_ref(
                ctx, accessor_node->data.binop.right, ctok->data);
            cc_ast_add_block_node(accessor_node->data.binop.right, var_node);
            cc_ast_add_block_node(node, accessor_node);
            parent_rerouted = true;
            matched_any = true;
        } break;
        default:
            break;
        }
    }
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
static bool cc_parse_unary_expression(cc_context* ctx, cc_ast_node* node)
{
    const cc_lexer_token* ctok;
    if (cc_parse_unary_sizeof(ctx, node))
        return true;

    if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL) {
        cc_ast_node *binop_node = NULL, *literal_node = NULL, *unop_node = NULL;
        switch (ctok->type) {
        case LEXER_TOKEN_PLUS: /* Prefix +*/
            cc_lex_token_consume(ctx);
            binop_node = cc_ast_create_binop_expr(ctx, node, AST_BINOP_PLUS);
            literal_node
                = cc_ast_create_literal(ctx, binop_node->data.binop.left, "0");
            break;
        case LEXER_TOKEN_MINUS: /* Prefix - */
            cc_lex_token_consume(ctx);
            binop_node = cc_ast_create_binop_expr(ctx, node, AST_BINOP_MINUS);
            literal_node
                = cc_ast_create_literal(ctx, binop_node->data.binop.left, "0");
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
                cc_parse_expression(ctx, node);
                CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RPAREN, "Expected ')'");
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
                cc_diag_error(ctx, "Expected an unary expression for +/-");
                goto error_handle;
            }
            cc_ast_add_block_node(node, binop_node);
            return true;
        } else if (unop_node != NULL) {
            if (!cc_parse_unary_expression(ctx, unop_node->data.unop.child)) {
                cc_diag_error(ctx, "Expected an unary expression for ++/--");
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

static bool cc_parse_expression(cc_context* ctx, cc_ast_node* node)
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

static bool cc_parse_declaration_specifier(
    cc_context* ctx, cc_ast_node* node, cc_ast_type* type)
{
    const cc_lexer_token* ctok;
    bool qualified_once = false;
    type->storage = AST_STORAGE_AUTO;
    /* Consume cv-qualifiers */
    while (cc_parse_storage_class_specifier(ctx, type)
        || cc_parse_type_specifier(ctx, node, type)
        || cc_parse_function_specifier(ctx, type)
        || cc_parse_type_qualifier(ctx, type))
        qualified_once = true;
    /* Parse pointers that can be of any depths */
    while ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
        && ctok->type == LEXER_TOKEN_ASTERISK) {
        qualified_once = true;
        cc_lex_token_consume(ctx);
        type->n_cv_qual++;
        if (type->n_cv_qual >= MAX_CV_QUALIFIERS) {
            cc_diag_error(ctx, "Exceeded maximum pointer depth");
            goto error_handle;
        }
        /* TODO: attribute sequence */
        while (cc_parse_type_qualifier(ctx, type))
            ; /* Consume qualifiers */
    }
    return qualified_once;
error_handle:
    return false;
}

static bool cc_parse_statment(cc_context* ctx, cc_ast_node* node)
{
    const cc_lexer_token* ctok;

    /* Empty statent */
    if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
        && ctok->type == LEXER_TOKEN_SEMICOLON) {
        cc_lex_token_consume(ctx);
        return true;
    }

    /* TODO: Attributes!!! */
    if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
        && ctok->type == LEXER_TOKEN_LBRACE) {
        cc_lex_token_consume(ctx);

        /* Full of compound statments - wrap this around a new block! */
        cc_ast_node* nblock = cc_ast_create_block(ctx, node);
        while (cc_parse_compund_statment(ctx, nblock))
            if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
                && ctok->type == LEXER_TOKEN_RBRACE)
                break;

        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RBRACE, "Expected '}'");
        cc_ast_add_block_node(node, nblock);
        return true;
    error_handle:
        cc_ast_destroy_node(nblock, true);
        return false;
    }
    return cc_parse_compund_statment(ctx, node);
}

static bool cc_parse_iteration_statment(cc_context* ctx, cc_ast_node* node)
{
    const cc_lexer_token* ctok;
    /* for ( <expr> ; <expr>; <expr>) <secondary-block> */
    if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
        && ctok->type == LEXER_TOKEN_for) {
        cc_lex_token_consume(ctx);

        /* for ( init; condition; step )  */
        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_LPAREN, "Expected '('");

        cc_ast_node* for_node = cc_ast_create_block(ctx, node);
        cc_ast_node* init_node = cc_ast_create_block(ctx, for_node);
        cc_parse_expression(ctx, init_node);

        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_SEMICOLON, "Expected ';'");

        cc_ast_node* if_node = cc_ast_create_if_expr(ctx, for_node);
        cc_parse_expression(ctx, if_node->data.if_expr.cond);

        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_SEMICOLON, "Expected ';'");

        cc_ast_node* step_node
            = cc_ast_create_block(ctx, if_node->data.if_expr.block);
        cc_parse_expression(ctx, step_node);

        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RPAREN, "Expected ')'");
        cc_ast_node* jmp_node
            = cc_ast_create_jump(ctx, if_node->data.if_expr.block, if_node);

        cc_ast_node *old_break_node = ctx->break_node,
                    *old_continue_node = ctx->continue_node;
        ctx->break_node = jmp_node;
        ctx->continue_node = if_node;
        cc_parse_statment(ctx, if_node->data.if_expr.block);
        ctx->break_node = old_break_node;
        ctx->continue_node = old_continue_node;

        /* Initialize, then evaluate, then do body and then perform step! */
        cc_ast_add_block_node(if_node->data.if_expr.block, step_node);
        cc_ast_add_block_node(if_node->data.if_expr.block, jmp_node);
        cc_ast_add_block_node(for_node, init_node); /* Init*/
        cc_ast_add_block_node(for_node, if_node); /* Perform "if" */
        cc_ast_add_block_node(node, for_node);
        return true;
    } else if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
        && ctok->type == LEXER_TOKEN_while) {
        cc_lex_token_consume(ctx);

        cc_ast_node* while_node = cc_ast_create_block(ctx, node);

        /* while ( condition ) secondary-block */
        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_LPAREN, "Expected '('");

        cc_ast_node* if_node = cc_ast_create_if_expr(ctx, while_node);
        cc_parse_expression(ctx, if_node->data.if_expr.cond);

        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RPAREN, "Expected ')'");

        cc_ast_node* jmp_node
            = cc_ast_create_jump(ctx, if_node->data.if_expr.block, if_node);

        cc_ast_node *old_break_node = ctx->break_node,
                    *old_continue_node = ctx->continue_node;
        ctx->break_node = jmp_node;
        ctx->continue_node = if_node;
        cc_parse_statment(ctx, if_node->data.if_expr.block); /* Body of while */
        ctx->break_node = old_break_node;
        ctx->continue_node = old_continue_node;

        /* Jump back to if condition is met (trust me the control flow for
           the program will remain equivalent) */
        cc_ast_add_block_node(if_node->data.if_expr.block, jmp_node);

        cc_ast_add_block_node(while_node, if_node);
        cc_ast_add_block_node(node, while_node);
        return true;
    } else if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
        && ctok->type == LEXER_TOKEN_do) {
        /* Run the secondary-block once, and if conditions are met,
           run it again until the conditions are no longer met. */
        /* Roughly:
           { body: <body> if (condition) { <jump to body> } } */
        cc_lex_token_consume(ctx);
        cc_ast_node* while_node = cc_ast_create_block(ctx, node);
        cc_ast_node* if_node = cc_ast_create_if_expr(ctx, while_node);
        cc_ast_node* jmp_node
            = cc_ast_create_jump(ctx, if_node->data.if_expr.block, while_node);

        /* do secondary-block while ( condition ) */
        cc_ast_node *old_break_node = ctx->break_node,
                    *old_continue_node = ctx->continue_node;
        ctx->break_node = jmp_node;
        ctx->continue_node = if_node;
        cc_parse_statment(ctx, while_node); /* Body of do-while */
        ctx->break_node = old_break_node;
        ctx->continue_node = old_continue_node;

        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_while, "Expected 'while'");
        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_LPAREN, "Expected '('");
        cc_parse_expression(ctx, if_node->data.if_expr.cond);
        /* Jump back to the while node body, if the condition is still met */
        cc_ast_add_block_node(if_node->data.if_expr.block, jmp_node);
        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RPAREN, "Expected ')'");
        cc_ast_add_block_node(while_node, if_node);
        cc_ast_add_block_node(node, while_node);
        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_SEMICOLON, "Expected ';'");
        return true;
    }
error_handle:
    return false;
}

static bool cc_parse_selection_statment(cc_context* ctx, cc_ast_node* node)
{
    const cc_lexer_token* ctok;
    /* if ( <expr> ) <secondary-block> else <secondary-block> */
    if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
        && ctok->type == LEXER_TOKEN_if) {
        cc_ast_node* if_node = cc_ast_create_if_expr(ctx, node);
        cc_lex_token_consume(ctx);

        /* Condition to evaluate as ( <expr> ) */
        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_LPAREN, "Expected '('");
        cc_parse_expression(ctx, if_node->data.if_expr.cond);
        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RPAREN, "Expected ')'");

        /* Body of the if - <secondary-block> */
        cc_parse_statment(ctx, if_node->data.if_expr.block);
        if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && ctok->type == LEXER_TOKEN_else) {
            /* Tail-else, usually if we leave this node empty it means
               the equivalent to a no-op, we can confidently allocate it
               and not use it. */
            cc_lex_token_consume(ctx);
            /* Secondary block */
            cc_parse_statment(ctx, if_node->data.if_expr.tail_else);
        }
        cc_ast_add_block_node(node, if_node);
    }

    /* TODO: Fix switch and handle them properly? */
    if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
        && ctok->type == LEXER_TOKEN_switch) {
        cc_lex_token_consume(ctx);

        /* Condition/The controlling expression*/
        cc_ast_node* switch_node = cc_ast_create_switch_expr(ctx, node);
        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_LPAREN, "Expected '('");
        cc_parse_expression(ctx, switch_node->data.switch_expr.control);
        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RPAREN, "Expected ')'");

        cc_ast_node* end_node
            = cc_ast_create_block(ctx, switch_node->data.switch_expr.block);

        cc_ast_node *old_break_node = ctx->break_node,
                    *old_continue_node = ctx->continue_node;
        ctx->break_node = end_node;
        ctx->continue_node = NULL;
        /* Body of the switch - <secondary-block> */
        cc_parse_statment(ctx, switch_node->data.switch_expr.block);
        ctx->break_node = old_break_node;
        ctx->continue_node = old_continue_node;

        cc_ast_add_block_node(switch_node->data.switch_expr.block, end_node);
        cc_ast_add_block_node(node, switch_node);
    }
error_handle:
    return false;
}

/* TODO: parse lbrace the brace thing for arrays wtf am i??? ?!?!?! */
static bool cc_parse_declarator_assignment_expression(
    cc_context* ctx, cc_ast_node* node, cc_ast_variable* var)
{
    cc_lexer_token* ctok;
    if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
        && ctok->type == LEXER_TOKEN_LBRACE) {
        cc_lex_token_consume(ctx);
        cc_parse_declarator_assignment_expression(ctx, node, var);
        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RBRACE, "Expected '}'");
    } else {
        while (cc_parse_assignment_expression(ctx, node, NULL)) {
            const cc_lexer_token* ctok;
            if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
                && ctok->type == LEXER_TOKEN_COMMA) {
                cc_lex_token_consume(ctx);
                continue;
            }
            break;
        }
    }
    return true;
error_handle:
    return false;
}

static bool cc_parse_declarator(
    cc_context* ctx, cc_ast_node* node, cc_ast_variable* var)
{
    cc_parse_declaration_specifier(ctx, node, &var->type);

    const cc_lexer_token* ctok;

    /* On cases such as struct b {} ; */
    if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
        && ctok->type == LEXER_TOKEN_SEMICOLON)
        return true;

    if ((ctok = cc_lex_token_peek(ctx, 0)) == NULL) {
        cc_diag_error(ctx, "Expected identifier or '(' for declarator");
        goto error_handle;
    }
    switch (ctok->type) {
    case LEXER_TOKEN_LPAREN: /* ( <declarator> ) */
        cc_lex_token_consume(ctx);
        cc_parse_declarator(ctx, node, var);
        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RPAREN, "Expected ')'");
        break;
    case LEXER_TOKEN_IDENT: { /* <ident> <attr> */
        const cc_ast_type* tpdef = cc_ast_find_typedef(ctok->data, node);
        cc_lex_token_consume(ctx);
        if (tpdef == NULL) {
            if (var->name != NULL) {
                cc_diag_error(ctx, "More than one identifier on declaration");
                goto error_handle;
            }
            var->name = cc_strdup(ctok->data);
        } else {
            cc_ast_copy_type(&var->type, tpdef);
        }
    } break;
    default:
        if (ctx->is_parsing_prototype || ctx->declaration_ident_optional) {
            if (var->type.mode == AST_TYPE_MODE_NONE) {
                cc_diag_error(ctx, "Unable to disambiguate");
                cc_lex_token_consume(ctx);
                goto error_handle;
            }
            goto ignore_missing_ident;
        }
        cc_diag_error(ctx, "Expected identifier or '(' for declarator");
        cc_lex_token_consume(ctx);
        goto error_handle;
    }

ignore_missing_ident:
    /* Parenthesis after this equates into a function */
    if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
        && ctok->type == LEXER_TOKEN_LPAREN) {
        cc_lex_token_consume(ctx);
        var->type.mode = AST_TYPE_MODE_FUNCTION;
        /* No storage specified? set extern then */
        /*if (var->type.storage == AST_STORAGE_AUTO) {
            var->type.storage = AST_STORAGE_EXTERN;
        }*/

        if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && ctok->type == LEXER_TOKEN_ELLIPSIS) {
            /* Fully variadic function (non-standard) */
            var->type.data.func.variadic = true;
            cc_lex_token_consume(ctx);
            CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RPAREN, "Expected ')'");
        } else if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && ctok->type == LEXER_TOKEN_void
            && (ctok = cc_lex_token_peek(ctx, 1)) != NULL
            && ctok->type == LEXER_TOKEN_RPAREN) {
            /* func(void), functions taking no parameters at all */
            cc_lex_token_consume(ctx);
            CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RPAREN, "Expected ')'");
        } else {
            cc_ast_variable virtual_param_var = { 0 };
            ctx->is_parsing_prototype = true;
            while (cc_parse_declarator(ctx, node, &virtual_param_var)) {
                var->type.data.func.params
                    = cc_realloc(var->type.data.func.params,
                        sizeof(cc_ast_variable)
                            * (var->type.data.func.n_params + 1));
                cc_ast_variable* param
                    = &var->type.data.func
                           .params[var->type.data.func.n_params++];
                cc_ast_copy_type(&param->type, &virtual_param_var.type);
                param->name = NULL;
                if (virtual_param_var.name != NULL)
                    param->name = cc_strdup(virtual_param_var.name);

                /* Destroy... */
                memset(&virtual_param_var, 0, sizeof(virtual_param_var));

                if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
                    && ctok->type == LEXER_TOKEN_COMMA) {
                    cc_lex_token_consume(ctx);
                    /* Ellipsis denotes variadic argument function */
                    if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
                        && ctok->type == LEXER_TOKEN_ELLIPSIS) {
                        var->type.data.func.variadic = true;
                        cc_lex_token_consume(ctx);
                        break; /* Finish off, no f(int, ..., int) can exist */
                    }
                    continue;
                }
                break;
            }
            ctx->is_parsing_prototype = false;
            CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RPAREN, "Expected ')'");
        }
    }

    while ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
        && ctok->type == LEXER_TOKEN_LBRACKET) {
        cc_lex_token_consume(ctx);

        var->type.n_cv_qual++; /* Increment for array (laundered) */
        if (var->type.n_cv_qual >= MAX_CV_QUALIFIERS) {
            cc_diag_error(ctx, "Array exceeds pointer depth size");
            goto error_handle;
        }
        var->type.cv_qual[var->type.n_cv_qual].array_size = 0;
        var->type.cv_qual[var->type.n_cv_qual].is_array = true;

        if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && ctok->type == LEXER_TOKEN_static)
            var->type.cv_qual[var->type.n_cv_qual].is_static_array = true;
        while (cc_parse_type_qualifier(ctx, &var->type))
            ; /* Parse type qualifiers list */
        if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && ctok->type == LEXER_TOKEN_static)
            var->type.cv_qual[var->type.n_cv_qual].is_static_array = true;

        cc_ast_literal literal = { 0 };
        cc_parse_constant_expression(ctx, node, &literal);
        var->type.cv_qual[var->type.n_cv_qual].array_size = literal.value.u;

        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RBRACKET, "Expected ']'");

        if (var->type.cv_qual[var->type.n_cv_qual].array_size
            > MAX_ARRAY_SIZE) {
            cc_diag_error(ctx, "Array exceeds maximum constant size");
            goto error_handle;
        }
    }

    /* If we're assigning on declaration we will have to expand a separate
       statment where we are assigning AND referencing this variable that
       is to be assigned. */
    if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
        && ctok->type == LEXER_TOKEN_ASSIGN) {
        cc_lex_token_consume(ctx);
        cc_ast_node* assign_node
            = cc_ast_create_binop_expr(ctx, node, AST_BINOP_ASSIGN);
        cc_ast_node* lhs_node = assign_node->data.binop.left;
        cc_ast_node* var_node = cc_ast_create_var_ref(ctx, lhs_node, var);
        /* Parse <var this-variable> = <expr> */
        cc_ast_add_block_node(lhs_node, var_node); /* Plug implicit var ref
                                                      into lhs */
        cc_parse_declarator_assignment_expression(
            ctx, assign_node->data.binop.right, var);
        cc_ast_add_block_node(node, assign_node); /* Add binop expr to block */
    }
    return true;
error_handle:
    return false;
}

/* Handles parsing of sucessive declarations, for example:
   int i, j;
   
   To avoid code repetition inwithinhence our parsing code
   as this is used by both compound and external declarations to declare
   multiple variables at once. */
static bool cc_parse_declarator_list(cc_context* ctx, cc_ast_node* node,
    cc_ast_variable* var, bool* is_parsing_typedef)
{
    const cc_lexer_token* ctok;
    if ((ctok = cc_lex_token_peek(ctx, 0)) == NULL)
        return false;

    /* TODO: Handle cases such as:
       typedef struct SomeThing {} NewName ident; */

    /* Declaration specifiers */
    bool decl_result;
comma_list_initializers: /* Jump here, reusing the variable's stack
                            location **but** copying over the type
                            with the various elements. */
    decl_result = cc_parse_declarator(ctx, node, var);

    /* A typedef can be treated as a variable UNTIL we exit the declarator
       parser loop. Once we exit it we will have to convert the variable
       into a typedef we can toy with.

       This state is updated accordingly on the type storage
       specifiers. */
    *is_parsing_typedef = ctx->is_parsing_typedef; /* Save temp */
    if (ctx->is_parsing_typedef) {
        if (var->name == NULL) {
            cc_diag_error(ctx, "Anonymous typedef");
            goto error_handle;
        }

        cc_ast_type ntpdef = { 0 };
        cc_ast_copy_type(&ntpdef, &var->type); /* Copy type over */
        ntpdef.name = cc_strdup(var->name);
        ntpdef.is_typedef = true;
        cc_ast_add_block_type(node, &ntpdef);
    }
    ctx->is_parsing_typedef = false;

    if (!decl_result)
        goto error_handle;

    if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
        && ctok->type == LEXER_TOKEN_COMMA) {
        cc_lex_token_consume(ctx);
        /* Save the type somewhere safe, destroy the var,
            recreate the var with our type and destroy the type. */
        cc_ast_type stype = { 0 };
        cc_ast_copy_type(&stype, &var->type);
        /* Assign global storage to the variable if it is not inside a
           function body. */
        if (!ctx->is_func_body && var->type.storage == AST_STORAGE_AUTO)
            var->type.storage = AST_STORAGE_GLOBAL;
        cc_ast_add_block_variable(node, var);
        memset(var, 0, sizeof(*var));
        cc_ast_copy_type(&var->type, &stype);
        cc_ast_destroy_type(&stype, false);
        goto comma_list_initializers;
    }
    return true;
error_handle:
    return false;
}

static bool cc_parse_compund_statment(cc_context* ctx, cc_ast_node* node)
{
    const cc_lexer_token* ctok;
    assert(node != NULL);
    if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL) {
        switch (ctok->type) {
        case LEXER_TOKEN_RBRACE: /* TODO: Why does } make this break??? */
            goto error_handle;
        case LEXER_TOKEN_case: {
            cc_ast_node* block_node = cc_ast_create_block(ctx, node);
            cc_lex_token_consume(ctx);

            block_node->data.block.is_case = true;
            block_node->ref_count++; /* Referenced by switch node */
            cc_parse_constant_expression(
                ctx, node, &block_node->data.block.case_val);
            CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_COLON, "Expected ':'");
            cc_parse_statment(ctx, block_node);
            cc_ast_add_block_node(node, block_node);
        }
            return true;
        case LEXER_TOKEN_default: {
            cc_ast_node* block_node = cc_ast_create_block(ctx, node);
            cc_lex_token_consume(ctx);
            CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_COLON, "Expected ':'");
            cc_parse_statment(ctx, block_node);

            block_node->data.block.is_case = true;
            block_node->data.block.is_default = true;
            block_node->ref_count++; /* Referenced by switch node */
            cc_ast_add_block_node(node, block_node);
        }
            return true;
        case LEXER_TOKEN_if:
        case LEXER_TOKEN_switch: {
            cc_parse_selection_statment(ctx, node);
        }
            return true;
        case LEXER_TOKEN_for:
        case LEXER_TOKEN_while:
        case LEXER_TOKEN_do: {
            cc_parse_iteration_statment(ctx, node);
        }
            return true;
        case LEXER_TOKEN_continue: {
            cc_lex_token_consume(ctx);
            cc_ast_node* continue_node
                = cc_ast_create_jump(ctx, node, ctx->continue_node);
            if (continue_node == NULL) {
                cc_diag_error(
                    ctx, "Continue not within lexicographical context");
                goto error_handle;
            }
            cc_ast_add_block_node(node, continue_node);
        } break;
        case LEXER_TOKEN_break: {
            cc_lex_token_consume(ctx);
            cc_ast_node* break_node
                = cc_ast_create_jump(ctx, node, ctx->break_node);
            if (break_node == NULL) {
                cc_diag_error(ctx, "Break not within lexicographical context");
                goto error_handle;
            }
            cc_ast_add_block_node(node, break_node);
        } break;
        case LEXER_TOKEN_return: {
            cc_ast_node* ret_node = cc_ast_create_ret_expr(ctx, node);
            cc_lex_token_consume(ctx);
            cc_parse_expression(ctx, ret_node->data.return_expr);
            cc_ast_add_block_node(node, ret_node);
        } break;
        case LEXER_TOKEN_IDENT: {
            const cc_ast_variable* var = cc_ast_find_variable(ctok->data, node);
            if (var == NULL)
                cc_ast_find_variable(ctok->data, node->parent);

            if (var != NULL) { /* Variable reference OR call/assignment */
                cc_parse_expression(ctx, node);
            } else { /* Type for declaration within compound stmt */
                /* Implicit function declarations, where the prototype
                   is missing from the function and it's declared in place
                   this exception exists for some fucking reason??? */
                if ((ctok = cc_lex_token_peek(ctx, 1)) != NULL
                    && ctok->type == LEXER_TOKEN_LPAREN) {
                    ctok = cc_lex_token_peek(ctx, 0); /* Identifier */
                    cc_ast_variable nvar = { 0 };
                    nvar.name = cc_strdup(ctok->data);
                    nvar.type.mode = AST_TYPE_MODE_FUNCTION;
                    nvar.type.storage = AST_STORAGE_EXTERN;
                    /* Variadic, basically meaning we have no fucking idea */
                    nvar.type.data.func.variadic = true;
                    nvar.type.data.func.return_type
                        = cc_zalloc(sizeof(cc_ast_type));
                    nvar.type.data.func.return_type->mode = AST_TYPE_MODE_INT;
                    cc_ast_add_block_variable(node, &nvar);
                    return cc_parse_compund_statment(ctx, node);
                } else {
                    cc_ast_variable nvar = { 0 };
                    bool is_parsing_typedef = false;
                    if (!cc_parse_declarator_list(
                            ctx, node, &nvar, &is_parsing_typedef))
                        goto error_handle;
                    if (!is_parsing_typedef)
                        cc_ast_add_block_variable(node, &nvar);
                }
            }
        } break;
        default: {
            /* First try interpreting as an expression, then if that
               does NOT work, fallback to the declarator */
            if (!cc_parse_expression(ctx, node)) {
                cc_ast_variable nvar = { 0 };
                bool is_parsing_typedef = false;
                if (!cc_parse_declarator_list(
                        ctx, node, &nvar, &is_parsing_typedef))
                    goto error_handle;
                if (!is_parsing_typedef)
                    cc_ast_add_block_variable(node, &nvar);
            }
        } break;
        }
    } else {
        return false;
    }

    /* Compound statments ends with semicolon! */
    CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_SEMICOLON, "Expected ';'");
    return true;
error_handle:
    return false;
}

static bool cc_parse_external_declaration(cc_context* ctx, cc_ast_node* node)
{
    const cc_lexer_token* ctok;
    if ((ctok = cc_lex_token_peek(ctx, 0)) == NULL)
        return false;

    /* TODO: Handle cases such as:
       typedef struct SomeThing {} NewName ident; */

    /* Declaration specifiers */
    cc_ast_variable var = { 0 };
    bool is_parsing_typedef = false;
    cc_parse_declarator_list(ctx, node, &var, &is_parsing_typedef);

    if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL) {
        switch (ctok->type) {
        case LEXER_TOKEN_SEMICOLON: /* Prototype/declaration */
            /* Function prototype usually ends up with ");" */
            if ((ctok = cc_lex_token_peek(ctx, -1)) != NULL
                && ctok->type == LEXER_TOKEN_RPAREN)
                var.type.mode = AST_TYPE_MODE_FUNCTION;
            cc_lex_token_consume(ctx);
            break;
        case LEXER_TOKEN_LBRACE: /* Function body */
            cc_lex_token_consume(ctx);
            if (is_parsing_typedef) {
                cc_diag_error(ctx, "Function definition after typedef");
                goto error_handle;
            }

            if (var.type.mode != AST_TYPE_MODE_FUNCTION) {
                cc_diag_error(ctx, "Unexpected '}' on non-function type");
                goto error_handle;
            }

            /* All functions that are not prototypes are treated as a variable. */
            if (var.type.storage == AST_STORAGE_EXTERN) {
                cc_diag_warning(ctx,
                    "Function '%s' declared extern but defined here", var.name);
                var.type.storage = AST_STORAGE_AUTO;
            }

            /* Variable for the function prototype (then replaced) */
            cc_ast_variable prot_var = { 0 };
            cc_ast_copy_type(&prot_var.type, &var.type); /* Copy safely */
            prot_var.name = cc_strdup(var.name);
            cc_ast_add_block_variable(node, &prot_var);

            /* And variable for the function itself */
            var.body = cc_ast_create_block(ctx, node);
            bool old_is_func_body = ctx->is_func_body;
            ctx->is_func_body = true;
            while (cc_parse_compund_statment(ctx, var.body))
                ;
            ctx->is_func_body = old_is_func_body;
            CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RBRACE, "Expected '}'");
            break;
        default:
            cc_diag_error(ctx, "Unexpected token in declarator");
            goto error_handle;
        }
    }

    if (var.name == NULL) {
        if (var.type.name == NULL) {
            cc_diag_error(ctx,
                "Anonymous external declaration of variable with type '%s'",
                var.type.name);
            goto error_handle;
        } else {
            return true;
        }
    }

    /* Only treat as a variable iff we're NOT parsing a typedef */
    if (!is_parsing_typedef) {
        /* Automatically give variables globality-scope if they don't
           have any other linkage specifiers. */
        if (var.type.storage == AST_STORAGE_AUTO)
            var.type.storage = AST_STORAGE_GLOBAL;
        cc_ast_add_block_variable(node, &var);
    }
    return true;
error_handle:
    cc_ast_destroy_var(&var, false);
    return false;
}

static bool cc_parse_translation_unit(cc_context* ctx, cc_ast_node* node)
{
    bool has_match = false;
    while (cc_parse_external_declaration(ctx, node))
        has_match = true;
    return has_match;
}

int cc_parse_top(cc_context* ctx)
{
    const cc_lexer_token* ctok;
    ctx->root = cc_ast_create_block(ctx, NULL); /* Block holding everything */
    ctx->stage = STAGE_PARSER;
    while ((ctok = cc_lex_token_peek(ctx, 0)) != NULL) {
        /* Line diagnostic information */
        if (ctok->type == LEXER_TOKEN_HASHTAG) {
            const cc_lexer_token* dtok;
            cc_lex_token_consume(ctx);
            if ((dtok = cc_lex_token_peek(ctx, 0)) != NULL
                && dtok->type == LEXER_TOKEN_NUMBER) {
                unsigned long int n_lines = strtoul(dtok->data, NULL, 10);
                cc_lex_token_consume(ctx);

                if ((dtok = cc_lex_token_peek(ctx, 0)) != NULL
                    && dtok->type == LEXER_TOKEN_STRING_LITERAL) {
                    const char* filename = dtok->data;
                    char flags = 0;
                    cc_lex_token_consume(ctx);

                    while ((dtok = cc_lex_token_peek(ctx, 0)) != NULL
                        && dtok->type == LEXER_TOKEN_NUMBER) {
                        flags |= 1 << (char)atoi(dtok->data);
                        cc_lex_token_consume(ctx);
                    }

                    if ((flags & (1 << 1)) != 0) { /* New file */
                        cc_diag_add_info(ctx,
                            (cc_diag_info) { .filename = cc_strdup(filename),
                                .line = n_lines });
                    } else if ((flags & (1 << 2)) != 0) { /* Return to file */
                        cc_diag_return_to_file(ctx,
                            (cc_diag_info) { .filename = cc_strdup(filename),
                                .line = n_lines });
                    }
                }
            }
        } else {
            cc_parse_translation_unit(ctx, ctx->root);
        }
    }
    return 0;
}
