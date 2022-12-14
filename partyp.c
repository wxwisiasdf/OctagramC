/* partyp.c - C parser for types, typedef, qualifier, etc. */
#include "partyp.h"
#include "ast.h"
#include "constevl.h"
#include "context.h"
#include "diag.h"
#include "lexer.h"
#include "optzer.h"
#include "parexpr.h"
#include "parser.h"
#include "util.h"
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static unsigned short cc_parse_attribute_literal_param(
    cc_context* ctx, cc_ast_node* node, cc_ast_type* type)
{
    cc_ast_literal r = { 0 };
    if (!cc_parse_constant_expression(ctx, node, &r)) {
        cc_diag_error(ctx, "Attribute with non-constant value");
        return 0;
    }
    return cc_ceval_literal_to_ushort(ctx, &r);
}

static bool cc_parse_type_attributes(
    cc_context* ctx, cc_ast_node* node, cc_ast_type* type)
{
    const cc_lexer_token* ctok = cc_lex_token_peek(ctx, 0);
    if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
        && ctok->type == LEXER_TOKEN_IDENT) {
        cc_lex_token_consume(ctx);
        if (!strcmp(ctok->data, "packed")) {
            type->data.s_or_u.packed = true;
        } else if (!strcmp(ctok->data, "aligned")
            || !strcmp(ctok->data, "alignment")
            || !strcmp(ctok->data, "align")) {
            type->min_alignment
                = cc_parse_attribute_literal_param(ctx, node, type);
        } else if (!strcmp(ctok->data, "max_align")
            || !strcmp(ctok->data, "max_alignment")
            || !strcmp(ctok->data, "max_aligned")) {
            type->max_alignment
                = cc_parse_attribute_literal_param(ctx, node, type);
        } else {
            cc_diag_warning(ctx, "Unknown attribute '%s'", ctok->data);
        }
    }
    return true;
}

static bool cc_parse_struct_or_union_attributes(
    cc_context* ctx, cc_ast_node* node, cc_ast_type* type)
{
    const cc_lexer_token* ctok = cc_lex_token_peek(ctx, 0);
    if ((ctok = cc_lex_token_peek(ctx, 0)) == NULL
        || ctok->type != LEXER_TOKEN_LBRACKET)
        return false;

    CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_LBRACKET, "Expected '['");
    CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_LBRACKET, "Expected '['");
    cc_parse_type_attributes(ctx, node, type);
    CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RBRACKET, "Expected ']'");
    CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RBRACKET, "Expected ']'");
    return true;
error_handle:
    return false;
}

bool cc_parse_struct_or_union_specifier(
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

    while (cc_parse_struct_or_union_attributes(ctx, node, type))
        ;
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
            virtual_member.type.data.num.bitint_bits = 0;

            /* Constexpression follows, evaluate it nicely :) */
            cc_ast_literal literal = { 0 };
            cc_parse_constant_expression(ctx, node, &literal);
            if (literal.is_signed && literal.value.s < 0) {
                cc_diag_error(ctx, "Number of bits can't be negative");
                goto error_handle;
            }
            virtual_member.type.data.num.bitint_bits = literal.value.u;
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

static bool cc_parse_enum_specifier(
    cc_context* ctx, cc_ast_node* node, cc_ast_type* type)
{
    const cc_lexer_token* ctok;
    if ((ctok = cc_lex_token_peek(ctx, 0)) == NULL
        || ctok->type != LEXER_TOKEN_enum)
        return false;

    cc_lex_token_consume(ctx);

    type->mode = AST_TYPE_MODE_ENUM;
    /* TODO: Attributes */
    if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
        && ctok->type == LEXER_TOKEN_IDENT) {
        cc_lex_token_consume(ctx);
        type->name = cc_strdup(ctok->data);
    }

    /* Enum without brace means declaration of a variable OR forward
       declaration... */
    if ((ctok = cc_lex_token_peek(ctx, 0)) == NULL
        || ctok->type != LEXER_TOKEN_LBRACE) {
        cc_ast_type* enum_type = cc_ast_find_type(type->name, node);
        if (enum_type != NULL) { /* We're using an already existing type? */
            if (type->name != NULL) {
                cc_free(type->name);
                type->name = NULL;
            }
            cc_ast_copy_type(type, enum_type);
        } else { /* Not an existing type, forward declaration... */
            /* ... */
        }
        return true;
    }

    CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_LBRACE, "Expected '{'");

    /* TODO: Attributes */
    if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
        && ctok->type == LEXER_TOKEN_RBRACE) {
        cc_diag_warning(ctx, "Empty enumerator");
        goto empty_memberlist;
    }

    /* Syntax is <ident> = <const-expr> , */
    cc_ast_literal seq_literal = { 0 }; /* Enumerator values start at 0 */
    memset(&type->data, 0, sizeof(type->data));
    while ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
        && ctok->type == LEXER_TOKEN_IDENT) {
        cc_lex_token_consume(ctx);
        cc_ast_enum_member member = { 0 };
        member.name = cc_strdup(ctok->data);
        /* Assignment of enumerator value. */
        member.literal = seq_literal;
        if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && ctok->type == LEXER_TOKEN_ASSIGN) {
            cc_lex_token_consume(ctx);

            cc_ast_node* const_expr = cc_ast_create_block(ctx, node);
            /* Parse like a normal expression */
            cc_parse_unary_expression(ctx, const_expr);
            if (!cc_ceval_constant_expression(
                    ctx, &const_expr, &member.literal)) {
                cc_diag_error(ctx, "Unable to evaluate constant expression");
                if (const_expr != NULL)
                    cc_ast_destroy_node(const_expr, true);
                return false;
            }
            if (const_expr != NULL)
                cc_ast_destroy_node(const_expr, true);

            seq_literal = member.literal;
            if (member.literal.is_float) {
                cc_diag_warning(
                    ctx, "Floating enumerator values will be truncated");
                member.literal.is_float = false;
                member.literal.is_signed = false;
                member.literal.value.s = (signed long)member.literal.value.d;
            }
        }
        if (seq_literal.is_signed)
            seq_literal.value.s++;
        else
            seq_literal.value.u++;

        /* Enumerator members are globally visible as constexpr
            evaluatible variables on the global context. */
        cc_ast_variable var = { 0 };
        var.type.mode = AST_TYPE_MODE_INT;
        /* Override type specification and enable constexpr evaluation */
        var.type.storage = AST_STORAGE_CONSTEXPR;
        var.name = cc_strdup(member.name);
        var.initializer = cc_ast_create_literal(ctx, node, member.literal);
        cc_ast_add_block_variable(node, &var);

        type->data.enumer.elems = cc_realloc_array(
            type->data.enumer.elems, type->data.enumer.n_elems + 1);
        type->data.enumer.elems[type->data.enumer.n_elems++] = member;

        if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && ctok->type == LEXER_TOKEN_COMMA) {
            cc_lex_token_consume(ctx);
            continue;
        }

        if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && ctok->type == LEXER_TOKEN_RBRACE)
            break;
    }

    for (size_t i = 0; i < type->data.enumer.n_elems; i++) {
        for (size_t j = 0; j < type->data.enumer.n_elems; j++) {
            if (i != j
                && !memcmp(&type->data.enumer.elems[i].literal,
                    &type->data.enumer.elems[j].literal,
                    sizeof(cc_ast_literal))) {
                cc_diag_error(ctx,
                    "Enumerator elements '%s' and '%s' with same value",
                    type->data.enumer.elems[i].name,
                    type->data.enumer.elems[j].name);
                CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RBRACE, "Expected '}'");
                goto error_handle;
            }
        }
    }

empty_memberlist:
    CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RBRACE, "Expected '}'");
    /* We will add non-anonymous enums. */
    if (type->name != NULL) /* Non-anonymous enum */
        cc_ast_add_block_type(node, type);
    return true;
error_handle:
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

/* We've been parsing what we tought was a type of a variable and not a
   function, so we will quickly "translate" the return type of the function
   from the current type of the function. For example if we parse:
   
   int printf(...);

   We will have a variable with type int, so we will move the int type into our
   return type. */
void cc_swap_func_decl(cc_ast_type* type)
{
    cc_ast_type* return_type = cc_zalloc(sizeof(cc_ast_type));
    cc_ast_copy_type(return_type, type);
    /* Reset type of variable */
    memset(type, 0, sizeof(cc_ast_type));
    type->mode = AST_TYPE_MODE_FUNCTION;
    type->data.func.return_type = return_type;
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

static bool cc_parse_type_specifier(
    cc_context* ctx, cc_ast_node* node, cc_ast_type* type)
{
    const cc_lexer_token* ctok = ctok = cc_lex_token_peek(ctx, 0);
    if (ctok == NULL)
        return false;
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
    case LEXER_TOKEN_unsigned:
        if (type->mode == AST_TYPE_MODE_FLOAT
            || type->mode == AST_TYPE_MODE_DOUBLE) {
            cc_diag_error(
                ctx, "Floating point can't have unsigned/signed specifiers");
            cc_lex_token_consume(ctx);
            goto error_handle;
        }
        if (type->mode == AST_TYPE_MODE_BOOL) {
            cc_diag_error(ctx, "Boolean with signed/unsigned specifiers");
            cc_lex_token_consume(ctx);
            goto error_handle;
        }
        type->data.num.is_signed = ctok->type == LEXER_TOKEN_signed;
        break;
    case LEXER_TOKEN__BitInt: {
        type->mode = AST_TYPE_MODE_BITINT;
        ctok = cc_lex_token_consume(ctx);
        ctok = cc_lex_token_consume(ctx);

        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_LPAREN, "Expected '('");
        cc_ast_literal literal = { 0 };
        if (!cc_parse_constant_expression(ctx, node, &literal)) {
            cc_diag_error(ctx, "Unable to parse expression");
            type->data.num.bitint_bits = 1;
            CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RPAREN, "Expected ')'");
            goto error_handle;
        }
        type->data.num.bitint_bits = cc_ceval_literal_to_ushort(ctx, &literal);
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
        return cc_parse_enum_specifier(ctx, node, type);
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
        type->storage |= AST_STORAGE_EXTERN;
        break;
    case LEXER_TOKEN_static:
        type->storage |= AST_STORAGE_STATIC;
        break;
    case LEXER_TOKEN_register:
        type->storage |= AST_STORAGE_REGISTER;
        break;
    case LEXER_TOKEN_thread_local:
        type->storage |= AST_STORAGE_THREAD_LOCAL;
        break;
    case LEXER_TOKEN_constexpr:
        type->storage |= AST_STORAGE_CONSTEXPR;
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

static bool cc_parse_declaration_specifier_attributes(
    cc_context* ctx, cc_ast_node* node, cc_ast_type* type)
{
    const cc_lexer_token* ctok = cc_lex_token_peek(ctx, 0);
    if ((ctok = cc_lex_token_peek(ctx, 0)) == NULL
        || ctok->type != LEXER_TOKEN_LBRACKET)
        return false;

    CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_LBRACKET, "Expected '['");
    CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_LBRACKET, "Expected '['");

    if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
        && ctok->type == LEXER_TOKEN_IDENT) {
        cc_lex_token_consume(ctx);
        if (!strcmp(ctok->data, "noreturn")) {
            type->data.func.no_return = true;
        } else if (!strcmp(ctok->data, "nodiscard")) {
            type->data.func.no_discard = true;
        } else if (!strcmp(ctok->data, "deprecated")) {
            type->data.func.deprecated = true;
        } else if (!strcmp(ctok->data, "variadic") || !strcmp(ctok->data, "var")
            || !strcmp(ctok->data, "variable")) {
            type->data.func.variadic = true;
        } else if (!strcmp(ctok->data, "naked")) {
            type->data.func.naked = true;
        } else if (!strcmp(ctok->data, "irq")) {
            type->data.func.irq = true;
        } else {
            cc_parse_type_attributes(ctx, node, type);
        }
    }

    CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RBRACKET, "Expected ']'");
    CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RBRACKET, "Expected ']'");
    return true;
error_handle:
    return false;
}

bool cc_parse_declaration_specifier(
    cc_context* ctx, cc_ast_node* node, cc_ast_type* type)
{
    const cc_lexer_token* ctok;
    bool qualified_once = false;
    type->storage = AST_STORAGE_AUTO;
    type->data.num.is_signed = ctx->is_default_signed;

    while (cc_parse_declaration_specifier_attributes(ctx, node, type))
        ;

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

static bool cc_parse_declarator_braced_initializer_element(
    cc_context* ctx, cc_ast_node* node, cc_ast_variable* var)
{
    const cc_lexer_token* ctok;
    bool named_element = false;

    while ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
        && ctok->type == LEXER_TOKEN_DOT) {
        cc_lex_token_consume(ctx);
        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_IDENT, "Expected 'ident'");
    }

    if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
        && ctok->type == LEXER_TOKEN_ASSIGN)
        cc_parse_assignment_expression(ctx, node, var);
    return true;
error_handle:
    return false;
}

bool cc_parse_declarator_braced_initializer(
    cc_context* ctx, cc_ast_node* node, cc_ast_variable* var)
{
    const cc_lexer_token* ctok;
    if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
        && ctok->type == LEXER_TOKEN_LBRACE) {
        cc_lex_token_consume(ctx);
        /* {0} is a shorthand to zero-initialize an structure */
        if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && ctok->type == LEXER_TOKEN_NUMBER && !strcmp(ctok->data, "0")) {
            cc_lex_token_consume(ctx);
            /* TODO: Zero-initialize */
        } else if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && ctok->type == LEXER_TOKEN_RBRACE) {
            /* N2900 Consistent, Warningless, and Intuitive Initialization with {} */
        } else if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && ctok->type != LEXER_TOKEN_RBRACE) {
            while ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
                && ctok->type != LEXER_TOKEN_RBRACE) {
                cc_parse_declarator_braced_initializer_element(ctx, node, var);
                if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
                    && ctok->type == LEXER_TOKEN_COMMA) {
                    cc_lex_token_consume(ctx);
                    continue;
                }
                break;
            }
        }
        /* Trailing declarator list comma */
        if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && ctok->type == LEXER_TOKEN_COMMA)
            cc_lex_token_consume(ctx);
        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RBRACE, "Expected '}'");
        return true;
    }
error_handle:
    return false;
}

/* TODO: parse lbrace the brace thing for arrays wtf am i??? ?!?!?! */
static bool cc_parse_declarator_assignment_expression(
    cc_context* ctx, cc_ast_node* node, cc_ast_variable* var)
{
    const cc_lexer_token* ctok;
    /* direct-declarator = { assignment-expr , } */
    /* or direct-declarator = assignment-expr */
    if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
        && ctok->type == LEXER_TOKEN_ASSIGN
        && (ctok = cc_lex_token_peek(ctx, 1)) != NULL
        && ctok->type == LEXER_TOKEN_LBRACE) {
        cc_lex_token_consume(ctx);
        return cc_parse_declarator_braced_initializer(ctx, node, var);
    } else if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
        && ctok->type == LEXER_TOKEN_ASSIGN) {
        cc_ast_node* var_node = cc_ast_create_var_ref(ctx, node, var);
        while (cc_parse_assignment_expression(ctx, node, var)) {
            const cc_lexer_token* ctok;
            if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
                && ctok->type == LEXER_TOKEN_COMMA) {
                cc_lex_token_consume(ctx);
                continue;
            }
            break;
        }
        return true;
    }
error_handle:
    return false;
}

bool cc_parse_declarator(
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
        cc_swap_func_decl(&var->type);

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
        cc_ast_type_cv* array_cv = &var->type.cv_qual[var->type.n_cv_qual];
        array_cv->is_array = true;

        if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && ctok->type == LEXER_TOKEN_static)
            var->type.cv_qual[var->type.n_cv_qual].is_static_array = true;
        while (cc_parse_type_qualifier(ctx, &var->type))
            ; /* Parse type qualifiers list */
        if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && ctok->type == LEXER_TOKEN_static)
            var->type.cv_qual[var->type.n_cv_qual].is_static_array = true;

        cc_ast_literal literal = { 0 };
        if (!cc_parse_constant_expression(ctx, node, &literal)) {
            cc_diag_warning(ctx, "Variable length arrays are not supported");
        }
        array_cv->array_size
            = cc_ceval_literal_to_ushort(ctx, &literal);

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
        cc_ast_node* assign_node = cc_ast_create_block(ctx, node);
        cc_parse_declarator_assignment_expression(ctx, assign_node, var);
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
bool cc_parse_declarator_list(cc_context* ctx, cc_ast_node* node,
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
