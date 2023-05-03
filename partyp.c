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
#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Symbols from PDPCLIB */
static const char* libc_names[140 + 3] = { "clearerr", "fclose", "feof",
    "ferror", "fflush", "fgetc", "fgetpos", "fgets", "fopen", "fprintf",
    "fputc", "fputs", "fread", "freopen", "fscanf", "fseek", "fsetpos", "ftell",
    "fwrite", "getc", "getchar", "gets", "perror", "printf", "putc", "putchar",
    "puts", "remove", "rename", "rewind", "scanf", "setbuf", "setvbuf",
    "sprintf", "sscanf", "tmpfile", "tmpnam", "ungetc", "vfprintf", "vprintf",
    "vsprintf", "memchr", "memcmp", "memcpy", "memmove", "memset", "strcat",
    "strchr", "strcmp", "strcoll", "strcpy", "strcspn", "strerror", "strlen",
    "strncat", "strncmp", "strncpy", "strpbrk", "strrchr", "strspn", "strstr",
    "strtok", "strxfrm", "abort", "abs", "atexit", "atof", "atoi", "atol",
    "bsearch", "calloc", "div", "exit", "free", "getenv", "labs", "ldiv",
    "malloc", "mblen", "mbstowcs", "mbtowc", "qsort", "rand", "realloc",
    "srand", "strtod", "strtol", "strtoul", "system", "wcstombs", "wctomb",
    "asctime", "clock", "ctime", "difftime", "gmtime", "localtime", "mktime",
    "strftime", "time", "raise", "signal", "localeconv", "setlocale", "isalnum",
    "isalpha", "iscntrl", "isdigit", "isgraph", "islower", "isprint", "ispunct",
    "isspace", "isupper", "isxdigit", "tolower", "toupper", "longjmp", "acos",
    "asin", "atan", "atan2", "ceil", "cos", "cosh", "exp", "fabs", "floor",
    "fmod", "frexp", "ldexp", "log", "log10", "modf", "pow", "sin", "sinh",
    "sqrt", "tan", "tanh",
    /* msvcrt part of pdpclib */
    "__gtin", "__gtout", "gterr" };

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
            type->data.shared->s_or_u.packed = true;
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
    const cc_lexer_token* ctok;
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
    const cc_lexer_token* ctok;
    cc_ast_variable virtual_member = { 0 };
    if ((ctok = cc_lex_token_peek(ctx, 0)) == NULL)
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
        abort();
    }

    while (cc_parse_struct_or_union_attributes(ctx, node, type))
        ;
    if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
        && ctok->type == LEXER_TOKEN_IDENT) {
        cc_ast_type* s_or_u_type;
        type->name = cc_strdup(ctok->data);

        s_or_u_type = cc_ast_find_type(type->name, node);
        if (s_or_u_type != NULL) {
            /* We're using an already existing type for this structure,
               i.e "struct name { ... };" -> "struct name a;" */
            cc_ast_copy_type(type, s_or_u_type);
            if (s_or_u_type->mode != AST_TYPE_MODE_STRUCT
                && s_or_u_type->mode != AST_TYPE_MODE_UNION) {
                cc_diag_error(
                    ctx, "%s isn't a valid struct/union", s_or_u_type->name);
                type->mode = AST_TYPE_MODE_STRUCT;
            }
            cc_lex_token_consume(ctx);
        } else {
            /* TODO: allocation for shared_type */
            assert(type->data.shared == NULL);
            type->data.shared = cc_zalloc(sizeof(cc_ast_shared_type));
            /* Could be a forward declaration of an struct? */
            cc_lex_token_consume(ctx);
        }
        if ((ctok = cc_lex_token_peek(ctx, 0)) == NULL
            || ctok->type != LEXER_TOKEN_LBRACE) {
            if (type->name == NULL) {
                cc_diag_error(ctx, "Forward declaration of anonymous %s",
                    type->mode == AST_TYPE_MODE_STRUCT ? "struct" : "union");
                goto error_handle;
            }
            cc_ast_add_block_type(node, type);
            return true;
        }
    } else {
        assert(type->data.shared == NULL);
        type->data.shared = cc_zalloc(sizeof(cc_ast_shared_type));
    }
    CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_LBRACE, "Expected '{'");

    /* TODO: Attributes */
    if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
        && ctok->type == LEXER_TOKEN_RBRACE)
        goto empty_memberlist;

    while (cc_parse_declarator(ctx, node, &virtual_member)) {
        if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && ctok->type == LEXER_TOKEN_COLON) {
            cc_ast_literal literal = { 0 };

            cc_lex_token_consume(ctx);
            virtual_member.type.mode = AST_TYPE_MODE_BITINT;
            virtual_member.type.data.num.bitint_bits = 0;

            /* Constexpression follows, evaluate it nicely :) */
            cc_parse_constant_expression(ctx, node, &literal);
            if (literal.is_signed && literal.value.s < 0) {
                cc_diag_error(ctx, "Number of bits can't be negative");
                goto error_handle;
            } else if (literal.is_float) {
                cc_diag_error(ctx, "Bits must be a whole integer");
                goto error_handle;
            } else if ((literal.is_signed && literal.value.s == 0)
                || (!literal.is_signed && literal.value.u == 0)) {
                cc_diag_error(ctx, "0-bit width in bitfield is invalid");
                goto error_handle;
            }
            if (literal.is_signed)
                virtual_member.type.data.num.bitint_bits
                    = (unsigned int)literal.value.s;
            else
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
    if (type->data.shared != NULL) {
        /* Empty structures, unions or enums do not require a diagnostic(?)
            but we'll provide one anyways. */
        if ((type->mode == AST_TYPE_MODE_ENUM
                && type->data.shared->enumer.n_elems == 0))
            cc_diag_warning(ctx, "Empty enum");
        else if ((type->mode == AST_TYPE_MODE_STRUCT
                     || type->mode == AST_TYPE_MODE_UNION)
            && type->data.shared->s_or_u.n_members == 0)
            cc_diag_warning(ctx, "Empty structure/union");
    }

    /* We will add structs that are not anonymous into the current defined
       types list so we can use them later as required. */
    if (type->name != NULL)
        cc_ast_add_block_type(node, type);
    return true;
error_handle:
    return false;
}

static bool cc_parse_enum_specifier(
    cc_context* ctx, cc_ast_node* node, cc_ast_type* type)
{
    cc_ast_literal seq_literal = { 0 }; /* Enumerator values start at 0 */
    const cc_lexer_token* ctok;
    size_t i;

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
                cc_strfree(type->name);
                type->name = NULL;
            }
            cc_ast_copy_type(type, enum_type);
            if (enum_type->mode != AST_TYPE_MODE_ENUM) {
                cc_diag_error(ctx, "%s isn't a valid enum", enum_type->name);
                type->mode = AST_TYPE_MODE_ENUM;
            }
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
    assert(type->data.shared == NULL);
    type->data.shared = cc_zalloc(sizeof(cc_ast_shared_type));
    while ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
        && ctok->type == LEXER_TOKEN_IDENT) {
        cc_ast_enum_member member = { 0 };
        cc_ast_variable var = { 0 };

        cc_lex_token_consume(ctx);
        member.name = cc_strdup(ctok->data);
        /* Assignment of enumerator value. */
        member.literal = seq_literal;
        if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && ctok->type == LEXER_TOKEN_ASSIGN) {
            cc_ast_node* const_expr;
            cc_lex_token_consume(ctx);

            const_expr = cc_ast_create_block(ctx, node);
            /* Parse like a normal expression */
            cc_parse_unary_expression(ctx, const_expr);
            if (!cc_ceval_constant_expression(
                    ctx, &const_expr, &member.literal)) {
                cc_diag_error(ctx, "Unable to evaluate constant expression");
                if (const_expr != NULL)
                    cc_ast_destroy_node(const_expr, true);
                goto error_handle;
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
        var.type.mode = AST_TYPE_MODE_INT;
        /* Override type specification and enable constexpr evaluation */
        var.storage = AST_STORAGE_CONSTEXPR;
        var.name = cc_strdup(member.name);
        var.initializer = cc_ast_create_literal(ctx, node, member.literal);
        cc_ast_add_or_replace_block_variable(node, &var);

        type->data.shared->enumer.elems
            = cc_realloc_array(type->data.shared->enumer.elems,
                type->data.shared->enumer.n_elems + 1);
        type->data.shared->enumer.elems[type->data.shared->enumer.n_elems++]
            = member;

        if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && ctok->type == LEXER_TOKEN_COMMA
            && (ctok = cc_lex_token_peek(ctx, 1)) != NULL
            && ctok->type == LEXER_TOKEN_RBRACE) {
            cc_lex_token_consume(ctx); /* Skip comma, right-brace is consumed
                                          after exiting the loop */
            cc_diag_warning(ctx, "Trailing comma in enum");
            break;
        } else if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && ctok->type == LEXER_TOKEN_COMMA) {
            cc_lex_token_consume(ctx);
            continue;
        } else if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && ctok->type == LEXER_TOKEN_RBRACE)
            break;
    }

    for (i = 0; i < type->data.shared->enumer.n_elems; i++) {
        size_t j;
        for (j = 0; j < type->data.shared->enumer.n_elems; j++) {
            if (i != j
                && !memcmp(&type->data.shared->enumer.elems[i].literal,
                    &type->data.shared->enumer.elems[j].literal,
                    sizeof(cc_ast_literal))) {
                cc_diag_error(ctx,
                    "Enumerator elements '%s' and '%s' with same value",
                    type->data.shared->enumer.elems[i].name,
                    type->data.shared->enumer.elems[j].name);
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

static bool cc_parse_function_specifier(cc_context* ctx, cc_ast_variable* var)
{
    const cc_lexer_token* ctok;
    if ((ctok = cc_lex_token_peek(ctx, 0)) == NULL)
        return false;
    switch (ctok->type) {
    case LEXER_TOKEN_inline:
        var->storage |= AST_STORAGE_INLINE;
        break;
    case LEXER_TOKEN__Noreturn:
        var->type.mode = AST_TYPE_MODE_FUNCTION;
        var->type.data.func.no_return = true;
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
        if (type->mode == AST_TYPE_MODE_FUNCTION
            || type->cv_qual[type->n_cv_qual].is_array)
            cc_diag_error(ctx,
                "_Atomic can't modify function pointers or "
                "arrays");
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
    const cc_lexer_token* ctok;
    if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
        && ctok->type == LEXER_TOKEN_IDENT) {
        const cc_ast_variable* tpdef
            = cc_ast_find_variable(cc_get_cfunc_name(ctx), ctok->data, node);
        if (tpdef == NULL || (tpdef->storage & AST_STORAGE_TYPEDEF) == 0)
            return false;
        cc_lex_token_consume(ctx);
        cc_ast_copy_type(type, &tpdef->type);
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
        cc_ast_node* typeof_expr;
        bool unqual = false;
        size_t i;

        if (ctok->type == LEXER_TOKEN_typeof_unqual)
            unqual = true;
        cc_lex_token_consume(ctx);

        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_LPAREN, "Expected '('");
        /* typeof ( <expr> ) */
        typeof_expr = cc_ast_create_block(ctx, node);
        cc_parse_expression(ctx, typeof_expr);
        if (!cc_ceval_deduce_type(ctx, typeof_expr, type)) {
            cc_diag_error(ctx, "Unable to deduce type for expression");
            goto error_handle;
        }
        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RPAREN, "Expected ')'");

        /* Unqualify if typeof_unqual is used */
        for (i = 0; unqual && i < type->n_cv_qual; i++) {
            cc_ast_type_cv* cv_qual = &type->cv_qual[i];
            cv_qual->is_atomic = cv_qual->is_const = cv_qual->is_restrict
                = cv_qual->is_volatile = false;
        }
        return true;
    }
error_handle:
    return false;
}

bool cc_parse_type_specifier(
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
        cc_ast_literal literal = { 0 };

        type->mode = AST_TYPE_MODE_BITINT;
        ctok = cc_lex_token_consume(ctx);
        ctok = cc_lex_token_consume(ctx);

        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_LPAREN, "Expected '('");
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
        type->mode = AST_TYPE_MODE_COMPLEX;
        break;
    case LEXER_TOKEN__Decimal32:
        type->mode = AST_TYPE_MODE_DECIMAL32;
        break;
    case LEXER_TOKEN__Decimal64:
        type->mode = AST_TYPE_MODE_DECIMAL64;
        break;
    case LEXER_TOKEN__Decimal128:
        type->mode = AST_TYPE_MODE_DECIMAL128;
        break;
    case LEXER_TOKEN___builtin_va_list:
        type->mode = AST_TYPE_MODE_VA_LIST;
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

static bool cc_parse_storage_class_specifier(
    cc_context* ctx, cc_ast_variable* var)
{
    const cc_lexer_token* ctok = ctok = cc_lex_token_peek(ctx, 0);
    if (ctok == NULL)
        return false;

    switch (ctok->type) {
    case LEXER_TOKEN_auto:
        var->storage |= AST_STORAGE_AUTO;
        break;
    case LEXER_TOKEN_extern:
        var->storage |= AST_STORAGE_EXTERN;
        break;
    case LEXER_TOKEN_static:
        var->storage |= AST_STORAGE_STATIC;
        break;
    case LEXER_TOKEN_register:
        var->storage |= AST_STORAGE_REGISTER;
        break;
    case LEXER_TOKEN_thread_local:
        var->storage |= AST_STORAGE_THREAD_LOCAL;
        break;
    case LEXER_TOKEN_constexpr:
        var->storage |= AST_STORAGE_CONSTEXPR;
        break;
    case LEXER_TOKEN_typedef:
        var->storage |= AST_STORAGE_TYPEDEF;
        break;
    default:
        return false;
    }
    /* Enforces type constraints */
    if ((var->storage & AST_STORAGE_THREAD_LOCAL) != 0) {
        enum cc_ast_storage tmp = var->storage & ~AST_STORAGE_THREAD_LOCAL;
        if (tmp == AST_STORAGE_NONE || (tmp & AST_STORAGE_STATIC) != 0
            || (tmp & AST_STORAGE_EXTERN) != 0)
            ;
        else
            cc_diag_error(ctx,
                "thread_local may only be used with static or"
                "extern");
    } else if ((var->storage & AST_STORAGE_CONSTEXPR) != 0) {
        enum cc_ast_storage tmp = var->storage & ~AST_STORAGE_CONSTEXPR;
        if (tmp == AST_STORAGE_NONE || (tmp & AST_STORAGE_AUTO) != 0
            || (tmp & AST_STORAGE_REGISTER) != 0
            || (tmp & AST_STORAGE_STATIC) != 0)
            ;
        else
            cc_diag_error(ctx,
                "constexpr may only be used with auto, register"
                " or static");
    } else if ((var->storage & AST_STORAGE_AUTO) != 0) {
        if ((var->storage & AST_STORAGE_TYPEDEF) != 0)
            cc_diag_error(ctx, "auto can't appear alongside typedef");
    } else if ((var->storage & AST_STORAGE_TYPEDEF) != 0) {
        if ((var->storage & ~AST_STORAGE_TYPEDEF) != 0)
            cc_diag_error(
                ctx, "typedef can't appear alongside other specifiers");
    }
error_handle: /* Normal finish */
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

static bool cc_parse_declaration_specifier(
    cc_context* ctx, cc_ast_node* node, cc_ast_variable* var)
{
    const cc_lexer_token* ctok;
    bool qualified_once = false;
    while (cc_parse_declaration_specifier_attributes(ctx, node, &var->type))
        ;
    /* Consume cv-qualifiers */
    while (cc_parse_storage_class_specifier(ctx, var)
        || cc_parse_type_specifier(ctx, node, &var->type)
        || cc_parse_function_specifier(ctx, var)
        || cc_parse_type_qualifier(ctx, &var->type))
        qualified_once = true;
    /* Parse pointers that can be of any depths */
    while ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
        && ctok->type == LEXER_TOKEN_ASTERISK) {
        qualified_once = true;
        cc_lex_token_consume(ctx);
        var->type.n_cv_qual++;
        if (var->type.n_cv_qual >= MAX_CV_QUALIFIERS) {
            cc_diag_error(ctx, "Exceeded maximum pointer depth");
            goto error_handle;
        }
        /* TODO: attribute sequence */
        while (cc_parse_type_qualifier(ctx, &var->type))
            ; /* Consume qualifiers */
    }
    return qualified_once;
error_handle:
    return false;
}

bool cc_parse_type_name(cc_context* ctx, cc_ast_node* node, cc_ast_type* type)
{
    const cc_lexer_token* ctok;
    bool qualified_once = false;
    size_t old_c_token = ctx->c_token;

    while (cc_parse_declaration_specifier_attributes(ctx, node, type))
        ;
    /* Consume cv-qualifiers */
    while (cc_parse_type_specifier(ctx, node, type)
        || cc_parse_type_qualifier(ctx, type))
        qualified_once = true;
    /* Parse pointers that can be of any depths */
    while ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
        && ctok->type == LEXER_TOKEN_ASTERISK) {
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

    /* Restore old qualifiactions */
    if (qualified_once == false)
        ctx->c_token = old_c_token;
    return qualified_once;
error_handle:
    return false;
}

static bool cc_parse_declarator_braced_initializer_element(
    cc_context* ctx, cc_ast_node* node, cc_ast_variable* var)
{
    const cc_lexer_token* ctok;
    if ((ctok = cc_lex_token_peek(ctx, 0)) == NULL)
        return false;

    if (ctok->type == LEXER_TOKEN_DOT) {
        while ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && ctok->type == LEXER_TOKEN_DOT) {
            cc_lex_token_consume(ctx);
            CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_IDENT, "Expected 'ident'");
        }
        if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && ctok->type == LEXER_TOKEN_ASSIGN)
            cc_parse_assignment_expression(ctx, node, NULL);
    } else {
        cc_parse_assignment_expression(ctx, node, NULL);
    }
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
            && ctok->type == LEXER_TOKEN_NUMBER && !strcmp(ctok->data, "0")
            && (ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && ctok->type == LEXER_TOKEN_RBRACE) {
            cc_lex_token_consume(ctx);
            /* TODO: Zero-initialize */
        } else if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && ctok->type == LEXER_TOKEN_RBRACE) {
            /* N2900 Consistent, Warningless, and Intuitive Initialization with {} */
        } else {
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
            /* Trailing declarator list comma */
            if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
                && ctok->type == LEXER_TOKEN_COMMA)
                cc_lex_token_consume(ctx);
        }
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
    const cc_lexer_token* ctok;
    cc_parse_declaration_specifier(ctx, node, var);

    /* Forward declarations such as "struct a;" or "union b;" */
    if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
        && ctok->type == LEXER_TOKEN_SEMICOLON) {
        if ((var->type.mode == AST_TYPE_MODE_ENUM
                || var->type.mode == AST_TYPE_MODE_STRUCT
                || var->type.mode == AST_TYPE_MODE_UNION)
            && var->type.data.shared == NULL) {
            assert(var->type.data.shared == NULL);
            var->type.data.shared = cc_zalloc(sizeof(cc_ast_shared_type));
        }
        return true;
    }

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
        const cc_ast_variable* other_var
            = cc_ast_find_variable(cc_get_cfunc_name(ctx), ctok->data, node);
        if (other_var == NULL
            || (other_var->storage & AST_STORAGE_TYPEDEF) == 0) {
            /* Not a typedef, so must be the identifier of this variable! */
            if (var->name != NULL) {
                cc_diag_error(ctx,
                    "More than one identifier on declaration (%s and %s)",
                    var->name, ctok->data);
                cc_lex_token_consume(ctx);
                goto error_handle;
            }
            var->name = cc_strdup(ctok->data);
        } else {
            assert(other_var != NULL);
            assert((other_var->storage & AST_STORAGE_TYPEDEF) != 0);
            cc_ast_copy_type(&var->type, &other_var->type);
        }
        cc_lex_token_consume(ctx);
    } break;
    default:
        if (ctx->is_parsing_prototype || ctx->abstract_declarator) {
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
        size_t i;
        cc_lex_token_consume(ctx);
        cc_swap_func_decl(&var->type);

        /* Check if this function is part of the libc declaration */
        for (i = 0; i < ARRAY_SIZE(libc_names); ++i)
            if (!strcmp(libc_names[i], var->name)) {
                var->type.data.func.builtin_libc = true;
                break;
            }

        /* No storage specified? set extern then */
        /*if (var->storage == AST_STORAGE_AUTO) {
            var->storage = AST_STORAGE_EXTERN;
        }*/

        if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && ctok->type == LEXER_TOKEN_ELLIPSIS) {
            /* Fully variadic function (non-standard) */
            var->type.data.func.variadic = true;
            cc_lex_token_consume(ctx);
            CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RPAREN, "Expected ')'");
            cc_diag_warning(ctx, "Fully variadic function is not portable");
        } else if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && ctok->type == LEXER_TOKEN_void
            && (ctok = cc_lex_token_peek(ctx, 1)) != NULL
            && ctok->type == LEXER_TOKEN_RPAREN) {
            /* func(void), functions taking no parameters at all */
            cc_lex_token_consume(ctx);
            CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RPAREN, "Expected ')'");
        } else if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && ctok->type == LEXER_TOKEN_RPAREN) {
            /* func(), functions taking any number of parameters */
            var->type.data.func.variadic = true;
            CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RPAREN, "Expected ')'");
        } else {
            cc_ast_variable virtual_param_var = { 0 };
            ctx->is_parsing_prototype = true;
            ctx->is_libc_decl = var->type.data.func.builtin_libc;
            while (cc_parse_declarator(ctx, node, &virtual_param_var)) {
                cc_ast_variable* param;
                var->type.data.func.params
                    = cc_realloc_array(var->type.data.func.params,
                        var->type.data.func.n_params + 1);
                param = &var->type.data.func
                             .params[var->type.data.func.n_params++];
                param->name = NULL;
                param->storage = AST_STORAGE_AUTO; /* Auto storage... */
                if (virtual_param_var.name != NULL)
                    param->name = cc_strdup(virtual_param_var.name);
                cc_ast_copy_type(&param->type, &virtual_param_var.type);

                /* Clear virtual parameter name */
                cc_ast_destroy_var(&virtual_param_var, false);
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
            ctx->is_parsing_prototype = ctx->is_libc_decl = false;
            CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RPAREN, "Expected ')'");
        }
    }

    while ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
        && ctok->type == LEXER_TOKEN_LBRACKET) {
        cc_ast_type_cv* array_cv;
        cc_ast_literal size_literal;
        cc_lex_token_consume(ctx);

        if (var->type.n_cv_qual >= MAX_CV_QUALIFIERS) {
            cc_diag_error(ctx, "Array exceeds pointer depth size");
            goto error_handle;
        }
        array_cv = &var->type.cv_qual[var->type.n_cv_qual];
        array_cv->is_array = true;

        if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && ctok->type == LEXER_TOKEN_static) {
            cc_lex_token_consume(ctx);
            var->type.cv_qual[var->type.n_cv_qual].is_static_array = true;
        }
        while (cc_parse_type_qualifier(ctx, &var->type))
            ; /* Parse type qualifiers list */
        if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && ctok->type == LEXER_TOKEN_static) {
            cc_lex_token_consume(ctx);
            var->type.cv_qual[var->type.n_cv_qual].is_static_array = true;
        }

        /* The size is an expression because we also need to support VLA
           arrays. Yes I'm aware this is quite terrifying for a lot of
           people who code C, but VLAs are pretty cool! */
        array_cv->array.size_expr = cc_ast_create_block(ctx, node);
        /* Array can have no size "char a[]" for example, so it isn't
           an error to have such arrays. */
        cc_parse_assignment_expression(ctx, array_cv->array.size_expr, NULL);
        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RBRACKET, "Expected ']'");

        /* Condense the VLA's size definition for easier evaluation */
        cc_optimizer_expr_condense(ctx, &array_cv->array.size_expr, true);
        /* VLA are Variable Length Arrays whose size is computed at runtime
           and are NOT constant in size */
        array_cv->is_vla = !cc_ceval_is_const(ctx, array_cv->array.size_expr);
        if (array_cv->is_vla) {

        } else {
            cc_ceval_constant_expression(
                ctx, &array_cv->array.size_expr, &size_literal);
            cc_ast_destroy_node(array_cv->array.size_expr, true);
            /* Assign a literal value, constant one too! */
            if (size_literal.value.u >= USHRT_MAX)
                cc_diag_error(
                    ctx, "Array exceeds %u elements", (unsigned int)USHRT_MAX);
            else if (size_literal.value.u == 0)
                cc_diag_warning(ctx, "0 length array will not be materialized");
            array_cv->array.size = (unsigned short)size_literal.value.u;
        }

        /* We allow arrays to exist at cv_qual index 0, because of cases like:
           static array[const static restrict 64]; */
        var->type.n_cv_qual++;
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
bool cc_parse_declarator_list(
    cc_context* ctx, cc_ast_node* node, cc_ast_variable* var)
{
    const cc_lexer_token* ctok;
    bool decl_result;
    if ((ctok = cc_lex_token_peek(ctx, 0)) == NULL)
        return false;

    /* Declaration specifiers */
comma_list_initializers: /* Jump here, reusing the variable's stack
                            location **but** copying over the type
                            with the various elements. */
    decl_result = cc_parse_declarator(ctx, node, var);

    /* A typedef can be treated as a variable UNTIL we exit the declarator
       parser loop. Once we exit it we will have to convert the variable
       into a typedef we can toy with.

       This state is updated accordingly on the type storage
       specifiers. */
    if (!ctx->is_func_body && var->storage == AST_STORAGE_NONE)
        var->storage = AST_STORAGE_GLOBAL;

    if (!decl_result)
        goto error_handle;

    if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
        && ctok->type == LEXER_TOKEN_COMMA) {
        cc_ast_type stype = { 0 };
        cc_lex_token_consume(ctx);
        /* Save the type somewhere safe, destroy the var,
            recreate the var with our type and destroy the type. */
        cc_ast_copy_type(&stype, &var->type);
        /* Assign global storage to the variable if it is not inside a
           function body. */
        if (!ctx->is_func_body && var->storage == AST_STORAGE_NONE)
            var->storage = AST_STORAGE_GLOBAL;
        cc_ast_add_or_replace_block_variable(node, var);
        memset(var, 0, sizeof(*var));
        cc_ast_copy_type(&var->type, &stype);
        cc_ast_destroy_type(&stype, false);
        goto comma_list_initializers;
    }
    return true;
error_handle:
    return false;
}
