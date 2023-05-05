/* parser.c - C parser an AST generator. */
#include "parser.h"
#include "ast.h"
#include "constevl.h"
#include "context.h"
#include "diag.h"
#include "lexer.h"
#include "optzer.h"
#include "parexpr.h"
#include "partyp.h"
#include "util.h"
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static bool cc_parse_compund_statment(cc_context* ctx, cc_ast_node* node);
static bool cc_parse_compund_statment_potential_declarator(
    cc_context* ctx, cc_ast_node* node, bool expect_semicolon);

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
        cc_ast_node* nblock;
        cc_lex_token_consume(ctx);

        /* Full of compound statments - wrap this around a new block! */
        nblock = cc_ast_create_block(ctx, node);
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
        cc_ast_node* for_node;
        cc_ast_node* if_node;
        cc_ast_node* step_node;
        cc_ast_node* jmp_node;
        cc_ast_node* old_break_node;
        cc_ast_node* old_continue_node;

        cc_lex_token_consume(ctx);

        /* for ( init; condition; step )  */
        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_LPAREN, "Expected '('");

        for_node = cc_ast_create_block(ctx, node);
        cc_parse_compund_statment_potential_declarator(ctx, for_node, false);

        /* Temporarily reassign parent of this node so the variables declared
           inside here are visable to the rest of the for loop */
        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_SEMICOLON, "Expected ';'");

        if_node = cc_ast_create_if_expr(ctx, for_node);
        cc_parse_expression(ctx, if_node->data.if_expr.cond);
        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_SEMICOLON, "Expected ';'");

        step_node = cc_ast_create_block(ctx, if_node->data.if_expr.block);
        cc_parse_expression(ctx, step_node);

        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RPAREN, "Expected ')'");
        jmp_node
            = cc_ast_create_jump(ctx, if_node->data.if_expr.block, if_node);

        old_break_node = ctx->break_node;
        old_continue_node = ctx->continue_node;
        ctx->break_node = jmp_node;
        ctx->continue_node = if_node;
        cc_parse_statment(ctx, if_node->data.if_expr.block);
        ctx->break_node = old_break_node;
        ctx->continue_node = old_continue_node;

        /* Initialize, then evaluate, then do body and then perform step! */
        cc_ast_add_block_node(if_node->data.if_expr.block, step_node);
        cc_ast_add_block_node(if_node->data.if_expr.block, jmp_node);
        cc_ast_add_block_node(for_node, if_node); /* Perform "if" */
        cc_ast_add_block_node(node, for_node);
        return true;
    } else if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
        && ctok->type == LEXER_TOKEN_while) {
        cc_ast_node* while_node;
        cc_ast_node* if_node;
        cc_ast_node* jmp_node;
        cc_ast_node* old_break_node;
        cc_ast_node* old_continue_node;

        cc_lex_token_consume(ctx);

        while_node = cc_ast_create_block(ctx, node);

        /* while ( condition ) secondary-block */
        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_LPAREN, "Expected '('");

        if_node = cc_ast_create_if_expr(ctx, while_node);
        cc_parse_expression(ctx, if_node->data.if_expr.cond);

        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RPAREN, "Expected ')'");

        jmp_node
            = cc_ast_create_jump(ctx, if_node->data.if_expr.block, if_node);

        old_break_node = ctx->break_node;
        old_continue_node = ctx->continue_node;
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
        cc_ast_node* while_node;
        cc_ast_node* if_node;
        cc_ast_node* jmp_node;
        cc_ast_node* old_break_node;
        cc_ast_node* old_continue_node;

        /* Run the secondary-block once, and if conditions are met,
           run it again until the conditions are no longer met. */
        /* Roughly:
           { body: <body> if (condition) { <jump to body> } } */
        cc_lex_token_consume(ctx);
        while_node = cc_ast_create_block(ctx, node);
        if_node = cc_ast_create_if_expr(ctx, while_node);
        jmp_node
            = cc_ast_create_jump(ctx, if_node->data.if_expr.block, while_node);

        /* do secondary-block while ( condition ) */
        old_break_node = ctx->break_node;
        old_continue_node = ctx->continue_node;
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
        bool has_braces = false;
        cc_ast_node* if_node = cc_ast_create_if_expr(ctx, node);
        cc_lex_token_consume(ctx);

        /* Condition to evaluate as ( <expr> ) */
        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_LPAREN, "Expected '('");
        cc_parse_expression(ctx, if_node->data.if_expr.cond);
        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RPAREN, "Expected ')'");

        /* Used for linting to check for a dangling else
           we can detect a dangling else if we have a nested if
           aka. 2 ifs, and then we find an else, we're able to
           alert the user about the encounter. */
        ++ctx->if_depth;
        if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && ctok->type == LEXER_TOKEN_LBRACE) {
            has_braces = true;
            ++ctx->if_w_braces_depth;
        }

        /* Body of the if - <secondary-block> */
        cc_parse_statment(ctx, if_node->data.if_expr.block);
        if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
            && ctok->type == LEXER_TOKEN_else) {
            /* Tail-else, usually if we leave this node empty it means
               the equivalent to a no-op, we can confidently allocate it
               and not use it. */
            cc_lex_token_consume(ctx);
            if (ctx->if_depth - ctx->if_w_braces_depth >= 2)
                cc_diag_warning(ctx, "Dangling else");
            /* Secondary block */
            cc_parse_statment(ctx, if_node->data.if_expr.tail_else);
        }

        --ctx->if_depth;
        if (has_braces)
            --ctx->if_w_braces_depth;

        cc_ast_add_block_node(node, if_node);
    }

    /* TODO: Fix switch and handle them properly? */
    if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL
        && ctok->type == LEXER_TOKEN_switch) {
        cc_ast_node* switch_node;
        cc_ast_node* end_node;
        cc_ast_node* old_break_node;
        cc_ast_node* old_continue_node;
        cc_ast_node* tnode;
        cc_ast_type type = { 0 };

        cc_lex_token_consume(ctx);

        /* Condition/The controlling expression*/
        switch_node = cc_ast_create_switch_expr(ctx, node);
        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_LPAREN, "Expected '('");
        cc_parse_expression(ctx, switch_node->data.switch_expr.control);
        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RPAREN, "Expected ')'");

        end_node
            = cc_ast_create_block(ctx, switch_node->data.switch_expr.block);

        old_break_node = ctx->break_node;
        old_continue_node = ctx->continue_node;
        ctx->break_node = end_node;
        ctx->continue_node = NULL;
        /* Body of the switch - <secondary-block> */
        cc_parse_statment(ctx, switch_node->data.switch_expr.block);

        /* Diagnostis for uncovered cases iff the element is an enumerator */

        tnode = switch_node->data.switch_expr.control;
        cc_optimizer_expr_condense(ctx, &tnode, false);
        if (tnode == NULL) {
            cc_diag_error(ctx, "Empty controlling expression");
            goto error_handle;
        }

        if (!cc_ceval_deduce_type(
                ctx, switch_node->data.switch_expr.control, &type)) {
            cc_diag_error(ctx, "Unable to deduce type of switch");
            cc_ast_destroy_node(end_node, true);
            cc_ast_destroy_node(switch_node, true);
            goto error_handle;
        }

        ctx->break_node = old_break_node;
        ctx->continue_node = old_continue_node;

        cc_ast_add_block_node(switch_node->data.switch_expr.block, end_node);
        cc_ast_add_block_node(node, switch_node);
    }
error_handle:
    return false;
}

/* Parses part of the compound statment which **could** potentially be a
   declaration of a new variable,  */
static bool cc_parse_compund_statment_potential_declarator(
    cc_context* ctx, cc_ast_node* node, bool expect_semicolon)
{
    const cc_lexer_token* ctok;
    /* First try interpreting as an expression, then if that
        does NOT work, fallback to the declarator */
    if (!cc_parse_expression(ctx, node)) {
        cc_ast_variable nvar = { 0 };
        if (!cc_parse_declarator_list(ctx, node, &nvar))
            goto error_handle;
        if (!nvar.name) {
            cc_diag_error(ctx, "Anonymous variable declared");
            cc_ast_destroy_var(&nvar, false);
            goto error_handle;
        }
        cc_ast_add_or_replace_block_variable(node, &nvar);
    }
    if (expect_semicolon)
        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_SEMICOLON, "Expected ';'");
    return true;
error_handle:
    if (expect_semicolon)
        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_SEMICOLON, "Expected ';'");
    return false;
}

static bool cc_parse_compund_statment(cc_context* ctx, cc_ast_node* node)
{
    const cc_lexer_token* ctok;
    if ((ctok = cc_lex_token_peek(ctx, 0)) == NULL)
        return false;

    /* Labelled statments... */
    if (ctok->type == LEXER_TOKEN_IDENT
        && (ctok = cc_lex_token_peek(ctx, 1)) != NULL
        && ctok->type == LEXER_TOKEN_COLON) {
        /* Obtain label identifier */
        ctok = cc_lex_token_peek(ctx, 0);
        cc_lex_token_consume(ctx); /* Ident */
        cc_lex_token_consume(ctx); /* Colon */
        assert(ctok->type == LEXER_TOKEN_IDENT);
        return true;
    } else {
        /* Reposition ctok */
        ctok = cc_lex_token_peek(ctx, 0);
    }

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
        return true;
    }
    case LEXER_TOKEN_default: {
        cc_ast_node* block_node = cc_ast_create_block(ctx, node);
        cc_lex_token_consume(ctx);
        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_COLON, "Expected ':'");
        cc_parse_statment(ctx, block_node);

        block_node->data.block.is_case = true;
        block_node->data.block.is_default = true;
        block_node->ref_count++; /* Referenced by switch node */
        cc_ast_add_block_node(node, block_node);
        return true;
    }
    case LEXER_TOKEN_if:
    case LEXER_TOKEN_switch: {
        cc_parse_selection_statment(ctx, node);
        return true;
    }
    case LEXER_TOKEN_for:
    case LEXER_TOKEN_while:
    case LEXER_TOKEN_do: {
        cc_parse_iteration_statment(ctx, node);
        return true;
    }
    case LEXER_TOKEN_goto: {
        cc_lex_token_consume(ctx);
        CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_IDENT, "Expected 'ident'");
    } break;
    case LEXER_TOKEN_continue: {
        cc_ast_node* continue_node;
        cc_lex_token_consume(ctx);
        if (ctx->continue_node == NULL) {
            cc_diag_error(ctx, "Continue not within lexicographical context");
            goto error_handle;
        }
        continue_node = cc_ast_create_jump(ctx, node, ctx->continue_node);
        cc_ast_add_block_node(node, continue_node);
    } break;
    case LEXER_TOKEN_break: {
        cc_ast_node* break_node;
        cc_lex_token_consume(ctx);
        if (ctx->break_node == NULL) {
            cc_diag_error(ctx, "Break not within lexicographical context");
            goto error_handle;
        }
        break_node = cc_ast_create_jump(ctx, node, ctx->break_node);
        cc_ast_add_block_node(node, break_node);
    } break;
    case LEXER_TOKEN_return: {
        cc_ast_node* ret_node;
        cc_lex_token_consume(ctx);
        ret_node = cc_ast_create_ret_expr(ctx, node);
        cc_parse_expression(ctx, ret_node->data.return_expr);
        cc_ast_add_block_node(node, ret_node);
    } break;
    case LEXER_TOKEN_IDENT: {
        const cc_ast_variable* var
            = cc_ast_find_variable(cc_get_cfunc_name(ctx), ctok->data.text, node);
        if (var == NULL)
            cc_ast_find_variable(
                cc_get_cfunc_name(ctx), ctok->data.text, node->parent);

        if (var != NULL && (var->storage & AST_STORAGE_TYPEDEF) == 0) {
            /* Variable reference OR call/assignment */
            cc_parse_expression(ctx, node);
        } else { /* Type for declaration within compound stmt */
            /* Implicit function declarations, where the prototype
                is missing from the function and it's declared in place
                this exception exists for some fucking reason??? */
            if ((ctok = cc_lex_token_peek(ctx, 1)) != NULL
                && ctok->type == LEXER_TOKEN_LPAREN) {
                cc_ast_variable nvar = { 0 };

                cc_diag_warning(ctx, "Implicit function declaration");

                ctok = cc_lex_token_peek(ctx, 0); /* Identifier */
                nvar.name = ctok->data.text;
                cc_swap_func_decl(&nvar.type);
                nvar.storage = AST_STORAGE_EXTERN;
                /* Variadic, basically meaning we have no fucking idea */
                nvar.type.data.func.variadic = true;
                nvar.type.data.func.return_type
                    = cc_zalloc(sizeof(cc_ast_type));
                nvar.type.data.func.return_type->mode = AST_TYPE_MODE_INT;
                cc_ast_add_or_replace_block_variable(node, &nvar);
                return cc_parse_compund_statment(ctx, node);
            } else {
                cc_ast_variable nvar = { 0 };
                if (!cc_parse_declarator_list(ctx, node, &nvar))
                    goto error_handle;
                cc_ast_add_or_replace_block_variable(node, &nvar);
            }
        }
    } break;
    default:
        return cc_parse_compund_statment_potential_declarator(ctx, node, true);
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
    cc_ast_variable var = { 0 };

    if ((ctok = cc_lex_token_peek(ctx, 0)) == NULL)
        return false;

    /* TODO: Handle cases such as:
       typedef struct SomeThing {} NewName ident; */

    /* Declaration specifiers */
    cc_parse_declarator_list(ctx, node, &var);
    if (!ctx->is_libc_decl && !var.type.data.func.builtin_libc) {
        if (var.name) {
            const char *name = cc_strview(var.name);
            if (name[0] == '_' && isupper(name[1])
                && !(var.storage & AST_STORAGE_EXTERN)) {
                cc_diag_warning(ctx, "Reserved identifier '%s'", name);
            } else if (name[0] == '_' && name[1] == '_'
                && !(var.storage & AST_STORAGE_EXTERN)) {
                cc_diag_warning(ctx, "Reserved identifier '%s'", name);
            }
        }
    }

    if ((ctok = cc_lex_token_peek(ctx, 0)) != NULL) {
        switch (ctok->type) {
        case LEXER_TOKEN_SEMICOLON: /* Prototype/declaration */
            cc_lex_token_consume(ctx);
            break;
        case LEXER_TOKEN_LBRACE: { /* Function body */
            const cc_ast_variable* old_ast_current_func;
            bool old_is_func_body;
            if ((var.storage & AST_STORAGE_TYPEDEF) != 0
                || (var.storage & AST_STORAGE_THREAD_LOCAL) != 0
                || (var.storage & AST_STORAGE_REGISTER) != 0) {
                cc_diag_error(ctx, "Invalid function definition specifiers");
                goto error_handle;
            } else if (var.type.mode != AST_TYPE_MODE_FUNCTION) {
                cc_diag_error(ctx, "Unexpected '{' on non-function type");
                goto error_handle;
            } else if ((var.storage & AST_STORAGE_EXTERN) != 0) {
                /* All functions that are not prototypes are treated as a variable. */
                cc_diag_warning(ctx,
                    "Function '%s' declared extern but defined here", cc_strview(var.name));
                var.storage &= ~AST_STORAGE_EXTERN;
                var.storage |= AST_STORAGE_GLOBAL;
            }

            cc_lex_token_consume(ctx);
            cc_ast_add_or_replace_block_variable(node, &var);

            /* And variable for the function itself */
            var.body = cc_ast_create_block(ctx, node);
            old_is_func_body = ctx->is_func_body;
            ctx->is_func_body = true;
            old_ast_current_func = ctx->ast_current_func;
            ctx->ast_current_func = &var;
            while (cc_parse_compund_statment(ctx, var.body))
                ;
            cc_optimizer_expr_condense(ctx, &var.body, true);
            ctx->ast_current_func = old_ast_current_func;
            ctx->is_func_body = old_is_func_body;
            CC_PARSE_EXPECT(ctx, ctok, LEXER_TOKEN_RBRACE, "Expected '}'");
            cc_ast_add_or_replace_block_variable(node, &var);
            return true;
        }
        default:
            cc_diag_error(ctx, "Unexpected token in declarator");
            goto error_handle;
        }
    }

    if (!var.name) {
        if (!var.type.name) {
            if (var.type.mode == AST_TYPE_MODE_ENUM) {
                /* An anonymous enumerator might be undesirable for a variety
                   of reasons, however this isn't an error, so we can continue
                   normal execution. */
                cc_diag_warning(ctx, "Anonymous enum");
                if (!var.name)
                    var.name = cc_strdup(cc_get_anon_name(ctx));
                if (!var.type.name)
                    var.type.name = cc_strdup(cc_get_anon_name(ctx));
            } else {
                cc_diag_error(
                    ctx, "Anonymous external declaration of variable");
                goto error_handle;
            }
        } else {
            return true;
        }
    }

    /* Automatically give variables globality-scope if they don't
        have any other linkage specifiers. */
    if (var.storage == AST_STORAGE_NONE)
        var.storage = AST_STORAGE_GLOBAL;
    cc_ast_add_or_replace_block_variable(node, &var);
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
    cc_parse_translation_unit(ctx, ctx->root);
    return 0;
}
