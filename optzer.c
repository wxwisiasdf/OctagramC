/* optzer.c - C optimizer, redundancy deleter, and dead code remover. */
#include "optzer.h"
#include "ast.h"
#include "constevl.h"
#include "context.h"
#include "parser.h"
#include "util.h"
#include <assert.h>

bool cc_optimizer_is_empty_block(const cc_ast_node* node)
{
    if (node->type != AST_NODE_BLOCK)
        return false;
    return node->data.block.n_children == 0 && node->data.block.n_vars == 0;
}

/* Merges and squashes a block. */
void cc_optimizer_merge_block(
    cc_context* ctx, cc_ast_node** pnode, bool managed)
{
    cc_ast_node* node = *pnode;
    size_t i;

    if (node == NULL || node->type != AST_NODE_BLOCK)
        return;

    for (i = 0; i < node->data.block.n_children; ++i) {
        cc_ast_node* tnode = &node->data.block.children[i];
        cc_optimizer_merge_block(ctx, &tnode, false);
        if (tnode == NULL) /* If removed then just remove from block */
            cc_ast_remove_block_node(node, i--);
    }

    /* Do not remove nodes that are jumped into */
    if (node->ref_count)
        return;
    /* Do not delete case labels we use to jump */
    if (node->data.block.is_case)
        return;

    /* Empty block with no effect whatsoever, typedefs are NOT accounted
        as a typedef doesn't affect the overall program that much */
    if (cc_optimizer_is_empty_block(node)) {
        cc_ast_destroy_node(node, managed);
        *pnode = NULL;
    }
    /* Blocks of the form { <expr> } can be coalesced to simply form
        <expr> */
    else if (node->data.block.n_children == 1 && node->data.block.n_types == 0
        && node->data.block.n_vars == 0) {
        /* Copy <expr> over */
        cc_ast_node new_node = node->data.block.children[0];
        new_node.parent = node->parent;
        /* Destroy the now (invalid) block - however DO NOT DEALLOCATE
            it as we can use the memory again but with our new expr type */
        cc_free(node->data.block.children);
        node->data.block.children = NULL;
        node->data.block.n_children = 0;
        cc_ast_destroy_node(node, false);
        **pnode = new_node; /* Copy */
    }
}

/* Condense/simplify expressions of an AST tree and remove redundancies
   Use funcbody to disallow block elimination on function bodies */
void cc_optimizer_expr_condense(
    cc_context* ctx, cc_ast_node** pnode, bool managed)
{
    cc_ast_node* node = *pnode;
    if (node == NULL)
        return;

    switch (node->type) {
    case AST_NODE_BINOP:
        cc_optimizer_expr_condense(ctx, &node->data.binop.left, true);
        cc_optimizer_expr_condense(ctx, &node->data.binop.right, true);

        if (node->ref_count) /* Do not remove nodes that are jumped into */
            return;

        /* No-op means we coalesce */
        if (node->data.binop.left != NULL && node->data.binop.right == NULL) {
            cc_ast_node new_node = *node->data.binop.left;
            new_node.parent = node->parent;
            node->data.binop.left = NULL;
            cc_ast_destroy_node(node, false);
            **pnode = new_node;
            return;
        } else if (node->data.binop.left == NULL
            && node->data.binop.right != NULL) {
            cc_ast_node new_node = *node->data.binop.right;
            new_node.parent = node->parent;
            node->data.binop.right = NULL;
            cc_ast_destroy_node(node, false);
            **pnode = new_node;
            return;
        }

        /* Binop operation between two constants */
        assert(node->data.binop.left != NULL && node->data.binop.right != NULL);
        if (cc_ceval_is_const(ctx, node->data.binop.left)
            && cc_ceval_is_const(ctx, node->data.binop.right)) {
            cc_ast_node new_node = { 0 };
            new_node.type = AST_NODE_LITERAL;
            new_node.data.literal = cc_ceval_eval(ctx, node);
            new_node.parent = node->parent;
            node->data.binop.right = NULL;
            cc_ast_destroy_node(node, false);
            **pnode = new_node;
        }
        break;
    case AST_NODE_BLOCK: {
        size_t i;
        for (i = 0; i < node->data.block.n_vars; i++) {
            cc_ast_variable* var = &node->data.block.vars[i];
            size_t j;
            if (var->type.mode == AST_TYPE_MODE_FUNCTION) {
                const cc_ast_variable* old_func = ctx->ast_current_func;
                ctx->ast_current_func = var;
                cc_optimizer_expr_condense(ctx, &var->body, true);
                ctx->ast_current_func = old_func;
            }
            /* Condense expressions for possible VLAs */
            for (j = 0; j < var->type.n_cv_qual; ++j)
                if (var->type.cv_qual[j].is_array
                    && var->type.cv_qual[j].is_vla)
                    cc_optimizer_expr_condense(
                        ctx, &var->type.cv_qual[j].array.size_expr, true);
        }
        for (i = 0; i < node->data.block.n_children; i++) {
            cc_ast_node* tnode = &node->data.block.children[i];
            cc_optimizer_expr_condense(ctx, &tnode, false);
            if (tnode == NULL) /* If removed then just remove from block */
                cc_ast_remove_block_node(node, i--);
        }
        cc_optimizer_merge_block(ctx, pnode, managed);
    } break;
    case AST_NODE_CALL: {
        size_t i;
        cc_optimizer_expr_condense(ctx, &node->data.call.call_expr, true);
        for (i = 0; i < node->data.call.n_params; i++) {
            cc_ast_node* tnode = &node->data.call.params[i];
            cc_optimizer_expr_condense(ctx, &tnode, false);
            assert(tnode != NULL); /* Empty parameters should be handled by
                                      the parsing stage, not us. */
        }

        /* Consteval the calls to functions */
        if (node->data.call.call_expr != NULL
            && node->data.call.call_expr->type == AST_NODE_VARIABLE) {
            cc_ast_variable* var = cc_ast_find_variable(cc_get_cfunc_name(ctx),
                node->data.call.call_expr->data.var.name, node);
            if (var != NULL && var->type.mode == AST_TYPE_MODE_FUNCTION
                && (var->storage & AST_STORAGE_CONSTEXPR) != 0) {
                /* Replace the call with the new constexpr-ed literal result */
                cc_ast_node new_node = { 0 };
                new_node.type = AST_NODE_LITERAL;
                new_node.data.literal = cc_ceval_eval(ctx, node);
                new_node.parent = node->parent;
                cc_ast_destroy_node(node, false);
                **pnode = new_node; /* Copying is easier & safer :) */
                return;
            }
        }
    } break;
    case AST_NODE_IF:
        cc_optimizer_expr_condense(ctx, &node->data.if_expr.cond, true);
        cc_optimizer_expr_condense(ctx, &node->data.if_expr.block, true);
        cc_optimizer_expr_condense(ctx, &node->data.if_expr.tail_else, true);
        break;
    case AST_NODE_RETURN:
        cc_optimizer_expr_condense(ctx, &node->data.return_expr, true);
        break;
    case AST_NODE_UNOP:
        cc_optimizer_expr_condense(ctx, &node->data.unop.child, true);
        break;
    case AST_NODE_SWITCH:
        cc_optimizer_expr_condense(ctx, &node->data.switch_expr.control, true);
        cc_optimizer_expr_condense(ctx, &node->data.switch_expr.block, true);
        break;
    case AST_NODE_FIELD_ACCESS:
        cc_optimizer_expr_condense(ctx, &node->data.field_access.left, true);
        break;
    case AST_NODE_JUMP:
    case AST_NODE_STRING_LITERAL:
    case AST_NODE_LITERAL:
    case AST_NODE_VARIABLE:
        break;
    default:
        cc_abort(__FILE__, __LINE__);
    }
}

int cc_optimizer_top(cc_context* ctx)
{
    /*cc_ast_iterate(ctx->root, &cc_optimizer_expr_condense, &ctx->root, true);*/
    cc_optimizer_expr_condense(ctx, &ctx->root, true);
    return 0;
}
