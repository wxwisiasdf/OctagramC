/* backend.c - Backend assist functions for generating assembly code */
#include "backend.h"
#include "ast.h"
#include "constevl.h"
#include "context.h"
#include "parser.h"
#include "util.h"
#include <assert.h>
#include <string.h>

unsigned int cc_backend_get_labelnum(cc_context* ctx)
{
    return ctx->backend_data->label_num++;
}

void cc_backend_spill_reg(cc_context* ctx, unsigned int regno)
{
    cc_backend_varmap ltmp = { 0 }, rtmp = { 0 };
    ltmp.flags = VARMAP_STACK;
    ltmp.offset = ctx->backend_data->stack_frame_size;
    rtmp.flags = VARMAP_REGISTER;
    rtmp.regno = regno;

    ctx->backend_data->gen_mov(ctx, &ltmp, &rtmp);

    ctx->backend_data->stack_frame_size += 4; /* Account for register... */
    ctx->backend_data->regs[regno].spilled = true;
    ctx->backend_data->regs[regno].used = false;
}

/* Spill registers into the stack/memory, num for the number of registers
   to make available */
void cc_backend_spill(cc_context* ctx, unsigned int num)
{
    /* Check if we even need to spill at all */
    unsigned int usable_cnt = 0;
    for (size_t i = 0; i < ctx->backend_data->n_regs; i++)
        if (!ctx->backend_data->is_reserved(i)
            && !ctx->backend_data->regs[i].used)
            usable_cnt++;
    if (usable_cnt >= num)
        return;

    for (size_t i = 0; i < ctx->backend_data->n_regs; i++)
        if (!ctx->backend_data->is_reserved(i)
            && ctx->backend_data->regs[i].used)
            cc_backend_spill_reg(ctx, i);
}

void cc_backend_unspill_reg(cc_context* ctx, unsigned int regno)
{
    cc_backend_varmap ltmp = { 0 }, rtmp = { 0 };
    ltmp.flags = VARMAP_REGISTER;
    ltmp.regno = regno;
    rtmp.flags = VARMAP_STACK;
    rtmp.offset = ctx->backend_data->stack_frame_size;

    ctx->backend_data->gen_mov(ctx, &ltmp, &rtmp);

    ctx->backend_data->stack_frame_size += 4; /* Account for register... */
    ctx->backend_data->regs[regno].spilled = false;
    cc_backend_free_register(ctx, regno);
}

/* Unspill all registers */
void cc_backend_unspill(cc_context* ctx)
{
    for (size_t i = 0; i < ctx->backend_data->n_regs; i++)
        if (!ctx->backend_data->is_reserved(i)
            && !ctx->backend_data->regs[i].used
            && ctx->backend_data->regs[i].spilled)
            cc_backend_unspill_reg(ctx, i);
}

void cc_backend_reserve_reg(cc_context* ctx, unsigned int regno)
{
    if (ctx->backend_data->regs[regno].used)
        cc_backend_spill_reg(ctx, regno);
    ctx->backend_data->regs[regno].used = true;
}

unsigned int cc_backend_alloc_register(cc_context* ctx)
{
    _Bool spilled = false;
try_again:
    for (size_t i = 0; i < ctx->backend_data->n_regs; i++) {
        if (!ctx->backend_data->is_reserved(i)
            && !ctx->backend_data->regs[i].used) {
            ctx->backend_data->regs[i].used = true;
            return i;
        }
    }

    cc_backend_spill(ctx, 1);
    if (!spilled) {
        spilled = true;
        goto try_again;
    }
    return -1;
}

void cc_backend_free_register(cc_context* ctx, int regno)
{
    ctx->backend_data->regs[regno].used = false;
}

void cc_backend_add_varmap(cc_context* ctx, const cc_ast_variable* restrict var)
{
    ctx->backend_data->varmaps = cc_realloc(ctx->backend_data->varmaps,
        sizeof(cc_backend_varmap) * (ctx->backend_data->n_varmaps + 1));
    cc_backend_varmap* vmap
        = &ctx->backend_data->varmaps[ctx->backend_data->n_varmaps++];

    /* Populate variable mapping */
    vmap->var = var;
    vmap->depth = ctx->backend_data->varmap_depth;
    if (var->type.storage == STORAGE_THREAD_LOCAL) {
        vmap->flags = VARMAP_THREAD_LOCAL;
    } else if (var->type.storage == STORAGE_STATIC
        || var->type.storage == STORAGE_GLOBAL) {
        vmap->flags = VARMAP_STATIC;
    } else {
        vmap->flags = VARMAP_STACK;
        /* Adjust location on stack */
        if (vmap->flags == VARMAP_STACK) {
            ctx->backend_data->stack_frame_size
                += ctx->backend_data->get_sizeof(ctx, &var->type);
            vmap->offset = ctx->backend_data->stack_frame_size;
        }
    }
}

void cc_backend_increment_varmap_depth(cc_context* ctx)
{
    ctx->backend_data->varmap_depth++;
}

void cc_backend_decrement_varmap_depth(cc_context* ctx)
{
    assert(ctx->backend_data->varmap_depth > 0);
    ctx->backend_data->varmap_depth--;
    /* Remove out-of-scope varmaps */
    for (size_t i = 0; i < ctx->backend_data->n_varmaps; i++) {
        cc_backend_varmap* vmap = &ctx->backend_data->varmaps[i];
        if (vmap->depth > ctx->backend_data->varmap_depth)
            memmove(&ctx->backend_data->varmaps[i],
                &ctx->backend_data->varmaps[i + 1],
                sizeof(*vmap) * (ctx->backend_data->n_varmaps - i - 1));
    }
}

cc_backend_varmap* cc_backend_find_var_varmap(
    cc_context* ctx, const cc_ast_variable* restrict var)
{
    for (size_t i = 0; i < ctx->backend_data->n_varmaps; i++) {
        cc_backend_varmap* vmap = &ctx->backend_data->varmaps[i];
        if (vmap->var->name != NULL && !strcmp(vmap->var->name, var->name))
            return vmap;
    }

    cc_diag_error(ctx, "No mapping for variable '%s'", var->name);
    return NULL;
}

/* Obtain the variable mapping for the given node */
cc_backend_varmap cc_backend_get_node_varmap(
    cc_context* ctx, const cc_ast_node* node)
{
    cc_backend_varmap vmap = { 0 };
    switch (node->type) {
    case AST_NODE_VARIABLE: {
        const cc_ast_variable* var
            = cc_ast_find_variable(node->data.var.name, node);
        return *cc_backend_find_var_varmap(ctx, var);
    } break;
    case AST_NODE_LITERAL:
        vmap.flags = VARMAP_CONSTANT;
        vmap.constant = node->data.literal.value.u;
        return vmap;
    case AST_NODE_STRING_LITERAL:
        vmap.flags = VARMAP_LITERAL;
        vmap.data = cc_strdup(node->data.string_literal.data);
        vmap.n_data = strlen(vmap.data + 1);
        return vmap;
    case AST_NODE_CALL:
        return ctx->backend_data->get_call_retval(ctx, node);
    case AST_NODE_BINOP: /* Allocate register for result of operation */
    case AST_NODE_UNOP:
        cc_backend_spill(ctx, 1);
        vmap.regno = cc_backend_alloc_register(ctx);
        vmap.flags = VARMAP_REGISTER;
        return vmap;
    case AST_NODE_BLOCK:
        return vmap;
    default:
        cc_diag_error(ctx, "Unrecognizable AST node %i", node->type);
        break;
    }
    return vmap;
}

/* Dump all the statics of this function, do not enter subfunctions/lambdas
   because they are treated as separate (albeit inlined) functions. */
void cc_backend_map_variables(cc_context* ctx, const cc_ast_node* node)
{
    if (node == NULL)
        return;
    switch (node->type) {
    case AST_NODE_UNOP:
        cc_backend_map_variables(ctx, node->data.unop.child);
        break;
    case AST_NODE_BINOP:
        cc_backend_map_variables(ctx, node->data.binop.left);
        cc_backend_map_variables(ctx, node->data.binop.right);
        break;
    case AST_NODE_BLOCK:
        for (size_t i = 0; i < node->data.block.n_vars; i++)
            ctx->backend_data->map_variable(ctx, &node->data.block.vars[i]);
        for (size_t i = 0; i < node->data.block.n_children; i++)
            cc_backend_map_variables(ctx, &node->data.block.children[i]);
        break;
    case AST_NODE_CALL: {
        cc_backend_map_variables(ctx, node->data.call.call_expr);
    } break;
    case AST_NODE_RETURN:
        cc_backend_map_variables(ctx, node->data.return_expr.value);
        break;
    case AST_NODE_IF:
        cc_backend_map_variables(ctx, node->data.if_expr.cond);
        cc_backend_map_variables(ctx, node->data.if_expr.block);
        cc_backend_map_variables(ctx, node->data.if_expr.tail_else);
        break;
    default: /* Skip the rest... */
        break;
    }
}

static void cc_backend_process_function(
    cc_context* ctx, const cc_ast_variable* var)
{
    assert(var->type.mode == TYPE_MODE_FUNCTION);

    cc_backend_increment_varmap_depth(ctx);
    ctx->backend_data->stack_frame_size = 0;
    for (size_t i = 0; i < var->type.data.func.n_params; i++) {
        const cc_ast_variable* param = &var->type.data.func.params[i];
        ctx->backend_data->stack_frame_size
            += ctx->backend_data->get_sizeof(ctx, &param->type);
        cc_backend_add_varmap(ctx, param);
    }

    const cc_ast_variable* old_current_func_var
        = ctx->backend_data->current_func_var;
    ctx->backend_data->current_func_var = var;
    if (var->body != NULL)
        cc_backend_map_variables(ctx, var->body);
    ctx->backend_data->map_variable(ctx, var);

    if (var->body != NULL) {
        if (var->body->type == AST_NODE_BLOCK) {
            for (size_t i = 0; i < var->body->data.block.n_vars; i++) {
                const cc_ast_variable* bvar = &var->body->data.block.vars[i];
                /* Automatically place on stack ^-^ */
                if (bvar->type.storage == STORAGE_AUTO)
                    ctx->backend_data->stack_frame_size
                        += ctx->backend_data->get_sizeof(ctx, &bvar->type);
                cc_backend_add_varmap(ctx, bvar);
            }
        }

        ctx->backend_data->gen_epilogue(ctx, var->body, var);
        cc_backend_process_node(ctx, var->body, NULL);
    }
    ctx->backend_data->current_func_var = old_current_func_var;
    cc_backend_decrement_varmap_depth(ctx);
}

static void cc_backend_process_call(cc_context* ctx, const cc_ast_node* node)
{
    unsigned int total_stack = 0;
    for (size_t i = 0; i < node->data.call.n_params; i++) {
        const cc_ast_node* param_node = &node->data.call.params[i];
        cc_ast_type param_type = { 0 };
        cc_ceval_deduce_type(ctx, param_node, &param_type);

        cc_backend_varmap pvmap = cc_backend_get_node_varmap(ctx, param_node);

        cc_backend_varmap svmap = { 0 };
        svmap.flags = VARMAP_STACK;
        svmap.offset = ctx->backend_data->stack_frame_size + total_stack;
        ctx->backend_data->gen_mov(ctx, &svmap, &pvmap);

        total_stack += ctx->backend_data->get_sizeof(ctx, &param_type);
    }
    if (node->data.call.call_expr == NULL)
        return;
    ctx->backend_data->gen_call(ctx, node->data.call.call_expr);
}

static void cc_backend_process_unop(
    cc_context* ctx, const cc_ast_node* node, const cc_backend_varmap* ovmap)
{
    assert(node->type == AST_NODE_UNOP);
    const cc_ast_node* child = node->data.unop.child;

    /*fprintf(ctx->out, "#backend-gen-unop\n");*/
    if (node->data.unop.op == AST_UNOP_POSTDEC
        || node->data.unop.op == AST_UNOP_POSTINC
        || node->data.unop.op == AST_UNOP_PREDEC
        || node->data.unop.op == AST_UNOP_PREINC) {
        /*fprintf(ctx->out, "#todo-properly-do-post/prefix-inc/dec\n");*/
        return;
    }

    cc_backend_varmap rvmap = cc_backend_get_node_varmap(ctx, child);
    cc_backend_varmap lvmap = *ovmap;
    ctx->backend_data->gen_unop(ctx, &lvmap, &rvmap, node->data.unop.op);
}

static void cc_backend_process_binop(
    cc_context* ctx, const cc_ast_node* node, const cc_backend_varmap* ovmap)
{
    assert(node->type == AST_NODE_BINOP);

    const cc_ast_node* lhs = node->data.binop.left;
    const cc_ast_node* rhs = node->data.binop.right;

    if (node->data.binop.op == AST_BINOP_DOT) {
        cc_ast_type type = { 0 };
        if (!cc_ceval_deduce_type(ctx, node->data.binop.left, &type)) {
            cc_diag_error(ctx, "Unable to deduce type");
            return;
        }
        return;
    }

    cc_backend_varmap lvmap = cc_backend_get_node_varmap(ctx, lhs);
    cc_backend_varmap rvmap = cc_backend_get_node_varmap(ctx, rhs);

    if (node->data.binop.op == AST_BINOP_ASSIGN) {
        if (lhs->type == AST_NODE_VARIABLE) {
            ctx->backend_data->gen_mov(ctx, &lvmap, &rvmap);
        } else if (lhs->type == AST_NODE_BINOP) { /* Computed address */
            cc_backend_process_binop(ctx, lhs, &lvmap);
        } else if (lhs->type == AST_NODE_UNOP) {
            cc_backend_process_unop(ctx, lhs, &lvmap);
        } else if (lhs->type == AST_NODE_BLOCK) {
            /* TODO: Process block */
        } else {
            cc_diag_error(ctx, "Unknown assignment LHS %i", lhs->type);
            return;
        }
        return;
    }

    if (rhs->type == AST_NODE_BINOP)
        cc_backend_process_binop(ctx, rhs, &rvmap);
    else if (rhs->type == AST_NODE_UNOP)
        cc_backend_process_unop(ctx, rhs, &rvmap);

    if (lhs->type == AST_NODE_BINOP)
        cc_backend_process_binop(ctx, lhs, &lvmap);
    else if (lhs->type == AST_NODE_UNOP)
        cc_backend_process_unop(ctx, lhs, &lvmap);

    /*fprintf(ctx->out, "#backend-gen-binop\n");*/

    if (!ctx->backend_data->gen_binop(
            ctx, &lvmap, &rvmap, node->data.binop.op)) {
        cc_backend_spill_reg(ctx, 2);
        cc_backend_varmap nrvmap = { 0 };
        nrvmap.regno = cc_backend_alloc_register(ctx);
        nrvmap.flags = VARMAP_REGISTER;
        ctx->backend_data->gen_mov(ctx, &nrvmap, &rvmap);
        if (!ctx->backend_data->gen_binop(
                ctx, &lvmap, &nrvmap, node->data.binop.op)) {
            cc_backend_varmap nlvmap = { 0 };
            nlvmap.regno = cc_backend_alloc_register(ctx);
            nlvmap.flags = VARMAP_REGISTER;
            ctx->backend_data->gen_mov(ctx, &nlvmap, &lvmap);
            if (!ctx->backend_data->gen_binop(
                    ctx, &nlvmap, &nrvmap, node->data.binop.op)) {
                cc_diag_error(ctx, "Impossible to reload\n");
                return;
            }
            ctx->backend_data->gen_mov(ctx, &lvmap, &nlvmap);
            cc_backend_free_register(ctx, nlvmap.regno);
            lvmap = nlvmap;
        }
        rvmap = nrvmap;
        cc_backend_free_register(ctx, nrvmap.regno);
    }

    if (ovmap != NULL)
        ctx->backend_data->gen_mov(ctx, ovmap, &lvmap);
}

static void cc_backend_process_if(
    cc_context* ctx, const cc_ast_node* node, const cc_backend_varmap* ovmap)
{
    assert(node->type == AST_NODE_IF);
    cc_backend_spill(ctx, 1);
    cc_backend_varmap rvmap = { 0 };
    rvmap.flags = VARMAP_REGISTER;
    rvmap.regno = cc_backend_alloc_register(ctx);
    cc_backend_process_node(ctx, node->data.if_expr.cond, &rvmap);

    cc_backend_varmap lvmap = { 0 };
    lvmap.flags = VARMAP_CONSTANT;
    lvmap.constant = 1;
    ctx->backend_data->gen_branch(
        ctx, node->data.if_expr.block, &lvmap, &rvmap, AST_BINOP_COND_EQ);
    if (node->data.if_expr.tail_else) {
        ctx->backend_data->gen_branch(ctx, node->data.if_expr.tail_else, &lvmap,
            &rvmap, AST_BINOP_COND_NEQ);
    } else {
        assert(node->parent != NULL);
        ctx->backend_data->gen_jump(ctx, node->parent);
    }
    cc_backend_free_register(ctx, rvmap.regno);

    printf("\nBLOCK\n");
    cc_ast_print(node->data.if_expr.block, 0);
    printf("\n");
    
    cc_backend_process_node(ctx, node->data.if_expr.block, ovmap);
    cc_backend_process_node(ctx, node->data.if_expr.tail_else, ovmap);
}

static const cc_ast_node** cc_ast_collect_cases(
    const cc_ast_node* node, const cc_ast_node*** list, size_t* n_list)
{
    if (node == NULL)
        return *list;
    switch (node->type) {
    case AST_NODE_BINOP:
        cc_ast_collect_cases(node->data.binop.left, list, n_list);
        cc_ast_collect_cases(node->data.binop.right, list, n_list);
        break;
    case AST_NODE_BLOCK:
        if (node->data.block.is_case) {
            (*list) = cc_realloc(*list, sizeof(*list) * (*n_list + 1));
            (*list)[(*n_list)++] = node;
        }

        for (size_t i = 0; i < node->data.block.n_vars; i++) {
            cc_ast_variable* var = &node->data.block.vars[i];
            cc_ast_collect_cases(var->body, list, n_list);
        }
        for (size_t i = 0; i < node->data.block.n_children; i++)
            cc_ast_collect_cases(&node->data.block.children[i], list, n_list);
        break;
    case AST_NODE_CALL:
        cc_ast_collect_cases(node->data.call.call_expr, list, n_list);
        for (size_t i = 0; i < node->data.call.n_params; i++)
            cc_ast_collect_cases(&node->data.call.params[i], list, n_list);
        break;
    case AST_NODE_IF:
        cc_ast_collect_cases(node->data.if_expr.cond, list, n_list);
        cc_ast_collect_cases(node->data.if_expr.block, list, n_list);
        cc_ast_collect_cases(node->data.if_expr.tail_else, list, n_list);
        break;
    case AST_NODE_RETURN:
        cc_ast_collect_cases(node->data.return_expr.value, list, n_list);
        break;
    case AST_NODE_UNOP:
        cc_ast_collect_cases(node->data.unop.child, list, n_list);
        break;
    case AST_NODE_JUMP:
    case AST_NODE_STRING_LITERAL:
    case AST_NODE_LITERAL:
    case AST_NODE_VARIABLE:
        break;
    default:
        break;
    }
    return *list;
}

void cc_backend_process_node(
    cc_context* ctx, const cc_ast_node* node, cc_backend_varmap* ovmap)
{
    if (node == NULL)
        return;

    ctx->diag_node = node;
    switch (node->type) {
    case AST_NODE_UNOP:
        cc_backend_process_unop(ctx, node, ovmap);
        break;
    case AST_NODE_BINOP:
        cc_backend_process_binop(ctx, node, ovmap);
        break;
    case AST_NODE_BLOCK:
        cc_backend_increment_varmap_depth(ctx);
        for (size_t i = 0; i < node->data.block.n_vars; i++) {
            const cc_ast_variable* var = &node->data.block.vars[i];
            cc_backend_add_varmap(ctx, var);
            if (var->type.mode != TYPE_MODE_FUNCTION)
                ctx->backend_data->map_variable(ctx, var);
        }

        for (size_t i = 0; i < node->data.block.n_vars; i++) {
            const cc_ast_variable* var = &node->data.block.vars[i];
            if (var->type.mode == TYPE_MODE_FUNCTION)
                cc_backend_process_function(ctx, var);
        }

        for (size_t i = 0; i < node->data.block.n_children; i++) {
            const cc_ast_node* child = &node->data.block.children[i];
            const cc_ast_node* old_outermost_stmt
                = ctx->backend_data->outermost_stmt;
            ctx->backend_data->outermost_stmt = child;
            cc_backend_process_node(ctx, child, ovmap);
            ctx->backend_data->outermost_stmt = old_outermost_stmt;
        }
        cc_backend_decrement_varmap_depth(ctx);
        break;
    case AST_NODE_CALL:
        cc_backend_process_call(ctx, node);
        break;
    case AST_NODE_RETURN:
        ctx->backend_data->gen_prologue(ctx, node->data.return_expr.value,
            ctx->backend_data->current_func_var);
        break;
    case AST_NODE_IF:
        cc_backend_process_if(ctx, node, ovmap);
        break;
    case AST_NODE_JUMP: {
        cc_ast_node* fnode
            = cc_ast_find_label_id(ctx, ctx->root, node->data.jump.label_id);
        if (fnode == NULL) {
            cc_diag_error(ctx, "Jump label is out of reach");
            break;
        }
        ctx->backend_data->gen_jump(ctx, fnode);
    } break;
    case AST_NODE_SWITCH: {
        /* We only evalaute the switch control expression once */
        cc_backend_varmap vmap = { 0 };
        vmap.regno = cc_backend_alloc_register(ctx);
        vmap.flags = VARMAP_REGISTER;
        cc_backend_process_node(ctx, node->data.switch_expr.control, &vmap);

        const cc_ast_node** list = NULL;
        size_t n_list = 0;
        cc_ast_collect_cases(node->data.switch_expr.block, &list, &n_list);

        const cc_ast_node* default_node = NULL;
        for (size_t i = 0; i < n_list; i++) {
            assert(list[i]->data.block.is_case);
            if (list[i]->data.block.is_default) {
                default_node = list[i];
            } else {
                cc_backend_varmap onevmap = { 0 };
                onevmap.flags = VARMAP_CONSTANT;
                onevmap.constant = list[i]->data.block.case_val;
                ctx->backend_data->gen_branch(
                    ctx, list[i], &vmap, &onevmap, AST_BINOP_COND_EQ);
            }
        }
        if (default_node != NULL)
            ctx->backend_data->gen_jump(ctx, default_node);
        cc_free(list);

        cc_backend_process_node(ctx, node->data.switch_expr.block, &vmap);
    } break;
    default:
        if (ovmap == NULL) {
            cc_ast_print(node, 0);
            cc_diag_error(ctx, "Node without specified varmap for output");
            break;
        }
        cc_backend_varmap vvmap = cc_backend_get_node_varmap(ctx, node);
        ctx->backend_data->gen_mov(ctx, ovmap, &vvmap);
        break;
    }
}

void cc_backend_init(
    cc_context* ctx, const char* reg_names[], unsigned int n_regs)
{
    ctx->backend_data = cc_zalloc(sizeof(cc_backend_context));
    ctx->backend_data->label_num = cc_ast_alloc_label_id();

    ctx->backend_data->reg_names = reg_names;
    ctx->backend_data->regs = cc_zalloc(sizeof(cc_backend_reginfo) * n_regs);
    ctx->backend_data->n_regs = n_regs;
}

void cc_backend_deinit(cc_context* ctx)
{
    ctx->backend_data->deinit(ctx);
    cc_free(ctx->backend_data->varmaps);
    cc_free(ctx->backend_data);
}
