/* ast.c - Abstract Syntax Tree utility functions */
#include "ast.h"
#include "context.h"
#include "lexer.h"
#include "optzer.h"
#include "util.h"
#include <assert.h>
#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Prevent infinite recursion for printing functions */
static int body_print_lock = 0;

unsigned short cc_ast_alloc_label_id(cc_context* ctx)
{
    if ((ctx->label_id + 1) >= USHRT_MAX)
        cc_diag_warning(ctx, "Ran out of labels to assign");
    return ctx->label_id++;
}

cc_ast_node* cc_ast_create_any(
    cc_context* ctx, cc_ast_node* parent, enum cc_ast_node_type type)
{
    cc_ast_node* node = cc_zalloc(sizeof(cc_ast_node));
    node->parent = parent;
    node->type = type;
    node->label_id = cc_ast_alloc_label_id(ctx);

    node->info = ctx->tokens[ctx->c_token].info;
    node->info.filename = cc_strdup(ctx->tokens[ctx->c_token].info.filename);
    return node;
}

cc_ast_node* cc_ast_create_block(cc_context* ctx, cc_ast_node* parent)
{
    return cc_ast_create_any(ctx, parent, AST_NODE_BLOCK);
}

cc_ast_node* cc_ast_create_binop_expr(
    cc_context* ctx, cc_ast_node* parent, enum cc_ast_binop_type type)
{
    cc_ast_node* node = cc_ast_create_any(ctx, parent, AST_NODE_BINOP);
    node->data.binop.op = type;
    node->data.binop.left = cc_ast_create_block(ctx, node);
    node->data.binop.right = cc_ast_create_block(ctx, node);
    return node;
}

cc_ast_node* cc_ast_create_unop_expr(
    cc_context* ctx, cc_ast_node* parent, enum cc_ast_unop_type type)
{
    cc_ast_node* node = cc_ast_create_any(ctx, parent, AST_NODE_UNOP);
    node->data.unop.op = type;
    node->data.unop.child = cc_ast_create_block(ctx, node);
    return node;
}

cc_ast_node* cc_ast_create_if_expr(cc_context* ctx, cc_ast_node* parent)
{
    cc_ast_node* node = cc_ast_create_any(ctx, parent, AST_NODE_IF);
    node->data.if_expr.block = cc_ast_create_block(ctx, node);
    node->data.if_expr.cond = cc_ast_create_block(ctx, node);
    node->data.if_expr.tail_else = cc_ast_create_block(ctx, node);
    return node;
}

cc_ast_node* cc_ast_create_field_access(cc_context* ctx, cc_ast_node* parent,
    const char *field_name)
{
    cc_ast_node* node = cc_ast_create_any(ctx, parent, AST_NODE_FIELD_ACCESS);
    node->data.field_access.left = cc_ast_create_block(ctx, node);
    node->data.field_access.field_name = cc_strdup(field_name);
    return node;
}

cc_ast_node* cc_ast_create_switch_expr(cc_context* ctx, cc_ast_node* parent)
{
    cc_ast_node* node = cc_ast_create_any(ctx, parent, AST_NODE_SWITCH);
    node->data.switch_expr.control = cc_ast_create_block(ctx, node);
    node->data.switch_expr.block = cc_ast_create_block(ctx, node);
    return node;
}

cc_ast_node* cc_ast_create_ret_expr(cc_context* ctx, cc_ast_node* parent)
{
    cc_ast_node* node = cc_ast_create_any(ctx, parent, AST_NODE_RETURN);
    node->data.return_expr = cc_ast_create_block(ctx, node);
    return node;
}

cc_ast_node* cc_ast_create_var_ref(
    cc_context* ctx, cc_ast_node* parent, const cc_ast_variable* var)
{
    cc_ast_node* node = cc_ast_create_any(ctx, parent, AST_NODE_VARIABLE);
    node->data.var.name = cc_strdup(var->name);
    return node;
}

cc_ast_node* cc_ast_create_call(cc_context* ctx, cc_ast_node* parent)
{
    cc_ast_node* node = cc_ast_create_any(ctx, parent, AST_NODE_CALL);
    node->data.call.call_expr = cc_ast_create_block(ctx, node);
    return node;
}

cc_ast_node* cc_ast_create_string_literal(
    cc_context* ctx, cc_ast_node* parent, const char* s)
{
    cc_ast_node* node = cc_ast_create_any(ctx, parent, AST_NODE_STRING_LITERAL);
    node->data.string_literal = cc_strdup(s);
    return node;
}

cc_ast_node* cc_ast_create_literal_from_str(
    cc_context* ctx, cc_ast_node* parent, const char* s)
{
    cc_ast_node* node = cc_ast_create_any(ctx, parent, AST_NODE_LITERAL);
    cc_ast_literal* literal = &node->data.literal;
    if (s[0] == '0' && s[1] == '\0') {
        literal->value.u = 0;
        literal->is_signed = ctx->is_default_signed;
        literal->is_float = false;
    } else if (strchr(s, '.') != NULL) {
        literal->value.d = strtod(s, NULL);
        literal->is_signed = false;
        literal->is_float = true;
    } else if (s[0] == '0') {
        int base = 8;
        switch (s[1]) {
        case 'o':
            base = 8;
            break;
        case 'd':
            base = 10;
            break;
        case 'b':
            base = 2;
            break;
        case 'x':
        case 'h':
            base = 16;
            break;
        default:
            abort();
        }
        literal->value.u = strtoul(&s[2], NULL, base);
        literal->is_signed = ctx->is_default_signed;
        literal->is_float = false;
    } else {
        literal->value.u = strtoul(s, NULL, 10);
        literal->is_signed = ctx->is_default_signed;
        literal->is_float = false;
    }
    return node;
}

cc_ast_node* cc_ast_create_literal(
    cc_context* ctx, cc_ast_node* parent, cc_ast_literal literal)
{
    cc_ast_node* node = cc_ast_create_any(ctx, parent, AST_NODE_LITERAL);
    node->data.literal = literal;
    return node;
}

cc_ast_node* cc_ast_create_jump(
    cc_context* ctx, cc_ast_node* parent, cc_ast_node* target)
{
    cc_ast_node* node = cc_ast_create_any(ctx, parent, AST_NODE_JUMP);
    node->data.jump_label_id = target->label_id;
    assert(target->ref_count < USHRT_MAX);
    target->ref_count++;
    return node;
}

void cc_ast_add_block_node(
    cc_ast_node* restrict block, const cc_ast_node* restrict child)
{
    assert(block != NULL && block->type == AST_NODE_BLOCK);
    assert(child != NULL && child->parent == block);
    assert(block->type == AST_NODE_NONE
        || block->type == AST_NODE_BINOP
        || block->type == AST_NODE_UNOP
        || block->type == AST_NODE_BLOCK
        || block->type == AST_NODE_JUMP
        || block->type == AST_NODE_IF
        || block->type == AST_NODE_RETURN
        || block->type == AST_NODE_CALL
        || block->type == AST_NODE_VARIABLE
        || block->type == AST_NODE_LITERAL
        || block->type == AST_NODE_STRING_LITERAL
        || block->type == AST_NODE_SWITCH
        || block->type == AST_NODE_REGISTER
        || block->type == AST_NODE_FIELD_ACCESS);
    block->data.block.children = cc_realloc_array(
        block->data.block.children, block->data.block.n_children + 1);
    block->data.block.children[block->data.block.n_children++] = *child;
}

void cc_ast_add_block_type(cc_ast_node* block, const cc_ast_type* type)
{
    assert(block != NULL && block->type == AST_NODE_BLOCK);
    assert(type->mode != AST_TYPE_MODE_NONE);
    assert(type->name != NULL);
    block->data.block.types = cc_realloc_array(
        block->data.block.types, block->data.block.n_types + 1);
    block->data.block.types[block->data.block.n_types++] = *type;
}

/* Remove a node from a block, management of the node itself is responsabili-
   -ty of the caller not us */
void cc_ast_remove_block_node(cc_ast_node* block, size_t i)
{
    assert(block != NULL && block->type == AST_NODE_BLOCK);
    assert(i < block->data.block.n_children);
    assert(block->data.block.children[i].ref_count == 0);
    memmove(&block->data.block.children[i], &block->data.block.children[i + 1],
        sizeof(cc_ast_node) * (block->data.block.n_children - i - 1));
    block->data.block.n_children--;
}

void cc_ast_add_block_variable(cc_ast_node* block, const cc_ast_variable* var)
{
    size_t i;
    assert(block->type == AST_NODE_BLOCK);
    assert(var->name != NULL);

    for (i = 0; i < block->data.block.n_vars; i++) {
        cc_ast_variable* bvar = &block->data.block.vars[i];
        if (!strcmp(bvar->name, var->name)) {
            /* TODO: Check that the declaration is the same */
            cc_ast_destroy_var(bvar, false);
            *bvar = *var;
            return;
        }
    }

    block->data.block.vars = cc_realloc_array(
        block->data.block.vars, block->data.block.n_vars + 1);
    block->data.block.vars[block->data.block.n_vars++] = *var;
}

void cc_ast_add_call_param(
    cc_ast_node* restrict call, const cc_ast_node* restrict param)
{
    assert(param->parent == call);
    call->data.call.params = cc_realloc_array(
        call->data.call.params, call->data.call.n_params + 1);
    call->data.call.params[call->data.call.n_params++] = *param;
}

void cc_ast_destroy_type(cc_ast_type* type, bool managed)
{
    if (type == NULL)
        return;
    switch (type->mode) {
    case AST_TYPE_MODE_FUNCTION:
        if (type->data.func.params != NULL) {
            size_t i;
            for (i = 0; i < type->data.func.n_params; i++) {
                cc_ast_destroy_type(&type->data.func.params[i].type, false);
                cc_strfree(type->data.func.params[i].name);
            }
            cc_free(type->data.func.params);
        }

        if (type->data.func.return_type != NULL)
            cc_ast_destroy_type(type->data.func.return_type, true);
        break;
    default:
        break;
    }
    if (managed)
        cc_free(type);
}

void cc_ast_destroy_var(cc_ast_variable* var, bool managed)
{
    assert(var != NULL);
    cc_ast_destroy_type(&var->type, false);
    /*cc_free(var->name);*/
    if (managed)
        cc_free(var);
}

void cc_ast_destroy_node(cc_ast_node* node, bool managed)
{
    if (node == NULL)
        return;

    assert(node->parent != node && node->ref_count == 0);
    switch (node->type) {
    case AST_NODE_STRING_LITERAL:
        cc_free(node->data.string_literal);
        break;
    case AST_NODE_BINOP:
        cc_ast_destroy_node(node->data.binop.left, true);
        cc_ast_destroy_node(node->data.binop.right, true);
        break;
    case AST_NODE_CALL: {
        size_t i;
        cc_ast_destroy_node(node->data.call.call_expr, true);
        for (i = 0; i < node->data.call.n_params; i++)
            cc_ast_destroy_node(&node->data.call.params[i], false);
        cc_free(node->data.call.params);
    } break;
    case AST_NODE_BLOCK: {
        size_t i;
        for (i = 0; i < node->data.block.n_vars; i++)
            cc_ast_destroy_var(&node->data.block.vars[i], false);
        cc_free(node->data.block.vars);
        for (i = 0; i < node->data.block.n_children; i++)
            cc_ast_destroy_node(&node->data.block.children[i], false);
        cc_free(node->data.block.children);
    } break;
    case AST_NODE_RETURN:
        cc_ast_destroy_node(node->data.return_expr, true);
        break;
    case AST_NODE_VARIABLE:
        cc_strfree(node->data.var.name);
        break;
    default:
        break;
    }

    if (managed)
        cc_free(node);
}

/* TODO: This finding algorithm is fucking cursed, we need something better! */
static cc_ast_variable* cc_ast_find_variable_1(
    const char* fn_name, const char* name, const cc_ast_node* node)
{
    if (node == NULL)
        return NULL;

    if (node->type == AST_NODE_BLOCK) {
        size_t i;
        for (i = 0; i < node->data.block.n_vars; i++) {
            cc_ast_variable* var = &node->data.block.vars[i];
            if (var->name != NULL && !strcmp(var->name, name))
                return var;

            /* Check parameters for functions but only the current function
               we're checking against. */
            if (fn_name != NULL && var->type.mode == AST_TYPE_MODE_FUNCTION
                && !strcmp(var->name, fn_name)) {
                size_t j;
                for (j = 0; j < var->type.data.func.n_params; j++) {
                    cc_ast_variable* param = &var->type.data.func.params[j];
                    /* Unnamed parameters are supported and valid */
                    if (param->name == NULL)
                        continue;

                    if (!strcmp(param->name, name))
                        return param;
                }
            }
        }
    }
    return cc_ast_find_variable_1(fn_name, name, node->parent);
}
cc_ast_variable* cc_ast_find_variable(
    const char* fn_name, const char* name, const cc_ast_node* node)
{
    return cc_ast_find_variable_1(fn_name, name, node);
}

cc_ast_node* cc_ast_find_label(const char* name, const cc_ast_node* node)
{
    if (node == NULL)
        return NULL;
    if (node->type == AST_NODE_BLOCK) {
        size_t i;
        for (i = 0; i < node->data.block.n_children; i++) {
            cc_ast_node* bnode = &node->data.block.children[i];
            if ((bnode = cc_ast_find_label(name, bnode)) != NULL)
                return bnode;
        }

        for (i = 0; i < node->data.block.n_vars; i++) {
            cc_ast_variable* var = &node->data.block.vars[i];
            /* Check parameters for functions */
            if (var->type.mode == AST_TYPE_MODE_FUNCTION) {
                cc_ast_node* fnode = cc_ast_find_label(name, var->body);
                if (fnode != NULL)
                    return fnode;
            }
        }
    } else if (node->type == AST_NODE_SWITCH) {
        cc_ast_node* bnode;
        if ((bnode = cc_ast_find_label(name, node->data.switch_expr.control))
            != NULL)
            return bnode;
        if ((bnode = cc_ast_find_label(name, node->data.switch_expr.block))
            != NULL)
            return bnode;
    }
    return cc_ast_find_label(name, node->parent);
}

cc_ast_type* cc_ast_find_typedef(const char* name, cc_ast_node* node)
{
    if (node == NULL)
        return NULL;
    if (node->type == AST_NODE_BLOCK) {
        size_t i;
        for (i = 0; i < node->data.block.n_types; i++) {
            cc_ast_type* type = &node->data.block.types[i];
            if (type->is_typedef && !strcmp(type->name, name))
                return type;
        }
    }
    return cc_ast_find_typedef(name, node->parent);
}

cc_ast_type* cc_ast_find_type(const char* name, cc_ast_node* node)
{
    if (node == NULL)
        return NULL;
    if (node->type == AST_NODE_BLOCK) {
        size_t i;
        for (i = 0; i < node->data.block.n_types; i++) {
            cc_ast_type* type = &node->data.block.types[i];
            if (!strcmp(type->name, name))
                return type;
        }
    }
    return cc_ast_find_type(name, node->parent);
}

static cc_ast_node* cc_ast_create_generic(
    cc_context* ctx, cc_ast_node* parent, enum cc_ast_node_type type)
{
    switch (type) {
    case AST_NODE_BINOP:
        return cc_ast_create_binop_expr(ctx, parent, AST_BINOP_NONE);
    case AST_NODE_BLOCK:
        return cc_ast_create_block(ctx, parent);
    case AST_NODE_CALL:
        return cc_ast_create_call(ctx, parent);
    case AST_NODE_FIELD_ACCESS:
        return cc_ast_create_any(ctx, parent, AST_NODE_FIELD_ACCESS);
    case AST_NODE_IF:
        return cc_ast_create_if_expr(ctx, parent);
    case AST_NODE_JUMP:
        return cc_ast_create_jump(ctx, parent, NULL);
    case AST_NODE_LITERAL: {
        cc_ast_literal l = { 0 };
        return cc_ast_create_literal(ctx, parent, l);
    }
    case AST_NODE_RETURN:
        return cc_ast_create_ret_expr(ctx, parent);
    case AST_NODE_STRING_LITERAL:
        return cc_ast_create_any(ctx, parent, AST_NODE_STRING_LITERAL);
    case AST_NODE_SWITCH:
        return cc_ast_create_switch_expr(ctx, parent);
    case AST_NODE_UNOP:
        return cc_ast_create_unop_expr(ctx, parent, AST_UNOP_NONE);
    case AST_NODE_VARIABLE:
        return cc_ast_create_any(ctx, parent, AST_NODE_VARIABLE);
    default:
        break;
    }
    return NULL;
}

void cc_ast_copy_node(cc_context* ctx, cc_ast_node* restrict dest,
    const cc_ast_node* restrict src)
{
    if (dest == NULL || src == NULL)
        return;
    assert(dest->type == src->type);
    dest->parent = src->parent;
    /* Label-Id is unique to every node regardless of copying */
    switch (src->type) {
    case AST_NODE_VARIABLE:
        dest->data.var.name = cc_strdup(src->data.var.name);
        break;
    case AST_NODE_LITERAL:
        dest->data.literal = src->data.literal;
        break;
    case AST_NODE_STRING_LITERAL:
        dest->data.string_literal = cc_strdup(src->data.string_literal);
        break;
    case AST_NODE_BINOP:
        dest->data.binop.op = src->data.binop.op;
        cc_ast_copy_node(ctx, dest->data.binop.left, src->data.binop.left);
        cc_ast_copy_node(ctx, dest->data.binop.right, src->data.binop.right);
        break;
    case AST_NODE_CALL: {
        size_t i;
        cc_ast_copy_node(
            ctx, dest->data.call.call_expr, src->data.call.call_expr);
        dest->data.call.n_params = src->data.call.n_params;
        dest->data.call.params = cc_realloc_array(
            dest->data.call.params, dest->data.call.n_params);
        /* Clear all of the params's data */
        memset(dest->data.call.params, 0,
            sizeof(cc_ast_node) * src->data.call.n_params);
        for (i = 0; i < src->data.call.n_params; i++) {
            cc_ast_node* expr_node = cc_ast_create_generic(
                ctx, dest, src->data.call.params[i].type);
            dest->data.call.params[i] = *expr_node;
            cc_free(expr_node);
            cc_ast_copy_node(
                ctx, &dest->data.call.params[i], &src->data.call.params[i]);
        }
    } break;
    case AST_NODE_BLOCK: {
        size_t i;
        assert(src->data.block.n_vars == 0);
        assert(src->data.block.n_types == 0);
        assert(dest->data.block.n_children == 0); /* Can't handle child */
        dest->data.block.n_children = src->data.block.n_children;
        dest->data.block.children = cc_realloc_array(
            dest->data.block.children, dest->data.block.n_children);
        /* Clear all of the children's data */
        memset(dest->data.block.children, 0,
            sizeof(cc_ast_node) * src->data.block.n_children);
        for (i = 0; i < src->data.block.n_children; i++) {
            cc_ast_node* expr_node = cc_ast_create_generic(
                ctx, dest, src->data.block.children[i].type);
            dest->data.block.children[i] = *expr_node;
            cc_free(expr_node);
            cc_ast_copy_node(ctx, &dest->data.block.children[i],
                &src->data.block.children[i]);
        }
    } break;
    default:
        assert(0);
        break;
    }
}

void cc_ast_copy_type(
    cc_ast_type* restrict dest, const cc_ast_type* restrict src)
{
    memset(dest, 0, sizeof(*dest));
    dest->mode = src->mode;
    dest->is_typedef = src->is_typedef;

    dest->max_alignment = src->max_alignment;
    dest->min_alignment = src->min_alignment;
    dest->n_cv_qual = src->n_cv_qual;
    memcpy(dest->cv_qual, src->cv_qual, sizeof(src->cv_qual));

    if (src->name != NULL)
        dest->name = cc_strdup(src->name);

    if (src->mode == AST_TYPE_MODE_FUNCTION) { /* Make copies of things */
        size_t i;

        dest->data.func.n_params = src->data.func.n_params;
        dest->data.func.params = cc_realloc_array(
            dest->data.func.params, dest->data.func.n_params);
        memset(dest->data.func.params, 0,
            sizeof(cc_ast_variable) * dest->data.func.n_params);
        for (i = 0; i < dest->data.func.n_params; i++) {
            cc_ast_copy_type(&dest->data.func.params[i].type,
                &src->data.func.params[i].type);
            if (src->data.func.params[i].name != NULL)
                dest->data.func.params[i].name
                    = cc_strdup(src->data.func.params[i].name);
        }
        dest->data.func.variadic = src->data.func.variadic;

        assert(src->data.func.return_type != NULL);
        dest->data.func.return_type
            = cc_malloc(sizeof(*dest->data.func.return_type));
        cc_ast_copy_type(
            dest->data.func.return_type, src->data.func.return_type);
    } else if (src->mode == AST_TYPE_MODE_STRUCT
        || src->mode == AST_TYPE_MODE_UNION) {
        size_t i;

        dest->data.s_or_u.n_members = src->data.s_or_u.n_members;
        dest->data.s_or_u.members = cc_realloc_array(
            dest->data.s_or_u.members, dest->data.s_or_u.n_members);
        for (i = 0; i < dest->data.s_or_u.n_members; i++) {
            const cc_ast_variable* src_param = &src->data.s_or_u.members[i];
            cc_ast_variable* dest_param = &dest->data.s_or_u.members[i];
            cc_ast_copy_type(&dest_param->type, &src_param->type);
            if (src_param->name)
                dest_param->name = cc_strdup(src_param->name);
        }
    } else if (src->mode == AST_TYPE_MODE_ENUM) {
        size_t i;

        dest->data.enumer.n_elems = src->data.enumer.n_elems;
        dest->data.enumer.elems = cc_realloc_array(
            dest->data.enumer.elems, dest->data.enumer.n_elems);
        for (i = 0; i < dest->data.enumer.n_elems; i++) {
            const cc_ast_enum_member* src_member = &src->data.enumer.elems[i];
            cc_ast_enum_member* dest_member = &dest->data.enumer.elems[i];
            if (src_member->name)
                dest_member->name = cc_strdup(src_member->name);
            dest_member->literal = src_member->literal;
        }
    } else {
        dest->data.num.is_signed = src->data.num.is_signed;
        dest->data.num.is_longer = src->data.num.is_longer;
        dest->data.num.bitint_bits = src->data.num.bitint_bits;
    }
}

void cc_ast_add_type_member(
    cc_ast_type* restrict dest, const cc_ast_variable* restrict src)
{
    assert(dest->mode == AST_TYPE_MODE_STRUCT
        || dest->mode == AST_TYPE_MODE_UNION);
    dest->data.s_or_u.members = cc_realloc_array(
        dest->data.s_or_u.members, dest->data.s_or_u.n_members + 1);
    dest->data.s_or_u.members[dest->data.s_or_u.n_members++] = *src;
}

cc_ast_variable* cc_ast_get_field_of(const cc_ast_type* type, const char* field)
{
    size_t i;
    assert(type->mode == AST_TYPE_MODE_STRUCT
        || type->mode == AST_TYPE_MODE_UNION);
    for (i = 0; i < type->data.s_or_u.n_members; i++)
        if (!strcmp(type->data.s_or_u.members[i].name, field))
            return &type->data.s_or_u.members[i];
    return NULL;
}

#if 0
static void cc_ast_iterate_1(const cc_ast_node *node,
                             void (*on_each)(const cc_ast_node *node, va_list arg),
                             va_list arg) {
    if (node == NULL) return;
    switch (node->type) {
    case AST_NODE_BINOP:
        cc_ast_iterate_1(node->data.binop.left, filter, on_each, arg);
        cc_ast_iterate_1(node->data.binop.right, filter, on_each, arg);
        break;
    case AST_NODE_BLOCK:
        for (i = 0; i < node->data.block.n_vars; i++)
            cc_ast_iterate_1(node->data.block.vars[i].body, filter, on_each, arg);
        for (i = 0; i < node->data.block.n_children; i++)
            cc_ast_iterate_1(&node->data.block.children[i], filter, on_each, arg);
        break;
    case AST_NODE_CALL:
        cc_ast_iterate_1(node->data.call.call_expr, filter, on_each, arg);
        for (i = 0; i < node->data.call.n_params; i++)
            cc_ast_iterate_1(&node->data.call.params[i], filter, on_each, arg);
        break;
    case AST_NODE_IF:
        cc_ast_iterate_1(node->data.if_expr.cond, filter, on_each, arg);
        cc_ast_iterate_1(node->data.if_expr.block, filter, on_each, arg);
        cc_ast_iterate_1(node->data.if_expr.tail_else, filter, on_each, arg);
        break;
    case AST_NODE_RETURN:
        cc_ast_iterate_1(node->data.return_expr, filter, on_each, arg);
        break;
    case AST_NODE_JUMP:
    case AST_NODE_STRING_LITERAL:
    case AST_NODE_LITERAL:
    case AST_NODE_VARIABLE:
        break;
    case AST_NODE_UNOP:
        cc_ast_iterate_1(node->data.unop.child, filter, on_each, arg);
        break;
    default:
        break;
    }
    on_each(node, arg);
}

void cc_ast_iterate(const cc_ast_node *node,
                    void (*on_each)(const cc_ast_node *node, va_list arg),
                    ...) {
    va_list arg;
    va_start(arg, on_each);
    cc_ast_iterate_1(node, filter, on_each, arg);
    va_end(arg);
}
#endif

cc_ast_node* cc_ast_find_label_id(
    cc_context* ctx, cc_ast_node* node, unsigned short id)
{
    cc_ast_node* fnode;
    if (node == NULL)
        return NULL;
    if (node->label_id == id)
        return node;

    switch (node->type) {
    case AST_NODE_BINOP:
        if ((fnode = cc_ast_find_label_id(ctx, node->data.binop.left, id))
            != NULL)
            return fnode;
        if ((fnode = cc_ast_find_label_id(ctx, node->data.binop.right, id))
            != NULL)
            return fnode;
        break;
    case AST_NODE_BLOCK: {
        size_t i;
        for (i = 0; i < node->data.block.n_vars; i++) {
            cc_ast_variable* var = &node->data.block.vars[i];
            if (var->type.mode == AST_TYPE_MODE_FUNCTION && var->body != NULL
                && (fnode = cc_ast_find_label_id(ctx, var->body, id)) != NULL)
                return fnode;
        }
        for (i = 0; i < node->data.block.n_children; i++)
            if ((fnode = cc_ast_find_label_id(
                     ctx, &node->data.block.children[i], id))
                != NULL)
                return fnode;
    } break;
    case AST_NODE_CALL: {
        if ((fnode = cc_ast_find_label_id(ctx, node->data.call.call_expr, id))
            != NULL)
            return fnode;
    } break;
    case AST_NODE_RETURN:
        if ((fnode = cc_ast_find_label_id(ctx, node->data.return_expr, id))
            != NULL)
            return fnode;
        break;
    case AST_NODE_SWITCH:
        if ((fnode
                = cc_ast_find_label_id(ctx, node->data.switch_expr.control, id))
            != NULL)
            return fnode;
        if ((fnode
                = cc_ast_find_label_id(ctx, node->data.switch_expr.block, id))
            != NULL)
            return fnode;
        break;
    case AST_NODE_IF:
        if ((fnode = cc_ast_find_label_id(ctx, node->data.if_expr.cond, id))
            != NULL)
            return fnode;
        if ((fnode = cc_ast_find_label_id(ctx, node->data.if_expr.block, id))
            != NULL)
            return fnode;
        if ((fnode
                = cc_ast_find_label_id(ctx, node->data.if_expr.tail_else, id))
            != NULL)
            return fnode;
        break;
    default: /* Skip the rest... */
        break;
    }
    return NULL;
}

void cc_ast_node_iter(cc_context* ctx, cc_ast_node* node,
    void (*transform)(cc_context* ctx, cc_ast_node* node))
{
    if (node == NULL)
        return;

    switch (node->type) {
    case AST_NODE_BINOP:
        cc_ast_node_iter(ctx, node->data.binop.left, transform);
        cc_ast_node_iter(ctx, node->data.binop.right, transform);
        break;
    case AST_NODE_BLOCK: {
        size_t i;
        for (i = 0; i < node->data.block.n_vars; i++) {
            const cc_ast_variable* var = &node->data.block.vars[i];
            if (var->type.mode == AST_TYPE_MODE_FUNCTION && var->body != NULL)
                cc_ast_node_iter(ctx, var->body, transform);
        }
        for (i = 0; i < node->data.block.n_children; i++)
            cc_ast_node_iter(ctx, &node->data.block.children[i], transform);
    } break;
    case AST_NODE_CALL: {
        size_t i;
        cc_ast_node_iter(ctx, node->data.call.call_expr, transform);
        for (i = 0; i < node->data.call.n_params; i++)
            cc_ast_node_iter(ctx, &node->data.call.params[i], transform);
    } break;
    case AST_NODE_SWITCH:
        cc_ast_node_iter(ctx, node->data.switch_expr.control, transform);
        cc_ast_node_iter(ctx, node->data.switch_expr.block, transform);
        break;
    case AST_NODE_IF:
        cc_ast_node_iter(ctx, node->data.if_expr.cond, transform);
        cc_ast_node_iter(ctx, node->data.if_expr.block, transform);
        cc_ast_node_iter(ctx, node->data.if_expr.tail_else, transform);
        break;
    case AST_NODE_RETURN:
        cc_ast_node_iter(ctx, node->data.return_expr, transform);
        break;
    case AST_NODE_UNOP:
        cc_ast_node_iter(ctx, node->data.unop.child, transform);
        break;
    case AST_NODE_JUMP:
    case AST_NODE_LITERAL:
    case AST_NODE_STRING_LITERAL:
    case AST_NODE_VARIABLE:
        break;
    default:
        break;
    }
}

static const char* cc_ast_get_binop_op_name(enum cc_ast_binop_type op)
{
    switch (op) {
    case AST_BINOP_NONE:
        return "?";
    case AST_BINOP_GT:
        return ">";
    case AST_BINOP_GTE:
        return ">=";
    case AST_BINOP_LT:
        return "<";
    case AST_BINOP_LTE:
        return "<=";
    case AST_BINOP_ASSIGN:
        return "=";
    case AST_BINOP_AND:
        return "&";
    case AST_BINOP_COND_AND:
        return "&&";
    case AST_BINOP_COND_EQ:
        return "==";
    case AST_BINOP_COND_NEQ:
        return "!=";
    case AST_BINOP_COND_OR:
        return "||";
    case AST_BINOP_DIV:
        return "/";
    case AST_BINOP_LSHIFT:
        return "<<";
    case AST_BINOP_SUB:
        return "-";
    case AST_BINOP_MOD:
        return "%%";
    case AST_BINOP_MUL:
        return "*";
    case AST_BINOP_OR:
        return "|";
    case AST_BINOP_ADD:
        return "+";
    case AST_BINOP_RSHIFT:
        return ">>";
    case AST_BINOP_XOR:
        return "^";
    default:
        abort();
    }
}

static void cc_ast_print_var(const cc_ast_variable* var, const char* name)
{
    size_t i;
    if (var == NULL) {
        if (name == NULL)
            printf("<var (null)>");
        else
            printf("<var %s (unscoped)>", name);
        return;
    }

    ++body_print_lock;
    printf("<var %s ", var->name);

    switch (var->type.mode) {
    case AST_TYPE_MODE_FUNCTION:
        printf("(fn)");
        break;
    case AST_TYPE_MODE_INT:
        printf("(int)");
        break;
    default:
        printf("(*)");
        break;
    }

    for (i = 0; i <= var->type.n_cv_qual; ++i)
        if (var->type.cv_qual[i].is_array) {
            printf("[");
            if (var->type.cv_qual[i].is_vla)
                cc_ast_print(var->type.cv_qual[i].array.size_expr);
            else
                printf("%u", (unsigned int)var->type.cv_qual[i].array.size);
            printf("]");
        }

    if ((var->storage & AST_STORAGE_AUTO) != 0)
        printf("auto,");
    if ((var->storage & AST_STORAGE_CONSTEXPR) != 0)
        printf("constexpr,");
    if ((var->storage & AST_STORAGE_EXTERN) != 0)
        printf("extern,");
    if ((var->storage & AST_STORAGE_GLOBAL) != 0)
        printf("global,");
    if ((var->storage & AST_STORAGE_INLINE) != 0)
        printf("inline,");
    if ((var->storage & AST_STORAGE_REGISTER) != 0)
        printf("register,");
    if ((var->storage & AST_STORAGE_STATIC) != 0)
        printf("static,");
    if ((var->storage & AST_STORAGE_THREAD_LOCAL) != 0)
        printf("thread_local,");

    if (var->type.mode == AST_TYPE_MODE_FUNCTION) {
        /*printf("(return ");
        cc_ast_print_var(var->type.data.func.return_type, NULL);
        printf(")");*/
        printf("(");
        for (i = 0; i < var->type.data.func.n_params; ++i)
            cc_ast_print_var(&var->type.data.func.params[i], NULL);
        if (var->type.data.func.variadic)
            printf("<...>");
        printf(")");
        if (body_print_lock <= 1 && var->body != NULL)
            cc_ast_print(var->body);
    }
    printf(">");
    --body_print_lock;
}

void cc_ast_print(const cc_ast_node* node)
{
    if (node == NULL) {
        printf("<null>");
        return;
    }

    if (node->ref_count > 0)
        printf("(label-id %u)", node->label_id);
    switch (node->type) {
    case AST_NODE_BINOP:
        printf("<binop{");
        cc_ast_print(node->data.binop.left);
        printf("}%s{", cc_ast_get_binop_op_name(node->data.binop.op));
        cc_ast_print(node->data.binop.right);
        printf("}>");
        break;
    case AST_NODE_BLOCK: {
        size_t i;
        printf("<block(%u){", (unsigned int)node->data.block.n_children);
        if (node->data.block.is_case)
            printf("case %lu,%s", node->data.block.case_val.value.u,
                node->data.block.is_default ? "default" : "case");

        for (i = 0; i < node->data.block.n_types; i++) {
            const cc_ast_type* type = &node->data.block.types[i];
            printf("type %s;\n", type->name);
        }

        for (i = 0; i < node->data.block.n_vars; i++)
            cc_ast_print_var(&node->data.block.vars[i], NULL);

        for (i = 0; i < node->data.block.n_children; i++) {
            cc_ast_print(&node->data.block.children[i]);
            if (i + 1 < node->data.block.n_children)
                printf(",");
        }
        printf("}>");
    } break;
    case AST_NODE_CALL: {
        size_t i;
        printf("<call{");
        cc_ast_print(node->data.call.call_expr);
        printf("}params{");
        for (i = 0; i < node->data.call.n_params; i++) {
            cc_ast_print(&node->data.call.params[i]);
            printf(",");
        }
        printf("}>");
    } break;
    case AST_NODE_SWITCH:
        printf("<switch{");
        cc_ast_print(node->data.switch_expr.control);
        printf("}block{");
        cc_ast_print(node->data.switch_expr.block);
        printf("}>");
        break;
    case AST_NODE_IF:
        printf("<if{");
        cc_ast_print(node->data.if_expr.cond);
        printf("}then{");
        cc_ast_print(node->data.if_expr.block);
        printf("}else{");
        cc_ast_print(node->data.if_expr.tail_else);
        printf("}>");
        break;
    case AST_NODE_JUMP:
        printf("<jump-to %u>", node->data.jump_label_id);
        break;
    case AST_NODE_LITERAL:
        if (node->data.literal.is_signed)
            printf("<literal %lii>", node->data.literal.value.s);
        else
            printf("<literal %luu>", node->data.literal.value.u);
        break;
    case AST_NODE_RETURN:
        printf("<return{");
        cc_ast_print(node->data.return_expr);
        printf("}>");
        break;
    case AST_NODE_STRING_LITERAL:
        printf("<string-literal %s>", node->data.string_literal);
        break;
    case AST_NODE_UNOP:
        printf("<unop ");
        switch (node->data.unop.op) {
        case AST_UNOP_CAST:
            printf("cast(mode=%i)", node->data.unop.cast.mode);
            break;
        case AST_UNOP_COND_NOT:
            printf("!");
            break;
        case AST_UNOP_DEREF:
            printf("deref");
            break;
        case AST_UNOP_NOT:
            printf("~");
            break;
        case AST_UNOP_POSTDEC:
            printf("x--");
            break;
        case AST_UNOP_POSTINC:
            printf("x++");
            break;
        case AST_UNOP_PREDEC:
            printf("--x");
            break;
        case AST_UNOP_PREINC:
            printf("++x");
            break;
        case AST_UNOP_REF:
            printf("ref");
            break;
        default:
            printf("???");
            break;
        }
        cc_ast_print(node->data.unop.child);
        printf(">");
        break;
    case AST_NODE_VARIABLE:
        cc_ast_print_var(cc_ast_find_variable(NULL, node->data.var.name, node),
            node->data.var.name);
        break;
    case AST_NODE_FIELD_ACCESS:
        printf("<field-access(");
        cc_ast_print(node->data.field_access.left);
        printf(")%s>", node->data.field_access.field_name);
        break;
    default:
        printf("<?{%i}>", node->type);
        break;
    }
}
