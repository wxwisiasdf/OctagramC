/* ast.c - Abstract Syntax Tree utility functions */
#include "ast.h"
#include "context.h"
#include "lexer.h"
#include "optzer.h"
#include "util.h"
#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

unsigned int cc_ast_alloc_label_id(void)
{
    static unsigned int label_id = 0;
    return label_id++;
}

static cc_ast_node* cc_ast_create_any(
    cc_context* ctx, cc_ast_node* parent, enum cc_ast_node_type type)
{
    cc_ast_node* node = cc_zalloc(sizeof(cc_ast_node));
    node->parent = parent;
    node->type = type;
    node->label_id = cc_ast_alloc_label_id();

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
    node->data.return_expr.value = cc_ast_create_block(ctx, node);
    return node;
}

cc_ast_node* cc_ast_create_var_ref(
    cc_context* ctx, cc_ast_node* parent, const cc_ast_variable* var)
{
    cc_ast_node* node = cc_ast_create_any(ctx, parent, AST_NODE_VARIABLE);
    node->data.var.name = cc_strdup(var->name);
    return node;
}

cc_ast_node* cc_ast_create_field_ref(
    cc_context* ctx, cc_ast_node* parent, const char* fieldname)
{
    cc_ast_node* node = cc_ast_create_any(ctx, parent, AST_NODE_VARIABLE);
    node->data.var.name = cc_strdup(fieldname);
    node->data.var.is_field = true;
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
    node->data.string_literal.data = cc_strdup(s);
    return node;
}

cc_ast_node* cc_ast_create_literal(
    cc_context* ctx, cc_ast_node* parent, const char* s)
{
    cc_ast_node* node = cc_ast_create_any(ctx, parent, AST_NODE_LITERAL);
    /* TODO: Better parsing of the literals into integers */
    node->data.literal.value.s = atoi(s);
    return node;
}

cc_ast_node* cc_ast_create_jump(
    cc_context* ctx, cc_ast_node* parent, cc_ast_node* target)
{
    cc_ast_node* node = cc_ast_create_any(ctx, parent, AST_NODE_JUMP);
    node->data.jump.label_id = target->label_id;
    target->ref_count++;
    return node;
}

void cc_ast_add_block_node(
    cc_ast_node* restrict block, const cc_ast_node* restrict child)
{
    assert(block != NULL && block->type == AST_NODE_BLOCK);
    assert(child != NULL && child->parent == block);

    block->data.block.children = cc_realloc(block->data.block.children,
        sizeof(cc_ast_node) * (block->data.block.n_children + 1));
    block->data.block.children[block->data.block.n_children++] = *child;
}

void cc_ast_add_block_typedef(cc_ast_node* block, const cc_ast_typedef* tpdef)
{
    assert(block != NULL && block->type == AST_NODE_BLOCK);
    assert(tpdef->name != NULL);
    block->data.block.typedefs = cc_realloc(block->data.block.typedefs,
        sizeof(cc_ast_typedef) * (block->data.block.n_typedefs + 1));
    block->data.block.typedefs[block->data.block.n_typedefs++] = *tpdef;
}

void cc_ast_add_block_type(cc_ast_node* block, const cc_ast_type* type)
{
    assert(block != NULL && block->type == AST_NODE_BLOCK);
    assert(type->mode != AST_TYPE_MODE_NONE);
    assert(type->name != NULL);
    block->data.block.types = cc_realloc(block->data.block.types,
        sizeof(cc_ast_type) * (block->data.block.n_types + 1));
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
    assert(block->type == AST_NODE_BLOCK);
    assert(var->name != NULL);

    for (size_t i = 0; i < block->data.block.n_vars; i++) {
        cc_ast_variable* bvar = &block->data.block.vars[i];
        if (!strcmp(bvar->name, var->name)) {
            /* TODO: Check that the declaration is the same */
            cc_ast_destroy_var(bvar, false);
            *bvar = *var;
            return;
        }
    }

    block->data.block.vars = cc_realloc(block->data.block.vars,
        sizeof(cc_ast_variable) * (block->data.block.n_vars + 1));
    block->data.block.vars[block->data.block.n_vars++] = *var;
}

void cc_ast_add_call_param(
    cc_ast_node* restrict call, const cc_ast_node* restrict param)
{
    assert(param->parent == call);
    call->data.call.params = cc_realloc(call->data.call.params,
        sizeof(cc_ast_node) * (call->data.call.n_params + 1));
    call->data.call.params[call->data.call.n_params++] = *param;
}

void cc_ast_destroy_type(cc_ast_type* type, _Bool managed)
{
    if (type == NULL)
        return;
    switch (type->mode) {
    case AST_TYPE_MODE_FUNCTION:
        if (type->data.func.params != NULL) {
            for (size_t i = 0; i < type->data.func.n_params; i++) {
                cc_ast_destroy_type(&type->data.func.params[i].type, false);
                cc_free(type->data.func.params[i].name);
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

void cc_ast_destroy_var(cc_ast_variable* var, _Bool managed)
{
    assert(var != NULL);
    cc_ast_destroy_type(&var->type, false);
    /*cc_free(var->name);*/
    if (managed)
        cc_free(var);
}

void cc_ast_destroy_node(cc_ast_node* node, _Bool managed)
{
    if (node == NULL)
        return;
    assert(node->parent != node);
    assert(node->ref_count == 0);

    switch (node->type) {
    case AST_NODE_STRING_LITERAL:
        cc_free(node->data.string_literal.data);
        break;
    case AST_NODE_BINOP:
        cc_ast_destroy_node(node->data.binop.left, true);
        cc_ast_destroy_node(node->data.binop.right, true);
        break;
    case AST_NODE_CALL:
        cc_ast_destroy_node(node->data.call.call_expr, true);
        for (size_t i = 0; i < node->data.call.n_params; i++)
            cc_ast_destroy_node(&node->data.call.params[i], false);
        cc_free(node->data.call.params);
        break;
    case AST_NODE_BLOCK:
        for (size_t i = 0; i < node->data.block.n_vars; i++)
            cc_ast_destroy_var(&node->data.block.vars[i], false);
        cc_free(node->data.block.vars);

        for (size_t i = 0; i < node->data.block.n_children; i++)
            cc_ast_destroy_node(&node->data.block.children[i], false);
        cc_free(node->data.block.children);
        break;
    case AST_NODE_RETURN:
        cc_ast_destroy_node(node->data.return_expr.value, true);
        break;
    case AST_NODE_VARIABLE:
        cc_free(node->data.var.name);
        break;
    default:
        break;
    }

    if (managed)
        cc_free(node);
}

/* ========================================================================== */
cc_ast_variable* cc_ast_find_variable(const char* name, const cc_ast_node* node)
{
    if (node == NULL)
        return NULL;
    if (node->type == AST_NODE_BLOCK) {
        for (size_t i = 0; i < node->data.block.n_vars; i++) {
            cc_ast_variable* var = &node->data.block.vars[i];

            if (var->name != NULL && !strcmp(var->name, name))
                return var;

            /* Check parameters for functions */
            if (var->type.mode == AST_TYPE_MODE_FUNCTION) {
                for (size_t j = 0; j < var->type.data.func.n_params; j++) {
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
    return cc_ast_find_variable(name, node->parent);
}

cc_ast_node* cc_ast_find_label(const char* name, const cc_ast_node* node)
{
    if (node == NULL)
        return NULL;
    if (node->type == AST_NODE_BLOCK) {
        for (size_t i = 0; i < node->data.block.n_children; i++) {
            cc_ast_node* bnode = &node->data.block.children[i];
            if ((bnode = cc_ast_find_label(name, bnode)) != NULL)
                return bnode;
        }

        for (size_t i = 0; i < node->data.block.n_vars; i++) {
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

cc_ast_typedef* cc_ast_find_typedef(const char* name, cc_ast_node* node)
{
    if (node == NULL)
        return NULL;
    if (node->type == AST_NODE_BLOCK) {
        for (size_t i = 0; i < node->data.block.n_typedefs; i++) {
            cc_ast_typedef* tpdef = &node->data.block.typedefs[i];
            if (!strcmp(tpdef->name, name))
                return tpdef;
        }
    }
    return cc_ast_find_typedef(name, node->parent);
}

cc_ast_type* cc_ast_find_type(const char* name, cc_ast_node* node)
{
    if (node == NULL)
        return NULL;
    if (node->type == AST_NODE_BLOCK) {
        for (size_t i = 0; i < node->data.block.n_types; i++) {
            cc_ast_type* type = &node->data.block.types[i];
            if (!strcmp(type->name, name))
                return type;
        }
    }
    return cc_ast_find_type(name, node->parent);
}

void cc_ast_copy_node(
    cc_ast_node* restrict dest, const cc_ast_node* restrict src)
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
        dest->data.string_literal.data
            = cc_strdup(src->data.string_literal.data);
        break;
    case AST_NODE_BINOP:
        cc_ast_copy_node(dest->data.binop.left, src->data.binop.left);
        cc_ast_copy_node(dest->data.binop.right, src->data.binop.right);
        break;
    case AST_NODE_BLOCK:
        assert(
            src->data.block.n_typedefs == 0); /* Don't know how to transfer */
        assert(src->data.block.n_vars == 0);
        assert(src->data.block.n_types == 0);

        assert(dest->data.block.n_children == 0); /* Can't handle child */

        dest->data.block.n_children = src->data.block.n_children;
        dest->data.block.children = cc_realloc(dest->data.block.children,
            sizeof(cc_ast_node) * dest->data.block.n_children);

        for (size_t i = 0; i < src->data.block.n_children; i++) {
            memset(&dest->data.block.children[i], 0, sizeof(cc_ast_node));
            dest->data.block.children[i].type
                = src->data.block.children[i].type;
            cc_ast_copy_node(
                &dest->data.block.children[i], &src->data.block.children[i]);
        }
        break;
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

    if (src->name != NULL)
        dest->name = cc_strdup(src->name);

    if (src->mode == AST_TYPE_MODE_FUNCTION) { /* Make copies of things */
        dest->data.func.n_params = src->data.func.n_params;
        dest->data.func.params = cc_realloc(dest->data.func.params,
            sizeof(cc_ast_variable) * dest->data.func.n_params);
        memset(dest->data.func.params, 0,
            sizeof(cc_ast_variable) * dest->data.func.n_params);
        for (size_t i = 0; i < dest->data.func.n_params; i++) {
            cc_ast_copy_type(&dest->data.func.params[i].type,
                &src->data.func.params[i].type);
            if (src->data.func.params[i].name != NULL)
                dest->data.func.params[i].name
                    = cc_strdup(src->data.func.params[i].name);
        }

        if (src->data.func.return_type != NULL) {
            dest->data.func.return_type
                = cc_malloc(sizeof(*dest->data.func.return_type));
            cc_ast_copy_type(
                dest->data.func.return_type, src->data.func.return_type);
        }
    } else if (src->mode == AST_TYPE_MODE_STRUCT || src->mode == AST_TYPE_MODE_UNION) {
        dest->data.s_or_u.n_members = src->data.s_or_u.n_members;
        dest->data.s_or_u.members = cc_realloc(dest->data.s_or_u.members,
            sizeof(cc_ast_variable) * dest->data.s_or_u.n_members);

        for (size_t i = 0; i < dest->data.s_or_u.n_members; i++) {
            const cc_ast_variable* src_param = &src->data.s_or_u.members[i];
            cc_ast_variable* dest_param = &dest->data.s_or_u.members[i];
            cc_ast_copy_type(&dest_param->type, &src_param->type);
            if (src_param->name)
                dest_param->name = cc_strdup(src_param->name);
        }
    }
}

void cc_ast_add_type_member(
    cc_ast_type* restrict dest, const cc_ast_variable* restrict src)
{
    assert(dest->mode == AST_TYPE_MODE_STRUCT || dest->mode == AST_TYPE_MODE_UNION);
    dest->data.s_or_u.members = cc_realloc(dest->data.s_or_u.members,
        sizeof(*dest->data.s_or_u.members) * (dest->data.s_or_u.n_members + 1));
    dest->data.s_or_u.members[dest->data.s_or_u.n_members++] = *src;
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
        for (size_t i = 0; i < node->data.block.n_vars; i++)
            cc_ast_iterate_1(node->data.block.vars[i].body, filter, on_each, arg);
        for (size_t i = 0; i < node->data.block.n_children; i++)
            cc_ast_iterate_1(&node->data.block.children[i], filter, on_each, arg);
        break;
    case AST_NODE_CALL:
        cc_ast_iterate_1(node->data.call.call_expr, filter, on_each, arg);
        for (size_t i = 0; i < node->data.call.n_params; i++)
            cc_ast_iterate_1(&node->data.call.params[i], filter, on_each, arg);
        break;
    case AST_NODE_IF:
        cc_ast_iterate_1(node->data.if_expr.cond, filter, on_each, arg);
        cc_ast_iterate_1(node->data.if_expr.block, filter, on_each, arg);
        cc_ast_iterate_1(node->data.if_expr.tail_else, filter, on_each, arg);
        break;
    case AST_NODE_RETURN:
        cc_ast_iterate_1(node->data.return_expr.value, filter, on_each, arg);
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
    cc_context* ctx, cc_ast_node* node, unsigned int id)
{
    if (node == NULL)
        return NULL;
    if (node->label_id == id)
        return node;

    cc_ast_node* fnode;
    switch (node->type) {
    case AST_NODE_BINOP:
        if ((fnode = cc_ast_find_label_id(ctx, node->data.binop.left, id))
            != NULL)
            return fnode;
        if ((fnode = cc_ast_find_label_id(ctx, node->data.binop.right, id))
            != NULL)
            return fnode;
        break;
    case AST_NODE_BLOCK:
        for (size_t i = 0; i < node->data.block.n_vars; i++) {
            cc_ast_variable* var = &node->data.block.vars[i];
            if (var->type.mode == AST_TYPE_MODE_FUNCTION && var->body != NULL
                && (fnode = cc_ast_find_label_id(ctx, var->body, id)) != NULL)
                return fnode;
        }

        for (size_t i = 0; i < node->data.block.n_children; i++)
            if ((fnode = cc_ast_find_label_id(
                     ctx, &node->data.block.children[i], id))
                != NULL)
                return fnode;
        break;
    case AST_NODE_CALL: {
        if ((fnode = cc_ast_find_label_id(ctx, node->data.call.call_expr, id))
            != NULL)
            return fnode;
    } break;
    case AST_NODE_RETURN:
        if ((fnode
                = cc_ast_find_label_id(ctx, node->data.return_expr.value, id))
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

void cc_ast_print(const cc_ast_node* node, int ident)
{
    if (node == NULL) {
        printf("<null>");
        return;
    }

    if (node->ref_count > 0)
        printf("(label-id %u)", node->label_id);
    switch (node->type) {
    case AST_NODE_BINOP:
        printf("<binop (");
        cc_ast_print(node->data.binop.left, ident);
        printf(") ");
        switch (node->data.binop.op) {
        case AST_BINOP_NONE:
            printf("XXX");
            break;
        case AST_BINOP_GT:
            printf(">");
            break;
        case AST_BINOP_GTE:
            printf(">=");
            break;
        case AST_BINOP_LT:
            printf("<");
            break;
        case AST_BINOP_LTE:
            printf("<=");
            break;
        case AST_BINOP_ASSIGN:
            printf("=");
            break;
        case AST_BINOP_AND:
            printf("AND");
            break;
        case AST_BINOP_ARROW:
            printf("->");
            break;
        case AST_BINOP_COND_AND:
            printf("&&");
            break;
        case AST_BINOP_COND_EQ:
            printf("==");
            break;
        case AST_BINOP_COND_NEQ:
            printf("!=");
            break;
        case AST_BINOP_COND_OR:
            printf("||");
            break;
        case AST_BINOP_DIV:
            printf("/");
            break;
        case AST_BINOP_DOT:
            printf(".");
            break;
        case AST_BINOP_LSHIFT:
            printf("<<");
            break;
        case AST_BINOP_MINUS:
            printf("-");
            break;
        case AST_BINOP_MOD:
            printf("%%");
            break;
        case AST_BINOP_MUL:
            printf("*");
            break;
        case AST_BINOP_OR:
            printf("|");
            break;
        case AST_BINOP_PLUS:
            printf("+");
            break;
        case AST_BINOP_RSHIFT:
            printf(">>");
            break;
        case AST_BINOP_XOR:
            printf("^");
            break;
        default:
            printf("(%i?)", node->data.binop.op);
            break;
        }
        printf(" (");
        cc_ast_print(node->data.binop.right, ident);
        printf(")>");
        break;
    case AST_NODE_BLOCK:
        printf("{ ");
        if (node->data.block.is_case)
            printf("case(%u,%s)", node->data.block.case_val,
                node->data.block.is_default ? "default" : "case");

        for (size_t i = 0; i < node->data.block.n_typedefs; i++) {
            const cc_ast_typedef* tpdef = &node->data.block.typedefs[i];
            printf("typedef %s;\n", tpdef->name);
        }

        for (size_t i = 0; i < node->data.block.n_vars; i++) {
            const cc_ast_variable* var = &node->data.block.vars[i];
            printf("var %s", var->name);
            if (var->type.mode == AST_TYPE_MODE_FUNCTION) {
                printf("(");
                for (size_t j = 0; j < var->type.data.func.n_params; j++)
                    printf("%zu=%s, ", j, var->type.data.func.params[j].name);
                printf(")");
                if (var->body != NULL)
                    cc_ast_print(var->body, ident + 1);
            }
            printf(";");
        }

        for (size_t i = 0; i < node->data.block.n_children; i++) {
            cc_ast_print(&node->data.block.children[i], ident + 1);
            printf(";");
        }
        printf("}");
        break;
    case AST_NODE_CALL:
        printf("<call (");
        cc_ast_print(node->data.call.call_expr, 0);
        printf(") params(");
        for (size_t i = 0; i < node->data.call.n_params; i++) {
            cc_ast_print(&node->data.call.params[i], 0);
            printf("; ");
        }
        printf(")>");
        break;
    case AST_NODE_SWITCH:
        printf("<switch (");
        cc_ast_print(node->data.switch_expr.control, 0);
        printf(") block (");
        cc_ast_print(node->data.switch_expr.block, 0);
        printf(")>");
        break;
    case AST_NODE_IF:
        printf("<if (");
        cc_ast_print(node->data.if_expr.cond, 0);
        printf(") then (");
        cc_ast_print(node->data.if_expr.block, 0);
        printf(") else (");
        cc_ast_print(node->data.if_expr.tail_else, 0);
        printf(")>");
        break;
    case AST_NODE_JUMP:
        printf("<jump-to %u>", node->data.jump.label_id);
        break;
    case AST_NODE_LITERAL:
        if (node->data.literal.is_signed)
            printf("<literal %lli>", node->data.literal.value.s);
        else
            printf("<literal %llu>", node->data.literal.value.u);
        break;
    case AST_NODE_RETURN:
        printf("<return (");
        cc_ast_print(node->data.return_expr.value, ident);
        printf(")>");
        break;
    case AST_NODE_STRING_LITERAL:
        printf("<string-literal %s>", node->data.string_literal.data);
        break;
    case AST_NODE_UNOP:
        printf("<unop ");
        switch (node->data.unop.op) {
        case AST_UNOP_CAST:
            printf("cast");
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
        cc_ast_print(node->data.unop.child, 0);
        printf(">");
        break;
    case AST_NODE_VARIABLE: {
        const cc_ast_variable* var
            = cc_ast_find_variable(node->data.var.name, node);
        printf("<var %s ",
            node->data.var.is_field ? node->data.var.name : var->name);
        if (!node->data.var.is_field) {
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
        }
        printf(">");
    } break;
    default:
        printf("<?(%i)>", node->type);
        break;
    }
}
