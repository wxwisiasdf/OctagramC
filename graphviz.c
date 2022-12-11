/* graphviz.c - Output DOT graphviz files from the AST */
#include "ast.h"
#include "context.h"
#include <stdio.h>

static void cc_graphviz_print(cc_context* ctx, cc_ast_node* node)
{
    if (node == NULL)
        return;

    if (node->parent != NULL)
        fprintf(
            ctx->out, "n%u -> n%u;", node->parent->label_id, node->label_id);

    switch (node->type) {
    case AST_NODE_BINOP:
        fprintf(ctx->out, "n%u [label=\"Binary ", node->label_id);
        switch (node->data.binop.op) {
        case AST_BINOP_GT:
            fprintf(ctx->out, ">");
            break;
        case AST_BINOP_GTE:
            fprintf(ctx->out, ">=");
            break;
        case AST_BINOP_LT:
            fprintf(ctx->out, "<");
            break;
        case AST_BINOP_LTE:
            fprintf(ctx->out, "<=");
            break;
        case AST_BINOP_ASSIGN:
            fprintf(ctx->out, "=");
            break;
        case AST_BINOP_AND:
            fprintf(ctx->out, "AND");
            break;
        case AST_BINOP_ARROW:
            fprintf(ctx->out, "->");
            break;
        case AST_BINOP_COND_AND:
            fprintf(ctx->out, "&&");
            break;
        case AST_BINOP_COND_EQ:
            fprintf(ctx->out, "==");
            break;
        case AST_BINOP_COND_NEQ:
            fprintf(ctx->out, "!=");
            break;
        case AST_BINOP_COND_OR:
            fprintf(ctx->out, "||");
            break;
        case AST_BINOP_DIV:
            fprintf(ctx->out, "/");
            break;
        case AST_BINOP_DOT:
            fprintf(ctx->out, ".");
            break;
        case AST_BINOP_LSHIFT:
            fprintf(ctx->out, "<<");
            break;
        case AST_BINOP_MINUS:
            fprintf(ctx->out, "-");
            break;
        case AST_BINOP_MOD:
            fprintf(ctx->out, "%%");
            break;
        case AST_BINOP_MUL:
            fprintf(ctx->out, "*");
            break;
        case AST_BINOP_OR:
            fprintf(ctx->out, "|");
            break;
        case AST_BINOP_PLUS:
            fprintf(ctx->out, "+");
            break;
        case AST_BINOP_RSHIFT:
            fprintf(ctx->out, ">>");
            break;
        case AST_BINOP_XOR:
            fprintf(ctx->out, "^");
            break;
        default:
            fprintf(ctx->out, "?(%i)", node->data.binop.op);
            break;
        }
        fprintf(ctx->out, "\"];");
        cc_graphviz_print(ctx, node->data.binop.left);
        cc_graphviz_print(ctx, node->data.binop.right);
        break;
    case AST_NODE_BLOCK:
        fprintf(ctx->out, "subgraph cluster_%i {\n", node->label_id);
        fprintf(ctx->out,
            "label=\"Block #%i\"; node [style=filled];color=blue;\n",
            node->label_id);
        fprintf(ctx->out, "n%i [label = \"Block #%i\"];", node->label_id,
            node->label_id);
        for (size_t i = 0; i < node->data.block.n_vars; i++) {
            const cc_ast_variable* var = &node->data.block.vars[i];
            if (var->type.mode == AST_TYPE_MODE_FUNCTION)
                if (var->body != NULL)
                    cc_graphviz_print(ctx, var->body);
        }

        for (size_t i = 0; i < node->data.block.n_children; i++)
            cc_graphviz_print(ctx, &node->data.block.children[i]);
        fprintf(ctx->out, "}");
        break;
    case AST_NODE_CALL:
        fprintf(ctx->out, "n%u [shape=Mdiamond];", node->label_id);
        fprintf(ctx->out, "n%u [label=\"call\"];", node->label_id);
        cc_graphviz_print(ctx, node->data.call.call_expr);
        for (size_t i = 0; i < node->data.call.n_params; i++)
            cc_graphviz_print(ctx, &node->data.call.params[i]);
        break;
    case AST_NODE_IF:
        fprintf(ctx->out, "n%u [shape=Mdiamond];", node->label_id);
        fprintf(ctx->out, "n%u [label=\"if\"];", node->label_id);
        cc_graphviz_print(ctx, node->data.if_expr.cond);
        cc_graphviz_print(ctx, node->data.if_expr.block);
        cc_graphviz_print(ctx, node->data.if_expr.tail_else);
        break;
    case AST_NODE_JUMP:
        fprintf(ctx->out, "n%u [shape=Mdiamond];", node->label_id);
        fprintf(ctx->out, "n%u [label=\"jump %u\"];", node->label_id,
            node->data.jump.label_id);
        fprintf(
            ctx->out, "n%u -> n%u;", node->label_id, node->data.jump.label_id);
        break;
    case AST_NODE_LITERAL:
        fprintf(ctx->out, "n%u [label=\"", node->label_id);
        if (node->data.literal.is_signed)
            fprintf(ctx->out, "%li", node->data.literal.value.s);
        else
            fprintf(ctx->out, "%lu", node->data.literal.value.u);
        fprintf(ctx->out, "\"];");
        break;
    case AST_NODE_RETURN:
        fprintf(ctx->out, "n%u [shape=Mdiamond];", node->label_id);
        fprintf(ctx->out, "n%u [label=\"return\"];", node->label_id);
        cc_graphviz_print(ctx, node->data.return_expr.value);
        break;
    case AST_NODE_STRING_LITERAL:
        fprintf(ctx->out, "n%u [label=\"\\\"%s\\\"\"];", node->label_id,
            node->data.string_literal.data);
        break;
    case AST_NODE_UNOP:
        fprintf(ctx->out, "n%u [label=\"Unary ", node->label_id);
        switch (node->data.unop.op) {
        case AST_UNOP_CAST:
            fprintf(ctx->out, "cast");
            break;
        case AST_UNOP_COND_NOT:
            fprintf(ctx->out, "!");
            break;
        case AST_UNOP_DEREF:
            fprintf(ctx->out, "* (Deref)");
            break;
        case AST_UNOP_NOT:
            fprintf(ctx->out, "~");
            break;
        case AST_UNOP_POSTDEC:
            fprintf(ctx->out, "()--");
            break;
        case AST_UNOP_POSTINC:
            fprintf(ctx->out, "()++");
            break;
        case AST_UNOP_PREDEC:
            fprintf(ctx->out, "--()");
            break;
        case AST_UNOP_PREINC:
            fprintf(ctx->out, "++()");
            break;
        case AST_UNOP_REF:
            fprintf(ctx->out, "& (Ref)");
            break;
        default:
            fprintf(ctx->out, "?");
            break;
        }
        fprintf(ctx->out, "\"];");
        cc_graphviz_print(ctx, node->data.unop.child);
        break;
    case AST_NODE_VARIABLE: {
        const cc_ast_variable* var
            = cc_ast_find_variable(node->data.var.name, node);
        fprintf(ctx->out, "n%u [label=\"%s\"];", node->label_id, var->name);
    } break;
    default:
        fprintf(ctx->out, "n%u [label=\"???\"];", node->type);
        break;
    }
}

int cc_graphviz_top(cc_context* ctx)
{
    fprintf(ctx->out, "digraph G {\n");
    cc_graphviz_print(ctx, ctx->root);
    fprintf(ctx->out, "}\n");
    return 0;
}
