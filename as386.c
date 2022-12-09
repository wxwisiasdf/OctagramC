/* as386.c - Assembly code generation for 386 machines */
#include "as386.h"
#include "ast.h"
#include "backend.h"
#include "context.h"
#include "diag.h"
#include "parser.h"
#include "util.h"
#include <assert.h>
#include <stdio.h>

enum cc_as386_reg {
    AS386_NONE = -1,
    AS386_EAX = 0,
    AS386_EBX = 1,
    AS386_ECX = 2,
    AS386_EDX = 3,
    AS386_ESI = 4,
    AS386_EDI = 5,
    AS386_EBP = 6,
    AS386_ESP = 7,
    AS386_NUM_REGS = 8,
};

typedef struct cc_as386_context {
    cc_backend_varmap latest_varmap; /* Latest relevant varmap */
} cc_as386_context;

static void cc_as386_process_binop(
    cc_context* ctx, const cc_ast_node* node, const cc_backend_varmap* ovmap);

static const char* reg_names[AS386_NUM_REGS]
    = { "%eax", "%ebx", "%ecx", "%edx", "%esi", "%edi", "%ebp", "%esp" };

unsigned int cc_as386_get_sizeof(cc_context* ctx, const cc_ast_type* type)
{
    size_t sizeof_ptr = 4;
    if (type->n_cv_qual > 0) /* Pointer types */
        return sizeof_ptr;

    switch (type->mode) {
    case TYPE_MODE_CHAR:
        return 1;
    case TYPE_MODE_BOOL:
        return 1;
    case TYPE_MODE_INT:
        return 4;
    case TYPE_MODE_SHORT:
        return 2;
    case TYPE_MODE_LONG:
        return 8;
    case TYPE_MODE_FUNCTION:
        return sizeof_ptr;
    default:
        break;
    }
    cc_diag_error(ctx, "Unknown sizeof");
    return 0;
}

_Bool cc_as386_is_reserved_reg(enum cc_as386_reg regno)
{
    return regno == AS386_EBP || regno == AS386_ESP;
}

static void cc_as386_process_node(cc_context* ctx, const cc_ast_node* node);

static void cc_as386_print_varmap(
    cc_context* ctx, const cc_backend_varmap* vmap)
{
    switch (vmap->flags) {
    case VARMAP_REGISTER:
        fprintf(ctx->out, "%s", reg_names[vmap->regno]);
        break;
    case VARMAP_STACK:
        fprintf(ctx->out, "-%u(%%ebp)", vmap->offset);
        break;
    case VARMAP_THREAD_LOCAL:
        fprintf(ctx->out, "%%gs::%s@ntpoff", vmap->var->name);
        break;
    case VARMAP_CONSTANT:
        fprintf(ctx->out, "$%lu", vmap->constant);
        break;
    default:
        cc_diag_error(ctx, "Invalid varmap %i", vmap->flags);
        break;
    }
}

_Bool cc_as386_gen_mov(cc_context* ctx, const cc_backend_varmap* lvmap,
    const cc_backend_varmap* rvmap)
{
    assert(
        !(lvmap->flags == VARMAP_CONSTANT && rvmap->flags == VARMAP_CONSTANT));

    /* No need for move */
    if (lvmap->flags == VARMAP_REGISTER && rvmap->flags == VARMAP_REGISTER
        && lvmap->regno == rvmap->regno)
        return true;

    /* Constant 0 */
    if (lvmap->flags == VARMAP_REGISTER && rvmap->flags == VARMAP_CONSTANT
        && rvmap->constant == 0) {
        fprintf(ctx->out, "\txorl\t%s, %s\n", reg_names[lvmap->regno],
            reg_names[lvmap->regno]);
        return true;
    }

    /* General mov operand modes */
    if ((lvmap->flags == VARMAP_REGISTER || lvmap->flags == VARMAP_STACK
            || lvmap->flags == VARMAP_THREAD_LOCAL)
        && rvmap->flags == VARMAP_REGISTER) {
        fprintf(ctx->out, "\tmovl\t");
        cc_as386_print_varmap(ctx, rvmap);
        fprintf(ctx->out, ", ");
        cc_as386_print_varmap(ctx, lvmap);
        fprintf(ctx->out, "\n");
        return true;
    } else if (lvmap->flags == VARMAP_REGISTER
        && (rvmap->flags == VARMAP_REGISTER || rvmap->flags == VARMAP_STACK
            || rvmap->flags == VARMAP_THREAD_LOCAL
            || rvmap->flags == VARMAP_CONSTANT)) {
        fprintf(ctx->out, "\tmovl\t");
        cc_as386_print_varmap(ctx, rvmap);
        fprintf(ctx->out, ", ");
        cc_as386_print_varmap(ctx, lvmap);
        fprintf(ctx->out, "\n");
        return true;
    } else if (lvmap->flags == VARMAP_STACK
        && (rvmap->flags == VARMAP_REGISTER
            || rvmap->flags == VARMAP_CONSTANT)) {
        fprintf(ctx->out, "\tmovl\t");
        cc_as386_print_varmap(ctx, rvmap);
        fprintf(ctx->out, ", ");
        cc_as386_print_varmap(ctx, lvmap);
        fprintf(ctx->out, "\n");
        return true;
    }

    if (lvmap->flags == VARMAP_LITERAL || rvmap->flags == VARMAP_LITERAL) {
        unsigned int branch_label_id = cc_backend_get_labelnum(ctx);
        unsigned int literal_label_id = cc_backend_get_labelnum(ctx);

        fprintf(ctx->out, "\tjmp\tL%i\n", branch_label_id);
        fprintf(ctx->out, "L%i:\n", literal_label_id);
        if (lvmap->flags == VARMAP_LITERAL)
            fprintf(ctx->out, "\t.string \"%s\"\n", lvmap->data);
        else if (rvmap->flags == VARMAP_LITERAL)
            fprintf(ctx->out, "\t.string \"%s\"\n", rvmap->data);
        fprintf(ctx->out, "L%i:\n", branch_label_id);

        if (lvmap->flags == VARMAP_REGISTER && rvmap->flags == VARMAP_LITERAL) {
            fprintf(ctx->out, "\tmovl\tL%i, %s\n", literal_label_id,
                reg_names[lvmap->regno]);
        } else if (lvmap->flags == VARMAP_LITERAL
            && rvmap->flags == VARMAP_REGISTER) {
            fprintf(ctx->out, "\tmovl\t%s, L%i\n", reg_names[rvmap->regno],
                literal_label_id);
        } else if (lvmap->flags == VARMAP_STACK
            && rvmap->flags == VARMAP_LITERAL) {
            fprintf(ctx->out, "\tmovl\tL%i, -%u(%%ebp)\n", literal_label_id,
                lvmap->offset);
        } else if (lvmap->flags == VARMAP_LITERAL
            && rvmap->flags == VARMAP_STACK) {
            fprintf(ctx->out, "\tmovl\t-%u(%%ebp), L%i\n", rvmap->offset,
                literal_label_id);
        }
        return true;
    }

    if (lvmap->flags == VARMAP_STACK && rvmap->flags == VARMAP_STACK) {
        cc_backend_varmap mvmap = {};
        cc_backend_spill(ctx, 1);
        mvmap.regno = cc_backend_alloc_register(ctx);
        mvmap.flags = VARMAP_REGISTER;

        fprintf(ctx->out, "\tmovl\t-%u(%%ebp), %s\n", rvmap->offset,
            reg_names[mvmap.regno]);
        fprintf(ctx->out, "\tmovl\t%s, -%u(%%ebp)\n", reg_names[mvmap.regno],
            lvmap->offset);
        return true;
    }

    cc_diag_error(ctx, "Impossible constraints for move from %i->%i",
        rvmap->flags, lvmap->flags);
    return false;
}

void cc_as386_gen_epilogue(cc_context* ctx, const cc_ast_node* node)
{
    if (node->type == AST_NODE_BLOCK) {
        for (size_t i = 0; i < node->data.block.n_vars; i++) {
            const cc_ast_variable* bvar = &node->data.block.vars[i];
            ctx->backend_data->stack_frame_size
                += cc_as386_get_sizeof(ctx, &bvar->type);
            cc_backend_add_stack_var(ctx, bvar);
        }
    }
    fprintf(ctx->out, "#epilogue\n");
    fprintf(ctx->out, "\tpushl\t%%ebp\n");
    fprintf(ctx->out, "\tandl\t-$%u, %%esp\n",
        ctx->backend_data->min_stack_alignment);
    fprintf(ctx->out, "\tmovl\t%%esp, %%ebp\n");
    fprintf(
        ctx->out, "\tsubl\t$%u, %%esp\n", ctx->backend_data->stack_frame_size);
}

cc_backend_varmap cc_as386_get_call_retval(
    cc_context* ctx, const cc_ast_node* node)
{
    cc_backend_varmap vmap = {};
    cc_backend_reserve_reg(ctx, AS386_EAX);
    vmap.regno = AS386_EAX;
    vmap.flags = VARMAP_REGISTER;
    return vmap;
}

/* Generate a jump to the given node */
_Bool cc_as386_gen_jump(cc_context *ctx, const cc_ast_node *node)
{
    fprintf(ctx->out, "\tjmp\tL%i\n", node->label_id);
    return true;
}

_Bool cc_as386_gen_call(cc_context* ctx, const cc_ast_node* node)
{
    switch (node->type) {
    case AST_NODE_VARIABLE: {
        const cc_ast_variable* var
            = cc_ast_find_variable(node->data.var.name, node);
        fprintf(ctx->out, "\tcall\t%s\n", var->name);
    }
        return true;
    default:
        return false;
    }
    return false;
}

_Bool cc_as386_gen_unop(cc_context* ctx, const cc_backend_varmap* lvmap,
    const cc_backend_varmap* rvmap, enum cc_ast_unop_type type)
{
    if ((lvmap->flags == VARMAP_STACK || lvmap->flags == VARMAP_STATIC
            || lvmap->flags == VARMAP_THREAD_LOCAL)
        && (rvmap->flags == VARMAP_STACK || rvmap->flags == VARMAP_STATIC
            || rvmap->flags == VARMAP_THREAD_LOCAL))
        return false;

    switch (type) {
    case AST_UNOP_DEREF: {
        cc_backend_spill(ctx, 1);
        cc_backend_varmap nlvmap = {};
        nlvmap.regno = cc_backend_alloc_register(ctx);
        nlvmap.flags = VARMAP_REGISTER;
        fprintf(ctx->out, "\tleal\t");
        cc_as386_print_varmap(ctx, rvmap);
        fprintf(ctx->out, ", ");
        cc_as386_print_varmap(ctx, &nlvmap);
        fprintf(ctx->out, "\n");

        fprintf(ctx->out, "\tmovl\t(");
        cc_as386_print_varmap(ctx, &nlvmap);
        fprintf(ctx->out, "), ");
        cc_as386_print_varmap(ctx, lvmap);
        fprintf(ctx->out, "\n");
    } break;
    case AST_UNOP_REF:
        break;
    default:
        cc_diag_error(ctx, "Unrecognized unop type %u", type);
        break;
    }
    return true;
}

_Bool cc_as386_gen_binop(cc_context* ctx, const cc_backend_varmap* lvmap,
    const cc_backend_varmap* rvmap, enum cc_ast_binop_type type)
{
    if ((lvmap->flags == VARMAP_STACK || lvmap->flags == VARMAP_STATIC
            || lvmap->flags == VARMAP_THREAD_LOCAL)
        && (rvmap->flags == VARMAP_STACK || rvmap->flags == VARMAP_STATIC
            || rvmap->flags == VARMAP_THREAD_LOCAL))
        return false;

    switch (type) {
    case AST_BINOP_PLUS:
    case AST_BINOP_MINUS:
    case AST_BINOP_AND:
    case AST_BINOP_OR:
    case AST_BINOP_XOR: {
        const char* insn = NULL;
        switch (type) {
        case AST_BINOP_PLUS:
            insn = "add";
            break;
        case AST_BINOP_MINUS:
            insn = "sub";
            break;
        case AST_BINOP_AND:
            insn = "and";
            break;
        case AST_BINOP_OR:
            insn = "or";
            break;
        case AST_BINOP_XOR:
            insn = "xor";
            break;
        default:
            break;
        }
        fprintf(ctx->out, "\t%sl\t", insn);
        cc_as386_print_varmap(ctx, rvmap);
        fprintf(ctx->out, ", ");
        cc_as386_print_varmap(ctx, lvmap);
        fprintf(ctx->out, "\n");
    } break;
    case AST_BINOP_MUL:
        fprintf(ctx->out, "\tmull\t");
        cc_as386_print_varmap(ctx, rvmap);
        fprintf(ctx->out, ", ");
        cc_as386_print_varmap(ctx, lvmap);
        fprintf(ctx->out, "\n");
        break;
    case AST_BINOP_DIV:
        fprintf(ctx->out, "\tdivl\t");
        cc_as386_print_varmap(ctx, rvmap);
        fprintf(ctx->out, ", ");
        cc_as386_print_varmap(ctx, lvmap);
        fprintf(ctx->out, "\n");
        break;
    case AST_BINOP_LSHIFT:
        fprintf(ctx->out, "\tshll\t");
        cc_as386_print_varmap(ctx, rvmap);
        fprintf(ctx->out, ", ");
        cc_as386_print_varmap(ctx, lvmap);
        fprintf(ctx->out, "\n");
        break;
    case AST_BINOP_RSHIFT:
        fprintf(ctx->out, "\tshrl\t");
        cc_as386_print_varmap(ctx, rvmap);
        fprintf(ctx->out, ", ");
        cc_as386_print_varmap(ctx, lvmap);
        fprintf(ctx->out, "\n");
        break;
    case AST_BINOP_GT:
    case AST_BINOP_GTE:
    case AST_BINOP_LT:
    case AST_BINOP_LTE:
    case AST_BINOP_COND_EQ:
    case AST_BINOP_COND_NEQ: {
        unsigned int branch_lnum = cc_backend_get_labelnum(ctx);
        unsigned int finish_lnum = cc_backend_get_labelnum(ctx);
        cc_backend_varmap constant = {};
        constant.flags = VARMAP_CONSTANT;

        const char* jmp_insn = "jmp";
        switch (type) {
        case AST_BINOP_GT:
            jmp_insn = "jg";
            break;
        case AST_BINOP_GTE:
            jmp_insn = "jge";
            break;
        case AST_BINOP_LT:
            jmp_insn = "jl";
            break;
        case AST_BINOP_LTE:
            jmp_insn = "jle";
            break;
        case AST_BINOP_COND_EQ:
            jmp_insn = "je";
            break;
        case AST_BINOP_COND_NEQ:
            jmp_insn = "jne";
            break;
        default:
            break;
        }

        fprintf(ctx->out, "\tcmp\t%s, %s\n", reg_names[lvmap->regno],
            reg_names[rvmap->regno]);
        fprintf(ctx->out, "\t%s\tL%u\n", jmp_insn, branch_lnum);
        constant.constant = 1ul;
        ctx->backend_data->gen_mov(ctx, lvmap, &constant);
        fprintf(ctx->out, "\tjmp\tL%u\n", finish_lnum);

        fprintf(ctx->out, "L%u:\n", branch_lnum);
        constant.constant = 0ul;
        ctx->backend_data->gen_mov(ctx, lvmap, &constant);

        fprintf(ctx->out, "L%u:\n", finish_lnum);
    } break;
    default:
        cc_diag_error(ctx, "Unrecognized binop type %u", type);
        break;
    }
    return true;
}

void cc_as386_gen_prologue(cc_context* ctx, const cc_ast_node* node)
{
    fprintf(ctx->out, "#prologue\n");
    cc_backend_unspill(ctx);
    if (node != NULL) {
        /* TODO: Generate & return on EAX */
        cc_backend_varmap lvmap = {};
        cc_backend_reserve_reg(ctx, AS386_EAX);
        lvmap.flags = VARMAP_REGISTER;
        lvmap.regno = AS386_EAX;
        cc_backend_process_node(ctx, node, &lvmap);
    }
    fprintf(ctx->out, "\tleave\n");
    fprintf(ctx->out, "\tret\n");
}

_Bool cc_as386_map_variable(cc_context* ctx, const cc_ast_variable* var)
{
    if (var->type.mode == TYPE_MODE_FUNCTION)
        return false;
    if (var->type.storage == STORAGE_STATIC) {
        cc_backend_add_static_var(ctx, var);
        fprintf(ctx->out, "%s:\n", var->name);
        fprintf(ctx->out, "\t.zero %u\n", cc_as386_get_sizeof(ctx, &var->type));
    } else if (var->type.storage == STORAGE_AUTO) {
        cc_backend_add_stack_var(ctx, var);
        fprintf(ctx->out, "#stack-var %s\n", var->name);
    }
    return true;
}

int cc_as386_top(cc_context* ctx)
{
    ctx->asgen_data = cc_zalloc(sizeof(cc_as386_context));
    cc_backend_init(ctx, reg_names, AS386_NUM_REGS);
    ctx->backend_data->min_stack_alignment = 16;
    ctx->backend_data->is_reserved = &cc_as386_is_reserved_reg;
    ctx->backend_data->get_sizeof = &cc_as386_get_sizeof;
    ctx->backend_data->gen_mov = &cc_as386_gen_mov;
    ctx->backend_data->get_call_retval = &cc_as386_get_call_retval;
    ctx->backend_data->map_variable = &cc_as386_map_variable;
    ctx->backend_data->gen_epilogue = &cc_as386_gen_epilogue;
    ctx->backend_data->gen_prologue = &cc_as386_gen_prologue;
    ctx->backend_data->gen_call = &cc_as386_gen_call;
    ctx->backend_data->gen_jump = &cc_as386_gen_jump;
    ctx->backend_data->gen_binop = &cc_as386_gen_binop;
    ctx->backend_data->gen_unop = &cc_as386_gen_unop;
    ctx->stage = STAGE_AST;
    cc_backend_process_node(ctx, ctx->root, NULL);
    cc_backend_deinit(ctx);
    cc_free(ctx->asgen_data);
    return 0;
}
