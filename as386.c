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

enum cc_as386_reg_group {
    AS386_REG_GROUP_ALL,
};

enum cc_as386_reg {
    AS386_NONE = -1,
    AS386_EAX = 0,
    AS386_EBX,
    AS386_ECX,
    AS386_EDX,
    AS386_ESI,
    AS386_EDI,
    AS386_EBP,
    AS386_ESP,
    AS386_NUM_REGS,
};

static const char* reg_names[AS386_NUM_REGS]
    = { "%eax", "%ebx", "%ecx", "%edx", "%esi", "%edi", "%ebp", "%esp" };

cc_backend_pattern patterns[] = {
    { .insn_fmt = "\tandl\t%s,%s",
        .node = PATMAT_NODE_BINOP(AST_BINOP_AND,
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL),
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL), 32) },
    { .insn_fmt = "\torl\t%s,%s\n",
        .node = PATMAT_NODE_BINOP(AST_BINOP_OR,
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL),
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL), 32) },
    { .insn_fmt = "\txorl\t%s,%s\n",
        .node = PATMAT_NODE_BINOP(AST_BINOP_XOR,
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL),
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL), 32) },
    { .insn_fmt = "\tshll\t%s,%s\n",
        .node = PATMAT_NODE_BINOP(AST_BINOP_LSHIFT,
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL),
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL), 32) },
    { .insn_fmt = "\tshrl\t%s,%s\n",
        .node = PATMAT_NODE_BINOP(AST_BINOP_RSHIFT,
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL),
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL), 32) },
    { .insn_fmt = "\taddl\t%s,%s\n",
        .node = PATMAT_NODE_BINOP(AST_BINOP_PLUS,
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL),
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL), 32) },
    { .insn_fmt = "\tsubl\t%s,%s\n",
        .node = PATMAT_NODE_BINOP(AST_BINOP_MINUS,
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL),
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL), 32) },

    { .insn_fmt = "\tandw\t%s,%s\n",
        .node = PATMAT_NODE_BINOP(AST_BINOP_AND,
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL),
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL), 16) },
    { .insn_fmt = "\torw\t%s,%s\n",
        .node = PATMAT_NODE_BINOP(AST_BINOP_OR,
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL),
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL), 16) },
    { .insn_fmt = "\txorw\t%s,%s\n",
        .node = PATMAT_NODE_BINOP(AST_BINOP_XOR,
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL),
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL), 16) },
    { .insn_fmt = "\tshlw\t%s,%s\n",
        .node = PATMAT_NODE_BINOP(AST_BINOP_LSHIFT,
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL),
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL), 16) },
    { .insn_fmt = "\tshrw\t%s,%s\n",
        .node = PATMAT_NODE_BINOP(AST_BINOP_RSHIFT,
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL),
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL), 16) },
    { .insn_fmt = "\taddw\t%s,%s\n",
        .node = PATMAT_NODE_BINOP(AST_BINOP_PLUS,
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL),
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL), 16) },
    { .insn_fmt = "\tsubw\t%s,%s\n",
        .node = PATMAT_NODE_BINOP(AST_BINOP_MINUS,
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL),
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL), 16) },

    { .insn_fmt = "\tandb\t%s,%s\n",
        .node = PATMAT_NODE_BINOP(AST_BINOP_AND,
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL),
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL), 8) },
    { .insn_fmt = "\torb\t%s,%s\n",
        .node = PATMAT_NODE_BINOP(AST_BINOP_OR,
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL),
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL), 8) },
    { .insn_fmt = "\txorb\t%s,%s\n",
        .node = PATMAT_NODE_BINOP(AST_BINOP_XOR,
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL),
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL), 8) },
    { .insn_fmt = "\tshlb\t%s,%s\n",
        .node = PATMAT_NODE_BINOP(AST_BINOP_LSHIFT,
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL),
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL), 8) },
    { .insn_fmt = "\tshrb\t%s,%s\n",
        .node = PATMAT_NODE_BINOP(AST_BINOP_RSHIFT,
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL),
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL), 8) },
    { .insn_fmt = "\taddb\t%s,%s\n",
        .node = PATMAT_NODE_BINOP(AST_BINOP_PLUS,
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL),
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL), 8) },
    { .insn_fmt = "\tsubb\t%s,%s\n",
        .node = PATMAT_NODE_BINOP(AST_BINOP_MINUS,
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL),
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL), 8) },

    { .insn_fmt = "\tmovl\t%s,%s\n",
        .node = PATMAT_NODE_BINOP(AST_BINOP_ASSIGN,
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL),
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL), 32) },

    { .insn_fmt = "\tmovl\t%s,%s\n",
        .node = PATMAT_NODE_BINOP(AST_BINOP_ASSIGN,
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL),
            PATMAT_NODE_UNOP(AST_UNOP_DEREF, NULL, 32), 32) },
    { .insn_fmt = "\tmovl\t%s,%s\n",
        .node = PATMAT_NODE_BINOP(AST_BINOP_ASSIGN,
            PATMAT_NODE_UNOP(AST_UNOP_DEREF, NULL, 32),
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL), 32) },

    { .insn_fmt = "\tmovw\t%s,%s\n",
        .node = PATMAT_NODE_BINOP(AST_BINOP_ASSIGN,
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL),
            PATMAT_NODE_UNOP(AST_UNOP_DEREF, NULL, 16), 16) },
    { .insn_fmt = "\tmovw\t%s,%s\n",
        .node = PATMAT_NODE_BINOP(AST_BINOP_ASSIGN,
            PATMAT_NODE_UNOP(AST_UNOP_DEREF, NULL, 16),
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL), 16) },

    { .insn_fmt = "\tmovb\t%s,%s\n",
        .node = PATMAT_NODE_BINOP(AST_BINOP_ASSIGN,
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL),
            PATMAT_NODE_UNOP(AST_UNOP_DEREF, NULL, 8), 8) },
    { .insn_fmt = "\tmovb\t%s,%s\n",
        .node = PATMAT_NODE_BINOP(AST_BINOP_ASSIGN,
            PATMAT_NODE_UNOP(AST_UNOP_DEREF, NULL, 8),
            PATMAT_NODE_REGISTER(AS386_REG_GROUP_ALL), 8) },
};

typedef struct cc_as386_context {
    cc_backend_varmap latest_varmap; /* Latest relevant varmap */
} cc_as386_context;

unsigned int cc_as386_get_sizeof(cc_context* ctx, const cc_ast_type* type)
{
    size_t sizeof_ptr = 4;
    if (type->n_cv_qual > 0) /* Pointer types */
        return sizeof_ptr;

    switch (type->mode) {
    case AST_TYPE_MODE_VOID:
        return 0;
    case AST_TYPE_MODE_CHAR:
        return 1;
    case AST_TYPE_MODE_BOOL:
        return 1;
    case AST_TYPE_MODE_INT:
        return 4;
    case AST_TYPE_MODE_SHORT:
        return 2;
    case AST_TYPE_MODE_LONG:
        return 8;
    case AST_TYPE_MODE_FUNCTION:
        return sizeof_ptr;
    case AST_TYPE_MODE_ENUM:
        return 4;
    case AST_TYPE_MODE_STRUCT: {
        size_t total = 0;
        for (size_t i = 0; i < type->data.s_or_u.n_members; i++)
            total += ctx->backend_data->get_sizeof(
                ctx, &type->data.s_or_u.members[i].type);
        return total;
    }
    case AST_TYPE_MODE_UNION: {
        size_t upper_lim = 0;
        for (size_t i = 0; i < type->data.s_or_u.n_members; i++) {
            size_t count = ctx->backend_data->get_sizeof(
                ctx, &type->data.s_or_u.members[i].type);
            upper_lim = count > upper_lim ? count : upper_lim;
        }
        return upper_lim;
    }
    default:
        break;
    }
    cc_diag_error(ctx, "Unknown sizeof for %i(*%i)", type->mode, type->n_cv_qual);
    return 0;
}

bool cc_as386_is_reserved_reg(unsigned int regno)
{
    return regno == AS386_EBP || regno == AS386_ESP;
}

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
        if (vmap->literal.is_signed)
            fprintf(ctx->out, "$%li", vmap->literal.value.s);
        else
            fprintf(ctx->out, "$%lu", vmap->literal.value.u);
        break;
    default:
        cc_diag_error(ctx, "Invalid varmap %i", vmap->flags);
        break;
    }
}

bool cc_as386_gen_mov(cc_context* ctx, const cc_backend_varmap* lvmap,
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
        && rvmap->literal.value.u == 0) {
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
        unsigned short branch_label_id = cc_ast_alloc_label_id(ctx);
        unsigned short literal_label_id = cc_ast_alloc_label_id(ctx);

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
        cc_backend_varmap mvmap = { 0 };
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

bool cc_as386_gen_epilogue(
    cc_context* ctx, const cc_ast_node* node, const cc_ast_variable* var)
{
    if (node->type == AST_NODE_BLOCK) {
        for (size_t i = 0; i < node->data.block.n_vars; i++) {
            const cc_ast_variable* bvar = &node->data.block.vars[i];
            ctx->backend_data->stack_frame_size
                += cc_as386_get_sizeof(ctx, &bvar->type);
            cc_backend_add_varmap(ctx, bvar);
        }
    }
    fprintf(ctx->out, "#epilogue\n");
    fprintf(ctx->out, "\tpushl\t%%ebp\n");
    fprintf(ctx->out, "\tandl\t-$%u, %%esp\n",
        ctx->backend_data->min_stack_alignment);
    fprintf(ctx->out, "\tmovl\t%%esp, %%ebp\n");
    fprintf(
        ctx->out, "\tsubl\t$%u, %%esp\n", ctx->backend_data->stack_frame_size);
    return true;
}

cc_backend_varmap cc_as386_get_call_retval(
    cc_context* ctx, const cc_ast_node* node)
{
    cc_backend_varmap vmap = { 0 };
    cc_backend_reserve_reg(ctx, AS386_EAX);
    vmap.regno = AS386_EAX;
    vmap.flags = VARMAP_REGISTER;
    return vmap;
}

/* Generate a jump to the given node */
bool cc_as386_gen_jump(cc_context* ctx, const cc_ast_node* node)
{
    fprintf(ctx->out, "\tjmp\tL%i\n", node->label_id);
    return true;
}

bool cc_as386_gen_call(cc_context* ctx, const cc_ast_node* node)
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

bool cc_as386_gen_unop(cc_context* ctx, const cc_backend_varmap* lvmap,
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
        cc_backend_varmap nlvmap = { 0 };
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

bool cc_as386_gen_binop(cc_context* ctx, const cc_backend_varmap* lvmap,
    const cc_backend_varmap* rvmap, enum cc_ast_binop_type type,
    const cc_backend_varmap *ovmap)
{
    if ((lvmap->flags == VARMAP_STACK || lvmap->flags == VARMAP_STATIC
            || lvmap->flags == VARMAP_THREAD_LOCAL)
        && (rvmap->flags == VARMAP_STACK || rvmap->flags == VARMAP_STATIC
            || rvmap->flags == VARMAP_THREAD_LOCAL))
        return false;

    switch (type) {
    case AST_BINOP_COND_AND:
    case AST_BINOP_COND_OR:
        break;
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
        unsigned short branch_lnum = cc_ast_alloc_label_id(ctx);
        unsigned short finish_lnum = cc_ast_alloc_label_id(ctx);
        cc_backend_varmap literal = { 0 };
        literal.flags = VARMAP_CONSTANT;

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
        literal.literal = (cc_ast_literal) {
            .is_signed = false,
            .value.u = 1,
        };
        ctx->backend_data->gen_mov(ctx, lvmap, &literal);
        fprintf(ctx->out, "\tjmp\tL%u\n", finish_lnum);

        fprintf(ctx->out, "L%u:\n", branch_lnum);
        literal.literal = (cc_ast_literal) {
            .is_signed = false,
            .value.u = 0,
        };
        ctx->backend_data->gen_mov(ctx, lvmap, &literal);
        fprintf(ctx->out, "L%u:\n", finish_lnum);
    } break;
    default:
        cc_diag_error(ctx, "Unrecognized binop type %u", type);
        break;
    }
    return true;
}

bool cc_as386_gen_branch(cc_context* ctx, const cc_ast_node* node,
    const cc_backend_varmap* lvmap, const cc_backend_varmap* rvmap,
    enum cc_ast_binop_type type)
{
    fprintf(ctx->out, "#switch-case %i\n", type);
    switch (type) {
    case AST_BINOP_GT:
    case AST_BINOP_GTE:
    case AST_BINOP_LT:
    case AST_BINOP_LTE:
    case AST_BINOP_COND_EQ:
    case AST_BINOP_COND_NEQ: {
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
        fprintf(ctx->out, "\tcmp\t");
        cc_as386_print_varmap(ctx, lvmap);
        fprintf(ctx->out, ",");
        cc_as386_print_varmap(ctx, rvmap);
        fprintf(ctx->out, "\n");
        fprintf(ctx->out, "\t%s\tL%u\n", jmp_insn, node->label_id);
    }
        return true;
    default:
        cc_diag_error(ctx, "Can't generate branch for binop %i", type);
        break;
    }
    return false;
}

bool cc_as386_gen_prologue(
    cc_context* ctx, const cc_ast_node* node, const cc_ast_variable* var)
{
    assert(var != NULL);
    fprintf(ctx->out, "#prologue-for-%s\n", var->name);
    cc_backend_unspill(ctx);
    if (node != NULL) {
        /* TODO: Generate & return on EAX */
        cc_backend_varmap lvmap = { 0 };
        cc_backend_reserve_reg(ctx, AS386_EAX);
        lvmap.flags = VARMAP_REGISTER;
        lvmap.regno = AS386_EAX;
        cc_backend_process_node(ctx, node, &lvmap);
    }
    fprintf(ctx->out, "\tleave\n");
    fprintf(ctx->out, "\tret\n");
    return true;
}

bool cc_as386_map_variable(cc_context* ctx, const cc_ast_variable* var)
{
    if (var->type.mode == AST_TYPE_MODE_FUNCTION) {
        if (var->type.storage == AST_STORAGE_STATIC) {
            cc_backend_add_varmap(ctx, var);
            fprintf(ctx->out, "%s:\n", var->name);
        } else if (var->type.storage == AST_STORAGE_AUTO) {
            cc_backend_add_varmap(ctx, var);
            fprintf(ctx->out, ".global %s\n", var->name);
            fprintf(ctx->out, "%s:\n", var->name);
        }
        return true;
    }

    if (var->type.storage == AST_STORAGE_STATIC) {
        cc_backend_add_varmap(ctx, var);
        fprintf(ctx->out, "%s:\n", var->name);
        fprintf(ctx->out, "\t.zero %u\n", cc_as386_get_sizeof(ctx, &var->type));
    } else if (var->type.storage == AST_STORAGE_AUTO) {
        cc_backend_add_varmap(ctx, var);
        fprintf(ctx->out, "#stack-var %s\n", var->name);
    }
    return true;
}

static void cc_as386_deinit(cc_context* ctx) { cc_free(ctx->asgen_data); }

int cc_as386_init(cc_context* ctx)
{
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
    ctx->backend_data->gen_branch = &cc_as386_gen_branch;
    ctx->backend_data->deinit = &cc_as386_deinit;
    return 0;
}
