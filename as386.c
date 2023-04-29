/* as386.c - Assembly code generation for 386 machines */
#include "as386.h"
#include "ast.h"
#include "context.h"
#include "diag.h"
#include "parser.h"
#include "ssa.h"
#include "util.h"
#include <assert.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

enum cc_as386_reg_group { AS386_REG_GROUP_ALL };

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
    AS386_NUM_REGS
};

static const char* reg_names[AS386_NUM_REGS]
    = { "%eax", "%ebx", "%ecx", "%edx", "%esi", "%edi", "%ebp", "%esp" };

typedef struct cc_as386_context {
    bool regs[AS386_NUM_REGS];
    bool temp[AS386_NUM_REGS];
    unsigned int reg_mapping[AS386_NUM_REGS]; /* Reg mapping for tmpids */
    unsigned int stack_offset;
    bool s_text;
    bool s_data;
} cc_as386_context;

static cc_as386_context* cc_as386_get_ctx(cc_context* ctx)
{
    return (cc_as386_context*)ctx->asgen_data;
}

static bool cc_as386_is_alloc_reg(enum cc_as386_reg regno)
{
    return !(regno == AS386_EBP || regno == AS386_ESP);
}

static enum cc_as386_reg cc_as386_regalloc(cc_context* ctx)
{
    cc_as386_context* actx = cc_as386_get_ctx(ctx);
    size_t i;
    for (i = 0; i < AS386_NUM_REGS; i++) {
        if (!cc_as386_is_alloc_reg(i))
            continue;
        if (!actx->regs[i]) {
            actx->reg_mapping[i] = 0;
            actx->regs[i] = true;
            actx->temp[i] = false;
            return (enum cc_as386_reg)i;
        }
    }
    abort();
}

static enum cc_as386_reg cc_as386_regalloc_tmp(
    cc_context* ctx, unsigned int tmpid)
{
    cc_as386_context* actx = cc_as386_get_ctx(ctx);
    size_t i;
    for (i = 0; i < AS386_NUM_REGS; i++) {
        if (!cc_as386_is_alloc_reg(i))
            continue;
        if (!actx->regs[i]) {
            actx->reg_mapping[i] = tmpid;
            actx->regs[i] = true;
            actx->temp[i] = false;
            return (enum cc_as386_reg)i;
        }
    }
    abort();
}

static void cc_as386_regfree(cc_context* ctx, enum cc_as386_reg regno)
{
    cc_as386_context* actx = cc_as386_get_ctx(ctx);
    actx->regs[regno] = false;
}

static void cc_as386_regfree_tmpid(cc_context* ctx, unsigned int tmpid)
{
    cc_as386_context* actx = cc_as386_get_ctx(ctx);
    size_t i;
    for (i = 0; i < AS386_NUM_REGS; i++) {
        if (!cc_as386_is_alloc_reg(i))
            continue;

        if (actx->regs[i] && actx->reg_mapping[i] == tmpid) {
            cc_as386_regfree(ctx, i);
            return;
        }
    }
    abort();
}

static enum cc_as386_reg cc_as386_get_tmpreg(
    cc_context* ctx, unsigned int tmpid)
{
    cc_as386_context* actx = cc_as386_get_ctx(ctx);
    size_t i;
    for (i = 0; i < AS386_NUM_REGS; i++) {
        if (!cc_as386_is_alloc_reg(i))
            continue;

        if (actx->regs[i] && actx->reg_mapping[i] == tmpid)
            return i;
    }
    abort();
}

static unsigned int cc_as386_get_alignof(
    cc_context* ctx, const cc_ast_type* type)
{
    return 0;
}

static unsigned int cc_as386_get_sizeof(
    cc_context* ctx, const cc_ast_type* type)
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
    case AST_TYPE_MODE_FLOAT:
    case AST_TYPE_MODE_DOUBLE:
        return 4;
    case AST_TYPE_MODE_FUNCTION:
        return sizeof_ptr;
    case AST_TYPE_MODE_ENUM:
        return 4;
    case AST_TYPE_MODE_STRUCT: {
        size_t total = 0;
        size_t i;
        for (i = 0; i < type->data.s_or_u.n_members; i++)
            total += ctx->get_sizeof(ctx, &type->data.s_or_u.members[i].type);
        return total;
    }
    case AST_TYPE_MODE_UNION: {
        size_t upper_lim = 0;
        size_t i;
        for (i = 0; i < type->data.s_or_u.n_members; i++) {
            size_t count
                = ctx->get_sizeof(ctx, &type->data.s_or_u.members[i].type);
            upper_lim = count > upper_lim ? count : upper_lim;
        }
        return upper_lim;
    }
    default:
        break;
    }
    cc_diag_error(
        ctx, "Unknown sizeof for %i(*%i)", type->mode, type->n_cv_qual);
    return 0;
}

static unsigned int cc_as386_get_offsetof(
    cc_context* ctx, const cc_ast_type* type, const char* field)
{
    size_t upper_lim = 0;
    size_t i;
    assert(type->mode == AST_TYPE_MODE_STRUCT
        || type->mode == AST_TYPE_MODE_UNION);

    /* Unions have no offset */
    if (type->mode == AST_TYPE_MODE_UNION)
        return 0;

    for (i = 0; i < type->data.s_or_u.n_members; i++) {
        size_t count = ctx->get_sizeof(ctx, &type->data.s_or_u.members[i].type);
        if (!strcmp(type->data.s_or_u.members[i].name, field))
            return upper_lim;
        upper_lim = count > upper_lim ? count : upper_lim;
    }
    abort();
}

static void cc_as386_gen_assign(
    cc_context* ctx, const cc_ssa_param* lhs, const cc_ssa_param* rhs)
{
    /* Redundant gens? */
    if (cc_ssa_is_param_same(lhs, rhs))
        return;

    switch (lhs->type) {
    case SSA_PARAM_VARIABLE: {
        enum cc_as386_reg val_regno = cc_as386_regalloc(ctx);
        enum cc_as386_reg ptr_regno = cc_as386_regalloc(ctx);
        switch (rhs->type) {
        case SSA_PARAM_CONSTANT:
            fprintf(ctx->out, "\tmovl\t%s,$%lu\n", reg_names[val_regno],
                rhs->data.constant.value.u);
            if (rhs->data.constant.is_negative)
                fprintf(ctx->out, "\tmull\t$-1,%s\n", reg_names[val_regno]);
            break;
        case SSA_PARAM_VARIABLE:
            fprintf(ctx->out, "\tmovl\t$_%s,%s\n", rhs->data.var_name,
                reg_names[val_regno]);
            break;
        case SSA_PARAM_TMPVAR:
            fprintf(ctx->out, "\tmovl\t%s,%s\n",
                reg_names[cc_as386_get_tmpreg(ctx, rhs->data.tmpid)],
                reg_names[val_regno]);
            break;
        case SSA_PARAM_STRING_LITERAL:
            fprintf(ctx->out, "\tmovl\t$__ms_%u,%s\n", rhs->data.string.tmpid,
                reg_names[val_regno]);
            break;
        default:
            abort();
        }
        fprintf(ctx->out, "\tmovl\t$%s,_%s\n", reg_names[val_regno],
            lhs->data.var_name);
        fprintf(ctx->out, "\tmovl\t$%s,%s\n", reg_names[val_regno],
            reg_names[ptr_regno]);
        cc_as386_regfree(ctx, ptr_regno);
        cc_as386_regfree(ctx, val_regno);
    } break;
    case SSA_PARAM_TMPVAR:
        switch (rhs->type) {
        case SSA_PARAM_CONSTANT:
            fprintf(ctx->out, "\tmovl\t$%lu,%s\n", rhs->data.constant.value.u,
                reg_names[cc_as386_get_tmpreg(ctx, lhs->data.tmpid)]);
            if (rhs->data.constant.is_negative)
                fprintf(ctx->out, "\timull\t$-1,%s\n",
                    reg_names[cc_as386_get_tmpreg(ctx, lhs->data.tmpid)]);
            break;
        case SSA_PARAM_VARIABLE:
            fprintf(ctx->out, "\tmovl\t$_%s,%s\n", rhs->data.var_name,
                reg_names[cc_as386_get_tmpreg(ctx, lhs->data.tmpid)]);
            break;
        case SSA_PARAM_TMPVAR:
            fprintf(ctx->out, "\tmovl\t%s,%s\n",
                reg_names[cc_as386_get_tmpreg(ctx, rhs->data.tmpid)],
                reg_names[cc_as386_get_tmpreg(ctx, lhs->data.tmpid)]);
            break;
        case SSA_PARAM_RETVAL:
            fprintf(ctx->out, "\tmovl\t%%eax,%s\n",
                reg_names[cc_as386_get_tmpreg(ctx, lhs->data.tmpid)]);
            break;
        default:
            abort();
        }
        break;
    case SSA_PARAM_RETVAL:
        switch (rhs->type) {
        case SSA_PARAM_CONSTANT:
            fprintf(
                ctx->out, "\tmovl\t$%lu,%%eax\n", rhs->data.constant.value.u);
            if (rhs->data.constant.is_negative)
                fprintf(ctx->out, "\tmull\t$-1,%%eax\n");
            break;
        case SSA_PARAM_VARIABLE:
            fprintf(ctx->out, "\tmovl\t$_%s,%%eax\n", rhs->data.var_name);
            break;
        case SSA_PARAM_TMPVAR:
            fprintf(ctx->out, "\tmovl\t%s,%%eax\n",
                reg_names[cc_as386_get_tmpreg(ctx, rhs->data.tmpid)]);
            break;
        default:
            abort();
        }
        break;
    default:
        abort();
    }
}

static void cc_as386_gen_store_from(
    cc_context* ctx, const cc_ssa_param* lhs, const cc_ssa_param* rhs)
{
    /* Redundant gens? */
    if (cc_ssa_is_param_same(lhs, rhs))
        return;

    switch (lhs->type) {
    case SSA_PARAM_VARIABLE: {
        switch (rhs->type) {
        case SSA_PARAM_VARIABLE:
            if (lhs->size == rhs->size) {
                switch (lhs->size | rhs->size) {
                case 1:
                    fprintf(ctx->out, "\tmovb\t($_%s),($_%s)\n",
                        rhs->data.var_name, lhs->data.var_name);
                    break;
                case 2:
                    fprintf(ctx->out, "\tmovw\t($_%s),($_%s)\n",
                        rhs->data.var_name, lhs->data.var_name);
                    break;
                case 4:
                    fprintf(ctx->out, "\tmovl\t($_%s),($_%s)\n",
                        rhs->data.var_name, lhs->data.var_name);
                    break;
                case 8:
                    fprintf(ctx->out, "\tmovq\t($_%s),($_%s)\n",
                        rhs->data.var_name, lhs->data.var_name);
                    break;
                default:
                    fprintf(ctx->out, "\tpushl\t%%ecx\n");
                    fprintf(ctx->out, "1:\n");
                    fprintf(ctx->out, "\tmovl\t$%u,%%ecx\n",
                        (unsigned char)lhs->size);
                    fprintf(ctx->out, "\tmovb\t($_%s),($_%s)\n",
                        rhs->data.var_name, lhs->data.var_name);
                    fprintf(ctx->out, "\tloop\t1b\n");
                    fprintf(ctx->out, "\tpopl\t%%ecx\n");
                    break;
                }
            } else {
                abort();
            }
            break;
        case SSA_PARAM_TMPVAR:
            if (lhs->size == rhs->size) {
                switch (lhs->size | rhs->size) {
                case 1:
                    fprintf(ctx->out, "\tmovb\t%s,($_%s)\n",
                        reg_names[cc_as386_get_tmpreg(ctx, rhs->data.tmpid)],
                        lhs->data.var_name);
                    break;
                case 2:
                    fprintf(ctx->out, "\tmovw\t%s,($_%s)\n",
                        reg_names[cc_as386_get_tmpreg(ctx, rhs->data.tmpid)],
                        lhs->data.var_name);
                    break;
                case 4:
                    fprintf(ctx->out, "\tmovl\t%s,($_%s)\n",
                        reg_names[cc_as386_get_tmpreg(ctx, rhs->data.tmpid)],
                        lhs->data.var_name);
                    break;
                case 8:
                    fprintf(ctx->out, "\tmovq\t%s,($_%s)\n",
                        reg_names[cc_as386_get_tmpreg(ctx, rhs->data.tmpid)],
                        lhs->data.var_name);
                    break;
                default:
                    abort();
                }
            } else {
                abort();
            }
            break;
        default:
            abort();
        }
    } break;
    default:
        abort();
    }
}

static void cc_as386_gen_load_from(
    cc_context* ctx, const cc_ssa_param* lhs, const cc_ssa_param* rhs)
{
    /* Redundant gens? */
    if (cc_ssa_is_param_same(lhs, rhs))
        return;

    abort();
}

static void cc_as386_gen_call_param(
    cc_context* ctx, const cc_ssa_param* param, unsigned short offset)
{
    cc_ssa_param tmp
        = cc_ssa_tempvar_param_1(ctx, param->is_signed, param->size);
    cc_as386_regalloc_tmp(ctx, tmp.data.tmpid);
    switch (param->type) {
    case SSA_PARAM_CONSTANT:
        fprintf(ctx->out, "\tmovl\t%lu,%u(%%esp)\n",
            param->data.constant.value.u, offset);
        break;
    case SSA_PARAM_VARIABLE:
        fprintf(ctx->out, "\tmovl\t$_%s,%%edi\n", param->data.var_name);
        fprintf(ctx->out, "\tmovl\t%%edi,%u(%%esp)\n", offset);
        break;
    case SSA_PARAM_TMPVAR:
        fprintf(ctx->out, "\tmovl\t%s,%u(%%esp)\n",
            reg_names[cc_as386_get_tmpreg(ctx, param->data.tmpid)], offset);
        break;
    case SSA_PARAM_RETVAL:
        fprintf(ctx->out, "\tmovl\t%%eax,%u(%%esp)\n", offset);
        break;
    case SSA_PARAM_STRING_LITERAL:
        fprintf(ctx->out, "\tmovl\t$__ms_%u,%s\n", param->data.string.tmpid,
            reg_names[cc_as386_get_tmpreg(ctx, tmp.data.tmpid)]);
        fprintf(ctx->out, "\tmovl\t%s,%u(%%esp)\n",
            reg_names[cc_as386_get_tmpreg(ctx, tmp.data.tmpid)], offset);
        break;
    default:
        abort();
    }
    cc_as386_regfree_tmpid(ctx, tmp.data.tmpid);
}

static void cc_as386_process_call(cc_context* ctx, const cc_ssa_token* tok)
{
    const cc_ssa_param* call_param = &tok->data.call.right;
    const cc_ssa_param* call_retval = &tok->data.call.left;
    unsigned short offset = 0;
    size_t i;

    assert(tok->type == SSA_TOKEN_CALL);
    for (i = 0; i < tok->data.call.n_params; i++) {
        const cc_ssa_param* param = &tok->data.call.params[i];
        cc_as386_gen_call_param(ctx, param, offset);
        offset += param->size;
    }

    switch (call_param->type) {
    case SSA_PARAM_VARIABLE:
        fprintf(ctx->out, "\tcall\t_%s\n", call_param->data.var_name);
        break;
    case SSA_PARAM_TMPVAR:
        fprintf(ctx->out, "\tcall\t%s\n",
            reg_names[cc_as386_get_tmpreg(ctx, call_param->data.tmpid)]);
        break;
    case SSA_PARAM_RETVAL:
        fprintf(ctx->out, "\tcall\t%%eax\n");
        break;
    default:
        abort();
    }

    switch (call_retval->type) {
    case SSA_PARAM_VARIABLE: {
        enum cc_as386_reg val_regno = cc_as386_regalloc(ctx);
        enum cc_as386_reg ptr_regno = cc_as386_regalloc(ctx);
        fprintf(ctx->out, "\tmovl\t%%eax,%s\n", reg_names[val_regno]);
        fprintf(ctx->out, "\tmovl\t$%s,($_%s)\n", reg_names[val_regno],
            call_retval->data.var_name);
        fprintf(ctx->out, "\tmovl\t$%s,%s\n", reg_names[val_regno],
            reg_names[ptr_regno]);
        cc_as386_regfree(ctx, ptr_regno);
        cc_as386_regfree(ctx, val_regno);
    } break;
    case SSA_PARAM_TMPVAR:
        fprintf(ctx->out, "\tmovl\t%%eax,%s\n",
            reg_names[cc_as386_get_tmpreg(ctx, call_retval->data.tmpid)]);
        break;
    case SSA_PARAM_RETVAL:
        /* Tail call, simply goes straight to eax :-) */
        break;
    default:
        abort();
    }
}

static void cc_as386_process_branch(cc_context* ctx, const cc_ssa_token* tok)
{
    const cc_ssa_param* on_true_param;
    const cc_ssa_param* on_false_param;
    assert(tok->type == SSA_TOKEN_BRANCH);

    on_true_param = &tok->data.branch.t_branch;
    switch (on_true_param->type) {
    case SSA_PARAM_VARIABLE:
        fprintf(ctx->out, "\tje\t_%s\n", on_true_param->data.var_name);
        break;
    case SSA_PARAM_TMPVAR:
        fprintf(ctx->out, "\tje\t%s\n",
            reg_names[cc_as386_get_tmpreg(ctx, on_true_param->data.tmpid)]);
        break;
    case SSA_PARAM_RETVAL:
        fprintf(ctx->out, "\tje\t%%eax\n");
        break;
    case SSA_PARAM_LABEL:
        fprintf(ctx->out, "\tje\tL%i\n", on_true_param->data.label_id);
        break;
    default:
        abort();
    }

    on_false_param = &tok->data.branch.f_branch;
    switch (on_false_param->type) {
    case SSA_PARAM_VARIABLE:
        fprintf(ctx->out, "\tjmp\t$_%s\n", on_false_param->data.var_name);
        break;
    case SSA_PARAM_TMPVAR:
        fprintf(ctx->out, "\tjmp\t%s\n",
            reg_names[cc_as386_get_tmpreg(ctx, on_false_param->data.tmpid)]);
        break;
    case SSA_PARAM_RETVAL:
        fprintf(ctx->out, "\tjmp\t%%eax\n");
        break;
    case SSA_PARAM_LABEL:
        fprintf(ctx->out, "\tjmp\tL%i\n", on_false_param->data.label_id);
        break;
    default:
        abort();
    }
}

static void cc_as386_gen_binop_arith(cc_context* ctx, const cc_ssa_token* tok)
{
    const char* insn_name;
    cc_ssa_param lhs = tok->data.binop.left;
    cc_ssa_param rhs[2];
    cc_ssa_param tmp[2];

    rhs[0] = tok->data.binop.right;
    rhs[1] = tok->data.binop.extra;

    tmp[0] = cc_ssa_tempvar_param_1(ctx, false, 4);
    tmp[1] = cc_ssa_tempvar_param_1(ctx, false, 4);

    cc_as386_regalloc_tmp(ctx, tmp[0].data.tmpid);
    cc_as386_regalloc_tmp(ctx, tmp[1].data.tmpid);

    cc_as386_gen_assign(ctx, &tmp[0], &rhs[0]);
    cc_as386_gen_assign(ctx, &tmp[1], &rhs[1]);

    switch (tok->type) {
    case SSA_TOKEN_ADD:
        insn_name = "add";
        break;
    case SSA_TOKEN_SUB:
        insn_name = "sub";
        break;
    case SSA_TOKEN_MUL:
        insn_name = "mul";
        break;
    case SSA_TOKEN_DIV:
        insn_name = "div";
        break;
    case SSA_TOKEN_OR:
        insn_name = "or";
        break;
    case SSA_TOKEN_XOR:
        insn_name = "xor";
        break;
    case SSA_TOKEN_AND:
        insn_name = "and";
        break;
    case SSA_TOKEN_LT:
    case SSA_TOKEN_LTE:
    case SSA_TOKEN_GT:
    case SSA_TOKEN_GTE:
    case SSA_TOKEN_EQ:
    case SSA_TOKEN_NEQ: {
        insn_name = tok->type == SSA_TOKEN_LT ? "jl"
            : tok->type == SSA_TOKEN_LTE      ? "jle"
            : tok->type == SSA_TOKEN_GT       ? "jg"
            : tok->type == SSA_TOKEN_GTE      ? "jge"
            : tok->type == SSA_TOKEN_EQ       ? "je"
                                              : "jne";
        fprintf(ctx->out, "\t%s\t1f\n", insn_name);
        fprintf(ctx->out, "1:\n");
        fprintf(ctx->out, "\tmov\t$1,%s\n",
            reg_names[cc_as386_get_tmpreg(ctx, tmp[0].data.tmpid)]);
        fprintf(ctx->out, "\tjmp\t1f\n");
        fprintf(ctx->out, "1:\n");
        fprintf(ctx->out, "\tmov\t$0,%s\n",
            reg_names[cc_as386_get_tmpreg(ctx, tmp[0].data.tmpid)]);
    }
        goto end;
    default:
        abort();
    }
    fprintf(ctx->out, "\t%sl\t%s,%s\n", insn_name,
        reg_names[cc_as386_get_tmpreg(ctx, tmp[0].data.tmpid)],
        reg_names[cc_as386_get_tmpreg(ctx, tmp[1].data.tmpid)]);

end:
    cc_as386_gen_assign(ctx, &lhs, &tmp[0]);
    cc_as386_regfree_tmpid(ctx, tmp[0].data.tmpid);
    cc_as386_regfree_tmpid(ctx, tmp[1].data.tmpid);
}

static void cc_as386_process_token(cc_context* ctx, const cc_ssa_token* tok)
{
    const cc_ssa_param* lhs = cc_ssa_get_lhs_param(tok);
    if (lhs != NULL && lhs->type == SSA_PARAM_TMPVAR)
        cc_as386_regalloc_tmp(ctx, lhs->data.tmpid);

    switch (tok->type) {
    case SSA_TOKEN_RET:
        fprintf(ctx->out, "\tmovl\t%%ebp,%%esp\n");
        fprintf(ctx->out, "\tpopl\t%%ebp\n");
        fprintf(ctx->out, "\tret\n");
        break;
    case SSA_TOKEN_LABEL:
        fprintf(ctx->out, "L%i:\n", tok->data.label_id);
        break;
    case SSA_TOKEN_ADD:
    case SSA_TOKEN_SUB:
    case SSA_TOKEN_MUL:
    case SSA_TOKEN_DIV:
    case SSA_TOKEN_OR:
    case SSA_TOKEN_XOR:
    case SSA_TOKEN_AND:
    case SSA_TOKEN_GT:
    case SSA_TOKEN_GTE:
    case SSA_TOKEN_LT:
    case SSA_TOKEN_LTE:
    case SSA_TOKEN_EQ:
    case SSA_TOKEN_NEQ:
        cc_as386_gen_binop_arith(ctx, tok);
        break;
    case SSA_TOKEN_ASSIGN:
        cc_as386_gen_assign(ctx, &tok->data.unop.left, &tok->data.unop.right);
        break;
    case SSA_TOKEN_STORE_FROM:
        cc_as386_gen_store_from(
            ctx, &tok->data.unop.left, &tok->data.unop.right);
        break;
    case SSA_TOKEN_LOAD_FROM:
        cc_as386_gen_load_from(
            ctx, &tok->data.unop.left, &tok->data.unop.right);
        break;
    case SSA_TOKEN_CALL:
        cc_as386_process_call(ctx, tok);
        break;
    case SSA_TOKEN_BRANCH:
        cc_as386_process_branch(ctx, tok);
        break;
    case SSA_TOKEN_DROP:
        cc_as386_regfree_tmpid(ctx, tok->data.dropped.data.tmpid);
        break;
    case SSA_TOKEN_ALLOCA:
        break;
    default:
        abort();
    }
}

static void cc_as386_colstring_param(cc_context* ctx, const cc_ssa_param* param)
{
    if (param->type == SSA_PARAM_STRING_LITERAL)
        fprintf(ctx->out, "__ms_%u:\n\t.ascii \"%s\\0\"\n",
            param->data.string.tmpid, param->data.string.literal);
}
/* Helper function for cc_as386_colstring_func */
static void cc_as386_colstring_call(cc_context* ctx, const cc_ssa_token* tok)
{
    size_t i;
    for (i = 0; i < tok->data.call.n_params; i++)
        cc_as386_colstring_param(ctx, &tok->data.call.params[i]);
}
/* Helper function for cc_as386_colstring_func */
static void cc_as386_colstring_alloca(cc_context* ctx, const cc_ssa_token* tok)
{
    cc_as386_context* actx = cc_as386_get_ctx(ctx);
    const cc_ssa_param* lhs = &tok->data.alloca.left;
    const cc_ssa_param* size = &tok->data.alloca.size;
    /* Alloca for variables MAY include static initializations and
       storage declarations! */
    if (lhs->type == SSA_PARAM_VARIABLE) {
        if (!actx->s_data) {
            fprintf(ctx->out, ".data\n");
            actx->s_data = true;
            actx->s_text = false;
        }

        if ((lhs->storage & SSA_STORAGE_GLOBAL) != 0
            || (lhs->storage & SSA_STORAGE_STATIC) != 0) {
            if ((lhs->storage & SSA_STORAGE_GLOBAL) != 0)
                fprintf(ctx->out, ".globl\t_%s\n", lhs->data.var_name);
            fprintf(ctx->out, "\t.align\t4\n");
            fprintf(ctx->out, "_%s:\n", lhs->data.var_name);
            assert(size->data.constant.is_float == false);
            fprintf(ctx->out, "\t.space\t%lu\n", size->data.constant.value.u);
        }
    }
}

void cc_as386_process_func(cc_context* ctx, const cc_ssa_func* func)
{
    cc_as386_context* actx = cc_as386_get_ctx(ctx);
    size_t i;

    /* Reset context for register allocation! */
    for (i = 0; i < AS386_NUM_REGS; i++)
        actx->regs[i] = false;

    if (!actx->s_text) {
        fprintf(ctx->out, ".text\n");
        actx->s_data = false;
        actx->s_text = true;
    }

    if ((func->ast_var->type.storage & AST_STORAGE_GLOBAL) != 0)
        fprintf(ctx->out, ".globl\t_%s\n", func->ast_var->name);

    fprintf(ctx->out, "_%s:\n", func->ast_var->name);
    fprintf(ctx->out, "\tpushl\t%%ebp\n");
    fprintf(ctx->out, "\tmovl\t%%esp,%%ebp\n");

    memset(actx->regs, 0, sizeof(actx->regs));

    /* Stack offset for local non-VLA variables */
    actx->stack_offset = 0;
    for (i = 0; i < func->ast_var->type.data.func.n_params; i++) {
        const cc_ast_variable* param = &func->ast_var->type.data.func.params[i];
        actx->stack_offset += ctx->get_sizeof(ctx, &param->type);
    }
    if (actx->stack_offset > 0)
        fprintf(ctx->out, "\taddl\t$%u,%%esp\n", actx->stack_offset);

    /* Perform alignment required by ABI */
    if (ctx->min_stack_alignment != 0)
        fprintf(ctx->out, "\tandl\t$%u,%%esp\n", ctx->min_stack_alignment);

    /* Process all tokens of this function */
    for (i = 0; i < func->n_tokens; i++)
        cc_as386_process_token(ctx, &func->tokens[i]);

    for (i = 0; i < func->n_tokens; i++) {
        const cc_ssa_token* tok = &func->tokens[i];
        switch (tok->type) {
        case SSA_TOKEN_ASSIGN:
        case SSA_TOKEN_ZERO_EXT:
        case SSA_TOKEN_SIGN_EXT:
        case SSA_TOKEN_LOAD_FROM:
        case SSA_TOKEN_STORE_FROM:
            cc_as386_colstring_param(ctx, &tok->data.unop.left);
            cc_as386_colstring_param(ctx, &tok->data.unop.right);
            break;
        case SSA_TOKEN_ADD:
        case SSA_TOKEN_SUB:
        case SSA_TOKEN_AND:
        case SSA_TOKEN_COMPARE:
        case SSA_TOKEN_DIV:
        case SSA_TOKEN_MUL:
        case SSA_TOKEN_OR:
        case SSA_TOKEN_XOR:
        case SSA_TOKEN_GT:
        case SSA_TOKEN_GTE:
        case SSA_TOKEN_LT:
        case SSA_TOKEN_LTE:
        case SSA_TOKEN_EQ:
        case SSA_TOKEN_NEQ:
            cc_as386_colstring_param(ctx, &tok->data.binop.left);
            cc_as386_colstring_param(ctx, &tok->data.binop.right);
            cc_as386_colstring_param(ctx, &tok->data.binop.extra);
            break;
        case SSA_TOKEN_CALL:
            cc_as386_colstring_call(ctx, tok);
            break;
        case SSA_TOKEN_ALLOCA:
            cc_as386_colstring_alloca(ctx, tok);
            break;
        case SSA_TOKEN_BRANCH:
            cc_as386_colstring_param(ctx, &tok->data.branch.eval);
            cc_as386_colstring_param(ctx, &tok->data.branch.t_branch);
            cc_as386_colstring_param(ctx, &tok->data.branch.f_branch);
            break;
        case SSA_TOKEN_RET:
        case SSA_TOKEN_LABEL:
            /* No operation. */
            break;
        case SSA_TOKEN_DROP:
            break;
        default:
            abort();
        }
    }
}

static void cc_as386_deinit(cc_context* ctx) { cc_free(ctx->asgen_data); }

int cc_as386_init(cc_context* ctx)
{
    ctx->asgen_data = cc_zalloc(sizeof(cc_as386_context));
    ctx->min_stack_alignment = 16;
    ctx->get_sizeof = &cc_as386_get_sizeof;
    ctx->get_alignof = &cc_as386_get_alignof;
    ctx->get_offsetof = &cc_as386_get_offsetof;
    ctx->process_ssa_func = &cc_as386_process_func;
    return 0;
}
