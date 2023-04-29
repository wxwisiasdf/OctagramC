/* mf370.c - Assembly code generation for i370 machines */
#include "mf370.h"
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

enum cc_mf370_reg {
    MF370_NONE = -1,
    MF370_R0 = 0,
    MF370_R1,
    MF370_R2,
    MF370_R3,
    MF370_R4,
    MF370_R5,
    MF370_R6,
    MF370_R7,
    MF370_R8,
    MF370_R9,
    MF370_R10,
    MF370_R11,
    MF370_R12,
    MF370_R13,
    MF370_R14,
    MF370_R15,
    MF370_NUM_REGS
};

static const char* reg_names[MF370_NUM_REGS] = { "R0", "R1", "R2", "R3", "R4",
    "R5", "R6", "R7", "R8", "R9", "R10", "R11", "R12", "R13", "R14", "R15" };

typedef struct cc_mf370_context {
    bool regs[MF370_NUM_REGS];
    unsigned int reg_mapping[MF370_NUM_REGS]; /* Reg mapping for tmpids */
} cc_mf370_context;

static cc_mf370_context* cc_mf370_get_ctx(cc_context* ctx)
{
    return (cc_mf370_context*)ctx->asgen_data;
}

static bool cc_mf370_is_alloc_reg(enum cc_mf370_reg regno)
{
    return !(regno == MF370_R12 || regno == MF370_R13 || regno == MF370_R14
        || regno == MF370_R15);
}

static enum cc_mf370_reg cc_mf370_regalloc(cc_context* ctx, unsigned int tmpid)
{
    cc_mf370_context* actx = cc_mf370_get_ctx(ctx);
    size_t i;
    for (i = 0; i < MF370_NUM_REGS; i++) {
        if (!cc_mf370_is_alloc_reg(i))
            continue;

        if (!actx->regs[i]) {
            actx->regs[i] = true;
            actx->reg_mapping[i] = tmpid;
            return (enum cc_mf370_reg)i;
        }
    }
    abort();
}

static void cc_mf370_regfree(cc_context* ctx, enum cc_mf370_reg regno)
{
    cc_mf370_context* actx = cc_mf370_get_ctx(ctx);
    actx->regs[regno] = false;
}

static void cc_mf370_regfree_tmpid(cc_context* ctx, unsigned int tmpid)
{
    cc_mf370_context* actx = cc_mf370_get_ctx(ctx);
    size_t i;
    for (i = 0; i < MF370_NUM_REGS; i++) {
        if (!cc_mf370_is_alloc_reg(i))
            continue;

        if (actx->regs[i] && actx->reg_mapping[i] == tmpid) {
            cc_mf370_regfree(ctx, i);
            return;
        }
    }
    abort();
}

static enum cc_mf370_reg cc_mf370_get_tmpreg(
    cc_context* ctx, unsigned int tmpid)
{
    cc_mf370_context* actx = cc_mf370_get_ctx(ctx);
    size_t i;
    for (i = 0; i < MF370_NUM_REGS; i++) {
        if (!cc_mf370_is_alloc_reg(i))
            continue;

        if (actx->regs[i] && actx->reg_mapping[i] == tmpid)
            return i;
    }
    abort();
}

static unsigned int cc_mf370_get_alignof(
    cc_context* ctx, const cc_ast_type* type)
{
    return 0;
}

static unsigned int cc_mf370_get_sizeof(
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

static unsigned int cc_mf370_get_offsetof(
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

static const char* cc_mf370_logical_label(const char* name)
{
    static char buf[8];
    size_t n = strlen(name) >= sizeof(buf) - 1 ? sizeof(buf) : strlen(name) + 1;
    size_t i;
    memcpy(buf, name, n);
    buf[n - 1] = '\0';
    for (i = 0; i < sizeof(buf); i++) {
        if (buf[i] == '_')
            buf[i] = '@';
        buf[i] = toupper(buf[i]);
    }
    return buf;
}

static void cc_mf370_gen_assign(
    cc_context* ctx, const cc_ssa_param* lhs, const cc_ssa_param* rhs)
{
    /* Redundant gen s*/
    if (cc_ssa_is_param_same(lhs, rhs))
        return;

    switch (lhs->type) {
    case SSA_PARAM_VARIABLE: {
        enum cc_mf370_reg val_regno;
        enum cc_mf370_reg ptr_regno;

        val_regno = cc_mf370_regalloc(ctx, USHRT_MAX - 1);
        switch (rhs->type) {
        case SSA_PARAM_CONSTANT:
            fprintf(ctx->out, "\tL\t%s,=F'%lu'\n", reg_names[val_regno],
                rhs->data.constant.value.u);
            if (rhs->data.constant.is_negative)
                fprintf(ctx->out, "\tM\t%s,=F'-1'\n", reg_names[val_regno]);
            break;
        case SSA_PARAM_VARIABLE:
            fprintf(ctx->out, "\tLA\t%s,=A(%s)\n", reg_names[val_regno],
                cc_mf370_logical_label(rhs->data.var_name));
            fprintf(ctx->out, "\tL\tR0,(%s)\n", reg_names[val_regno]);
            break;
        case SSA_PARAM_TMPVAR:
            fprintf(ctx->out, "\tLR\tR0,%s\n", reg_names[val_regno]);
            break;
        default:
            abort();
        }

        ptr_regno = cc_mf370_regalloc(ctx, USHRT_MAX - 2);
        fprintf(ctx->out, "\tLA\t%s,=A(%s)\n", reg_names[ptr_regno],
            cc_mf370_logical_label(lhs->data.var_name));
        fprintf(ctx->out, "\tST\t%s,(%s)\n", reg_names[val_regno],
            reg_names[ptr_regno]);
        cc_mf370_regfree(ctx, ptr_regno);
        cc_mf370_regfree(ctx, val_regno);
    } break;
    case SSA_PARAM_TMPVAR:
        switch (rhs->type) {
        case SSA_PARAM_CONSTANT:
            fprintf(ctx->out, "\tL\tR0,=F'%lu'\n", rhs->data.constant.value.u);
            if (rhs->data.constant.is_negative)
                fprintf(ctx->out, "\tM\tR0,-1\n");
            break;
        case SSA_PARAM_VARIABLE:
            fprintf(ctx->out, "\tLA\tR0,=A(%s)\n",
                cc_mf370_logical_label(rhs->data.var_name));
            fprintf(ctx->out, "\tL\tR0,(R0)\n");
            break;
        case SSA_PARAM_TMPVAR:
            fprintf(ctx->out, "\tLR\tR0,R0\n");
            break;
        default:
            abort();
        }
        break;
    case SSA_PARAM_RETVAL:
        switch (rhs->type) {
        case SSA_PARAM_CONSTANT:
            fprintf(ctx->out, "\tL\tR15,=F'%lu'\n", rhs->data.constant.value.u);
            if (rhs->data.constant.is_negative)
                fprintf(ctx->out, "\tM\tR15,-1\n");
            break;
        case SSA_PARAM_VARIABLE:
            fprintf(ctx->out, "\tLA\tR15,=A(%s)\n",
                cc_mf370_logical_label(rhs->data.var_name));
            fprintf(ctx->out, "\tL\tR15,(R0)\n");
            break;
        case SSA_PARAM_TMPVAR:
            fprintf(ctx->out, "\tLR\tR15,R0\n");
            break;
        default:
            abort();
        }
        break;
    default:
        abort();
    }
}

static void cc_mf370_gen_call_param(
    cc_context* ctx, const cc_ssa_param* param, unsigned short offset)
{
    switch (param->type) {
    case SSA_PARAM_VARIABLE:
        fprintf(ctx->out, "\tLA\tR1,=A(%s)\n",
            cc_mf370_logical_label(param->data.var_name));
        fprintf(ctx->out, "\tL\tR0,(R1)\n");
        fprintf(ctx->out, "\tST\tR0,%u(R13)\n", offset);
        break;
    case SSA_PARAM_TMPVAR:
        fprintf(ctx->out, "\tST\tR0,%u(R13)\n", offset);
        break;
    case SSA_PARAM_RETVAL:
        fprintf(ctx->out, "\tST\tR15,%u(R13)\n", offset);
        break;
    case SSA_PARAM_STRING_LITERAL:
        fprintf(ctx->out, "\tLA\tR1,=A(@@A%u)\n", param->data.string.tmpid);
        fprintf(ctx->out, "\tST\tR1,%u(R13)\n", offset);
        break;
    default:
        abort();
    }
}

static void cc_mf370_process_call(cc_context* ctx, const cc_ssa_token* tok)
{
    unsigned short offset = 0;
    size_t i;
    assert(tok->type == SSA_TOKEN_CALL);
    for (i = 0; i < tok->data.call.n_params; i++) {
        const cc_ssa_param* param = &tok->data.call.params[i];
        cc_mf370_gen_call_param(ctx, param, offset);
        offset += param->size;
    }
}

static void cc_mf370_gen_binop_arith(cc_context* ctx, const cc_ssa_token* tok)
{
    cc_ssa_param lhs = tok->data.binop.left;
    cc_ssa_param rhs[2];
    rhs[0] = tok->data.binop.right;
    rhs[1] = tok->data.binop.extra;

    cc_ssa_param tmp[2];
    tmp[0] = cc_ssa_tempvar_param_1(ctx, false, 4);
    tmp[1] = cc_ssa_tempvar_param_1(ctx, false, 4);
    cc_mf370_gen_assign(ctx, &tmp[0], &rhs[0]);
    cc_mf370_gen_assign(ctx, &tmp[1], &rhs[1]);

    const char* insn_name;
    switch (tok->type) {
    case SSA_TOKEN_ADD:
        insn_name = "A";
        break;
    case SSA_TOKEN_SUB:
        insn_name = "S";
        break;
    case SSA_TOKEN_MUL:
        insn_name = "M";
        break;
    case SSA_TOKEN_DIV:
        insn_name = "D";
        break;
    case SSA_TOKEN_OR:
        insn_name = "O";
        break;
    case SSA_TOKEN_XOR:
        insn_name = "X";
        break;
    case SSA_TOKEN_AND:
        insn_name = "N";
        break;
    default:
        abort();
    }

    fprintf(ctx->out, "\t%sR\tR0,R0\n", insn_name);
    cc_mf370_gen_assign(ctx, &lhs, &tmp[0]);
}

static void cc_mf370_process_token(cc_context* ctx, const cc_ssa_token* tok)
{
    switch (tok->type) {
    case SSA_TOKEN_RET:
        fprintf(ctx->out, "\tRETURN	(14,12),RC=(15)\n");
        break;
    case SSA_TOKEN_LABEL:
        fprintf(ctx->out, "L%-6i\tDS\t0H\n", tok->data.label_id);
        break;
    case SSA_TOKEN_ADD:
    case SSA_TOKEN_SUB:
    case SSA_TOKEN_MUL:
    case SSA_TOKEN_DIV:
    case SSA_TOKEN_OR:
    case SSA_TOKEN_XOR:
    case SSA_TOKEN_AND:
        cc_mf370_gen_binop_arith(ctx, tok);
        break;
    case SSA_TOKEN_ASSIGN:
        cc_mf370_gen_assign(ctx, &tok->data.unop.left, &tok->data.unop.right);
        break;
    case SSA_TOKEN_CALL:
        cc_mf370_process_call(ctx, tok);
        break;
    case SSA_TOKEN_ALLOCA:
        break;
    default:
        abort();
    }
}

static void cc_mf370_colstring_param(cc_context* ctx, const cc_ssa_param* param)
{
    if (param->type == SSA_PARAM_STRING_LITERAL)
        fprintf(ctx->out, "@@A%u\tDC\t'%s'\n", param->data.string.tmpid,
            param->data.string.literal);
}

/* Helper function for cc_mf370_colstring_func */
static void cc_mf370_colstring_binop(cc_context* ctx, const cc_ssa_token* tok)
{
    cc_mf370_colstring_param(ctx, &tok->data.binop.left);
    cc_mf370_colstring_param(ctx, &tok->data.binop.right);
    cc_mf370_colstring_param(ctx, &tok->data.binop.extra);
}

/* Helper function for cc_mf370_colstring_func */
static void cc_mf370_colstring_unop(cc_context* ctx, const cc_ssa_token* tok)
{
    cc_mf370_colstring_param(ctx, &tok->data.unop.left);
    cc_mf370_colstring_param(ctx, &tok->data.unop.right);
}

/* Helper function for cc_mf370_colstring_func */
static void cc_mf370_colstring_call(cc_context* ctx, const cc_ssa_token* tok)
{
    size_t i;
    for (i = 0; i < tok->data.call.n_params; i++)
        cc_mf370_colstring_param(ctx, &tok->data.call.params[i]);
}

void cc_mf370_process_func(cc_context* ctx, const cc_ssa_func* func)
{
    const char* name = func->ast_var->name;
    size_t i;

    /* TODO: I forgot how you're supposed to do alloc/drop on hlasm */
    fprintf(ctx->out, "* X-epilogue\n");
    fprintf(ctx->out, "\tPUSH\tUSING\n");
    fprintf(ctx->out, "\tDROP\t,\n");
    fprintf(ctx->out, "\tENTRY\t%-7s\n", cc_mf370_logical_label(name));
    fprintf(ctx->out, "%-7s\tDS\t0H\n", cc_mf370_logical_label(name));
    fprintf(
        ctx->out, "\tSAVE\t(R14,R12),,%-7s\n", cc_mf370_logical_label(name));
    fprintf(ctx->out, "\tLR\tR12,R15\n");
    fprintf(ctx->out, "\tUSING\tR13,R14\n");
    assert(ctx->min_stack_alignment == 0);

    /* Process all tokens of this function */
    for (i = 0; i < func->n_tokens; i++)
        cc_mf370_process_token(ctx, &func->tokens[i]);

    fprintf(ctx->out, "\tPOP\tUSING\n");
    fprintf(ctx->out, "\tLTORG\t,\n");

    for (i = 0; i < func->n_tokens; i++) {
        const cc_ssa_token* tok = &func->tokens[i];
        switch (tok->type) {
        case SSA_TOKEN_ASSIGN:
        case SSA_TOKEN_ZERO_EXT:
        case SSA_TOKEN_SIGN_EXT:
        case SSA_TOKEN_LOAD_FROM:
        case SSA_TOKEN_STORE_FROM:
            cc_mf370_colstring_unop(ctx, tok);
            break;
        case SSA_TOKEN_ADD:
        case SSA_TOKEN_SUB:
        case SSA_TOKEN_AND:
        case SSA_TOKEN_BRANCH:
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
            cc_mf370_colstring_binop(ctx, tok);
            break;
        case SSA_TOKEN_CALL:
            cc_mf370_colstring_call(ctx, tok);
            break;
        case SSA_TOKEN_RET:
        case SSA_TOKEN_LABEL:
        case SSA_TOKEN_ALLOCA:
            /* No operation */
            break;
        default:
            abort();
        }
    }

    cc_mf370_context* actx = cc_mf370_get_ctx(ctx);
    memset(actx->regs, 0, sizeof(actx->regs));
}

static void cc_mf370_deinit(cc_context* ctx) { cc_free(ctx->asgen_data); }
int cc_mf370_init(cc_context* ctx)
{
    ctx->asgen_data = cc_zalloc(sizeof(cc_mf370_context));
    ctx->min_stack_alignment = 0;
    ctx->get_sizeof = &cc_mf370_get_sizeof;
    ctx->get_alignof = &cc_mf370_get_alignof;
    ctx->get_offsetof = &cc_mf370_get_offsetof;
    ctx->process_ssa_func = &cc_mf370_process_func;
    return 0;
}
