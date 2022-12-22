/* mf370.c - Assembly code generation for i370 machines */
#include "mf370.h"
#include "ast.h"
#include "context.h"
#include "diag.h"
#include "parser.h"
#include "ssa.h"
#include "util.h"
#include <assert.h>
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
    MF370_NUM_REGS,
};

static const char* reg_names[MF370_NUM_REGS] = { "R0", "R1", "R2", "R3", "R4",
    "R5", "R6", "R7", "R8", "R9", "R10", "R11", "R12", "R13", "R14", "R15" };

typedef struct cc_mf370_context {
    bool regs[MF370_NUM_REGS];
} cc_mf370_context;

static cc_mf370_context* cc_mf370_get_ctx(cc_context* ctx)
{
    return (cc_mf370_context*)ctx->asgen_data;
}

static unsigned short cc_mf370_regalloc(cc_context* ctx)
{
    cc_mf370_context* actx = cc_mf370_get_ctx(ctx);
    for (size_t i = 0; i < MF370_NUM_REGS; i++) {
        if (i == MF370_R12 || i == MF370_R13 || i == MF370_R14
            || i == MF370_R15)
            continue;

        if (!actx->regs[i])
            return (enum cc_mf370_reg)i;
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
    case AST_TYPE_MODE_FUNCTION:
        return sizeof_ptr;
    case AST_TYPE_MODE_ENUM:
        return 4;
    case AST_TYPE_MODE_STRUCT: {
        size_t total = 0;
        for (size_t i = 0; i < type->data.s_or_u.n_members; i++)
            total += ctx->get_sizeof(ctx, &type->data.s_or_u.members[i].type);
        return total;
    }
    case AST_TYPE_MODE_UNION: {
        size_t upper_lim = 0;
        for (size_t i = 0; i < type->data.s_or_u.n_members; i++) {
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
    assert(type->mode == AST_TYPE_MODE_STRUCT
        || type->mode == AST_TYPE_MODE_UNION);
    
    /* Unions have no offset */
    if(type->mode == AST_TYPE_MODE_UNION)
        return 0;

    size_t upper_lim = 0;
    for (size_t i = 0; i < type->data.s_or_u.n_members; i++) {
        size_t count
            = ctx->get_sizeof(ctx, &type->data.s_or_u.members[i].type);
        if(!strcmp(type->data.s_or_u.members[i].name, field))
            return upper_lim;
        upper_lim = count > upper_lim ? count : upper_lim;
    }
    abort();
}

static const char* cc_mf370_logical_label(const char* name)
{
    static char buf[8];
    size_t n = strlen(name) >= sizeof(buf) - 1 ? sizeof(buf) : strlen(name) + 1;
    memcpy(buf, name, n);
    buf[n - 1] = '\0';

    for (size_t i = 0; i < sizeof(buf); i++) {
        if (buf[i] == '_')
            buf[i] = '@';
        buf[i] = toupper(buf[i]);
    }
    return buf;
}

static void cc_mf370_gen_assign(
    cc_context* ctx, const cc_ssa_param* lhs, const cc_ssa_param* rhs)
{
    switch (lhs->type) {
    case SSA_PARAM_VARIABLE:
        switch (rhs->type) {
        case SSA_PARAM_CONSTANT:
            fprintf(ctx->out, "\tL\tR0,%lu\n", rhs->data.constant.value.u);
            if (rhs->data.constant.is_negative)
                fprintf(ctx->out, "\tM\tR0,-1\n");
            break;
        case SSA_PARAM_VARIABLE:
            fprintf(ctx->out, "\tLA\tR0,=A(%s)\n",
                cc_mf370_logical_label(rhs->data.var_name));
            fprintf(ctx->out, "\tLR\tR0,(R0)\n");
            break;
        case SSA_PARAM_TMPVAR:
            fprintf(ctx->out, "\tLR\tR0,R0\n");
            break;
        default:
            abort();
        }
        fprintf(ctx->out, "\tLA\tR1,=A(%s)\n",
            cc_mf370_logical_label(lhs->data.var_name));
        fprintf(ctx->out, "\tST\tR0,(R1)\n");
        break;
    case SSA_PARAM_TMPVAR:
        switch (rhs->type) {
        case SSA_PARAM_CONSTANT:
            fprintf(ctx->out, "\tL\tR0,%lu\n", rhs->data.constant.value.u);
            if (rhs->data.constant.is_negative)
                fprintf(ctx->out, "\tM\tR0,-1\n");
            break;
        case SSA_PARAM_VARIABLE:
            fprintf(ctx->out, "\tLA\tR0,=A(%s)\n",
                cc_mf370_logical_label(rhs->data.var_name));
            fprintf(ctx->out, "\tLR\tR0,(R0)\n");
            break;
        case SSA_PARAM_TMPVAR:
            fprintf(ctx->out, "\tLR\tR0,R0\n");
            break;
        default:
            abort();
        }
        break;
    default:
        break;
    }
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
    case SSA_TOKEN_AND: {
        cc_ssa_param lhs = tok->data.binop.left;
        cc_ssa_param rhs_1 = tok->data.binop.right;
        cc_ssa_param rhs_2 = tok->data.binop.extra;

        cc_ssa_param tmp = cc_ssa_tempvar_param_1(ctx, false, 4);

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

        cc_mf370_gen_assign(ctx, &lhs, &tmp);
    } break;
    case SSA_TOKEN_ASSIGN: {
        cc_mf370_gen_assign(ctx, &tok->data.unop.left, &tok->data.unop.right);
    } break;
    default:
        break;
        abort();
    }
}

void cc_mf370_process_func(cc_context* ctx, const cc_ssa_func* func)
{
    const char* name = func->ast_var->name;
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
    for (size_t i = 0; i < func->n_tokens; i++)
        cc_mf370_process_token(ctx, &func->tokens[i]);

    fprintf(ctx->out, "\tPOP\tUSING\n");
    fprintf(ctx->out, "\tLTORG\t,\n");
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
