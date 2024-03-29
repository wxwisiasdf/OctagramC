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

/* Names for the registers in i686. */
static const char* reg32_names[AS386_NUM_REGS]
    = { "%eax", "%ebx", "%ecx", "%edx", "%esi", "%edi", "%ebp", "%esp" };
static const char* reg16_names[AS386_NUM_REGS]
    = { "%ax", "%bx", "%cx", "%dx", "%si", "%di", "%bp", "%sp" };
static const char* reg8h_names[AS386_NUM_REGS]
    = { "%ah", "%bh", "%ch", "%dh", NULL, NULL, NULL, NULL };
static const char* reg8l_names[AS386_NUM_REGS]
    = { "%al", "%bl", "%cl", "%dl", "%sil", "%dil", "%bpl", "%spl" };

typedef struct cc_as386_context {
    bool r_used[AS386_NUM_REGS]; /* Is the register currently being used? */
    bool r_pinned[AS386_NUM_REGS]; /* Can they be spilled? */
    unsigned short r_mapping[AS386_NUM_REGS]
                            [UCHAR_MAX]; /* tmpids assigned to each register */
    unsigned char r_spills
        [AS386_NUM_REGS]; /* How many times this register has been spilled? */
    bool s_text;
    bool s_data;

#define NUM_STACK_SLOTS 32
    struct cc_as386_stack_slot {
        union {
            cc_string_key var_name; /* Variables in slot */
            unsigned short tmpid;
        } data;
        unsigned int
            offset; /* Offset of the slot (relative to the start of the frame) */
        bool is_var;
    } stack_slots[NUM_STACK_SLOTS];
    unsigned char
        n_stack_slots; /* Current stack slot (out of the NUM_STACK_SLOTS) */
    unsigned int total_stack_offset;
} cc_as386_context;

static cc_as386_context* cc_as386_get_ctx(cc_context* ctx)
{
    return (cc_as386_context*)ctx->asgen_data;
}

static bool cc_as386_is_alloc_reg(enum cc_as386_reg regno)
{
    return !(regno == AS386_EBP || regno == AS386_ESP);
}

static unsigned int cc_as386_get_stack_var_offset(
    cc_context* ctx, cc_string_key key)
{
    cc_as386_context* actx = cc_as386_get_ctx(ctx);
    unsigned char i;
    for (i = 0; i < actx->n_stack_slots; ++i)
        if (actx->stack_slots[i].is_var
            && actx->stack_slots[i].data.var_name == key)
            return actx->stack_slots[i].offset;
    cc_abort(__FILE__, __LINE__);
}
static bool cc_as386_is_stack_var(cc_context* ctx, cc_string_key key)
{
    cc_as386_context* actx = cc_as386_get_ctx(ctx);
    unsigned char i;
    for (i = 0; i < actx->n_stack_slots; ++i)
        if (actx->stack_slots[i].is_var
            && actx->stack_slots[i].data.var_name == key)
            return true;
    return false;
}

static unsigned int cc_as386_get_stack_tmpid_offset(
    cc_context* ctx, unsigned short tmpid)
{
    cc_as386_context* actx = cc_as386_get_ctx(ctx);
    unsigned char i;
    for (i = 0; i < actx->n_stack_slots; ++i)
        if (!actx->stack_slots[i].is_var
            && actx->stack_slots[i].data.tmpid == tmpid)
            return actx->stack_slots[i].offset;
    cc_abort(__FILE__, __LINE__);
}

static bool cc_as386_is_stack_tmpid(cc_context* ctx, unsigned short tmpid)
{
    cc_as386_context* actx = cc_as386_get_ctx(ctx);
    unsigned char i;
    for (i = 0; i < actx->n_stack_slots; ++i)
        if (!actx->stack_slots[i].is_var
            && actx->stack_slots[i].data.tmpid == tmpid)
            return true;
    return false;
}

static enum cc_as386_reg cc_as386_get_tmpreg(
    cc_context* ctx, unsigned int tmpid)
{
    cc_as386_context* actx = cc_as386_get_ctx(ctx);
    enum cc_as386_reg regno;
    for (regno = 0; regno < AS386_NUM_REGS; ++regno) {
        if (cc_as386_is_alloc_reg(regno) && actx->r_used[regno]
            && actx->r_mapping[regno][actx->r_spills[regno]] == tmpid)
            return regno;
    }
    cc_abort(__FILE__, __LINE__);
}

/* Spills a register, returns the register that was spilled */
static enum cc_as386_reg cc_as386_regspill(cc_context* ctx)
{
    cc_as386_context* actx = cc_as386_get_ctx(ctx);
    enum cc_as386_reg least_regno = AS386_NONE;
    enum cc_as386_reg regno;
    for (regno = AS386_EAX; regno < AS386_NUM_REGS; ++regno) {
        if (!cc_as386_is_alloc_reg(regno) || actx->r_pinned[regno])
            continue;

        if (least_regno == AS386_NONE)
            least_regno = regno;
        else if (actx->r_spills[regno] <= actx->r_spills[least_regno])
            least_regno = regno;
    }
    assert(least_regno != AS386_NONE);
    assert(actx->r_used[least_regno] == true);
    ++actx->r_spills[least_regno];
    return least_regno;
}

static enum cc_as386_reg cc_as386_regalloc(cc_context* ctx, unsigned int tmpid)
{
    cc_as386_context* actx = cc_as386_get_ctx(ctx);
    enum cc_as386_reg regno;
    for (regno = AS386_EAX; regno < AS386_NUM_REGS; ++regno) {
        if (cc_as386_is_alloc_reg(regno) && !actx->r_used[regno]) {
            assert(!actx->r_spills[regno]);
            actx->r_mapping[regno][actx->r_spills[regno]] = tmpid;
            actx->r_used[regno] = true;
            return regno;
        }
    }

    regno = cc_as386_regspill(ctx);
    actx->r_mapping[regno][actx->r_spills[regno]] = tmpid;
    fprintf(ctx->out, "\tpushl\t%s\n", reg32_names[regno]);
    return regno;
}

static bool cc_as386_would_regspil(cc_context* ctx)
{
    const cc_as386_context* actx = cc_as386_get_ctx(ctx);
    enum cc_as386_reg regno;
    return true;
    for (regno = AS386_EAX; regno < AS386_NUM_REGS; ++regno)
        if (cc_as386_is_alloc_reg(regno) && !actx->r_used[regno])
            return false;
    return true;
}

static enum cc_as386_reg cc_as386_regalloc_pinned(
    cc_context* ctx, unsigned int tmpid)
{
    cc_as386_context* actx = cc_as386_get_ctx(ctx);
    enum cc_as386_reg regno = cc_as386_regalloc(ctx, tmpid);
    actx->r_pinned[regno] = true;
    return regno;
}

static void cc_as386_regfree(cc_context* ctx, enum cc_as386_reg regno)
{
    cc_as386_context* actx = cc_as386_get_ctx(ctx);
    assert(actx->r_used[regno]);
    if (actx->r_spills[regno] > 0) {
        fprintf(ctx->out, "\tpopl\t%s\n", reg32_names[regno]);
        --actx->r_spills[regno];
    } else {
        actx->r_pinned[regno] = actx->r_used[regno] = false;
    }
}

static void cc_as386_regfree_tmpid(cc_context* ctx, unsigned int tmpid)
{
    cc_as386_context* actx = cc_as386_get_ctx(ctx);
    enum cc_as386_reg regno;
    for (regno = AS386_EAX; regno < AS386_NUM_REGS; ++regno) {
        if (!cc_as386_is_alloc_reg(regno))
            continue;

        if (actx->r_used[regno]
            && actx->r_mapping[regno][actx->r_spills[regno]] == tmpid) {
            cc_as386_regfree(ctx, regno);
            return;
        }
    }
    cc_abort(__FILE__, __LINE__);
}

static const char** cc_as386_get_regset_by_size(size_t size)
{
    switch (size) {
    case 1:
        return reg8l_names;
    case 2:
        return reg16_names;
    case 4:
        return reg32_names;
    default:
        cc_abort(__FILE__, __LINE__);
    }
}

static const char* cc_as386_get_suffix_by_size(size_t size)
{
    switch (size) {
    case 1:
        return "b";
    case 2:
        return "w";
    case 4:
        return "l";
    case 8:
        return "q";
    default:
        cc_abort(__FILE__, __LINE__);
    }
}

static unsigned int cc_as386_get_alignof(
    cc_context* ctx, const cc_ast_type* type)
{
    return 0;
}

static unsigned int cc_as386_get_sizeof(
    cc_context* ctx, const cc_ast_type* type)
{
    unsigned int sizeof_ptr = 4;
    if (type->n_cv_qual > 0) {
        /* Pointer types */
        if (type->cv_qual[type->n_cv_qual].is_array) {
            cc_ast_type tmp_type = *type;
            unsigned int base_size;
            /* **Variable** sized arrays can't be deduced right-away! */
            assert(!type->cv_qual[type->n_cv_qual].is_vla);
            --tmp_type.n_cv_qual;
            base_size = cc_as386_get_sizeof(ctx, &tmp_type);
            ++tmp_type.n_cv_qual;
            return base_size * type->cv_qual[type->n_cv_qual].array.size;
        }
        return sizeof_ptr;
    }

    /* Variadic list is a pointer */
    if (type->mode == AST_TYPE_MODE_VA_LIST)
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
        for (i = 0; i < type->data.shared->s_or_u.n_members; i++)
            total += ctx->get_sizeof(
                ctx, &type->data.shared->s_or_u.members[i].type);
        return total;
    }
    case AST_TYPE_MODE_UNION: {
        size_t upper_lim = 0;
        size_t i;
        for (i = 0; i < type->data.shared->s_or_u.n_members; i++) {
            size_t count = ctx->get_sizeof(
                ctx, &type->data.shared->s_or_u.members[i].type);
            upper_lim = count > upper_lim ? count : upper_lim;
        }
        return upper_lim;
    }
    case AST_TYPE_MODE_BITINT: {
        unsigned short bits = type->data.num.bitint_bits;
        assert(bits != 0);
        return (bits + (8 - (bits % 8))) / 8;
    }
    default:
        break;
    }
    cc_diag_error(
        ctx, "Unknown sizeof for %i(*%i)", type->mode, type->n_cv_qual);
    return 0;
}

static unsigned int cc_as386_get_offsetof(
    cc_context* ctx, const cc_ast_type* type, cc_string_key field)
{
    size_t upper_lim = 0;
    size_t i;
    assert(type->mode == AST_TYPE_MODE_STRUCT
        || type->mode == AST_TYPE_MODE_UNION);

    /* Unions have no offset */
    if (type->mode == AST_TYPE_MODE_UNION)
        return 0;

    for (i = 0; i < type->data.shared->s_or_u.n_members; i++) {
        size_t count
            = ctx->get_sizeof(ctx, &type->data.shared->s_or_u.members[i].type);
        if (type->data.shared->s_or_u.members[i].name == field)
            return upper_lim;
        upper_lim = count > upper_lim ? count : upper_lim;
    }
    cc_abort(__FILE__, __LINE__);
}

static void cc_as386_print_generic_param(
    cc_context* restrict ctx, const cc_ssa_param* restrict param, bool address)
{
    switch (param->type) {
    case SSA_PARAM_VARIABLE:
        if (!cc_as386_is_stack_var(ctx, param->data.var_name)) {
            if (address)
                fprintf(ctx->out, "($_%s)", cc_strview(param->data.var_name));
            else
                fprintf(ctx->out, "$_%s", cc_strview(param->data.var_name));
        } else if (cc_as386_is_stack_var(ctx, param->data.var_name)) {
            fprintf(ctx->out, "-%u(%%esp)",
                cc_as386_get_stack_var_offset(ctx, param->data.var_name));
        }
        break;
    case SSA_PARAM_TMPVAR: {
        const char** param_reg_names = cc_as386_get_regset_by_size(param->size);
        if (address) {
            if (cc_as386_is_stack_tmpid(ctx, param->data.tmpid)) {
                fprintf(ctx->out, "-%u(%%esp)",
                    cc_as386_get_stack_tmpid_offset(ctx, param->data.tmpid));
            } else {
                fprintf(ctx->out, "(%s)",
                    param_reg_names[cc_as386_get_tmpreg(
                        ctx, param->data.tmpid)]);
            }
        } else {
            if (cc_as386_is_stack_tmpid(ctx, param->data.tmpid)) {
                fprintf(ctx->out, "-%u(%%esp)",
                    cc_as386_get_stack_tmpid_offset(ctx, param->data.tmpid));
            } else {
                fprintf(ctx->out, "%s",
                    param_reg_names[cc_as386_get_tmpreg(
                        ctx, param->data.tmpid)]);
            }
        }
    } break;
    case SSA_PARAM_CONSTANT:
        if (param->data.constant.is_float) {
            cc_abort(__FILE__, __LINE__);
        } else {
            if (address) {
                fprintf(ctx->out, "($%s%lu)",
                    param->data.constant.is_negative ? "-" : "",
                    param->data.constant.value.u);
            } else {
                fprintf(ctx->out, "$%s%lu",
                    param->data.constant.is_negative ? "-" : "",
                    param->data.constant.value.u);
            }
        }
        break;
    default:
        cc_abort(__FILE__, __LINE__);
    }
}

static void cc_as386_gen_assign(cc_context* restrict ctx, unsigned short tmpid,
    const cc_ssa_param* restrict rhs, unsigned short size)
{
    const char** lhs_reg_names;
    if (size > 4 || rhs->size > 4) {
        fprintf(ctx->out, "#todo: assignments for big datastructures!\n");
        return;
    }

    lhs_reg_names = cc_as386_get_regset_by_size(size);
    /* No redundant gens should be fed, we assume they've been removed by now */
    switch (rhs->type) {
    case SSA_PARAM_CONSTANT:
        if (rhs->data.constant.is_float) {
            cc_abort(__FILE__, __LINE__);
        } else {
            fprintf(ctx->out, "\tmov%s\t", cc_as386_get_suffix_by_size(size));
            cc_as386_print_generic_param(ctx, rhs, false);
            fprintf(ctx->out, ",%s\n",
                lhs_reg_names[cc_as386_get_tmpreg(ctx, tmpid)]);
        }
        break;
    case SSA_PARAM_VARIABLE:
        fprintf(ctx->out, "\tmov%s\t\n", cc_as386_get_suffix_by_size(size));
        cc_as386_print_generic_param(ctx, rhs, false);
        fprintf(
            ctx->out, ",%s\n", lhs_reg_names[cc_as386_get_tmpreg(ctx, tmpid)]);
        break;
    case SSA_PARAM_TMPVAR:
        if (cc_as386_is_stack_tmpid(ctx, tmpid)) {
            fprintf(ctx->out, "\tmov%s\t", cc_as386_get_suffix_by_size(size));
            cc_as386_print_generic_param(ctx, rhs, false);
            fprintf(ctx->out, ",-%u(%%esp)\n",
                cc_as386_get_stack_tmpid_offset(ctx, tmpid));
        } else {
            fprintf(ctx->out, "\tmov%s\t", cc_as386_get_suffix_by_size(size));
            cc_as386_print_generic_param(ctx, rhs, false);
            fprintf(ctx->out, ",%s\n",
                lhs_reg_names[cc_as386_get_tmpreg(ctx, tmpid)]);
        }
        break;
    default:
        cc_abort(__FILE__, __LINE__);
    }
}

static void cc_as386_gen_store_from(cc_context* restrict ctx,
    const cc_ssa_param* restrict lhs, const cc_ssa_param* restrict rhs)
{
    if (lhs->size > 4 || rhs->size > 4) {
        fprintf(ctx->out, "#todo: generate big loads\n");
        return;
    }
    assert(!cc_ssa_is_param_same(lhs, rhs));
    switch (lhs->type) {
    case SSA_PARAM_TMPVAR: {
        switch (rhs->type) {
        case SSA_PARAM_TMPVAR:
            fprintf(
                ctx->out, "\tmov%s\t", cc_as386_get_suffix_by_size(lhs->size));
            cc_as386_print_generic_param(ctx, rhs, false);
            fprintf(ctx->out, ",");
            cc_as386_print_generic_param(ctx, lhs, true);
            fprintf(ctx->out, "\n");
            break;
        case SSA_PARAM_CONSTANT:
            fprintf(
                ctx->out, "\tmov%s\t", cc_as386_get_suffix_by_size(lhs->size));
            cc_as386_print_generic_param(ctx, rhs, false);
            fprintf(ctx->out, ",");
            cc_as386_print_generic_param(ctx, lhs, true);
            fprintf(ctx->out, "\n");
            break;
        default:
            cc_abort(__FILE__, __LINE__);
        }
        break;
    }
    case SSA_PARAM_VARIABLE: {
        switch (rhs->type) {
        case SSA_PARAM_VARIABLE:
            fprintf(
                ctx->out, "\tmov%s\t", cc_as386_get_suffix_by_size(lhs->size));
            cc_as386_print_generic_param(ctx, rhs, false);
            fprintf(ctx->out, ",");
            cc_as386_print_generic_param(ctx, lhs, true);
            fprintf(ctx->out, "\n");
            break;
        case SSA_PARAM_TMPVAR:
            fprintf(
                ctx->out, "\tmov%s\t", cc_as386_get_suffix_by_size(lhs->size));
            cc_as386_print_generic_param(ctx, rhs, false);
            fprintf(ctx->out, ",");
            cc_as386_print_generic_param(ctx, lhs, true);
            fprintf(ctx->out, "\n");
            break;
        case SSA_PARAM_CONSTANT:
            fprintf(
                ctx->out, "\tmov%s\t", cc_as386_get_suffix_by_size(lhs->size));
            cc_as386_print_generic_param(ctx, rhs, false);
            fprintf(ctx->out, ",");
            cc_as386_print_generic_param(ctx, lhs, true);
            fprintf(ctx->out, "\n");
            break;
        default:
            cc_abort(__FILE__, __LINE__);
        }
    } break;
    default:
        cc_abort(__FILE__, __LINE__);
    }
}

static void cc_as386_gen_load_from(cc_context* restrict ctx,
    unsigned short tmpid, const cc_ssa_param* restrict rhs, unsigned short size)
{
    const char** lhs_reg_names = cc_as386_get_regset_by_size(size);
    switch (rhs->type) {
    case SSA_PARAM_VARIABLE:
        fprintf(ctx->out, "\tmov%s\t", cc_as386_get_suffix_by_size(rhs->size));
        cc_as386_print_generic_param(ctx, rhs, true);
        if (cc_as386_is_stack_tmpid(ctx, tmpid)) {
            fprintf(ctx->out, ",-%u(%%esp)\n",
                cc_as386_get_stack_tmpid_offset(ctx, tmpid));
        } else {
            fprintf(ctx->out, ",%s\n",
                lhs_reg_names[cc_as386_get_tmpreg(ctx, tmpid)]);
        }
        break;
    case SSA_PARAM_TMPVAR:
        assert(tmpid != rhs->data.tmpid);
        fprintf(ctx->out, "\tmov%s\t", cc_as386_get_suffix_by_size(rhs->size));
        cc_as386_print_generic_param(ctx, rhs, true);
        if (cc_as386_is_stack_tmpid(ctx, tmpid)) {
            fprintf(ctx->out, ",-%u(%%esp)\n",
                cc_as386_get_stack_tmpid_offset(ctx, tmpid));
        } else {
            fprintf(ctx->out, ",%s\n",
                lhs_reg_names[cc_as386_get_tmpreg(ctx, tmpid)]);
        }
        break;
    default:
        cc_abort(__FILE__, __LINE__);
    }
}

static void cc_as386_gen_call_param(
    cc_context* ctx, const cc_ssa_param* param, unsigned short offset)
{
    const char** param_reg_names;
    if (param->size > 4) {
        fprintf(ctx->out, "#todo: support for >4byte big parameters!\n");
        return;
    }

    param_reg_names = cc_as386_get_regset_by_size(param->size);
    switch (param->type) {
    case SSA_PARAM_CONSTANT:
        if (param->data.constant.is_float) {
            cc_abort(__FILE__, __LINE__);
        } else {
            fprintf(ctx->out, "\tmov%s\t%lu,%u(%%esp)\n",
                cc_as386_get_suffix_by_size(param->size),
                param->data.constant.value.u, offset);
        }
        break;
    case SSA_PARAM_VARIABLE:
        fprintf(ctx->out, "\tmov%s\t$_%s,%%edi\n",
            cc_as386_get_suffix_by_size(param->size),
            cc_strview(param->data.var_name));
        fprintf(ctx->out, "\tmov%s\t%%edi,%u(%%esp)\n",
            cc_as386_get_suffix_by_size(param->size), offset);
        break;
    case SSA_PARAM_TMPVAR:
        if (cc_as386_is_stack_tmpid(ctx, param->data.tmpid)) {
            enum cc_as386_reg tmpreg
                = cc_as386_regalloc_pinned(ctx, param->data.tmpid);
            fprintf(ctx->out, "\tmov%s\t-%u(%%esp),%s\n",
                cc_as386_get_suffix_by_size(param->size),
                cc_as386_get_stack_tmpid_offset(ctx, param->data.tmpid),
                param_reg_names[tmpreg]);
            fprintf(ctx->out, "\tmov%s\t%s,%u(%%esp)\n",
                cc_as386_get_suffix_by_size(param->size),
                param_reg_names[tmpreg], offset);
            cc_as386_regfree(ctx, tmpreg);
        } else {
            fprintf(ctx->out, "\tmov%s\t%s,%u(%%esp)\n",
                cc_as386_get_suffix_by_size(param->size),
                param_reg_names[cc_as386_get_tmpreg(ctx, param->data.tmpid)],
                offset);
        }
        break;
    case SSA_PARAM_STRING_LITERAL: {
        cc_ssa_param tmp
            = cc_ssa_tempvar_param_1(ctx, param->is_signed, param->size);
        enum cc_as386_reg tmp_reg
            = cc_as386_regalloc_pinned(ctx, tmp.data.tmpid);
        fprintf(ctx->out, "\tmov%s\t$__ms_%u,%s\n",
            cc_as386_get_suffix_by_size(param->size), param->data.string.tmpid,
            param_reg_names[tmp_reg]);
        fprintf(ctx->out, "\tmov%s\t%s,%u(%%esp)\n",
            cc_as386_get_suffix_by_size(param->size), param_reg_names[tmp_reg],
            offset);
        cc_as386_regfree(ctx, tmp_reg);
    } break;
    default:
        cc_abort(__FILE__, __LINE__);
    }
}

static void cc_as386_process_call(cc_context* ctx, const cc_ssa_token* tok)
{
    const cc_ssa_param* call_param = &tok->data.call.right;
    const cc_ssa_param* call_retval = &tok->data.call.left;
    unsigned short offset = 0;
    unsigned short stack_size = 0;
    size_t i;

    assert(tok->type == SSA_TOKEN_CALL);

    for (i = 0; i < tok->data.call.n_params; i++) {
        const cc_ssa_param* param = &tok->data.call.params[i];
        stack_size += param->size;
    }
    /* Make space for the stack */
    if (stack_size) {
        fprintf(ctx->out, ".cfi_def_cfa_offset -%u\n", stack_size);
        fprintf(ctx->out, "\tsubl\t$%u,%%esp\n", stack_size);
    }
    /* Emplace parameters onto the stack */
    for (i = 0; i < tok->data.call.n_params; i++) {
        const cc_ssa_param* param = &tok->data.call.params[i];
        cc_as386_gen_call_param(ctx, param, offset);
        offset += param->size;
    }
    switch (call_param->type) {
    case SSA_PARAM_VARIABLE:
        fprintf(
            ctx->out, "\tcall\t_%s\n", cc_strview(call_param->data.var_name));
        break;
    case SSA_PARAM_TMPVAR:
        fprintf(ctx->out, "\tcall\t%s\n",
            reg32_names[cc_as386_get_tmpreg(ctx, call_param->data.tmpid)]);
        break;
    default:
        cc_abort(__FILE__, __LINE__);
    }
    if (stack_size) {
        fprintf(ctx->out, ".cfi_def_cfa_offset %u\n", stack_size);
        fprintf(ctx->out, "\taddl\t$%u,%%esp\n", stack_size);
    }

    switch (call_retval->type) {
    case SSA_PARAM_VARIABLE: {
        enum cc_as386_reg val_regno = cc_as386_regalloc_pinned(ctx, 0);
        enum cc_as386_reg ptr_regno = cc_as386_regalloc_pinned(ctx, 0);
        fprintf(ctx->out, "\tmov%s\t%%eax,%s\n",
            cc_as386_get_suffix_by_size(call_retval->size),
            reg32_names[val_regno]);
        fprintf(ctx->out, "\tmov%s\t$%s,($_%s)\n",
            cc_as386_get_suffix_by_size(call_retval->size),
            reg32_names[val_regno], cc_strview(call_retval->data.var_name));
        fprintf(ctx->out, "\tmov%s\t$%s,%s\n",
            cc_as386_get_suffix_by_size(call_retval->size),
            reg32_names[val_regno], reg32_names[ptr_regno]);
        cc_as386_regfree(ctx, ptr_regno);
        cc_as386_regfree(ctx, val_regno);
    } break;
    case SSA_PARAM_TMPVAR: {
        if (cc_as386_is_stack_tmpid(ctx, call_retval->data.tmpid)) {
            fprintf(ctx->out, "\tmov%s\t%%eax,-%u(%%esp)\n",
                cc_as386_get_suffix_by_size(call_retval->size),
                cc_as386_get_stack_tmpid_offset(ctx, call_retval->data.tmpid));
        } else {
            enum cc_as386_reg regno
                = cc_as386_get_tmpreg(ctx, call_retval->data.tmpid);
            if (regno != AS386_EAX)
                fprintf(ctx->out, "\tmov%s\t%%eax,%s\n",
                    cc_as386_get_suffix_by_size(call_retval->size),
                    reg32_names[regno]);
        }
    } break;
    case SSA_PARAM_NONE:
        /* Discard result of call... */
        break;
    default:
        cc_abort(__FILE__, __LINE__);
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
        fprintf(
            ctx->out, "\tje\t$_%s\n", cc_strview(on_true_param->data.var_name));
        break;
    case SSA_PARAM_TMPVAR:
        fprintf(ctx->out, "\tje\t%s\n",
            reg32_names[cc_as386_get_tmpreg(ctx, on_true_param->data.tmpid)]);
        break;
    case SSA_PARAM_LABEL:
        fprintf(ctx->out, "\tje\tL%i\n", on_true_param->data.label_id);
        break;
    default:
        cc_abort(__FILE__, __LINE__);
    }

    on_false_param = &tok->data.branch.f_branch;
    switch (on_false_param->type) {
    case SSA_PARAM_VARIABLE:
        fprintf(ctx->out, "\tjmp\t$_%s\n",
            cc_strview(on_false_param->data.var_name));
        break;
    case SSA_PARAM_TMPVAR:
        fprintf(ctx->out, "\tjmp\t%s\n",
            reg32_names[cc_as386_get_tmpreg(ctx, on_false_param->data.tmpid)]);
        break;
    case SSA_PARAM_LABEL:
        fprintf(ctx->out, "\tjmp\tL%i\n", on_false_param->data.label_id);
        break;
    default:
        cc_abort(__FILE__, __LINE__);
    }
}

static void cc_as386_process_jump(cc_context* ctx, const cc_ssa_token* tok)
{
    assert(tok->type == SSA_TOKEN_JUMP);
    switch (tok->data.jump_target.type) {
    case SSA_PARAM_VARIABLE:
        fprintf(ctx->out, "\tljmp\t$_%s\n",
            cc_strview(tok->data.jump_target.data.var_name));
        break;
    case SSA_PARAM_TMPVAR: {
        const char** lhs_reg_names
            = cc_as386_get_regset_by_size(tok->data.jump_target.size);
        fprintf(ctx->out, "\tljmp\t%s\n",
            lhs_reg_names[cc_as386_get_tmpreg(
                ctx, tok->data.jump_target.data.tmpid)]);
    } break;
    case SSA_PARAM_LABEL:
        fprintf(ctx->out, "\tljmp\tL%i\n", tok->data.jump_target.data.label_id);
        break;
    default:
        cc_abort(__FILE__, __LINE__);
    }
}

static void cc_as386_gen_binop_arith(cc_context* ctx, const cc_ssa_token* tok)
{
    const char* insn_name;
    cc_ssa_param result_tmp
        = cc_ssa_tempvar_param_1(ctx, false, tok->data.binop.size);
    cc_as386_regalloc(ctx, result_tmp.data.tmpid);
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
    case SSA_TOKEN_MOD:
        insn_name = "mod";
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
    case SSA_TOKEN_LSHIFT:
        insn_name = "lsh";
        break;
    case SSA_TOKEN_RSHIFT:
        insn_name = "rsh";
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
        fprintf(ctx->out, "\t%sl\t1f\n", insn_name);
        fprintf(ctx->out, "1:\n");
        fprintf(ctx->out, "\tmovl\t$1,%s\n",
            reg32_names[cc_as386_get_tmpreg(ctx, result_tmp.data.tmpid)]);
        fprintf(ctx->out, "\tljmp\t1f\n");
        fprintf(ctx->out, "1:\n");
        fprintf(ctx->out, "\tmovl\t$0,%s\n",
            reg32_names[cc_as386_get_tmpreg(ctx, result_tmp.data.tmpid)]);
        goto end;
    }
    default:
        cc_abort(__FILE__, __LINE__);
    }
    fprintf(ctx->out, "\t%sl\t", insn_name);
    cc_as386_print_generic_param(ctx, &tok->data.binop.right, false);
    fprintf(ctx->out, ",");
    cc_as386_print_generic_param(ctx, &tok->data.binop.extra, false);
    fprintf(ctx->out, "\n");
    /*???*/
end:
    cc_as386_gen_assign(
        ctx, tok->data.binop.left_tmpid, &result_tmp, tok->data.binop.size);
    cc_as386_regfree_tmpid(ctx, result_tmp.data.tmpid);
}

static void cc_as386_gen_return(
    cc_context* ctx, const cc_ssa_token* tok, bool needs_frame)
{
    if (tok->data.retval.type != SSA_PARAM_NONE) {
        /* Return values are placed, by convention, into %eax. */
        const cc_ssa_param* param = &tok->data.retval;
        const char** param_reg_names = cc_as386_get_regset_by_size(param->size);
        switch (param->type) {
        case SSA_PARAM_CONSTANT:
            if (param->data.constant.is_float) {
                cc_abort(__FILE__, __LINE__);
            } else {
                fprintf(ctx->out, "\tmov%s\t$%s%lu,%%eax\n",
                    cc_as386_get_suffix_by_size(param->size),
                    param->data.constant.is_negative ? "-" : "",
                    param->data.constant.value.u);
            }
            break;
        case SSA_PARAM_VARIABLE:
            fprintf(ctx->out, "\tmov%s\t$_%s,%%eax\n",
                cc_as386_get_suffix_by_size(param->size),
                cc_strview(param->data.var_name));
            break;
        case SSA_PARAM_TMPVAR:
            if (cc_as386_is_stack_tmpid(ctx, param->data.tmpid)) {
                fprintf(ctx->out, "\tmov%s\t-%u(%%esp),%%eax\n",
                    cc_as386_get_suffix_by_size(param->size),
                    cc_as386_get_stack_tmpid_offset(ctx, param->data.tmpid));
            } else if (cc_as386_get_tmpreg(ctx, param->data.tmpid)
                != AS386_EAX) {
                fprintf(ctx->out, "\tmov%s\t%s,%%eax\n",
                    cc_as386_get_suffix_by_size(param->size),
                    param_reg_names[cc_as386_get_tmpreg(
                        ctx, param->data.tmpid)]);
            }
            break;
        default:
            cc_abort(__FILE__, __LINE__);
        }
    }

    if (needs_frame) {
        fprintf(ctx->out, "\tmovl\t%%ebp,%%esp\n");
        fprintf(ctx->out, "\tpopl\t%%ebp\n");
    }
    fprintf(ctx->out, "\tret\n");
}

static void cc_as386_process_token(
    cc_context* ctx, const cc_ssa_token* tok, bool needs_frame)
{
    unsigned short tmpid = cc_ssa_get_lhs_tmpid(tok);
    if (tmpid > 0) {
        if (cc_as386_would_regspil(ctx)) {
            cc_as386_context* actx = cc_as386_get_ctx(ctx);
            actx->stack_slots[actx->n_stack_slots].data.tmpid = tmpid;
            actx->stack_slots[actx->n_stack_slots].offset
                = actx->total_stack_offset += cc_ssa_get_lhs_size(tok);
            actx->stack_slots[actx->n_stack_slots].is_var = false;
            ++actx->n_stack_slots;
        } else {
            cc_as386_regalloc(ctx, tmpid);
        }
    }

    switch (tok->type) {
    case SSA_TOKEN_RET:
        cc_as386_gen_return(ctx, tok, needs_frame);
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
    case SSA_TOKEN_LSHIFT:
    case SSA_TOKEN_RSHIFT:
        cc_as386_gen_binop_arith(ctx, tok);
        break;
    case SSA_TOKEN_STORE_FROM:
        cc_as386_gen_store_from(
            ctx, &tok->data.unop.left, &tok->data.unop.right);
        break;
    case SSA_TOKEN_ASSIGN:
        cc_as386_gen_assign(ctx, tok->data.load.val_tmpid, &tok->data.load.addr,
            tok->data.load.size);
        break;
    case SSA_TOKEN_LOAD_FROM:
        cc_as386_gen_load_from(ctx, tok->data.load.val_tmpid,
            &tok->data.load.addr, tok->data.load.size);
        break;
    case SSA_TOKEN_CALL:
        cc_as386_process_call(ctx, tok);
        break;
    case SSA_TOKEN_BRANCH:
        cc_as386_process_branch(ctx, tok);
        break;
    case SSA_TOKEN_DROP:
        if (cc_as386_is_stack_tmpid(ctx, tok->data.dropped_tmpid)) {
            /* TODO: Drop stack tmpids */
        } else {
            cc_as386_regfree_tmpid(ctx, tok->data.dropped_tmpid);
        }
        break;
    case SSA_TOKEN_ALLOCA:
        break;
    case SSA_TOKEN_JUMP:
        cc_as386_process_jump(ctx, tok);
        break;
    default:
        cc_abort(__FILE__, __LINE__);
    }
}

static void cc_as386_colstring_param(cc_context* ctx, const cc_ssa_param* param)
{
    if (param->type == SSA_PARAM_STRING_LITERAL)
        fprintf(ctx->out, "__ms_%u:\n\t.ascii \"%s\\0\"\n",
            param->data.string.tmpid, cc_strview(param->data.string.literal));
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

        /* Externs are NOT defined here... */
        if ((lhs->storage & SSA_STORAGE_EXTERN) == 0
            && (lhs->storage & SSA_STORAGE_STACK) == 0) {
            if ((lhs->storage & SSA_STORAGE_GLOBAL) != 0)
                fprintf(
                    ctx->out, ".globl\t_%s\n", cc_strview(lhs->data.var_name));
            fprintf(ctx->out, "_%s:\n", cc_strview(lhs->data.var_name));
            assert(size->data.constant.is_float == false);
            fprintf(ctx->out, "\t.space\t%lu\n", size->data.constant.value.u);
        }
    }
}

static bool cc_as386_needs_frame_setup(const cc_ssa_func* func)
{
    size_t i;
    bool b = false;
    for (i = 0; i < func->n_tokens; ++i) {
        const cc_ssa_token* tok = &func->tokens[i];
        if (tok->type == SSA_TOKEN_ALLOCA) {
            if (tok->data.alloca.left.type == SSA_PARAM_VARIABLE) {
                /* Stack variable */
                if ((tok->data.alloca.left.storage & SSA_STORAGE_STACK) != 0) {
                    b = true;
                    break;
                }
            } else {
                /* Non-stack variable, unnamed temporal maybe? */
                b = true;
                break;
            }
        } else if (tok->type == SSA_TOKEN_CALL && tok->data.call.n_params > 0) {
            b = true;
            break;
        }
    }
    return b || func->ast_var->type.data.func.n_params > 0;
}

void cc_as386_process_func(cc_context* ctx, const cc_ssa_func* func)
{
    cc_as386_context* actx = cc_as386_get_ctx(ctx);
    size_t i;
    bool needs_frame = cc_as386_needs_frame_setup(func);

    /* Reset context for register allocation! */
    for (i = 0; i < AS386_NUM_REGS; ++i) {
        size_t j;
        actx->r_used[i] = actx->r_pinned[i] = false;
        actx->r_spills[i] = 0;
        for (j = 0; j < UCHAR_MAX; ++j)
            actx->r_mapping[i][j] = 0;
    }

    if (!actx->s_text) {
        fprintf(ctx->out, ".text\n");
        actx->s_data = false;
        actx->s_text = true;
    }

    if ((func->ast_var->storage & AST_STORAGE_GLOBAL) != 0)
        fprintf(ctx->out, ".globl\t_%s\n", cc_strview(func->ast_var->name));

    fprintf(ctx->out, "_%s:\n", cc_strview(func->ast_var->name));
    fprintf(ctx->out, ".cfi_startproc\n");

    /* Reset stack usage and prepare stack offset for local non-VLA variables */
    /* First begin with the function parameters */
    actx->n_stack_slots = actx->total_stack_offset = 0;
    for (i = 0; i < func->ast_var->type.data.func.n_params; i++) {
        const cc_ast_variable* param = &func->ast_var->type.data.func.params[i];
        unsigned int size = ctx->get_sizeof(ctx, &param->type);
        actx->stack_slots[i].data.var_name = param->name;
        actx->stack_slots[i].offset = actx->total_stack_offset += size;
        actx->stack_slots[i].is_var = true;
        ++actx->n_stack_slots;
    }
    /* Then go over the locals [of fixed size] within the function... */
    for (i = 0; i < func->n_tokens; ++i) {
        const cc_ssa_token* tok = &func->tokens[i];
        if (tok->type == SSA_TOKEN_ALLOCA) {
            if (tok->data.alloca.left.type == SSA_PARAM_VARIABLE
                && tok->data.alloca.size.type == SSA_PARAM_CONSTANT
                && (tok->data.alloca.left.storage & SSA_STORAGE_STACK) != 0) {
                unsigned int size = tok->data.alloca.size.data.constant.value.u;
                assert(tok->data.alloca.size.data.constant.is_negative == false
                    && tok->data.alloca.size.data.constant.is_float == false);
                actx->stack_slots[i].data.var_name
                    = tok->data.alloca.left.data.var_name;
                actx->stack_slots[i].offset = actx->total_stack_offset += size;
                actx->stack_slots[i].is_var = true;
                ++actx->n_stack_slots;
            }
        }
    }

    if (needs_frame) {
        fprintf(ctx->out, "\tpushl\t%%ebp\n");
        fprintf(ctx->out, "\tmovl\t%%esp,%%ebp\n");
    }

    if (needs_frame && actx->total_stack_offset > 0)
        fprintf(ctx->out, "\taddl\t$%u,%%esp\n", actx->total_stack_offset);

    /* Perform alignment required by ABI */
    if (needs_frame && ctx->min_stack_alignment != 0)
        fprintf(ctx->out, "\tandl\t$-%u,%%esp\n", ctx->min_stack_alignment);

    /* Process all tokens of this function */
    for (i = 0; i < func->n_tokens; i++)
        cc_as386_process_token(ctx, &func->tokens[i], needs_frame);

    fprintf(ctx->out, ".cfi_endproc\n");

    for (i = 0; i < func->n_tokens; i++) {
        const cc_ssa_token* tok = &func->tokens[i];
        switch (tok->type) {
        case SSA_TOKEN_STORE_FROM:
            cc_as386_colstring_param(ctx, &tok->data.unop.left);
            cc_as386_colstring_param(ctx, &tok->data.unop.right);
            break;
        case SSA_TOKEN_ASSIGN:
        case SSA_TOKEN_LOAD_FROM:
            cc_as386_colstring_param(ctx, &tok->data.load.addr);
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
        case SSA_TOKEN_LSHIFT:
        case SSA_TOKEN_RSHIFT:
            cc_as386_colstring_param(ctx, &tok->data.binop.right);
            cc_as386_colstring_param(ctx, &tok->data.binop.extra);
            break;
        case SSA_TOKEN_CALL: {
            size_t j;
            for (j = 0; j < tok->data.call.n_params; ++j)
                cc_as386_colstring_param(ctx, &tok->data.call.params[j]);
        } break;
        case SSA_TOKEN_ALLOCA:
            cc_as386_colstring_alloca(ctx, tok);
            break;
        case SSA_TOKEN_BRANCH:
            cc_as386_colstring_param(ctx, &tok->data.branch.eval);
            cc_as386_colstring_param(ctx, &tok->data.branch.t_branch);
            cc_as386_colstring_param(ctx, &tok->data.branch.f_branch);
            break;
        case SSA_TOKEN_JUMP:
            cc_as386_colstring_param(ctx, &tok->data.jump_target);
            break;
        case SSA_TOKEN_RET:
        case SSA_TOKEN_LABEL:
            /* No operation. */
            break;
        case SSA_TOKEN_DROP:
            break;
        default:
            cc_abort(__FILE__, __LINE__);
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
