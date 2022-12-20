/* mf370.c - Assembly code generation for i370 machines */
#include "mf370.h"
#include "ast.h"
#include "context.h"
#include "diag.h"
#include "parser.h"
#include "util.h"
#include <assert.h>
#include <stdio.h>
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

typedef struct cc_mf370_context {
    int tmp;
} cc_mf370_context;

static const char* reg_names[MF370_NUM_REGS] = { "R0", "R1", "R2", "R3", "R4",
    "R5", "R6", "R7", "R8", "R9", "R10", "R11", "R12", "R13", "R14", "R15" };

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

unsigned int cc_mf370_get_sizeof(cc_context* ctx, const cc_ast_type* type)
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

static void cc_mf370_deinit(cc_context* ctx) { cc_free(ctx->asgen_data); }
int cc_mf370_init(cc_context* ctx)
{
    ctx->asgen_data = cc_zalloc(sizeof(cc_mf370_context));
    ctx->min_stack_alignment = 16;
    ctx->get_sizeof = &cc_mf370_get_sizeof;
    return 0;
}
