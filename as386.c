/* as386.c - Assembly code generation for 386 machines */
#include "as386.h"
#include "ast.h"
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

typedef struct cc_as386_context {
    int tmp;
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

static void cc_as386_deinit(cc_context* ctx) { cc_free(ctx->asgen_data); }

int cc_as386_init(cc_context* ctx)
{
    ctx->asgen_data = cc_zalloc(sizeof(cc_as386_context));
    ctx->min_stack_alignment = 16;
    ctx->get_sizeof = &cc_as386_get_sizeof;
    return 0;
}
