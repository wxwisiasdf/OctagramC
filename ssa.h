#ifndef SSA_H
#define SSA_H 1

#include "context.h"

enum cc_ssa_param_type {
    SSA_PARAM_NONE, /* -- For temporaries */
    SSA_PARAM_CONSTANT, /* Constant literal */
    SSA_PARAM_STRING_LITERAL, /* Treated uniquely */
    SSA_PARAM_GLOBAL,
    SSA_PARAM_STACK,
    SSA_PARAM_STATIC,
    SSA_PARAM_REGISTER,
    SSA_PARAM_THREAD_LOCAL,
    SSA_PARAM_EXTERN,
};

typedef struct
{
    bool is_negative;
    unsigned int value;
} cc_ssa_constant;

typedef struct
{
    cc_ssa_param_type type;
    unsigned int version;
    char *name;
    union
    {
        cc_ssa_constant constant;
        char *string_literal;
    } data;
} cc_ssa_param;

enum cc_ssa_token_type {
    /* Unary/special node */
    SSA_TOKEN_NONE,
    SSA_TOKEN_LABEL,
    SSA_TOKEN_CALL,
    SSA_TOKEN_PHI,
    SSA_TOKEN_RET,
    SSA_TOKEN_BRANCH,
    /* Binary op */
    SSA_TOKEN_ADD,
    SSA_TOKEN_SUB,
    SSA_TOKEN_MUL,
    SSA_TOKEN_DIV,
    SSA_TOKEN_REM,
    SSA_TOKEN_LSHIFT,
    SSA_TOKEN_RSHIFT,
    SSA_TOKEN_OR,
    SSA_TOKEN_XOR,
    SSA_TOKEN_AND,
    SSA_TOKEN_COMPARE,
    SSA_TOKEN_GET_ELEMENT,
    SSA_TOKEN_SET_ELEMENT,
    /* Unary op */
    SSA_TOKEN_NOT,
    SSA_TOKEN_LOAD,
    SSA_TOKEN_STORE,
    SSA_TOKEN_ZERO_EXT,
    SSA_TOKEN_SIGN_EXT,
};

typedef struct
{
    cc_ssa_param left;

    cc_ssa_param right;
} cc_ssa_token;

void cc_ssa_top(cc_context *ctx);

#endif
