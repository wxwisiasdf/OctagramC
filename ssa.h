#ifndef SSA_H
#define SSA_H 1

#include "context.h"

enum cc_ssa_param_type {
    SSA_PARAM_NONE, /* -- For temporaries */
    SSA_PARAM_CONSTANT, /* Constant literal */
    SSA_PARAM_STRING_LITERAL, /* Treated uniquely */
    SSA_PARAM_VARIABLE, /* Non-temporal */
    SSA_PARAM_RETVAL,
    SSA_PARAM_TMPVAR, /* Unnamed temporal variable */
};

/* Same as ast.h storage linkage specifiers */
enum cc_ssa_storage {
    SSA_STORAGE_AUTO = 0x00,
    SSA_STORAGE_EXTERN = 0x01,
    SSA_STORAGE_STATIC = 0x02,
    SSA_STORAGE_REGISTER = 0x04,
    SSA_STORAGE_CONSTEXPR = 0x08,
    SSA_STORAGE_GLOBAL = 0x10,
    SSA_STORAGE_THREAD_LOCAL = 0x40,
    SSA_STORAGE_INLINE = 0x80,
};

typedef struct {
    bool is_negative : 1;
    bool is_float : 1;
    union {
        unsigned int u;
        double d;
    } value;
} cc_ssa_constant;

typedef struct {
    enum cc_ssa_param_type type;
    enum cc_ssa_storage storage;
    unsigned short size; /* Size of parameter in character units */
    bool is_signed; /* To treat this value as signed or unsigned */
    unsigned short version;
    union {
        cc_ssa_constant constant;
        const char* string_literal;
        const char* var_name; /* Name of parameter */
        unsigned short tmpid; /* Temporal Id of variable */
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
    SSA_TOKEN_ALLOCA,
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

typedef struct {
    enum cc_ssa_token_type type;
    cc_ssa_param left;
    cc_ssa_param right;
    union {
        cc_ssa_param extra; /* Extra node used by some binary operations */
        union {
            cc_ssa_param* params;
            size_t n_params;
        } call;
    } data;
} cc_ssa_token;

typedef struct {
    char* name;
    unsigned short bits;
    bool is_signed;
} cc_ssa_func_param;

typedef struct cc_ssa_func {
    struct cc_ast_variable* ast_var;
    cc_ssa_token* tokens;
    size_t n_tokens;
} cc_ssa_func;

void cc_ssa_top(cc_context* ctx);

#endif
