#ifndef SSA_H
#define SSA_H 1

#include "context.h"
#include "diag.h"

enum cc_ssa_param_type {
    SSA_PARAM_NONE, /* -- For temporaries */
    SSA_PARAM_CONSTANT, /* Constant literal */
    SSA_PARAM_STRING_LITERAL, /* Treated uniquely */
    SSA_PARAM_VARIABLE, /* Non-temporal */
    SSA_PARAM_RETVAL,
    SSA_PARAM_TMPVAR, /* Unnamed temporal variable */
    SSA_PARAM_REF_TMPVAR,
    SSA_PARAM_LABEL,
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
        unsigned long u;
        double d;
    } value;
} cc_ssa_constant;

typedef struct {
    enum cc_ssa_param_type type;
    enum cc_ssa_storage storage;
    unsigned short size; /* Size of parameter in character units */
    bool is_signed : 1; /* To treat this value as signed or unsigned */
    bool is_atomic : 1; /* If the value in question is atomically treated */
    bool is_volatile : 1; /* If the value's loads and stores are explicitly
                             not optimized away */
    unsigned short version;
    union {
        cc_ssa_constant constant;
        const char* var_name; /* Name of parameter */
        unsigned short tmpid; /* Temporal Id of variable */
        unsigned short label_id;
        struct {
            unsigned short tmpid;
            const char* literal;
        } string;
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
    SSA_TOKEN_GT,
    SSA_TOKEN_GTE,
    SSA_TOKEN_LT,
    SSA_TOKEN_LTE,
    SSA_TOKEN_EQ,
    SSA_TOKEN_NEQ,
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
    SSA_TOKEN_STORE_AT,
    SSA_TOKEN_LOAD_FROM,
    SSA_TOKEN_GET_ELEMENT,
    SSA_TOKEN_SET_ELEMENT,
    /* Unary op */
    SSA_TOKEN_NOT,
    SSA_TOKEN_ASSIGN,
    SSA_TOKEN_ZERO_EXT,
    SSA_TOKEN_SIGN_EXT,
};

typedef struct cc_ssa_token {
    enum cc_ssa_token_type type;
    cc_diag_info info;
    union {
        struct {
            cc_ssa_param left;
            cc_ssa_param right;
            cc_ssa_param extra;
        } binop;
        struct {
            cc_ssa_param left;
            cc_ssa_param right;
        } unop;
        struct {
            cc_ssa_param left;
            cc_ssa_param size;
            cc_ssa_param align;
        } alloca;
        struct {
            cc_ssa_param left;
            cc_ssa_param right;
            cc_ssa_param* params;
            size_t n_params;
        } call;
        struct {
            cc_ssa_param eval;
            cc_ssa_param t_branch;
            cc_ssa_param f_branch;
        } branch;
        unsigned short label_id;
    } data;
} cc_ssa_token;

typedef struct cc_ssa_func {
    const char* name;
    const struct cc_ast_variable* ast_var;
    cc_ssa_token* tokens;
    size_t n_tokens;
    /* String literal pools */
    const char** strings;
    size_t n_strings;
} cc_ssa_func;

cc_ssa_param cc_ssa_tempvar_param_1(
    cc_context* ctx, bool is_signed, unsigned short size);
cc_ssa_param cc_ssa_tempvar_param(
    cc_context* ctx, const struct cc_ast_type* base_type);
bool cc_ssa_is_param_same(
    const cc_ssa_param* restrict p1, const cc_ssa_param* restrict p2);
void cc_ssa_top(cc_context* ctx);

#endif
