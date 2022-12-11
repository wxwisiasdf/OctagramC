#ifndef AST_H
#define AST_H 1

#include "diag.h"
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>

/* AST Parsing */
#define MAX_CV_QUALIFIERS 3
#define MAX_ARRAY_SIZE 65536

typedef struct cc_ast_type_cv {
    _Bool is_const : 1;
    _Bool is_volatile : 1;
    _Bool is_restrict : 1;
    _Bool is_atomic : 1;
    _Bool is_array : 1; /* Treating this pointer as array? */
    _Bool is_static_array : 1; /* If the given size of the array is an static */
    unsigned int array_size; /* Size of the array! */
} cc_ast_type_cv;

enum cc_ast_storage {
    AST_STORAGE_AUTO = 0,
    AST_STORAGE_EXTERN,
    AST_STORAGE_STATIC,
    AST_STORAGE_REGISTER,
    AST_STORAGE_CONSTEXPR,
    AST_STORAGE_GLOBAL,
    AST_STORAGE_THREAD_LOCAL = 0x40,
    AST_STORAGE_INLINE = 0x80,
};

enum cc_ast_type_mode {
    AST_TYPE_MODE_NONE,
    AST_TYPE_MODE_VOID,
    AST_TYPE_MODE_CHAR,
    AST_TYPE_MODE_SHORT,
    AST_TYPE_MODE_INT,
    AST_TYPE_MODE_LONG,
    AST_TYPE_MODE_FLOAT,
    AST_TYPE_MODE_DOUBLE,
    AST_TYPE_MODE_BITINT,
    AST_TYPE_MODE_BOOL,
    AST_TYPE_MODE__COMPLEX,
    AST_TYPE_MODE__DECIMAL32,
    AST_TYPE_MODE__DECIMAL64,
    AST_TYPE_MODE__DECIMAL128,
    AST_TYPE_MODE_STRUCT,
    AST_TYPE_MODE_UNION,
    AST_TYPE_MODE_ENUM,
    AST_TYPE_MODE_FUNCTION
};

typedef struct cc_ast_type {
    enum cc_ast_type_mode mode;
    enum cc_ast_storage storage;
    cc_ast_type_cv cv_qual[MAX_CV_QUALIFIERS];
    size_t n_cv_qual; /* Number of cv qualifiers
                        0 = invalid
                        1 = <cv> <type> <ident>;
                        2 = <cv> *<cv> <type> <ident>;
                        and so on... */
    _Bool is_signed;
    _Bool is_longer; /* Is long-long? */
    size_t bitint_bits; /* _BitInt bits */
    char* name; /* Name is optional for some types */
    union {
        struct {
            _Bool no_return;
            struct cc_ast_type* return_type;
            struct cc_ast_variable* params;
            size_t n_params;
            _Bool variadic; /* Variadic functions */
        } func;
        struct {
            struct cc_ast_variable* members;
            size_t n_members;
        } s_or_u;
        struct {
            signed long* elems;
            size_t n_elem;
        } enumer;
    } data;
} cc_ast_type;

typedef struct cc_ast_typedef {
    cc_ast_type type;
    char* name;
} cc_ast_typedef;

typedef struct cc_ast_variable {
    cc_ast_type type;
    char* name;
    struct cc_ast_node* body; /* For functions */
    unsigned int id;
} cc_ast_variable;

enum cc_ast_node_type {
    AST_NODE_NONE,
    AST_NODE_BINOP,
    AST_NODE_UNOP,
    AST_NODE_BLOCK,
    AST_NODE_JUMP,
    AST_NODE_IF,
    AST_NODE_RETURN,
    AST_NODE_CALL,
    AST_NODE_VARIABLE,
    AST_NODE_LITERAL,
    AST_NODE_STRING_LITERAL,
    AST_NODE_SWITCH
};

enum cc_ast_binop_type {
    AST_BINOP_NONE,
    AST_BINOP_ASSIGN, /* Or set */
    AST_BINOP_PLUS,
    AST_BINOP_MINUS,
    AST_BINOP_MUL,
    AST_BINOP_DIV,
    AST_BINOP_MOD,
    AST_BINOP_LSHIFT,
    AST_BINOP_RSHIFT,
    AST_BINOP_XOR,
    AST_BINOP_AND,
    AST_BINOP_OR,
    AST_BINOP_ARROW,
    AST_BINOP_DOT,
    AST_BINOP_COND_EQ,
    AST_BINOP_COND_NEQ,
    AST_BINOP_COND_AND,
    AST_BINOP_COND_OR,
    AST_BINOP_GT,
    AST_BINOP_GTE,
    AST_BINOP_LT,
    AST_BINOP_LTE,
};

enum cc_ast_unop_type {
    AST_UNOP_NOT,
    AST_UNOP_COND_NOT,
    AST_UNOP_CAST,
    AST_UNOP_DEREF,
    AST_UNOP_REF,
    AST_UNOP_POSTINC,
    AST_UNOP_PREINC,
    AST_UNOP_POSTDEC,
    AST_UNOP_PREDEC,
};

typedef struct cc_ast_literal {
    _Bool is_signed;
    union {
        unsigned long long u;
        signed long long s;
    } value;
} cc_ast_literal;

typedef struct cc_ast_primitive_type {
    unsigned int access_size; /* Size of access, in bits */
    
} cc_ast_primitive_type;

typedef struct cc_ast_node {
    enum cc_ast_node_type type;
    struct cc_ast_node* parent;
    cc_diag_info info;
    unsigned int label_id;
    unsigned int ref_count; /* Label ref_count */
    union {
        cc_ast_literal literal;
        struct {
            char* data;
        } string_literal;
        struct {
            char* name;
            unsigned int version; /* Used by SSA */
            _Bool is_temporal;
            _Bool is_field; /* Treating this variable as a field rather than a
                               standalone thing. */
        } var;
        struct {
            struct cc_ast_node* call_expr;
            struct cc_ast_node* params;
            size_t n_params;
        } call;
        struct {
            unsigned int label_id; /* Label Id to jump to */
        } jump;
        struct {
            enum cc_ast_binop_type op;
            struct cc_ast_node* left;
            struct cc_ast_node* right;
        } binop;
        struct {
            enum cc_ast_unop_type op;
            struct cc_ast_node* child;
            cc_ast_type cast; /* Cast type */
        } unop;
        struct {
            struct cc_ast_node* children;
            size_t n_children;
            struct cc_ast_variable* vars;
            size_t n_vars;
            struct cc_ast_typedef* typedefs;
            size_t n_typedefs;
            struct cc_ast_type* types;
            size_t n_types;
            _Bool is_func; /* Is this a function? (handling for return
                              and stuff) */
            _Bool is_case; /* Switch statment cases */
            _Bool is_default; /* Default switch case */
            signed int case_val; /* Case value */
        } block;
        struct {
            struct cc_ast_node* cond;
            struct cc_ast_node* block;
            struct cc_ast_node* tail_else;
        } if_expr;
        struct {
            struct cc_ast_node* control;
            struct cc_ast_node* block;
        } switch_expr;
        struct {
            struct cc_ast_node* value; /* Return value */
        } return_expr;
        struct {
            char* name;
        } label;
    } data;
} cc_ast_node;

unsigned int cc_ast_alloc_label_id(void);
cc_ast_node* cc_ast_create_block(cc_context* ctx, cc_ast_node* parent);
cc_ast_node* cc_ast_create_binop_expr(
    cc_context* ctx, cc_ast_node* parent, enum cc_ast_binop_type type);
cc_ast_node* cc_ast_create_unop_expr(
    cc_context* ctx, cc_ast_node* parent, enum cc_ast_unop_type type);
cc_ast_node* cc_ast_create_if_expr(cc_context* ctx, cc_ast_node* parent);
cc_ast_node* cc_ast_create_switch_expr(cc_context* ctx, cc_ast_node* parent);
cc_ast_node* cc_ast_create_ret_expr(cc_context* ctx, cc_ast_node* parent);
cc_ast_node* cc_ast_create_var_ref(
    cc_context* ctx, cc_ast_node* parent, const cc_ast_variable* var);
cc_ast_node* cc_ast_create_field_ref(
    cc_context* ctx, cc_ast_node* parent, const char* fieldname);
cc_ast_node* cc_ast_create_call(cc_context* ctx, cc_ast_node* parent);
cc_ast_node* cc_ast_create_string_literal(
    cc_context* ctx, cc_ast_node* parent, const char* s);
cc_ast_node* cc_ast_create_literal(
    cc_context* ctx, cc_ast_node* parent, const char* s);
cc_ast_node* cc_ast_create_jump(
    cc_context* ctx, cc_ast_node* parent, cc_ast_node* target);
void cc_ast_add_block_node(
    cc_ast_node* restrict block, const cc_ast_node* restrict child);
void cc_ast_add_block_typedef(cc_ast_node* block, const cc_ast_typedef* tpdef);
void cc_ast_add_block_type(cc_ast_node* block, const cc_ast_type* type);
void cc_ast_remove_block_node(cc_ast_node* block, size_t i);
void cc_ast_add_block_variable(cc_ast_node* block, const cc_ast_variable* var);
void cc_ast_add_call_param(
    cc_ast_node* restrict call, const cc_ast_node* restrict param);
void cc_ast_destroy_type(cc_ast_type* type, _Bool managed);
void cc_ast_destroy_var(cc_ast_variable* var, _Bool managed);
void cc_ast_destroy_node(cc_ast_node* node, _Bool managed);
cc_ast_variable* cc_ast_find_variable(
    const char* name, const cc_ast_node* node);
cc_ast_node* cc_ast_find_label(const char* name, const cc_ast_node* node);
cc_ast_typedef* cc_ast_find_typedef(const char* name, cc_ast_node* node);
cc_ast_type* cc_ast_find_type(const char* name, cc_ast_node* node);
void cc_ast_copy_node(
    cc_ast_node* restrict dest, const cc_ast_node* restrict src);
void cc_ast_copy_type(
    cc_ast_type* restrict dest, const cc_ast_type* restrict src);
void cc_ast_add_type_member(
    cc_ast_type* restrict dest, const cc_ast_variable* restrict src);
#if 0
void cc_ast_iterate(const cc_ast_node *node,
                    void (*on_each)(const cc_ast_node *node, va_list arg),
                    ...);
#endif
cc_ast_node* cc_ast_find_label_id(
    cc_context* ctx, cc_ast_node* node, unsigned int id);
void cc_ast_print(const cc_ast_node* node, int ident);

#endif
