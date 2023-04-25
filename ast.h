#ifndef AST_H
#define AST_H 1

#ifdef __STDC__
#ifndef restrict
#define restrict
#endif
#endif

#include "diag.h"
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>

/* AST Parsing */
#define MAX_CV_QUALIFIERS 3

typedef struct cc_ast_node cc_ast_node;

typedef struct cc_ast_type_cv {
    bool is_const : 1;
    bool is_volatile : 1;
    bool is_restrict : 1;
    bool is_atomic : 1;
    bool is_array : 1; /* Treating this pointer as array? */
    bool is_static_array : 1; /* If the given size of the array is an static */
    cc_ast_node *array_size_expr; /* Expression for the size of the array */
} cc_ast_type_cv;

enum cc_ast_storage {
    AST_STORAGE_NONE = 0x00,
    AST_STORAGE_AUTO = 0x01,
    AST_STORAGE_EXTERN = 0x02,
    AST_STORAGE_STATIC = 0x04,
    AST_STORAGE_REGISTER = 0x08,
    AST_STORAGE_CONSTEXPR = 0x10,
    AST_STORAGE_GLOBAL = 0x20,
    AST_STORAGE_THREAD_LOCAL = 0x40,
    AST_STORAGE_INLINE = 0x80
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

typedef struct cc_ast_literal {
    bool is_signed : 1;
    bool is_float : 1;
    union {
        unsigned long u;
        signed long s;
        double d;
    } value;
} cc_ast_literal;

typedef struct cc_ast_enum_member {
    char* name;
    cc_ast_literal literal;
} cc_ast_enum_member;

typedef struct cc_ast_type {
    enum cc_ast_type_mode mode;
    enum cc_ast_storage storage;
    cc_ast_type_cv cv_qual[MAX_CV_QUALIFIERS];
    unsigned short n_cv_qual; /* Number of cv qualifiers
                               0 = <cv> <type> <ident>;
                               1 = <cv> *<cv> <type> <ident>;
                               2 = <cv> *<cv> *<cv> <type> <ident>;
                               and so on... */
    bool is_typedef : 1; /* Set for typedefs */
    char* name; /* Name is optional for some types */
    unsigned short min_alignment;
    unsigned short max_alignment;
    union {
        struct {
            bool is_signed : 1;
            bool is_longer : 1; /* Is long-long or long-double? */
            unsigned char bitint_bits; /* _BitInt bits */
        } num;
        struct {
            struct cc_ast_type* return_type;
            struct cc_ast_variable* params;
            size_t n_params;
            bool no_return : 1;
            bool no_discard : 1;
            bool deprecated : 1;
            bool naked : 1;
            bool irq : 1;
            bool variadic : 1; /* Variadic functions */
        } func;
        struct {
            bool packed : 1;
            struct cc_ast_variable* members;
            size_t n_members;
        } s_or_u;
        struct {
            cc_ast_enum_member* elems;
            size_t n_elems;
        } enumer;
    } data;
} cc_ast_type;

typedef struct cc_ast_variable {
    char* name;
    struct cc_ast_node* body; /* For functions. */
    struct cc_ast_node* initializer; /* Initializer for constexpr and such. */
    cc_ast_type type;
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
    AST_NODE_SWITCH,
    AST_NODE_REGISTER,
    AST_NODE_FIELD
};

enum cc_ast_binop_type {
    AST_BINOP_NONE,
    AST_BINOP_ASSIGN, /* Or set */
    AST_BINOP_ADD,
    AST_BINOP_SUB,
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
    AST_BINOP_LTE
};

enum cc_ast_unop_type {
    AST_UNOP_NONE,
    AST_UNOP_NOT,
    AST_UNOP_COND_NOT,
    AST_UNOP_CAST,
    AST_UNOP_DEREF,
    AST_UNOP_REF,
    AST_UNOP_POSTINC,
    AST_UNOP_PREINC,
    AST_UNOP_POSTDEC,
    AST_UNOP_PREDEC
};

typedef struct cc_ast_node {
    enum cc_ast_node_type type;
    struct cc_ast_node* parent;
    cc_diag_info info;
    unsigned short label_id;
    unsigned short ref_count; /* Label ref_count */
    unsigned short size_type; /* Size to operate upon */
    union {
        cc_ast_literal literal;
        const char* field_name;
        /* For pattern matching, we only use reg_group to specify which
           group of registers are allowed to be matched.
           
           For non-pattern matching we specify reg_num for specifying
           the hard register itself. */
        unsigned short reg_num;
        unsigned short reg_group;
        unsigned short jump_label_id; /* Label Id to jump to */
        char* string_literal;
        char* label_name;
        struct {
            char* name;
            unsigned short version; /* Used by SSA */
            bool is_temporal : 1;
        } var;
        struct {
            struct cc_ast_node* call_expr;
            struct cc_ast_node* params;
            size_t n_params;
        } call;
        struct {
            enum cc_ast_binop_type op;
            struct cc_ast_node* left;
            struct cc_ast_node* right;
            unsigned short bits; /* Bits for operation */
        } binop;
        struct {
            enum cc_ast_unop_type op;
            struct cc_ast_node* child;
            cc_ast_type cast; /* Cast type */
            unsigned short bits; /* Bits for operation */
        } unop;
        struct {
            struct cc_ast_node* children;
            size_t n_children;
            struct cc_ast_variable* vars;
            size_t n_vars;
            struct cc_ast_type* types;
            size_t n_types;
            cc_ast_literal case_val; /* Case value */
            bool is_case : 1; /* Switch statment cases */
            bool is_default : 1; /* Default switch case */
            bool hot : 1; /* Block is likely to execute many times */
            bool likely : 1; /* Block is likely to evaluate true */
            bool fallthru : 1; /* Ignore fallthru warnings and stuff */
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
        struct cc_ast_node* return_expr; /* Return value */
    } data;
} cc_ast_node;

unsigned short cc_ast_alloc_label_id(cc_context* ctx);
cc_ast_node* cc_ast_create_any(
    cc_context* ctx, cc_ast_node* parent, enum cc_ast_node_type type);
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
cc_ast_node* cc_ast_create_literal_from_str(
    cc_context* ctx, cc_ast_node* parent, const char* s);
cc_ast_node* cc_ast_create_literal(
    cc_context* ctx, cc_ast_node* parent, cc_ast_literal literal);
cc_ast_node* cc_ast_create_jump(
    cc_context* ctx, cc_ast_node* parent, cc_ast_node* target);
void cc_ast_add_block_node(
    cc_ast_node* restrict block, const cc_ast_node* restrict child);
void cc_ast_add_block_type(cc_ast_node* block, const cc_ast_type* type);
void cc_ast_remove_block_node(cc_ast_node* block, size_t i);
void cc_ast_add_block_variable(cc_ast_node* block, const cc_ast_variable* var);
void cc_ast_add_call_param(
    cc_ast_node* restrict call, const cc_ast_node* restrict param);
void cc_ast_destroy_type(cc_ast_type* type, bool managed);
void cc_ast_destroy_var(cc_ast_variable* var, bool managed);
void cc_ast_destroy_node(cc_ast_node* node, bool managed);
cc_ast_variable* cc_ast_find_variable(const char *fn_name,
    const char* name, const cc_ast_node* node);
cc_ast_node* cc_ast_find_label(const char* name, const cc_ast_node* node);
cc_ast_type* cc_ast_find_typedef(const char* name, cc_ast_node* node);
cc_ast_type* cc_ast_find_type(const char* name, cc_ast_node* node);
void cc_ast_copy_node(cc_context* ctx,
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
    cc_context* ctx, cc_ast_node* node, unsigned short id);
void cc_ast_print(const cc_ast_node* node);
cc_ast_variable* cc_ast_get_field_of(
    const cc_ast_type* type, const char* field);

#endif
