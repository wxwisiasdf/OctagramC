#ifndef BACKEND_H
#define BACKEND_H 1

#include "ast.h"
#include "context.h"
#include <stddef.h>

enum cc_backend_varmap_flags {
    VARMAP_REGISTER,
    VARMAP_CONSTANT,
    VARMAP_STACK,
    VARMAP_STATIC,
    VARMAP_THREAD_LOCAL,
    VARMAP_LITERAL,
};

typedef struct cc_backend_varmap {
    const cc_ast_variable* var;
    enum cc_backend_varmap_flags flags;
    unsigned int regno; /* Register number */
    unsigned int offset; /* Stack offset (if stack based) */
    unsigned long constant;
    char* data;
    size_t n_data;
} cc_backend_varmap;

typedef struct cc_backend_reginfo {
    _Bool used;
    _Bool spilled;
} cc_backend_reginfo;

typedef struct cc_backend_context {
    const cc_ast_node* outermost_stmt; /* For scheduling ++/--, &&, ||, etc */
    const cc_ast_node* if_outermost_stmt; /* If without a tail else will
                                             require scheduling too */
    cc_backend_varmap* varmaps;
    size_t n_varmaps;
    const char** reg_names;
    cc_backend_reginfo* regs;
    size_t n_regs;
    unsigned int stack_frame_size; /* Size of the frame for the current
                                      function */
    unsigned int min_stack_alignment; /* Minimum alignment for stack */
    unsigned int label_num; /* Label assignment numbers */
    _Bool (*is_reserved)(unsigned int regno);
    _Bool (*gen_mov)(cc_context* ctx, const cc_backend_varmap* lvmap,
        const cc_backend_varmap* rvmap);
    _Bool (*gen_prologue)(
        cc_context* ctx, const cc_ast_node* node, const cc_ast_variable* var);
    _Bool (*gen_epilogue)(
        cc_context* ctx, const cc_ast_node* node, const cc_ast_variable* var);
    _Bool (*gen_call)(cc_context* ctx, const cc_ast_node* node);
    unsigned int (*get_sizeof)(cc_context* ctx, const cc_ast_type* type);
    cc_backend_varmap (*get_call_retval)(
        cc_context* ctx, const cc_ast_node* node);
    _Bool (*gen_jump)(cc_context* ctx, const cc_ast_node* node);
    _Bool (*gen_binop)(cc_context* ctx, const cc_backend_varmap* lvmap,
        const cc_backend_varmap* rvmap, enum cc_ast_binop_type type);
    _Bool (*gen_unop)(cc_context* ctx, const cc_backend_varmap* lvmap,
        const cc_backend_varmap* rvmap, enum cc_ast_unop_type type);
    _Bool (*map_variable)(cc_context* ctx, const cc_ast_variable* var);
    _Bool (*gen_branch)(cc_context* ctx, const cc_ast_node* node,
        const cc_backend_varmap* lvmap, const cc_backend_varmap* rvmap,
        enum cc_ast_binop_type type);
    void (*deinit)(cc_context* ctx);
    const cc_ast_variable* current_func_var;
} cc_backend_context;

unsigned int cc_backend_get_labelnum(cc_context* ctx);
void cc_backend_spill_reg(cc_context* ctx, unsigned int regno);
void cc_backend_unspill_reg(cc_context* ctx, unsigned int regno);
void cc_backend_spill(cc_context* ctx, unsigned int num);
void cc_backend_unspill(cc_context* ctx);
void cc_backend_reserve_reg(cc_context* ctx, unsigned int regno);
unsigned int cc_backend_alloc_register(cc_context* ctx);
void cc_backend_free_register(cc_context* ctx, int regno);
void cc_backend_add_static_var(cc_context* ctx, const cc_ast_variable* var);
void cc_backend_add_stack_var(cc_context* ctx, const cc_ast_variable* var);
cc_backend_varmap* cc_backend_find_stack_var(
    cc_context* ctx, const cc_ast_variable* var);
cc_backend_varmap cc_backend_get_node_varmap(
    cc_context* ctx, const cc_ast_node* node);
void cc_backend_map_variables(cc_context* ctx, const cc_ast_node* node);
void cc_backend_process_node(
    cc_context* ctx, const cc_ast_node* node, cc_backend_varmap* ovmap);
void cc_backend_init(
    cc_context* ctx, const char* reg_names[], unsigned int n_regs);
void cc_backend_deinit(cc_context* ctx);

#endif
