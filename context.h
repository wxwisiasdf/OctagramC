#ifndef CONTEXT_H
#define CONTEXT_H 1

#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>

enum cc_stage {
    STAGE_LEXER,
    STAGE_PARSER,
    STAGE_AST,
    STAGE_SSA,
    STAGE_CODEGEN
};

/* State machine variables for parser, lexer, etc */
typedef struct cc_context {
    /* Common */
    FILE* fp;
    FILE* out;
    char* top_file;
    unsigned int tmpid; /* Unique temporal id assigners */
    enum cc_stage stage; /* Current stage of compilation */
    unsigned int error_cnt; /* Counter for errors */
    unsigned int label_id; /* Label Id assignation */

    /* Options */
    bool abstract_declarator; /* alignas/alignof/sizeof type-name
                                         ignores/doesn't fail when no identifier
                                         is specified. */
    bool print_ast; /* Printing of AST is allowed/disallowed */
    bool is_func_body; /* Parsing assigning automatic storage to variables
                           if global or pertaining to the stack of a functor. */
    bool is_default_signed; /* Default signedness of integers */
    bool parsing_sizeof; /* "Are we inside a sizeof parameter?" */
    struct cc_ast_type* sizeof_type; /* Type obtained from within sizeof */

    /* Lexer */
    struct cc_lexer_token* tokens;
    size_t n_tokens;
    size_t c_token; /* Current token index */
    const char* cbuf; /* Current logical line buffer */
    const char* cptr; /* Line pointer for diagnostics */

    /* Parser */
    struct cc_ast_node* root;
    struct cc_ast_node* continue_node; /* Node to jump to in continue */
    struct cc_ast_node* break_node; /* Node to jump to in break */
    bool is_parsing_prototype; /* Allow ignoring missing identifiers on
                                   parameters. */
    const struct cc_ast_variable* ast_current_func;
    unsigned short if_depth; /* Number of If's encountered */
    unsigned short if_w_braces_depth; /* Number of If's with braces
                                         encountered */
    unsigned short anon_count; /* Used for assigning anonymous Ids */
    char anon_name[6]; /* Temporal storage for anonymous names */
    union cc_ast_shared_type* shared_types; /* Everything a type needs to
                                               share is located here */
    bool is_libc_decl; /* Suppresses warnings for libc function decls. */

    /* Diagnostics */
    struct cc_diag_info* diag_infos;
    size_t n_diag_infos;
    const struct cc_ast_node* diag_node; /* Node for diagnostic */

    /* SSA */
    struct cc_ssa_func* ssa_funcs;
    size_t n_ssa_funcs;
    struct cc_ssa_func* ssa_current_func;
    struct cc_ssa_token* ssa_current_tok;
    struct cc_ssa_func* static_ctor_func;
    struct cc_ssa_func* static_dtor_func;

    bool
        func_has_return; /* Basic return detection for functions that should return  */

    /* Backend */
    void* asgen_data; /* Opaque pointer for assembly generation */
    void (*process_ssa_func)(
        struct cc_context* ctx, const struct cc_ssa_func* func);
    unsigned int (*get_sizeof)(
        struct cc_context* ctx, const struct cc_ast_type* type);
    unsigned int (*get_alignof)(
        struct cc_context* ctx, const struct cc_ast_type* type);
    unsigned int (*get_offsetof)(struct cc_context* ctx,
        const struct cc_ast_type* type, const char* field);
    unsigned short min_stack_alignment;
} cc_context;

const char* cc_get_cfunc_name(const cc_context* ctx);
const char* cc_get_anon_name(cc_context* ctx);

#endif
