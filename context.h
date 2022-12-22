#ifndef CONTEXT_H
#define CONTEXT_H 1

#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>

enum cc_stage {
    STAGE_LEXER,
    STAGE_PARSER,
    STAGE_AST,
};

struct cc_ast_type;

/* State machine variables for parser, lexer, etc */
typedef struct cc_context {
    FILE* fp;
    FILE* out;
    char* top_file;
    struct cc_lexer_token* tokens;
    size_t n_tokens;
    size_t c_token; /* Current token index */
    struct cc_ast_node* root;
    struct cc_ssa_func* ssa_funcs;
    size_t n_ssa_funcs;
    const char* cbuf; /* Current logical line buffer */
    const char* cptr; /* Line pointer for diagnostics */
    struct cc_diag_info* diag_infos;
    size_t n_diag_infos;
    enum cc_stage stage;
    const struct cc_ast_node* diag_node; /* Node for diagnostic */
    void* asgen_data; /* Opaque pointer for assembly generation */
    struct cc_ast_node* continue_node; /* Node to jump to in continue */
    struct cc_ast_node* break_node; /* Node to jump to in break */
    unsigned int error_cnt; /* Counter for errors */
    unsigned int label_id; /* Label Id assignation */
    bool is_parsing_prototype; /* Allow ignoring missing identifiers on
                                   parameters. */
    bool is_parsing_typedef; /* Handling for typedefs */
    bool declaration_ident_optional; /* alignas/alignof/sizeof type-name
                                         ignores/doesn't fail when no identifier
                                         is specified. */
    bool print_ast; /* Printing of AST is allowed/disallowed */
    bool is_func_body; /* Parsing assigning automatic storage to variables
                           if global or pertaining to the stack of a functor. */
    unsigned int (*get_sizeof)(
        struct cc_context* ctx, const struct cc_ast_type* type);
    unsigned short min_stack_alignment;
    struct cc_ssa_func* ssa_current_func;
    unsigned int tmpid;
    bool is_default_signed; /* Default signedness of integers */
    void (*process_ssa_func)(struct cc_context* ctx, const struct cc_ssa_func* func);
} cc_context;

#endif
