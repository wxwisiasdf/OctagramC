#ifndef CONTEXT_H
#define CONTEXT_H 1

#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>

typedef struct cc_lexer_token cc_lexer_token;
typedef struct cc_diag_info cc_diag_info;
typedef struct cc_ast_node cc_ast_node;
typedef struct cc_backend_context cc_backend_context;

enum cc_stage {
    STAGE_LEXER,
    STAGE_PARSER,
    STAGE_AST,
};

typedef struct cc_context {
    FILE* fp;
    FILE* out;
    char* top_file;

    cc_lexer_token* tokens;
    size_t n_tokens;
    size_t c_token; /* Current token index */

    cc_ast_node* root;

    const char* cbuf; /* Current logical line buffer */
    const char* cptr; /* Line pointer for diagnostics */

    cc_diag_info* diag_infos;
    size_t n_diag_infos;
    enum cc_stage stage;
    const cc_ast_node* diag_node; /* Node for diagnostic */

    void* asgen_data; /* Opaque pointer for assembly generation */
    cc_backend_context* backend_data;

    cc_ast_node* continue_node; /* Node to jump to in continue */
    cc_ast_node* break_node; /* Node to jump to in break */
} cc_context;

#endif
