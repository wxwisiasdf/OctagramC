#ifndef C_PARSE
#define C_PARSE 1

#include <stdbool.h>
#include <stddef.h>

typedef struct cc_context cc_context;
typedef struct cc_ast_type cc_ast_type;
typedef struct cc_ast_variable cc_ast_variable;
typedef struct cc_ast_node cc_ast_node;
typedef struct cc_ast_typedef cc_ast_typedef;

int cc_parse_top(cc_context* ctx);

#endif
