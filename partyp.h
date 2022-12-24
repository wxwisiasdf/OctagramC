#ifndef PARTYP_H
#define PARTYP_H 1

#include "ast.h"
#include "context.h"
#include "parser.h"
#include <stdbool.h>

bool cc_parse_struct_or_union_specifier(
    cc_context* ctx, cc_ast_node* node, cc_ast_type* type);
bool cc_parse_declaration_specifier(
    cc_context* ctx, cc_ast_node* node, cc_ast_type* type);
void cc_swap_func_decl(cc_ast_type* type);
bool cc_parse_declarator(
    cc_context* ctx, cc_ast_node* node, cc_ast_variable* var);
bool cc_parse_declarator_list(cc_context* ctx, cc_ast_node* node,
    cc_ast_variable* var, bool* is_parsing_typedef);

#endif
