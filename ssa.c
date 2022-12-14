#include "ssa.h"
#include "ast.h"

cc_ssa_param cc_ssa_literal_to_param(const cc_ast_literal *literal)
{
    return (cc_ssa_param){
        .type = SSA_PARAM_CONSTANT,
        .data.constant = (cc_ssa_constant){
            .is_negative = true,
            .value = 0,
        },
    };
}

void cc_ssa_top(cc_context *ctx)
{

}
