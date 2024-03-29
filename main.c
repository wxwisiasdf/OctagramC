/* main.c - Top file for the compiler.
   Doesn't preprocess C files, however it can compile them, so if a
   preprocessor is available one could do:
   
   cpp -ansi file.c | awk '!/^#/' | awk NF */
#ifdef TARGET_AS386
#include "as386.h"
#elif defined(TARGET_MF370)
#include "mf370.h"
#endif
#include "ast.h"
#include "context.h"
#include "diag.h"
#include "lexer.h"
#include "optzer.h"
#include "parser.h"
#include "ssa.h"
#include "util.h"
#include <assert.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ast.c"
#include "constevl.c"
/*#include "context.c"*/
#include "diag.c"
#include "lexer.c"
#include "optzer.c"
#include "parexpr.c"
#include "parser.c"
#include "partyp.c"
#include "ssa.c"
#include "util.c"
#ifdef TARGET_AS386
#include "as386.c"
#elif defined(TARGET_MF370)
#include "mf370.c"
#endif

cc_string_key cc_get_cfunc_name(const cc_context* ctx)
{
    if (ctx->ssa_current_func != NULL)
        return ctx->ssa_current_func->ast_var->name;
    if (ctx->ast_current_func != NULL)
        return ctx->ast_current_func->name;
    return 0;
}

const char* cc_get_anon_name(cc_context* ctx)
{
    assert(ctx->anon_count < USHRT_MAX);
    ctx->anon_name[0] = '_';
    ctx->anon_name[1] = '$';
    ctx->anon_name[3] = 'A' + ((ctx->anon_count >> 8) & 0xFF);
    ctx->anon_name[4] = 'A' + (ctx->anon_count & 0xFF);
    ctx->anon_name[5] = '\0';
    ++ctx->anon_count;
    return ctx->anon_name;
}

int main(int argc, char** argv)
{
    const char* output_filename = "out.asm";
    const char* input_filename = "main.c";
    cc_context ctx = { 0 };
    int i;

    ctx.alloc_reserve_factor = 1024;

    cc_alloc_init(true);
    for (i = 1; i < argc; i++) {
        if (!strcmp(argv[i], "-o")) {
            i++;
            if (i < argc) {
                output_filename = argv[i];

                if (ctx.out != NULL)
                    fclose(ctx.out);
                ctx.out = fopen(output_filename, "wt");
                if (ctx.out == NULL) {
                    cc_diag_error(
                        &ctx, "Unable to open file %s", output_filename);
                    return -1;
                }
                i++;
            }
        } else if (!strcmp(argv[i], "-print-ast")) {
            ctx.print_ast = true;
        } else if (!strcmp(argv[i], "-print-ssa")) {
            ctx.print_ssa = true;
        } else if (!strcmp(argv[i], "-h")) {
            printf("occ - A compiler for the C23 language, targeting 370 and "
                   "386\n");
            printf("-o [filename]\tSet output filename\n");
        } else {
            cc_diag_info info;

            input_filename = argv[i];

            if (ctx.fp != NULL)
                fclose(ctx.fp);
            ctx.fp = fopen(input_filename, "r");
            if (ctx.fp == NULL) {
                cc_diag_error(&ctx, "Unable to open file %s", input_filename);
                return -1;
            }

            info.filename = cc_strdup(input_filename);
            info.column = info.line = 0;
            cc_diag_add_info(&ctx, info);
        }
    }

    if (ctx.fp == NULL)
        ctx.fp = stdin;
    if (ctx.out == NULL)
        ctx.out = stdout;

    ctx.is_default_signed = true;

    cc_lex_top(&ctx); /* Start lexing and make the token stream*/
    if (!ctx.error_cnt) {
#ifdef TARGET_AS386
        cc_as386_init(&ctx); /* Start generating the assembly code */
#elif defined(TARGET_MF370)
        cc_mf370_init(&ctx);
#endif

        cc_parse_top(&ctx); /* Generate the AST */
        cc_lex_deinit(&ctx); /* Lexer information no longer needed */
        if (!ctx.error_cnt) {
            ctx.stage = STAGE_AST;
            if (ctx.print_ast) {
                printf("\nUnoptimized\n");
                cc_ast_print(ctx.root);
                printf("\n");
            }
            cc_optimizer_top(&ctx); /* Optimize the AST */
            if (ctx.print_ast) {
                printf("\nOptimized\n");
                cc_ast_print(ctx.root);
                printf("\n");
            }

            if (!ctx.error_cnt) {
                ctx.stage = STAGE_SSA;
                cc_ssa_top(&ctx);
                if (!ctx.error_cnt) {
                    size_t j;
                    ctx.stage = STAGE_CODEGEN;
                    /* First, process functions */
                    for (j = 0; j < ctx.n_ssa_funcs; ++j)
                        ctx.process_ssa_func(&ctx, &ctx.ssa_funcs[j]);
                }
                cc_ast_destroy_node(ctx.root, true);
            }
        }
        /*cc_backend_deinit(&ctx);*/
    }

    if (ctx.out != stdout)
        fclose(ctx.out);
    return !ctx.error_cnt ? EXIT_SUCCESS : EXIT_FAILURE;
}
