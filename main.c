/* main.c - Top file for the compiler.
   Doesn't preprocess C files, however it can compile them, so if a
   preprocessor is available one could do:
   
   cpp -ansi file.c | awk '!/^#/' | awk NF */
#include "as386.h"
#include "ast.h"
#include "context.h"
#include "diag.h"
#include "graphviz.h"
#include "lexer.h"
#include "mf370.h"
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

enum cc_output_target { TARGET_AS386, TARGET_MF370, TARGET_GRAPHVIZ };

int main(int argc, char** argv)
{
    const char* output_filename = "out.asm";
    const char* input_filename = "main.c";
    cc_context ctx = { 0 };

    cc_alloc_init(true);
    enum cc_output_target target = TARGET_AS386;
    for (int i = 1; i < argc; i++) {
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
        } else if (!strcmp(argv[i], "-386")) {
            target = TARGET_AS386;
        } else if (!strcmp(argv[i], "-370")) {
            target = TARGET_MF370;
        } else if (!strcmp(argv[i], "-gviz")) {
            target = TARGET_GRAPHVIZ;
        } else if (!strcmp(argv[i], "-h")) {
            printf("cc23 - A compiler for the C23 language, targeting 370 and "
                   "386\n");
            printf("-386\tGenerate 386 code\n");
            printf("-370\tGenerate 370 code\n");
            printf("-graphviz\tGenerate graphviz graphs\n");
            printf("-o [filename]\tSet output filename\n");
        } else {
            input_filename = argv[i];

            if (ctx.fp != NULL)
                fclose(ctx.fp);
            ctx.fp = fopen(input_filename, "rt");
            if (ctx.fp == NULL) {
                cc_diag_error(&ctx, "Unable to open file %s", input_filename);
                return -1;
            }
            cc_diag_add_info(&ctx,
                (cc_diag_info) {
                    .filename = cc_strdup(input_filename),
                    .column = 0,
                    .line = 0,
                });
        }
    }

    if (ctx.fp == NULL)
        ctx.fp = stdin;
    if (ctx.out == NULL)
        ctx.out = stdout;

    ctx.is_default_signed = true;

    cc_lex_top(&ctx); /* Start lexing and make the token stream*/
    if (!ctx.error_cnt) {
        switch (target) {
        case TARGET_AS386:
            cc_as386_init(&ctx); /* Start generating the assembly code */
            break;
        case TARGET_MF370:
            cc_mf370_init(&ctx);
            break;
        }

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

            cc_ssa_top(&ctx);

            switch (target) {
            case TARGET_AS386:
            case TARGET_MF370:
                /*cc_backend_process_node(&ctx, ctx.root, NULL);*/
                break;
            case TARGET_GRAPHVIZ:
                cc_graphviz_top(&ctx);
                break;
            }
        }
        cc_ast_destroy_node(ctx.root, true);
        /*cc_backend_deinit(&ctx);*/
    }

    if (ctx.out != stdout)
        fclose(ctx.out);
    return 0;
}
