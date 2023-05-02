typedef struct cc_context {
    int label_id;
} cc_context;

cc_context test;

unsigned short cc_ast_alloc_label_id(cc_context* ctx)
{
    if ((ctx->label_id + 1) >= 
                              (0x7fff * 2 + 1)
                                       )
        cc_diag_warning(ctx, "Ran out of labels to assign");
    return ctx->label_id++;
}
