#ifndef DIAG_H
#define DIAG_H 1

#include <stddef.h>

typedef struct cc_context cc_context;
typedef struct cc_diag_info {
    char* filename;
    size_t line;
    size_t column; /* Only used by tokens */
} cc_diag_info;

void cc_diag_error(cc_context* ctx, const char* fmt, ...);
void cc_diag_warning(cc_context* ctx, const char* fmt, ...);
void cc_diag_add_info(cc_context* ctx, cc_diag_info info);
void cc_diag_update_current(cc_context* ctx, cc_diag_info new_info);
void cc_diag_increment_linenum(cc_context* ctx);
void cc_diag_return_to_file(cc_context* ctx, cc_diag_info new_info);

#endif
