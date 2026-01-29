/*
 * shard - Deterministic, Zero-Copy Parallel Execution for R
 *
 * Native code registration and initialization
 */

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

/* Forward declarations of native routines */
SEXP C_create_shared_segment(SEXP x, SEXP backing, SEXP path, SEXP readonly);
SEXP C_attach_shared_segment(SEXP handle);
SEXP C_detach_shared_segment(SEXP segment);
SEXP C_create_buffer(SEXP type, SEXP dim, SEXP backing, SEXP init, SEXP path);
SEXP C_buffer_write(SEXP buffer, SEXP indices, SEXP values);
SEXP C_get_segment_info(SEXP segment);

/* Registration table */
static const R_CallMethodDef CallEntries[] = {
    {"C_create_shared_segment", (DL_FUNC) &C_create_shared_segment, 4},
    {"C_attach_shared_segment", (DL_FUNC) &C_attach_shared_segment, 1},
    {"C_detach_shared_segment", (DL_FUNC) &C_detach_shared_segment, 1},
    {"C_create_buffer",         (DL_FUNC) &C_create_buffer, 5},
    {"C_buffer_write",          (DL_FUNC) &C_buffer_write, 3},
    {"C_get_segment_info",      (DL_FUNC) &C_get_segment_info, 1},
    {NULL, NULL, 0}
};

/* Package initialization */
void R_init_shard(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
