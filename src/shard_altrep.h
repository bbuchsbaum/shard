/*
 * shard_altrep.h - ALTREP wrappers for zero-copy shared vectors
 *
 * Provides ALTREP-backed views for atomic vectors backed by shared memory.
 * Subsetting returns views (not copies). Tracks dataptr_calls and
 * materialize_calls for diagnostics. readonly=TRUE prevents write access.
 *
 * Supported types: integer, real (double), logical, raw
 */

#ifndef SHARD_ALTREP_H
#define SHARD_ALTREP_H

#include "shard_r.h"
#include <stddef.h>
#include <stdint.h>

/* Forward declaration */
typedef struct shard_segment shard_segment_t;

/*
 * Initialize ALTREP classes (called on package load)
 */
attribute_visible void shard_altrep_init(DllInfo *dll);

/*
 * Create an ALTREP vector backed by shared memory
 *
 * @param seg       Shared memory segment (external pointer)
 * @param type      R type: INTSXP, REALSXP, LGLSXP, or RAWSXP
 * @param offset    Byte offset into segment
 * @param length    Number of elements
 * @param readonly  If TRUE, treat shared memory as read-only
 * @param cow       Copy-on-write policy: "copy" (default) or "deny"
 * @return          ALTREP SEXP, or R_NilValue on error
 */
attribute_visible SEXP C_shard_altrep_create(SEXP seg, SEXP type, SEXP offset,
                                             SEXP length, SEXP readonly, SEXP cow);

/*
 * Create an ALTREP view (subset) of an existing ALTREP vector
 *
 * @param x         Existing ALTREP shared vector
 * @param start     Start index (0-based)
 * @param length    Number of elements
 * @return          ALTREP SEXP view, or R_NilValue on error
 */
attribute_visible SEXP C_shard_altrep_view(SEXP x, SEXP start, SEXP length);

/*
 * Get diagnostic counters for an ALTREP shared vector
 *
 * @param x         ALTREP shared vector
 * @return          List with dataptr_calls and materialize_calls
 */
attribute_visible SEXP C_shard_altrep_diagnostics(SEXP x);

/*
 * Check if an object is a shard ALTREP vector
 *
 * @param x         Any SEXP
 * @return          TRUE if shard ALTREP, FALSE otherwise
 */
attribute_visible SEXP C_is_shard_altrep(SEXP x);

/*
 * Get the underlying segment pointer from a shard ALTREP vector
 *
 * @param x         ALTREP shared vector
 * @return          External pointer to segment, or R_NilValue
 */
attribute_visible SEXP C_shard_altrep_segment(SEXP x);

/*
 * Reset diagnostic counters for an ALTREP shared vector
 *
 * @param x         ALTREP shared vector
 * @return          R_NilValue (invisibly)
 */
attribute_visible SEXP C_shard_altrep_reset_diagnostics(SEXP x);

/*
 * Materialize an ALTREP shared vector to a standard R vector
 *
 * Creates a copy of the shared data as a standard R vector.
 * Increments the materialize_calls counter.
 *
 * @param x         ALTREP shared vector
 * @return          Standard R vector with copied data
 */
attribute_visible SEXP C_shard_altrep_materialize(SEXP x);

#endif /* SHARD_ALTREP_H */
