/*
 * shard_affinity.h - Optional CPU affinity + madvise helpers
 */

#ifndef SHARD_AFFINITY_H
#define SHARD_AFFINITY_H

#include "shard_r.h"

attribute_visible SEXP C_shard_affinity_supported(void);
attribute_visible SEXP C_shard_set_affinity(SEXP cores);
attribute_visible SEXP C_shard_segment_madvise(SEXP seg_ptr, SEXP advice);

#endif /* SHARD_AFFINITY_H */

