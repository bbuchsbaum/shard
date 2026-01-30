/*
 * shard_affinity.c - Optional CPU affinity + mmap advice helpers
 *
 * These are best-effort and feature-gated by platform support.
 */

#include "shard_shm.h"
#include "shard_r.h"

#ifndef _WIN32
#include <sys/mman.h>
#endif

#if defined(__linux__)
#include <sched.h>
#include <errno.h>
#endif

attribute_visible SEXP C_shard_affinity_supported(void) {
#if defined(__linux__)
    return ScalarLogical(1);
#else
    return ScalarLogical(0);
#endif
}

attribute_visible SEXP C_shard_set_affinity(SEXP cores_) {
#if defined(__linux__)
    if (!isInteger(cores_)) {
        error("cores must be an integer vector");
    }
    R_xlen_t n = XLENGTH(cores_);
    cpu_set_t set;
    CPU_ZERO(&set);
    for (R_xlen_t i = 0; i < n; i++) {
        int c = INTEGER(cores_)[i];
        if (c == NA_INTEGER) continue;
        if (c < 0) continue;
        CPU_SET(c, &set);
    }
    int rc = sched_setaffinity(0, sizeof(set), &set);
    return ScalarLogical(rc == 0);
#else
    (void)cores_;
    return ScalarLogical(0);
#endif
}

/*
 * madvise wrapper for shard_segment_t.
 *
 * advice values:
 *  0 = normal
 *  1 = sequential
 *  2 = random
 *  3 = willneed
 *  4 = dontneed
 */
attribute_visible SEXP C_shard_segment_madvise(SEXP seg_ptr, SEXP advice_) {
#ifdef _WIN32
    (void)seg_ptr;
    (void)advice_;
    return ScalarLogical(0);
#else
    if (TYPEOF(seg_ptr) != EXTPTRSXP) error("seg_ptr must be an external pointer");
    shard_segment_t *seg = (shard_segment_t *)R_ExternalPtrAddr(seg_ptr);
    if (!seg) error("segment pointer is NULL");

    int advice = asInteger(advice_);
    if (advice == NA_INTEGER) advice = 0;

    int a = MADV_NORMAL;
    switch (advice) {
        case 0: a = MADV_NORMAL; break;
        case 1: a = MADV_SEQUENTIAL; break;
        case 2: a = MADV_RANDOM; break;
        case 3:
#ifdef MADV_WILLNEED
            a = MADV_WILLNEED;
            break;
#else
            a = MADV_NORMAL;
            break;
#endif
        case 4:
#ifdef MADV_DONTNEED
            a = MADV_DONTNEED;
            break;
#else
            a = MADV_NORMAL;
            break;
#endif
        default:
            a = MADV_NORMAL;
            break;
    }

    int rc = madvise(seg->addr, seg->size, a);
    return ScalarLogical(rc == 0);
#endif
}

