/*
 * shard_utils.c - small helper routines used from R
 */

#include "shard_utils.h"

#include <stdio.h>

SEXP C_shard_obj_address(SEXP x) {
    char buf[64];
    snprintf(buf, sizeof(buf), "%p", (void *)x);
    return ScalarString(mkChar(buf));
}

