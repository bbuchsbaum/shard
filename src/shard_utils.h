/*
 * shard_utils.h - small helper routines used from R
 */

#ifndef SHARD_UTILS_H
#define SHARD_UTILS_H

#include "shard_r.h"

/* Return a stable identity token for a SEXP (as a string). */
attribute_visible SEXP C_shard_obj_address(SEXP x);

#endif /* SHARD_UTILS_H */
