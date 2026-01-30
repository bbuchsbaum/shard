/*
 * shard_r.h - centralized R header includes
 *
 * Newer clang versions can warn about unknown warning groups referenced by
 * pragmas inside R's own headers. Treat these as noise and suppress them for
 * our compilation units without relying on non-portable Makevars flags.
 */

#ifndef SHARD_R_H
#define SHARD_R_H

#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunknown-warning-option"
#endif

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Visibility.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Altrep.h>

#ifdef __clang__
#pragma clang diagnostic pop
#endif

#endif /* SHARD_R_H */

