#include <Rcpp.h>

using namespace Rcpp;

/*
 * These functions were extracted from the `rlang` package to avoid extra
 * dependencies.
 */
#define FRAME_LOCK_MASK (1 << 14)
#define UNLOCK_FRAME(e) SET_ENVFLAGS(e, ENVFLAGS(e) & (~FRAME_LOCK_MASK))

// [[Rcpp::export]]
void rlang_env_unlock(SEXPREC* env) {
    UNLOCK_FRAME(env);
}

// [[Rcpp::export]]
void rlang_env_lock(SEXPREC* env) {
    UNLOCK_FRAME(env);
}
