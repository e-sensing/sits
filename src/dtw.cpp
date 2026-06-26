#include <Rcpp.h>

#include "./dtw.h"
#include "./dtw2vec.h"

using namespace Rcpp;

/**
 * Direct accessor to the vendored `dtw2vec` distance.
 *
 * @param x A `NumericVector` with the first time-series values.
 * @param y A `NumericVector` with the second time-series values.
 *
 * @return Raw `symmetric2` DTW distance between `x` and `y`.
 */
// [[Rcpp::export]]
double dtw2vec_cpp(NumericVector x, NumericVector y) {
    return incdtw::dtw2vec(x.begin(), x.size(), y.begin(), y.size());
}
