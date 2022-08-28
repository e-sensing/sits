#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

// [[Rcpp::export]]
arma::ucolvec label_max_prob(const arma::mat& x) {
    return arma::index_max(x, 1) + 1;
}
