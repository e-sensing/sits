#include <RcppArmadillo.h>
#include <cmath>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

// [[Rcpp::export]]
arma::mat C_entropy_probs(const arma::mat& x) {
    return -arma::sum(x % (arma::log(x) / log(x.n_cols)), 1);
}

// [[Rcpp::export]]
arma::mat C_margin_probs(const arma::mat& x) {

    arma::mat p(arma::size(x), arma::fill::none);
    p = arma::sort(x, "descend", 1); // sort each row descending
    return 1.0 - (p.col(0) - p.col(1));
}

// [[Rcpp::export]]
arma::mat C_least_probs(const arma::mat& x) {
    return 1.0 - arma::max(x, 1);
}
