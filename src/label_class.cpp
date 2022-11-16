#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

// [[Rcpp::export]]
arma::colvec C_label_max_prob(const arma::mat& x) {
    arma::colvec y(x.n_rows);
    arma::mat z = x;
    z.replace(arma::datum::nan, 0);
    for (arma::uword i = 0; i < x.n_rows; i++) {
        arma::rowvec x_values = z.row(i);
        if (all(x_values == 0)) {
            y.at(i) = arma::datum::nan;
        } else {
            y.at(i) = index_max(x_values) + 1;
        }
    }
    return y;
}
