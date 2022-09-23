#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

// [[Rcpp::export]]
arma::colvec C_label_max_prob(const arma::mat& X) {
    arma::colvec Y(X.n_rows);
    arma::mat Z = X;
    Z.replace(arma::datum::nan, 0);

    for (arma::uword i = 0; i < X.n_rows; i++) {
        arma::rowvec x_values = Z.row(i);

        if (all(x_values == 0)) {
            Y.at(i) = arma::datum::nan;
        } else {
            Y.at(i) = index_max(x_values) + 1;
        }
    }
    return Y;
}
