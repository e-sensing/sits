#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace std;

// This function normalizes a matrix, by considering a maximum and
//   a minimum value. Number of columns of data, min, and max should be
//   the same.

// [[Rcpp::export]]
arma::mat C_normalize_data(const arma::mat& data, const arma::rowvec& min,
                           const arma::rowvec& max) {
    if (data.n_cols != min.n_cols || min.n_cols != max.n_cols) {
        return data;
    }
    arma::mat res(arma::size(data));
    res = (data.each_row() - min).each_row() / (max - min);
    res.clamp(0.0001, 1.0);
    return res;
}

