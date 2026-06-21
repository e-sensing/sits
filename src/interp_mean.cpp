#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector C_interp_mean_vec(arma::vec& data) {
    // Find NA values
    arma::uvec na_idx = arma::find_finite(data);

    // If there is no NA values, return data as it is
    if (na_idx.n_elem == 0) {
        return wrap(data);
    }

    // Compute mean
    double med = arma::mean(data.elem(na_idx));

    // Replace NA values
    for (arma::uword i = 0; i < data.n_elem; ++i) {
        if (!std::isfinite(data[i])) {
            data[i] = med;
        }
    }

    return wrap(data);
}

// [[Rcpp::export]]
NumericMatrix C_interp_mean_mat(NumericMatrix& data) {
    int nrows = data.nrow();
    int ncols = data.ncol();

    NumericVector vec(ncols);

    for (int i = 0; i < nrows; i++) {
        arma::vec vec = data(i, _);

        data(i, _) = C_interp_mean_vec(vec);
    }

    return data;
}
