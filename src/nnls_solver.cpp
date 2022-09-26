//[[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;
using namespace std;
//using namespace arma; //included for simple mat. multiplication

// This function was implemented by Jakob Schwalb-Willmann in the
// RStoolbox package (GPL>=3).
// The source code can be found in
// https://github.com/bleutner/RStoolbox/tree/v0.3.0

//[[Rcpp::export]]
arma::mat C_nnls_solver(const arma::mat& x,
                        const arma::mat& em,
                        const bool rmse,
                        const int max_it = 400,
                        const float tol = 0.000001) {

    int n_fracs_in = em.n_rows;
    int n_fracs_out = (rmse) ? n_fracs_in + 1 : n_fracs_in;

    int n_pixels = x.n_rows;

    arma::mat s(n_pixels, n_fracs_out, arma::fill::zeros);

    arma::mat y(n_fracs_in, n_pixels), y_prior(n_fracs_in, n_pixels);
    y.fill(0);
    y_prior.fill(-9999);
    arma::mat y_diff = y - y_prior; // create a non-negative matrix

    // switching to arma here for nice matrix multiplication
    arma::mat emx = -em * x.t(); // negative A * b
    arma::mat em2 = em * em.t(); // A * transposed A

    arma::rowvec temporary(n_pixels);
    int j = 0;

    // execute solving loop
    while(j < max_it && arma::any(arma::max(arma::abs(y_diff), 0) > tol)) {
        y_prior = y;

        for (int k = 0; k < n_fracs_in; k++) {

            temporary = y.row(k) - emx.row(k) / em2(k,k);
            temporary.clamp(0, arma::datum::inf);

            arma::rowvec temp2 = temporary - y.row(k);
            temp2.elem(arma::find(temporary == y.row(k))).zeros();

            emx += em2.col(k) * temp2;

            y.row(k) = temporary;
        }
        y_diff = y - y_prior;
        ++j;
    }

    // predict values
    arma::mat probs = y.t();

    // calculate RMSE
    arma::mat preds = probs * em;
    arma::mat ppdiff = preds - x;
    arma::mat error = arma::sqrt(arma::mean(arma::pow(ppdiff, 2), 1));

    // normalization between 0 and 1
    arma::colvec prob_sum = arma::sum(probs, 1);
    for (arma::uword i = 0; i < probs.n_cols; i++) {
        probs.col(i) /= prob_sum;
    }

    // prepare output
    s.cols(0, n_fracs_in - 1) = probs;

    // should the rmse be added?
    if (rmse) {
        s.col(n_fracs_out - 1) = error;
    }
    return s;
}
