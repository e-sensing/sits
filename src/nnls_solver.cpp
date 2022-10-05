//[[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <math.h>

using namespace Rcpp;
using namespace std;
//using namespace arma; //included for simple mat. multiplication

// This function was implemented by Jakob Schwalb-Willmann in the
// RStoolbox package (GPL>=3).
// The source code can be found in
// https://github.com/bleutner/RStoolbox/tree/v0.3.0

//[[Rcpp::export]]
arma::mat batch_calc(const int &n_pixels, const int &max_lines_per_batch) {
    // number of batches
    int nb = ceil((float) n_pixels / max_lines_per_batch);
    // number of rows per batch
    int npb = ceil((float) n_pixels / nb) - 1;
    // result matrix initialized with zeros
    // where: 1st col = start line and 2nd col = end line
    arma::mat res(nb, 2, arma::fill::zeros);
    // initializing 2nd col with end line of the first batch
    res.at(0, 1) = npb;
    // temporary vector to allocate the current position of batches
    arma::rowvec tempv(2, arma::fill::zeros);
    for (int i = 1; i < nb; i++) {
        // calculate the current line from the batch in the previous step
        tempv.at(0) = (int) res.at(i - 1, 1) + res.at(i - 1, 0) + 1;
        // if the sum of the current row plus the number of rows per batch
        // is greater than the number of pixels we get the missing
        // rows of the batch
        if (tempv.at(0) + npb >= n_pixels) {
            int p = n_pixels % (int) tempv.at(0);
            tempv.at(1) = (int) p - 1;
        } else {
            tempv.at(1) = npb;
        }
        res.row(i) = tempv;
    }
    return res;
}

//[[Rcpp::export]]
arma::mat C_nnls_solver_batch(const arma::mat& x,
                              const arma::mat& em,
                              const bool rmse,
                              const int max_it = 400,
                              const float tol = 0.000001) {

    int n_fracs_in = em.n_rows;
    int n_fracs_out = (rmse) ? n_fracs_in + 1 : n_fracs_in;

    int n_pixels = x.n_rows;
    // we don't allocate this matrix because it grows according to the
    // insertions that are made
    arma::mat s;
    arma::mat batch_mat = batch_calc(n_pixels, 50000);

    for (arma::uword i = 0; i < batch_mat.n_rows; i++) {
        int start_row = batch_mat.at(i, 0);
        int end_row = start_row + batch_mat.at(i, 1);
        arma::mat sub_x = x.submat(start_row, 0, end_row, em.n_cols - 1);
        arma::mat res(sub_x.n_rows, n_fracs_out, arma::fill::zeros);

        int n_pixels = sub_x.n_rows;
        arma::mat y(n_fracs_in, n_pixels), y_prior(n_fracs_in, n_pixels);
        y.fill(0);
        y_prior.fill(-9999);
        arma::mat y_diff = y - y_prior; // create a non-negative matrix

        // switching to arma here for nice matrix multiplication
        arma::mat emx = -em * sub_x.t(); // negative A * b
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
        arma::mat ppdiff = preds - sub_x;
        arma::mat error = arma::sqrt(arma::mean(arma::pow(ppdiff, 2), 1));

        // normalization between 0 and 1
        arma::colvec prob_sum = arma::sum(probs, 1);
        for (arma::uword i = 0; i < probs.n_cols; i++) {
            probs.col(i) /= prob_sum;
        }

        // prepare output
        res.cols(0, n_fracs_in - 1) = probs;
        // should the rmse be added?
        if (rmse) {
            res.col(n_fracs_out - 1) = error;
        }
        s.insert_rows(start_row, res);
    }

    return s;
}


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

