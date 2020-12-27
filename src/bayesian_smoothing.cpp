#include <Rcpp.h>
#include <cmath>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector build_neigh(const NumericMatrix& data,
                          const NumericMatrix& window,
                          const int& i,
                          const int& j) {

    NumericVector neigh;

    for (int k = 0; k < window.rows(); ++k) {
        for (int l = 0; l < window.cols(); ++l) {
            int data_i = i + k - window.rows() / 2, data_j = j + l - window.cols() / 2;

            if (data_i >= 0 && data_j >= 0 &&
                data_i < data.nrow() && data_j < data.ncol() && window(k, l) > 0 &&
                !std::isnan(data(data_i, data_j))) {
                neigh.push_back(double(data(data_i, data_j) * window(k, l)));
            }
        }
    }

    return neigh;
}

double bayes_estimator_pixel(const double& p,
                             const NumericVector& neigh,
                             const double& smoothness,
                             const double& max_prob) {


    if (std::isnan(p)) return NAN;
    // calculate the log of the odds (p/1-p)
    NumericVector logit_neigh = log(neigh / (max_prob - neigh));
    double logit_p = log( p / (max_prob - p));
    // estimate variance of the neighborhood
    double local_var = var(logit_neigh);
    // w1 is controlled by the local variance
    // (higher local variances decrease confidence in the neighbors)
    double w1 = local_var / (smoothness + local_var);
    // w2 is controlled by local smoothness
    // (lower local variance increase confidence in the neighbors)
    double w2 = smoothness / (smoothness +  local_var);
    // calculate the bayesian logit value for the pixel
    double smooth_logit = w1 * logit_p + w2 * mean(logit_neigh);
    // calculate the bayesian probability for the pixel
    double prob_bay = exp(smooth_logit)*max_prob/(exp(smooth_logit) + 1);

    return prob_bay;
}

// [[Rcpp::export]]
NumericVector bayes_estimator(const NumericMatrix& data,
                              const NumericMatrix& window,
                              const double& smoothness,
                              const double& max_prob) {

    int nrows = data.nrow();
    int ncols = data.ncol();

    NumericVector result(nrows * ncols);

    int k = 0;
    for (int i = 0; i < nrows; ++i) {
        for (int j = 0; j < ncols; ++j) {

            NumericVector neigh = build_neigh(data, window, i, j);
            result(k++) =  bayes_estimator_pixel(data(i, j),
                                                   neigh,
                                                   smoothness,
                                                   max_prob);
        }
    }

    return result;
}




