#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

// global variables
arma::vec p_025 = {0.25};
arma::vec p_050 = {0.50};
arma::vec p_075 = {0.75};

// [[Rcpp::export]]
arma::vec C_temp_max(const arma::mat& mtx) {
    return arma::max(mtx, 1);
}

// [[Rcpp::export]]
arma::vec C_temp_min(const arma::mat& mtx) {
    return arma::min(mtx, 1);
}

// [[Rcpp::export]]
arma::vec C_temp_mean(const arma::mat& mtx) {
    return arma::mean(mtx, 1);
}

// [[Rcpp::export]]
arma::vec C_temp_median(const arma::mat& mtx) {
    return arma::median(mtx, 1);
}

// [[Rcpp::export]]
arma::vec C_temp_sum(const arma::mat& mtx) {
    return arma::sum(mtx, 1);
}

// [[Rcpp::export]]
arma::vec C_temp_std(const arma::mat& mtx) {
    return arma::stddev(mtx, 0, 1);
}

// [[Rcpp::export]]
arma::vec C_temp_skew(const arma::mat& mtx) {
    // skewness based on adjusted Fisher-Pearson coefficient
    const int n = mtx.n_cols;
    const double expS = 1.5;

    // adjusted factor
    double adj_factor = sqrt((n*(n-1)))/n-2;

    arma::vec m3 = arma::sum(arma::pow(mtx.each_col()- arma::mean(mtx, 1), 3), 1)/n;
    arma::vec s = arma::pow(arma::sum(arma::pow(mtx.each_col()- arma::mean(mtx, 1), 2), 1)/n, expS);

    return (m3/s)*adj_factor;
}

// [[Rcpp::export]]
arma::vec C_temp_kurt(const arma::mat& mtx) {
    // kurtosis based on pearson’s definition is used (normal ==> 3.0)
    const int n = mtx.n_cols;

    arma::vec m4 = arma::sum(arma::pow(mtx.each_col()- arma::mean(mtx, 1), 4), 1);
    arma::vec m2 = arma::pow(arma::sum(arma::pow(mtx.each_col()- arma::mean(mtx, 1), 2), 1), 2);

    return n*m4/m2;
}

// [[Rcpp::export]]
arma::vec C_temp_amplitude(const arma::mat& mtx) {
    return arma::max(mtx, 1) - arma::min(mtx, 1);
}

// [[Rcpp::export]]
arma::vec C_temp_fslope(const arma::mat& mtx) {
    return arma::max(arma::abs(arma::diff(mtx, 1, 1)), 1);
}

// [[Rcpp::export]]
arma::vec C_temp_abs_sum(const arma::mat& mtx) {
    return arma::sum(arma::abs(mtx), 1);
}

// [[Rcpp::export]]
arma::vec C_temp_amd(const arma::mat& mtx) {
    return arma::mean(arma::abs(arma::diff(mtx, 1, 1)), 1);
}

// [[Rcpp::export]]
arma::vec C_temp_mse(const arma::mat& mtx) {
    arma::mat metrics = mtx.t();
    return arma::mean(arma::pow(arma::abs(arma::trans(arma::fft(metrics))), 2), 1);
}

// [[Rcpp::export]]
arma::vec C_temp_fqr(const arma::mat& mtx) {
    return arma::quantile(mtx, p_025, 1);
}

// [[Rcpp::export]]
arma::vec C_temp_tqr(const arma::mat& mtx) {
    return arma::quantile(mtx, p_075, 1);
}

// [[Rcpp::export]]
arma::vec C_temp_iqr(const arma::mat& mtx) {
    arma::vec res = C_temp_tqr(mtx) - C_temp_fqr(mtx);
    return (res);
}
