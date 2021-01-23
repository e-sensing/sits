#include <Rcpp.h>
#include <cmath>
#include <tgmath.h>

using namespace Rcpp;

double conv_2D(const NumericMatrix& data,
               const NumericMatrix& kernel,
               const int& i,
               const int& j) {

    double value = 0;
    double w_sum = 0;

    for (int k = 0; k < kernel.rows(); ++k) {
        for (int l = 0; l < kernel.cols(); ++l) {
            int d_i = i + k - kernel.rows() / 2;
            int d_j = j + l - kernel.cols() / 2;

            if (d_i >= 0 && d_j >= 0 &&
                d_i < data.nrow() &&
                d_j < data.ncol() &&
                !std::isnan(data(d_i, d_j))){
                    value = value + data(d_i, d_j) * kernel(k, l);
                    w_sum = w_sum + kernel(k, l);
            }
        }
    }
    value = value/w_sum;

    return value;
}

double conv_2D_non_linear(const NumericMatrix& data,
                          const NumericMatrix& kernel,
                          const double& tau,
                          const double& scale_factor,
                          const int& i,
                          const int& j) {

    double value = 0;
    double w_sum = 0;

    for (int k = 0; k < kernel.rows(); ++k) {
        for (int l = 0; l < kernel.cols(); ++l) {
            int d_i = i + k - kernel.rows() / 2;
            int d_j = j + l - kernel.cols() / 2;

            if (d_i >= 0 && d_j >= 0 &&
                d_i < data.nrow() &&
                d_j < data.ncol() &&
                !std::isnan(data(d_i, d_j))) {
                double diff = abs(data(i,j) - data(d_i, d_j))*scale_factor;
                double smoother = exp(-diff/(2*pow(tau, 2.0)));
                value = value + data(d_i, d_j) * kernel(k, l) * smoother;
                w_sum = w_sum + kernel(k, l)*smoother;
            }
        }
    }
    value = value/w_sum;

    return value;
}


// [[Rcpp::export]]
NumericVector kernel_estimator(const NumericMatrix& data,
                               const NumericMatrix& kernel) {

    int nrows = data.nrow();
    int ncols = data.ncol();

    NumericVector result(nrows * ncols);

    int k = 0;
    for (int i = 0; i < nrows; ++i) {
        for (int j = 0; j < ncols; ++j) {
            result(k++) = conv_2D(data, kernel, i, j);
        }
    }
    return result;
}

// [[Rcpp::export]]
NumericVector kernel_estimator_non_linear(const NumericMatrix& data,
                                          const NumericMatrix& kernel,
                                          const double& tau,
                                          const double& scale_factor) {

    int nrows = data.nrow();
    int ncols = data.ncol();

    NumericVector result(nrows * ncols);

    int k = 0;
    for (int i = 0; i < nrows; ++i) {
        for (int j = 0; j < ncols; ++j) {
            result(k++) = conv_2D_non_linear(data, kernel, tau,
                   scale_factor, i, j);
        }
    }
    return result;
}



