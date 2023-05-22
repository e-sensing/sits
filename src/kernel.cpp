#include <Rcpp.h>

using namespace Rcpp;

typedef double _kernel_fun(const Rcpp::NumericVector&);

// compute outside indices of a vector as a mirror
IntegerVector locus_mirror(int size, int leg) {
    IntegerVector res(size + 2 * leg);
    for (int i = 0; i < res.length(); ++i) {
        if (i < leg)
            res(i) = leg - i - 1;
        else if (i < size + leg)
            res(i) = i - leg;
        else
            res(i) = 2 * size + leg - i - 1;
    }
    return res;
}

// kernel functions
inline double _median(const NumericVector& neigh) {
    return median(neigh, true);
}
inline double _mean(const NumericVector& neigh) {
    return mean(na_omit(neigh));
}
inline double _sd(const NumericVector& neigh) {
    return sd(na_omit(neigh));
}
inline double _min(const NumericVector& neigh) {
    return min(na_omit(neigh));
}
inline double _max(const NumericVector& neigh) {
    return max(na_omit(neigh));
}


NumericVector kernel_fun(const NumericMatrix& x, int ncols, int nrows,
                         int band, int window_size, _kernel_fun _fun) {
    // initialize result vectors
    NumericVector res(x.nrow());
    NumericVector neigh(window_size * window_size);
    if (window_size < 1) {
        res = x(_, band);
        return res;
    }
    // compute window leg
    int leg = window_size / 2;
    // compute locus mirror
    IntegerVector loci = locus_mirror(nrows, leg);
    IntegerVector locj = locus_mirror(ncols, leg);
    // compute values for each pixel
    for (int i = 0; i < nrows; ++i) {
        for (int j = 0; j < ncols; ++j) {
            // window
            for (int wi = 0; wi < window_size; ++wi)
                for (int wj = 0; wj < window_size; ++wj)
                    neigh(wi * window_size + wj) =
                        x(loci(wi + i) * ncols + locj(wj + j), band);
            // call specific function
            res(i * ncols + j) = _fun(neigh);
        }
    }
    return res;
}
// [[Rcpp::export]]
NumericVector C_kernel_median(const NumericMatrix& x, int ncols,
                              int nrows, int band, int window_size) {
    return kernel_fun(x, ncols, nrows, band, window_size, _median);
}
// [[Rcpp::export]]
NumericVector C_kernel_mean(const NumericMatrix& x, int ncols,
                            int nrows, int band, int window_size) {
    return kernel_fun(x, ncols, nrows, band, window_size, _mean);
}
// [[Rcpp::export]]
NumericVector C_kernel_sd(const NumericMatrix& x, int ncols,
                          int nrows, int band, int window_size) {
    return kernel_fun(x, ncols, nrows, band, window_size, _sd);
}
// [[Rcpp::export]]
NumericVector C_kernel_min(const NumericMatrix& x, int ncols,
                           int nrows, int band, int window_size) {
    return kernel_fun(x, ncols, nrows, band, window_size, _min);
}
// [[Rcpp::export]]
NumericVector C_kernel_max(const NumericMatrix& x, int ncols,
                           int nrows, int band, int window_size) {
    return kernel_fun(x, ncols, nrows, band, window_size, _max);
}
