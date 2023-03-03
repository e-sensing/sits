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
inline double _sum(const NumericVector& neigh) {
    return sum(na_omit(neigh));
}
inline double _mean(const NumericVector& neigh) {
    return mean(na_omit(neigh));
}
inline double _sd(const NumericVector& neigh) {
    return sd(na_omit(neigh));
}
inline double _var(const NumericVector& neigh) {
    return var(na_omit(neigh));
}
inline double _min(const NumericVector& neigh) {
    return min(na_omit(neigh));
}
inline double _max(const NumericVector& neigh) {
    return max(na_omit(neigh));
}
inline double _bayes_mean(const NumericVector& neigh){
    NumericVector n_neigh = clone(neigh);
    NumericVector n_pos;
    NumericVector n_neg;
    for(NumericVector::iterator it = n_neigh.begin(); it != n_neigh.end(); ++it) {
        if (*it >= 0)
            n_pos.push_back(*it);
        else
            n_neg.push_back(*it);
    }
    if (n_pos.size() >= n_neg.size())
        return mean(n_pos);
    else
        return mean(n_neg);
}
inline double _bayes_var(const NumericVector& neigh){
    NumericVector n_neigh = clone(neigh);
    NumericVector n_pos;
    NumericVector n_neg;
    for(NumericVector::iterator it = n_neigh.begin(); it != n_neigh.end(); ++it) {
        if (*it >= 0)
            n_pos.push_back(*it);
        else
            n_neg.push_back(*it);
    }
    if (n_pos.size() >= n_neg.size())
        return var(n_pos);
    else
        return var(n_neg);
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
            res(i * ncols + j, 0) = _fun(neigh);
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
NumericVector C_kernel_sum(const NumericMatrix& x, int ncols,
                           int nrows, int band, int window_size) {
    return kernel_fun(x, ncols, nrows, band, window_size, _sum);
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
NumericVector C_kernel_var(const NumericMatrix& x, int ncols,
                           int nrows, int band, int window_size) {
    return kernel_fun(x, ncols, nrows, band, window_size, _var);
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
// [[Rcpp::export]]
NumericMatrix C_kernel_bayes_mean(const NumericMatrix& x, int ncols,
                             int nrows, int window_size){

    NumericMatrix res(x.nrow(), x.ncol());

    for (int band = 0; band < x.ncol(); band++){
        res(_, band) = kernel_fun(x, ncols, nrows, band, window_size, _bayes_mean);
    }
    return res;
}
// [[Rcpp::export]]
NumericMatrix C_kernel_bayes_var(const NumericMatrix& x, int ncols,
                                  int nrows, int window_size){

    NumericMatrix res(x.nrow(), x.ncol());

    for (int band = 0; band < x.ncol(); band++){
        res(_, band) = kernel_fun(x, ncols, nrows, band, window_size, _bayes_var);
    }
    return res;
}
// [[Rcpp::export]]
NumericMatrix C_bayes_posterior(const NumericMatrix& x,
                                const NumericVector& s,
                                const NumericMatrix& m,
                                const NumericMatrix& v){
    NumericMatrix y(x);

    for (int label = 0; label < x.ncol(); label++){
        for (int pixel = 0; pixel < x.nrow(); pixel++){
            double d = (s(label) + v(pixel, label));
            y(pixel, label) = s(label)/d * m(pixel, label) +
                v(pixel, label)/d * x(pixel, label);
        }
    }
    return y;
}
