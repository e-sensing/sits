#include <Rcpp.h>

using namespace Rcpp;

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

// functions
void _median(NumericVector& res, int i, const NumericVector& neigh) {
    res(i) = median(neigh, true);
}
void _sum(NumericVector& res, int i, const NumericVector& neigh) {
    res(i) = sum(na_omit(neigh));
}
void _mean(NumericVector& res, int i, const NumericVector& neigh) {
    res(i) = mean(na_omit(neigh));
}
void _sd(NumericVector& res, int i, const NumericVector& neigh) {
    res(i) = sd(na_omit(neigh));
}
void _var(NumericVector& res, int i, const NumericVector& neigh) {
    res(i) = var(na_omit(neigh));
}
void _min(NumericVector& res, int i, const NumericVector& neigh) {
    res(i) = min(na_omit(neigh));
}
void _max(NumericVector& res, int i, const NumericVector& neigh) {
    res(i) = max(na_omit(neigh));
}

// [[Rcpp::export]]
NumericVector kernel_fun(const NumericMatrix& data,
                         const int band,
                         const int img_nrow,
                         const int img_ncol,
                         const int window_size,
                         const int fun) {

    // initialize result vectors
    NumericVector res(data.nrow());
    NumericVector neigh(window_size * window_size);
    if (window_size < 1) {
        res = data(_, band);
        return res;
    }

    // function
    if (fun >= 7) stop("invalid function index");
    void (*_fun[7])(NumericVector&, int, const NumericVector&) = {
        _median, _sum, _mean, _sd, _var, _min, _max
        //0      //1   //2    //3  //4   //5   //6
    };

    // compute window leg
    int leg = window_size / 2;

    // compute locus mirror
    IntegerVector loci = locus_mirror(img_nrow, leg);
    IntegerVector locj = locus_mirror(img_ncol, leg);

    // compute values for each pixel
    for (int i = 0; i < img_nrow; ++i) {
        for (int j = 0; j < img_ncol; ++j) {
            // window
            for (int wi = 0; wi < window_size; ++wi)
                for (int wj = 0; wj < window_size; ++wj)
                    neigh(wi * window_size + wj) =
                        data(loci(wi + i) * img_ncol + locj(wj + j), band);
            // call specific function
            (*_fun[fun])(res, i * img_ncol + j, neigh);
        }
    }
    return res;
}
