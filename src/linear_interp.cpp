#include <Rcpp.h>
#include <stdio.h>
#include <algorithm>
using namespace Rcpp;

//
// Adapted from Toby Speight's post below
// https://codereview.stackexchange.com/questions/208526/linearly-interpolate-na-values-in-rcppIntegervector
//

bool detect_start_na(const double& a, const double& b){
    return (!NumericVector::is_na(a) && NumericVector::is_na(b));
}

bool detect_end_na(const double& a, const double& b){
    return (NumericVector::is_na(a) && !NumericVector::is_na(b));
}

double na_interp(double& first_val, double& next_val, const int& gaps,
                 const int& i){
    // the minimum value of gaps is 2
    return (first_val + (i * (next_val - first_val) / gaps));
}

NumericVector na_linear_vector_interp(NumericVector& x) {
    // This function linearly interpolates to fill sequences of NA
    // values surrounded by valid numbers.
    // detect leading NAs
    NumericVector::iterator curr  = x.begin();
    NumericVector::iterator first = x.begin();
    NumericVector::iterator last  = x.end();
    while (NumericVector::is_na(*curr) && curr != last) {
        ++curr;
    }
    if (curr == x.end()) return x;
    while (first != curr) {
        *first++ = *curr;
    }
    // find the first NA after the first pixel
    while (true) {
        // curr points to a non NA pixel
        NumericVector::iterator num_to_na =
            std::adjacent_find(curr, last, detect_start_na);
        if (num_to_na == last) break;
        NumericVector::iterator na_to_num =
            std::adjacent_find(num_to_na, last, detect_end_na);
        // account for trailing NAs
        if (na_to_num == last) {
            double val = *num_to_na;
            while (num_to_na != last) {
                *num_to_na++ = val;
            }
            break;
        }
        // At this point, num_to_na points to the last number before
        // an interpolation block, and na_to_num points to the last NA
        // of that block.
        // After ++na_to_num below, both iterators point to numbers.
        // find out the gaps
        int gaps = std::distance(num_to_na, ++na_to_num);
        // generate the missing values
        int i = 0;
        double base = *num_to_na;
        double target = *na_to_num;
        while (++num_to_na != na_to_num) {
            *num_to_na = na_interp(base, target, gaps, ++i);
        }
        // Advance onward
        curr = na_to_num;
        if (curr == last) break;
    }
    return x;
}
// This function interpolates matrix with NA values using linear methods
// [[Rcpp::export]]
NumericMatrix linear_interp(NumericMatrix& mtx) {
    int nrows = mtx.nrow();
    int ncols = mtx.ncol();
    NumericVector vec(ncols);
    for (int i = 0; i < nrows; i++) {
        NumericVector vec = mtx(i, _);
        mtx(i, _) = na_linear_vector_interp(vec);
    }
    return mtx;
}

// [[Rcpp::export]]
NumericVector linear_interp_vec(NumericVector& vec) {
    return na_linear_vector_interp(vec);
}
// [[Rcpp::export]]
LogicalVector C_mask_na(const NumericMatrix& x) {
    int nrows = x.nrow();
    LogicalVector vec(nrows);
    for (int i = 0; i < nrows; i++) {
        vec(i) = any(is_na(x.row(i)));
    }
    return vec;
}
// [[Rcpp::export]]
NumericMatrix C_fill_na(const NumericMatrix& x, double fill) {
    int nrows = x.nrow();
    int ncols = x.ncol();
    NumericMatrix res(nrows, ncols, x.begin());

    for (int i = 0; i < nrows; i++) {
        if (any(is_na(res.row(i)))) {
            for (int j = 0; j < ncols; j++)
                res(i, j) = fill;
        }
    }
    return res;
}
