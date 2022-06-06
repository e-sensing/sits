#include <Rcpp.h>
#include <cmath>
#include <iostream>
using namespace Rcpp;

// This function calculates the least confidence
int least(IntegerVector& vec, const int& n){
    float pmax = 0;

    for (int i = 0; i < n; ++i){
        float prob_i = float(vec(i)/10000.);
        if (prob_i > pmax) {
            pmax = prob_i;
        }
    }
    float least = 1 - pmax;
    int i_least = (int)(10000 * least);
    return i_least;
}
// [[Rcpp::export]]
IntegerVector least_probs(const IntegerMatrix& mtx, const int& n) {

    int nrows = mtx.nrow();
    int ncols = mtx.ncol();

    IntegerVector vec(ncols);
    IntegerVector unc(nrows);

    for (int i = 0; i < nrows; i++) {
        IntegerVector vec = mtx(i, _);
        unc(i) = least(vec, n);
    }
    return unc;
}

