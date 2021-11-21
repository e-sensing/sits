#include <Rcpp.h>
#include <cmath>
#include <iostream>
using namespace Rcpp;

// This function calculates the entropy
int entropy(IntegerVector& vec, const int& n){
    float ent = 0;

    for (int i = 0; i < n; ++i){
        float prob_i = float(vec(i)/10000.);
        if (prob_i > 0.) {
            ent -= prob_i*(log(prob_i)/log(n));
        }
    }
    int i_ent = (int)(10000*ent);
    return i_ent;
}
// [[Rcpp::export]]
IntegerVector entropy_probs(const IntegerMatrix& mtx, const int& n) {

    int nrows = mtx.nrow();
    int ncols = mtx.ncol();

    IntegerVector vec(ncols);
    IntegerVector unc(nrows);

    for (int i = 0; i < nrows; i++) {
        IntegerVector vec = mtx(i, _);
        unc(i) = entropy(vec, n);
    }
    return unc;
}

