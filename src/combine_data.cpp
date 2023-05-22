#include <Rcpp.h>
#include <stdio.h>
using namespace Rcpp;
// This function calculates the weighted average of probs cubs

// [[Rcpp::export]]

NumericMatrix weighted_probs(const List& data_lst, const NumericVector& weights) {

    int n_classifiers = data_lst.length();
    NumericMatrix mat = data_lst[0];
    int nrows = mat.nrow();
    int ncols = mat.ncol();

    NumericMatrix new_data(nrows, ncols);

    for (int c = 0; c < n_classifiers; c++){
        NumericMatrix mat1 = data_lst[c];
        for (int i = 0; i < nrows; i++)
            for (int j = 0; j < ncols; j++)
                new_data(i,j) = new_data(i,j) + weights(c)*mat1(i,j);
    }

    return new_data;
}

// This function calculates the weighted average of probs cubs
// considering its uncertainty values

// [[Rcpp::export]]

NumericMatrix weighted_uncert_probs(const List& data_lst, const List& unc_lst) {

    int n_classifiers = data_lst.length();
    NumericMatrix mat    = data_lst[0];
    int nrows = mat.nrow();
    int ncols = mat.ncol();

    NumericMatrix new_data(nrows, ncols);
    NumericVector sum_unc(nrows);
    NumericMatrix unc_norm(nrows, n_classifiers);
    NumericMatrix conf(nrows, n_classifiers);
    NumericVector sum_conf(nrows);
    NumericMatrix weights(nrows, n_classifiers);

    // sum of the uncertainties per pixel
    for (int c = 0; c < n_classifiers; c++){
        NumericMatrix unc = unc_lst[c];  // uncert for classifier c
        for (int i = 0; i < nrows; i++) {
            sum_unc(i) += unc(i,0);
        }
    }
    // normalized uncertainties per pixel
    for (int c = 0; c < n_classifiers; c++){
        NumericMatrix unc = unc_lst[c];  // uncert for classifier c
        for (int i = 0; i < nrows; i++) {
            unc_norm(i,c) = unc(i,0)/sum_unc(i);
        }
    }
    // inverse uncertainties per pixel
    for (int c = 0; c < n_classifiers; c++){
        for (int i = 0; i < nrows; i++) {
            conf(i,c) = 1/unc_norm(i,c);
        }
    }
    // sum of the inverse uncertainties per pixel
    for (int i = 0; i < nrows; i++) {
        for (int c = 0; c < n_classifiers; c++){
            sum_conf(i) += conf(i,c);
        }
    }
    // weights to be applied
    for (int c = 0; c < n_classifiers; c++){
        for (int i = 0; i < nrows; i++) {
            weights(i,c) = conf(i,0)/sum_conf(i);
        }
    }
    // weighted average
    for (int c = 0; c < n_classifiers; c++){
        NumericMatrix mat1 = data_lst[c];
        for (int i = 0; i < nrows; i++)
            for (int j = 0; j < ncols; j++)
                new_data(i,j) = new_data(i,j) + weights(i,c)*mat1(i,j);
    }

    return new_data;
}
