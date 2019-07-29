#include <Rcpp.h>
#include <stdio.h>
using namespace Rcpp;

// This function normalizes a matrix, by considering a maximum and a minimum value

// [[Rcpp::export]]

NumericMatrix normalize_data(const NumericMatrix& data, const double& min, const double& max) {

    int nrows = data.nrow();
    int ncols = data.ncol();

    NumericMatrix new_data(nrows, ncols);

    new_data = (data - min) / (max - min);

    for (int i = 0; i < ncols; i++)
        for (int j = 0; j < nrows; j++){
            if (new_data(j,i) >= 1.0) new_data(j,i) = 1.0;
            if (new_data(j,i) <= 0.0) new_data(j,i) = 0.0001;
        }

    return new_data;
}
