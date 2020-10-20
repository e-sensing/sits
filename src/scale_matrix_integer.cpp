#include <Rcpp.h>
#include <stdio.h>
using namespace Rcpp;

// This function scales a numeric matrix and saves the result as integer

// [[Rcpp::export]]

IntegerMatrix scale_matrix_integer(NumericMatrix& data, const double& scale_factor) {

    int nrows = data.nrow();
    int ncols = data.ncol();

    IntegerMatrix new_data(nrows, ncols);

    for (int i = 0; i < nrows; i++)
        for (int j = 0; j < ncols; j++)
            new_data(i,j) = (int)(data(i,j) * scale_factor);

    return new_data;
}
