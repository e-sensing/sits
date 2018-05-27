#include <Rcpp.h>
#include <stdio.h>
using namespace Rcpp;

// This function preprocesses the raster brick data, by removing missing values and those
// smaller than the minimum value

// [[Rcpp::export]]

IntegerMatrix scale_matrix_integer(NumericMatrix data, const double& scale_factor) {

    int nrows = data.nrow();
    int ncols = data.ncol();

    IntegerMatrix new_data(nrows, ncols);

    for (int i = 0; i < ncols; i++)
        for (int j = 0; j < nrows; j++)
            new_data(i,j) = static_cast<int> (data(i,j) * scale_factor);

    return new_data;
}
