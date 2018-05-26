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

    new_data = data * scale_factor;

    return new_data;
}
