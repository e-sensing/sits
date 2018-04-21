#include <Rcpp.h>
#include <stdio.h>
using namespace Rcpp;

// This function preprocesses the raster brick data, by removing missing values and those
// smaller than the minimum value

// [[Rcpp::export]]

NumericMatrix scale_data(NumericMatrix data, const double& scale_factor, const double& adj_val = 0.0) {

    int nrows = data.nrow();
    int ncols = data.ncol();

    NumericMatrix new_data(nrows, ncols);

    new_data = data * scale_factor + adj_val;

    return new_data;
}
