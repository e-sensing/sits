#include <Rcpp.h>
#include <stdio.h>
using namespace Rcpp;

// This function preprocesses the raster brick data, by removing missing values and those
// smaller than the minimum value

// [[Rcpp::export]]

NumericMatrix normalize_data(const NumericMatrix& data, const double& mean, const double& std) {

    int nrows = data.nrow();
    int ncols = data.ncol();

    NumericMatrix new_data(nrows, ncols);

    new_data = (data - mean) / std;

    return new_data;
}
