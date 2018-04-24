#include <Rcpp.h>
#include <stdio.h>
using namespace Rcpp;

// This function preprocesses the raster brick data, by removing missing values and those
// smaller than the minimum value

// [[Rcpp::export]]

NumericMatrix normalize_data(const NumericMatrix& data, const double& med, const double& iqr) {

    int nrows = data.nrow();
    int ncols = data.ncol();

    double max = med + iqr;
    double min = med - iqr;

    NumericMatrix new_data(nrows, ncols);

    new_data = data;

    for (int i = 0; i < ncols; i++)
        for (int j = 0; j < nrows; j++){
            if (new_data(j,i) > max) new_data(j,i) = max;
            if (new_data(j,i) < min) new_data(j,i) = min;
        }

    new_data = (data - med) / iqr;

    return new_data;
}
