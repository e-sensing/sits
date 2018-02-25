#include <Rcpp.h>
#include <stdio.h>
using namespace Rcpp;

// This function preprocesses the raster brick data, by removing missing values and those
// smaller than the minimum value

// [[Rcpp::export]]

NumericMatrix preprocess_data(NumericMatrix data, const int& missing_value, const int& minimum_value,
                              const double& scale_factor) {

    int nrows = data.nrow();
    int ncols = data.ncol();

    int right = ncols - 1;

    NumericMatrix new_data(nrows, ncols);

    // first, take care of the boundary conditions
    for (int i = 0; i < nrows; i++){
        if (data(i, 0) == missing_value || data(i, 0) <= minimum_value)
            data(i, 0) = data(i, 1);
        if (data(i, right) == missing_value || data(i, right) <= minimum_value)
            data(i, right) = data(i, right - 1);
    }
    // then, process the inside of the matrix
    for (int i = 0; i < nrows; i++)
        for (int j = 1; j < right; j++)
            if (data(i, j) == missing_value || data(i, j) <= minimum_value)
                data(i, j) = (data(i, j - 1 ) + data (i, j + 1))/2;

    new_data = data * scale_factor;

    return new_data;
}
