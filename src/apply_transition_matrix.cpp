#include <Rcpp.h>
#include <stdio.h>
using namespace Rcpp;

// This function preprocesses the raster brick data, by removing missing values and those
// smaller than the minimum value

// [[Rcpp::export]]

NumericMatrix apply_transition_matrix(NumericMatrix data_before, NumericMatrix data,
                                      NumericMatrix transition_matrix) {

    int nrows = data.nrow();
    int ncols = data.ncol();

    NumericMatrix new_data(nrows, ncols);

    // first, take care of the boundary conditions
    for (int i = 0; i < nrows; i++){
        for (int j = 0; j < ncols; j++) {
           new_data(i,j) = transition_matrix(data_before(i,j), data(i,j));
        }
    }
    return new_data;
}
