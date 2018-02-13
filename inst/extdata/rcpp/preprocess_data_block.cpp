#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericMatrix preprocess_data_block(NumericMatrix data, int missing_value, int minimum_value,
                                double scale_factor, int nrows, int ncols, double adj_value) {

    // remove missing values, NA data and values below the minimum

    // consider boundary conditions
    // top row
    for (int i = 0; i < ncols; i++) {
        if (data(i,0) == missing_value || data(i,0) < minimum_value || R_IsNA(data(i,0)))
            data(i,0) = data(i, 1);
    }
    //bottom row
    int bottom = nrows - 1;
    for (int i = 0; i < ncols; i++) {
        if (data(i, bottom) == missing_value || data(i, bottom ) < minimum_value || R_IsNA(data(i, bottom)))
            data(i, bottom) = data(i, bottom - 1);
    }
    //left side
    for (int j = 0; j < nrows; j++) {
        if (data(0, j) == missing_value || data(0, j) < minimum_value || R_IsNA(data(0, j)))
            data(0, j) = data(1, j);
    }
    //right side
    int right = ncols - 1;
    for (int j = 0; j < nrows; j++) {
        if (data(right, j) == missing_value || data(right, j) < minimum_value || R_IsNA(data(right, j)))
            data(right, j) = data(right - 1, j);
    }
    //go through the inside of the image - remove missing values and scale data
    for (int i = 0; i < right; i++) {
        for (int j = 0; j < bottom; j++) {
            if (data(i,j) == missing_value || data(i,j) < minimum_value || R_IsNA(data(i,j)))
                    data(i,j) = (data(i, j - 1) + data(i, j + 1))/2;
            data(i,j) = double(data(i,j)) * scale_factor + adj_value;
        }
    }
    return data;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//
