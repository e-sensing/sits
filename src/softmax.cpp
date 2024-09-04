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
NumericMatrix softmax(NumericMatrix values) {
    // initialize result vectors
    int nrows = values.nrow();
    int ncols = values.ncol();
    NumericMatrix soft(values.nrow(), values.ncol());

    // get the maximum value
    double max_value = max(values);

    for (int i = 0; i < nrows; ++i) {
        double sum = 0;
        NumericVector row_values = values(i, _);
        // get the sum of exponentials (denominator)
        for (int j = 0; j < ncols; ++j){
            sum += exp(row_values(j) - max_value);
        }
        const double scale = max_value + log(sum);
        for (int j = 0; j < ncols; ++j) {
            soft(i,j) = exp(row_values(j) - scale);
        }
    }
    return(soft);
}
