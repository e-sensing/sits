#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector skater_smoother_fraction(const NumericMatrix& values,
                                       const int& nrows,
                                       const int& ncols,
                                       const int& ncuts
) {
    // initialize result vectors
    NumericMatrix res(values.nrow(), values.ncol());

    return res;
}

