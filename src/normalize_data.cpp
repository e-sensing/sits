#include <Rcpp.h>
#include <stdio.h>
using namespace Rcpp;

// This function preprocesses the raster brick data, by removing missing values and those
// smaller than the minimum value

// [[Rcpp::export]]

NumericMatrix normalize_data(const NumericMatrix& data, const double& quant_2, const double& quant_98) {

    int nrows = data.nrow();
    int ncols = data.ncol();

    NumericMatrix new_data(nrows, ncols);

    new_data = (data - quant_2) / (quant_98 - quant_2);

    for (int i = 0; i < ncols; i++)
        for (int j = 0; j < nrows; j++){
            if (new_data(j,i) >= 1.0) new_data(j,i) = 1.0;
            if (new_data(j,i) <= 0.0) new_data(j,i) = 0.001;
        }

    return new_data;
}
