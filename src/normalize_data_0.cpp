#include <Rcpp.h>

using namespace Rcpp;

// This function should be maintained to support old versions of model
//   normalization (done outside model functions).

// [[Rcpp::export]]
NumericMatrix C_normalize_data_0(const NumericMatrix& data, const double& min,
                                 const double& max) {

    int nrows = data.nrow();
    int ncols = data.ncol();

    NumericMatrix new_data(nrows, ncols);

    new_data = (data - min) / (max - min);

    for (int i = 0; i < ncols; i++)
        for (int j = 0; j < nrows; j++){
            if (new_data(j,i) >= 1.0) new_data(j,i) = 1.0;
            if (new_data(j,i) <= 0.0) new_data(j,i) = 0.0001;
        }

        return new_data;
}
