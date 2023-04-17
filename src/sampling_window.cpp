#include <Rcpp.h>
#include <vector>

using namespace Rcpp;

// [[Rcpp::export]]
DataFrame C_max_sampling(const NumericVector& x, int nrows,
                         int ncols, int window_size) {

    // initialize result vectors
    IntegerVector res_cell;
    NumericVector res_value;
    if (window_size < 1)
        return DataFrame::create(_["cell"] = res_cell, _["value"] = res_value);

    // compute values for each pixel
    for (int i = 0; i < nrows; i += window_size) {
        for (int j = 0; j < ncols; j += window_size) {
            int max_wi = std::min(nrows, i + window_size);
            int max_wj = std::min(ncols, j + window_size);
            int cell = i * ncols + j;
            int max_value = x(cell);
            for (int wi = i; wi < max_wi; ++wi) {
                for (int wj = j; wj < max_wj; ++wj) {
                    if (x(wi * ncols + wj) > max_value) {
                        cell = wi * ncols + wj;
                        max_value = x(cell);
                    }
                }
            }
            res_cell.push_back(cell);
            res_value.push_back(max_value);
        }
    }

    return DataFrame::create(_["cell"] = res_cell, _["value"] = res_value);
}
