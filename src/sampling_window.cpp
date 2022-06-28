#include <Rcpp.h>
#include <vector>

using namespace Rcpp;

// [[Rcpp::export]]
DataFrame max_sampling(const IntegerMatrix& data,
                       const int band,
                       const int img_nrow,
                       const int img_ncol,
                       const int window_size) {

    // initialize result vectors
    IntegerVector res_cell;
    IntegerVector res_value;
    if (window_size < 1)
        return DataFrame::create(_["cell"] = res_cell, _["value"] = res_value);

    // compute values for each pixel
    for (int i = 0; i < img_nrow; i += window_size) {
        for (int j = 0; j < img_ncol; j += window_size) {
            int max_wi = std::min(img_nrow, i + window_size);
            int max_wj = std::min(img_ncol, j + window_size);
            int cell = i * img_ncol + j;
            int max_value = data(cell, band);
            for (int wi = i; wi < max_wi; ++wi) {
                for (int wj = j; wj < max_wj; ++wj) {
                    if (data(wi * img_ncol + wj, band) > max_value) {
                        cell = wi * img_ncol + wj;
                        max_value = data(cell, band);
                    }
                }
            }
            res_cell.push_back(cell);
            res_value.push_back(max_value);
        }
    }

    return DataFrame::create(_["cell"] = res_cell, _["value"] = res_value);
}
