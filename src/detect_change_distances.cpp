#include <Rcpp.h>
#include "./dtw.h"

using namespace Rcpp;

/**
 * Convert `NumericMatrix` to 2D `std::vector`.
 *
 * @description
 * This function converts a `NumericMatrix` into a 2D `std::vector`.
 *
 * @param mat A `NumericMatrix` with single or multi variate time-series.
 */
std::vector<std::vector<double>> to_cpp_vector(NumericMatrix mat) {
    size_t rows = mat.nrow();
    size_t cols = mat.ncol();

    std::vector<std::vector<double>> result(rows, std::vector<double>(cols));

    for(size_t i = 0; i < rows; ++i) {
        for(size_t j = 0; j < cols; ++j) {
            result[i][j] = mat(i, j);
        }
    }

    return result;
}

/**
 * Dynamic Time Warping (DTW) distance.
 *
 * @description
 * This function calculates the Dynamic Time Warping (DTW) distance between
 * two time-series.
 *
 * @param x A `double *` Time-series data.
 * @param y A `double *` Self-Organizing Maps (SOM) codebook.
 * @param np `int` Number of points in arrays `p1` and `p2`.
 * @param nNA `int` Number of `NA` values in the arrays `p1` and `p2`.
 *
 * @reference
 * Giorgino, T. (2009). Computing and Visualizing Dynamic Time Warping
 * Alignments in R: The dtw Package. Journal of Statistical Software, 31(7),
 * 1–24. https://doi.org/10.18637/jss.v031.i07
 *
 * @return DTW distance.
 */
// [[Rcpp::export]]
double dtw_distance(
    const NumericMatrix& ts1,
    const NumericMatrix& ts2
)
{
    std::vector<std::vector<double>> ts1_vec = to_cpp_vector(ts1);
    std::vector<std::vector<double>> ts2_vec = to_cpp_vector(ts2);

    return (distance_dtw_op(ts1_vec, ts2_vec, 2));
}
