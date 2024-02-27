#include <Rcpp.h>

#include <cstdlib>
#include <vector>
#include <cmath>
#include <algorithm>
#include <stdexcept>

#include "./sits_types.h"

using namespace Rcpp;

/**
 * Compute the p-norm distance between two 1D C++ vectors.
 *
 * @description
 * The p-norm, also known as the Minkowski norm, is a generalized norm
 * calculation that includes several types of distances based on the value of p.
 *
 * Common values of p include:
 *
 *  - p = 1 for the Manhattan (city block) distance;
 *  - p = 2 for the Euclidean norm (distance).
 *
 * More details about p-norms can be found on Wikipedia:
 * https://en.wikipedia.org/wiki/Norm_(mathematics)#p-norm
 *
 * @param a A 1D vector representing the first point in an m-dimensional space.
 * @param b A 1D vector representing the second point in an m-dimensional space.
 * @param p The value of the norm to use, determining the type of distance
 *          calculated.
 *
 * @note Both vectors 'a' and 'b' must have the same number of dimensions.
 * @note This function was adapted from the DTW implementation found at:
 *       https://github.com/cjekel/DTW_cpp
 *
 * @return The p-norm distance between vectors 'a' and 'b'.
 */
double p_norm(std::vector<double> a, std::vector<double> b, double p)
{
    double d = 0;

    size_t index;
    size_t a_size = a.size();

    for (index = 0; index < a_size; index++)
    {
        d += std::pow(std::abs(a[index] - b[index]), p);
    }
    return std::pow(d, 1.0 / p);
}

/**
 * Compute the Dynamic Time Warping (DTW) distance between two 2D C++ vectors.
 *
 * @description
 * This function calculates the Dynamic Time Warping (DTW) distance between
 * two sequences that can have a different number of data points but must
 * share the same number of dimensions. An exception is thrown if the dimensions
 * of the input vectors do not match.
 *
 * For more information on DTW, visit:
 * https://en.wikipedia.org/wiki/Dynamic_time_warping
 *
 * @param a A 2D vector representing the first sequence
 * @param b A 2D vector representing the second sequence.
 * @param p The value of p-norm to use for distance calculation.
 *
 * @throws std::invalid_argument If the dimensions of 'a' and 'b' do not match.
 *
 * @note
 * Both vectors 'a', and 'b' should be structured as follows:
 *
 *  [number_of_data_points][number_of_dimensions]
 *
 * allowing the DTW distance computation to adapt to any p-norm value specified.
 *
 * @note The implementation of this DTW distance calculation was adapted from:
 *       https://github.com/cjekel/DTW_cpp
 *
 * @return The DTW distance between the two input sequences.
 */
double distance_dtw_op(std::vector<std::vector<double>> a,
                       std::vector<std::vector<double>> b,
                       double p)
{
    int n = a.size();
    int o = b.size();

    int a_m = a[0].size();
    int b_m = b[0].size();

    if (a_m != b_m)
    {
        throw std::invalid_argument(
            "a and b must have the same number of dimensions!"
        );
    }
    std::vector<std::vector<double>> d(n, std::vector<double>(o, 0.0));

    d[0][0] = p_norm(a[0], b[0], p);

    for (int i = 1; i < n; i++)
    {
        d[i][0] = d[i - 1][0] + p_norm(a[i], b[0], p);
    }
    for (int i = 1; i < o; i++)
    {
        d[0][i] = d[0][i - 1] + p_norm(a[0], b[i], p);
    }
    for (int i = 1; i < n; i++)
    {
        for (int j = 1; j < o; j++)
        {
            d[i][j] = p_norm(a[i], b[j], p) + std::fmin(
                std::fmin(d[i - 1][j], d[i][j - 1]), d[i - 1][j - 1]
            );
        }
    }
    return d[n - 1][o - 1];
}

/**
 * Dynamic Time Warping (DTW) distance wrapper.
 *
 * @description
 * This function calculates prepare data from `Kohonen` package and calculate
 * the DTW distance between two array of points.
 *
 * @param a A 2D vector representing the first sequence.
 * @param b A 2D vector representing the second sequence.
 * @param np Number of points in vectors `a` and `b`.
 * @param nNA Number of NA values in the vectors `a` and `b`.
 *
 * @note The function signature was created following the `Kohonen` R package
 *        specifications for custom distance functions.
 *
 *
 * @return The DTW distance between the two input sequences.
 */
double kohonen_dtw(double *p1, double *p2, int np, int nNA)
{
    std::vector<double> p1_data(p1, p1 + np);
    std::vector<double> p2_data(p2, p2 + np);

    std::vector<std::vector<double>> p1_vec = {p1_data};
    std::vector<std::vector<double>> p2_vec = {p2_data};

    // p-norm fixed in 2 (equivalent to euclidean distance)
    return (distance_dtw_op(p1_vec, p2_vec, 2));
}

// [[Rcpp::export]]
Rcpp::XPtr<DistanceFunctionPtr> dtw()
{
    // Returns a External Pointer, which is used by the `kohonen` package
    // https://cran.r-project.org/doc/manuals/R-exts.html#External-pointers-and-weak-references
    return (Rcpp::XPtr<DistanceFunctionPtr>(new DistanceFunctionPtr(
            &kohonen_dtw)));
}
