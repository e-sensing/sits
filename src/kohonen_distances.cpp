#include <Rcpp.h>

#include "./sits_types.h"

using namespace Rcpp;

/**
 * Compute the p-norm between two time-series.
 *
 * @description
 * The `p-norm`, also known as the `Minkowski space`, is a generalized norm
 * calculation that includes several types of distances based on the value
 * of `p`.
 *
 * Common values of `p` include:
 *
 *  - `p = 1` for the Manhattan (city block) distance;
 *  - `p = 2` for the Euclidean norm (distance).
 *
 * More details about p-norms can be found on Wikipedia:
 * https://en.wikipedia.org/wiki/Norm_(mathematics)#p-norm
 *
 * @param a A `std::vector<double>` with time-series values.
 * @param b A `std::vector<double>` with time-series values.
 * @param p A `double` value of the norm to use, determining the type of
 *          distance calculated.
 *
 * @note
 * Both vectors `a` and `b` must have the same length.
 *
 * @note
 * The implementation of this DTW distance calculation was adapted from the
 * `DTW_cpp` single header library (https://github.com/cjekel/DTW_cpp).
 *
 * @return The `p-norm` value between vectors `a` and `b`.
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
 * Dynamic Time Warping (DTW) distance.
 *
 * @description
 * This function calculates the Dynamic Time Warping (DTW) distance between
 * two time-series.
 *
 * @param x A `std::vector<std::vector<double>>` with time-series values.
 * @param y A `std::vector<std::vector<double>>` with time-series values.
 *
 * @reference
 * Giorgino, T. (2009). Computing and Visualizing Dynamic Time Warping
 * Alignments in R: The dtw Package. Journal of Statistical Software, 31(7),
 * 1–24. https://doi.org/10.18637/jss.v031.i07
 *
 * @note
 * The implementation of this DTW distance calculation was adapted from the
 * `DTW_cpp` single header library (https://github.com/cjekel/DTW_cpp).
 *
 * @return DTW distance.
 */
double distance_dtw_op(std::vector<std::vector<double>> a,
                       std::vector<std::vector<double>> b,
                       double p)
{
    int n = a.size();
    int o = b.size();

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
 * @note
 * The implementation of this DTW distance calculation was adapted from the
 * `DTW_cpp` single header library (https://github.com/cjekel/DTW_cpp).
 *
 * @return DTW distance.
 */
double kohonen_dtw_op(double *p1, double *p2, int np, int nNA)
{
    std::vector<double> p1_data(p1, p1 + np);
    std::vector<double> p2_data(p2, p2 + np);

    std::vector<std::vector<double>> p1_vec = {p1_data};
    std::vector<std::vector<double>> p2_vec = {p2_data};

    // p-norm fixed in 2 (equivalent to euclidean distance)
    return (distance_dtw_op(p1_vec, p2_vec, 2));
}

/**
 * Euclidean distance.
 *
 * @description
 * This function calculates the Euclidean distance between two time-series.
 *
 * @param x A `double *` Time-series data.
 * @param y A `double *` Self-Organizing Maps (SOM) codebook.
 * @param np `int` Number of points in arrays `p1` and `p2`.
 * @param nNA `int` Number of `NA` values in the arrays `p1` and `p2`.
 *
 * @note
 * The implementation of this function was adapted from the `kohonen` R Package.
 * The code is open-source, under the GPL license, and is available on
 * GitHub (https://github.com/rwehrens/kohonen)
 *
 * @return Euclidean distance.
 */
double kohonen_euclidean_op(double *data, double *codes, int n, int nNA) {
    double tmp, d = 0.0;
    for (int i = 0; i < n; ++i) {
        tmp = data[i] - codes[i];
        d += tmp * tmp;
    }
    d = sqrt(d);
    return d;
}


/**
 * Shared pointer factory of the Dynamic Time Warping (DTW) distance function.
 *
 * @description
 * This factory function creates a shared pointer of the Dynamic Time
 * Warping (DTW) distance function.
 *
 * @return Shared pointer of the DTW function.
 */
// [[Rcpp::export]]
XPtr<DistanceFunctionPtr> kohonen_dtw()
{
    return (XPtr<DistanceFunctionPtr>(new DistanceFunctionPtr(
            &kohonen_dtw_op)));
}

/**
 * Shared pointer factory of the Euclidean distance function.
 *
 * @description
 * This factory function creates a shared pointer of the Euclidean distance
 * function.
 *
 * @return Shared pointer of the DTW function.
 */
// [[Rcpp::export]]
XPtr<DistanceFunctionPtr> kohonen_euclidean() {
    return (XPtr<DistanceFunctionPtr>(new DistanceFunctionPtr(
            &kohonen_euclidean_op)));
}

/**
 * Object distances.
 *
 * @description
 * This function computes the distances between all objects of the data matrix.
 *
 * @param data              A `NumericMatrix` with the objects used to
 *                          calculate the distances.
 * @param numVars           A `IntegerVector` with the number of variables
 *                          represented in the `data` variable.
 * @param numNAs            A `IntegerMatrix` Number of points NAs in the
 *                          matrix cells.
 * @param distanceFunction  A `XPtr<DistanceFunctionPtr>` poiting to
 *                          a distance function.
 * @param weights           A `NumericVector` with the map `weights`.
 *
 * @note
 * The implementation of this function was adapted from the `kohonen` R Package.
 * The code is open-source, under the GPL license, and is available on
 * GitHub (https://github.com/rwehrens/kohonen)
 *
 * @return The lower triangle of the distance matrix as a vector.
 */
// [[Rcpp::export]]
NumericVector kohonen_object_distances(
    NumericMatrix data,
    IntegerVector numVars,
    IntegerMatrix numNAs,
    XPtr<DistanceFunctionPtr> distanceFunction,
    NumericVector weights)
{
    int numObjects = data.ncol();
    int totalVars = data.nrow();
    int numLayers = numVars.size();

    NumericVector offsets(numLayers);
    NumericVector distances((numObjects * (numObjects - 1)) / 2);

    totalVars = 0;
    for (int l = 0; l < numLayers; l++) {
        offsets[l] = totalVars;
        totalVars += numVars[l];
    }

    double *pWeights = REAL(weights);
    double *pDistances = REAL(distances);
    int *pNumVars = INTEGER(numVars);
    int *pNumNAs = INTEGER(numNAs);

    /* Get the distance function pointers. */
    DistanceFunctionPtr distanceFunctionPtr = *(distanceFunction);

    int ix = 0;
      for (int i = 0; i < numObjects - 1; ++i) {
        for (int j = i + 1; j < numObjects; ++j) {
          pDistances[ix] = 0.0;
          for (int l = 0; l < numLayers; ++l) {
            pDistances[ix] += pWeights[l] * (*distanceFunctionPtr)(
              &data[i * totalVars + offsets[l]],
              &data[j * totalVars + offsets[l]],
              pNumVars[l],
              pNumNAs[i * numLayers + l]);
          }
          ix++;
        }
      }

    return distances;
}
