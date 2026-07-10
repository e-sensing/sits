#include <Rcpp.h>

#include "./sits_types.h"
#include "./dtw2vec.h"

using namespace Rcpp;

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
 * This function is a thin facade over the vendored `dtw2vec` algorithm (see
 * `dtw2vec.h`), a port of the `IncDTW` R package. It computes the
 * `symmetric2` DTW distance with an unconstrained warping window using O(np)
 *  memory (no cost matrix is allocated).
 *
 * @return DTW distance.
 */
double kohonen_dtw_op(double *p1, double *p2, int np, int nNA)
{
    // Facade over the vendored `dtw2vec` algorithm. Both series have the same
    // length (`np`), `nNA` is unused because `sits` forbids NA in SOM input
    return incdtw::dtw2vec(p1, np, p2, np);
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
 * Cosine distance.
 *
 * @description
 * This function calculates the cosine distance between two vectors, defined as
 * `1 - cosine_similarity`. It is intended for embedding vectors (e.g. AlphaEarth
 * or encoder outputs), where the meaningful comparison is the angle between the
 * full feature vectors rather than their magnitudes.
 *
 * @param data A `double *` Time-series/embedding data.
 * @param codes A `double *` Self-Organizing Maps (SOM) codebook.
 * @param n `int` Number of points in arrays `data` and `codes`.
 * @param nNA `int` Number of `NA` values in the arrays (unused; `sits` forbids
 *       NA in SOM input).
 *
 * @note
 * The Self-Organizing Map selects the Best Matching Unit by minimizing the
 * distance, so this function return a distance (`1 - similarity`), not the
 * similarity itself. When either vector has (near) zero norm the cosine is
 * undefined. In that case, the maximum distance (`1.0`) is returned.
 *
 * @return Cosine distance in the range [0, 2].
 */
double kohonen_cosine_op(double *data, double *codes, int n, int nNA) {
    double dot = 0.0;
    double norm_data = 0.0;
    double norm_codes = 0.0;

    for (int i = 0; i < n; ++i) {
        dot += data[i] * codes[i];
        
        norm_data += data[i] * data[i];
        norm_codes += codes[i] * codes[i];
    }

    double denom = sqrt(norm_data) * sqrt(norm_codes);
    
    if (denom < 1e-12) {
        return 1.0;
    }

    // 1 - cosine similarity
    return 1.0 - dot / denom;
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
 * Shared pointer factory of the cosine distance function.
 *
 * @description
 * This factory function creates a shared pointer of the cosine distance
 * function.
 *
 * @return Shared pointer of the cosine function.
 */
// [[Rcpp::export]]
XPtr<DistanceFunctionPtr> kohonen_cosine() {
    return (XPtr<DistanceFunctionPtr>(new DistanceFunctionPtr(
            &kohonen_cosine_op)));
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
