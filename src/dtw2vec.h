#pragma once

namespace incdtw {

/**
 * Vector-based Dynamic Time Warping (DTW) distance.
 *
 * @description
 * Computes the (raw) DTW distance between two univariate time-series using the
 * `symmetric2` step pattern and an unconstrained warping window. Only the
 * scalar distance is returned: no warping path or cost matrix is built. Memory
 * is O(min-bounded) - two rolling column buffers of length `nx` - instead of
 * the O(nx * ny) full matrix.
 *
 * @param x  Pointer to the first time-series values.
 * @param nx Number of points in `x`.
 * @param y  Pointer to the second time-series values.
 * @param ny Number of points in `y`.
 *
 * @note
 * This is a port of the `dtw2vec` algorithm from the `IncDTW` R
 * package (function `cpp_dtw2vec`, step pattern `symmetric2`).
 *
 * @return Raw `symmetric2` DTW distance between `x` and `y`.
 */
double dtw2vec(const double *x, int nx, const double *y, int ny);

}
