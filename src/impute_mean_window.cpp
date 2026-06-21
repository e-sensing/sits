#include <Rcpp.h>
using namespace Rcpp;

//
// Code adapted from ImputeTS R Package
// https://github.com/SteffenMoritz/imputeTS/commit/163f8e645f31f961bddae29e2e94e2044f1a125d
//

// Created enum of integers to avoid string comparisons during window calculations
enum WeightingType {
  WT_SIMPLE = 0,
  WT_LINEAR = 1,
  WT_EXPONENTIAL = 2
};

inline bool isNA(double x) {
    return NumericVector::is_na(x);
}

inline WeightingType parse_weighting(const String& w) {
  if (w == "simple")      return WT_SIMPLE;
  if (w == "linear")      return WT_LINEAR;
  if (w == "exponential") return WT_EXPONENTIAL;

  stop("Error: Invalid weighting strategy for moving window impute method");
}

// [[Rcpp::export]]
NumericVector C_interp_mean_window_vec(NumericVector data, int k, String weighting) {
  const int n = data.size();

  // Keep original values for lookup (do not overwrite while imputing)
  NumericVector tempdata = clone(data);

  WeightingType wt = parse_weighting(weighting);

  for (int i = 0; i < n; ++i) {
    if (!isNA(tempdata[i])) {
      continue;
    }

    // Maximum distance we ``could`` go before leaving the vector
    const int maxLeft      = i;
    const int maxRight     = n - 1 - i;
    const int maxPossibleD = (maxLeft > maxRight) ? maxLeft : maxRight;

    int    nonNaCount = 0;
    double valueSum   = 0.0; // sum of (weight * value)
    double weightSum  = 0.0; // sum of weights

    // For exponential weights, we can update weight per distance
    double expWeightForD = 0.5; // 1 / 2^1

    for (int d = 1; d <= maxPossibleD; ++d) {
      double w = 1.0;

      switch (wt) {
      case WT_SIMPLE:
        // SMA: all observations in the window are equally weighted for
        // calculating the mean. So, all neighbors have weight 1 (simple mean)
        w = 1.0;

        break;

      case WT_LINEAR:
        // LWMA: weights decrease in arithmetical progression. The observations
        // directly next to a central value i, have weight 1/2, the observations
        // one further away (i-2,i+2) have weight 1/3, the next (i-3,i+3) have weight 1/4, ...
        // So, this this case, we use 1 / (current distance + 1):
        w = 1.0 / (static_cast<double>(d) + 1.0);

        break;

      case WT_EXPONENTIAL:
        // EWMA: uses weighting factors which decrease exponentially.
        // The observations directly next to a central value i, have weight 1/2^1,
        // the observations one further away (i-2,i+2) have weight 1/2^2, ...
        // So, in this case, to avoid ``pow``, we use current distance directly in a
        // ``double`` value (original version as calculating using a vector)
        if (d == 1) {
          // the neighbors in the left / right positions of current ``d``
          // have ``Weight = 1/2 ^ 1 = 0.5``
          expWeightForD = 0.5;
        } else {
          // Calculate exponential just accumulating multiplications
          expWeightForD *= 0.5;
        }

        w = expWeightForD;

        break;
      }

      // Left neighbor of current ``d``. Do not forget this is the
      // window, starting in time ``i``.
      const int jLeft = i - d;
      if (jLeft >= 0) {

        // Left neighbor value
        const double v = tempdata[jLeft];

        // If it is not NA, add it to the sum
        if (!isNA(v)) {
          ++nonNaCount;

          valueSum  += w * v;
          weightSum += w;
        }
      }

      // Right neighbor of current ``d``. Do not forget this is the
      // window, starting in time ``i``.
      const int jRight = i + d;
      if (jRight < n) {

        // Right neighbor value
        const double v = tempdata[jRight];

        // If it is not NA, add it to the sum
        if (!isNA(v)) {
          ++nonNaCount;

          valueSum  += w * v;
          weightSum += w;
        }
      }

      // We must include at least distance k.
      // After we've gone out to ``k`` and have at least ``2`` non-NA neighbors,
      // we can stop expanding the window.
      if (d >= k && nonNaCount >= 2) {
        break;
      }
    }

    // Fallback: if we somehow have no neighbors, keep NA.
    if (nonNaCount == 0 || weightSum == 0.0) {
      continue;
    }

    // As we are using accumulators and generate windows and weights on-the-fly
    // we can just calculate `mean` with the weights generated using distance * n
    // For ``simple`` weighting: all weights are ``1``
    data[i] = valueSum / weightSum;
  }

  return data;
}

// [[Rcpp::export]]
NumericMatrix C_interp_mean_window_mat(NumericMatrix data, int k, String weighting) {
  const int nrows = data.nrow();

  for (int i = 0; i < nrows; ++i) {
    // Create a new vector for the row. Easy and safe way to avoid modifying the
    // original matrix.
    NumericVector row = data(i, _);

    // Use moving window!
    data(i, _) = C_interp_mean_window_vec(row, k, weighting);
  }

  // Return!
  return data;
}
