#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// compute outside indices of a vector as a mirror
IntegerVector locus_neigh(int size, int leg) {
    IntegerVector res(size + 2 * leg);
    for (int i = 0; i < res.length(); ++i) {
        if (i < leg)
            res(i) = leg - i - 1;
        else if (i < size + leg)
            res(i) = i - leg;
        else
            res(i) = 2 * size + leg - i - 1;
    }
    return res;
}

// [[Rcpp::export]]
NumericVector bayes_smoother_fraction(const NumericMatrix& logits,
                                      const int& nrows,
                                      const int& ncols,
                                      const int& window_size,
                                      const NumericVector& smoothness,
                                      const double& neigh_fraction
) {
    // initialize result vectors
    NumericMatrix res(logits.nrow(), logits.ncol());
    NumericVector neigh(window_size * window_size);
    // compute window leg
    int leg = window_size / 2;
    // compute locus mirror
    IntegerVector loci = locus_neigh(nrows, leg);
    IntegerVector locj = locus_neigh(ncols, leg);
    // compute values for each pixel
    for (int i = 0; i < nrows; ++i) {
        for (int j = 0; j < ncols; ++j) {
            // for all bands
            for (int band = 0; band < logits.ncol(); ++band) {
                // compute the neighborhood
                for (int wi = 0; wi < window_size; ++wi)
                    for (int wj = 0; wj < window_size; ++wj) {
                        neigh(wi * window_size + wj) =
                            logits(loci(wi + i) * ncols + locj(wj + j), band);
                    }
                // remove NA
                NumericVector neigh2 = na_omit(neigh);
                if (neigh_fraction < 1.0)
                    neigh2.sort(true);
                // compute number of neighbors to be used
                int neigh_high = std::ceil(neigh_fraction * neigh2.length());
                // create a vector to store the highest values
                NumericVector high_values(neigh_high);
                // copy the highest values to the new vector
                int nh = 0;
                for(NumericVector::iterator it = neigh2.begin();
                    it != neigh2.begin() + neigh_high; ++it) {
                    high_values(nh++) = (*it);
                }
                // get the estimates for prior
                // normal with mean m0 and variance s0
                double s0 = var(noNA(high_values));
                double m0 = mean(noNA(high_values));
                // get the current value
                double x0 = logits(i * ncols + j, band);
                if (std::isnan(x0) || s0 < 1e-04) {
                    res(i * ncols + j, band) = m0;
                } else {
                    // weight for Bayesian estimator
                    double w = s0/(s0 + smoothness(band));
                    // apply Bayesian smoother
                    res(i * ncols + j, band) = w * x0 + (1 - w) * m0;
                }
            }
        }
    }
    return res;
}
