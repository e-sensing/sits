#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <functional>
#include <vector>
using namespace Rcpp;

// compute outside indices of a vector as a mirror
std::vector<int> locus_neigh(int size, int leg) {
    std::vector<int> res(size + 2 * leg);
    for (int i = 0; i < (int) res.size(); ++i) {
        if (i < leg)
            res[i] = leg - i - 1;
        else if (i < size + leg)
            res[i] = i - leg;
        else
            res[i] = 2 * size + leg - i - 1;
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
    const int npixels = logits.nrow();
    const int nbands = logits.ncol();
    NumericMatrix res(npixels, nbands);
    // compute window leg
    const int leg = window_size / 2;
    // compute locus mirror
    const std::vector<int> loci = locus_neigh(nrows, leg);
    const std::vector<int> locj = locus_neigh(ncols, leg);
    // one buffer for the neighborhood of every pixel, no R allocation
    std::vector<double> neigh(window_size * window_size);
    // a band is one column of logits, so its pixels are contiguous
    for (int band = 0; band < nbands; ++band) {
        const double* values = &logits(0, band);
        double* out = &res(0, band);
        const double smooth = smoothness(band);
        for (int i = 0; i < nrows; ++i) {
            for (int j = 0; j < ncols; ++j) {
                // the neighborhood without NA
                int n = 0;
                for (int wi = 0; wi < window_size; ++wi) {
                    const int row = loci[wi + i] * ncols;
                    for (int wj = 0; wj < window_size; ++wj) {
                        const double v = values[row + locj[wj + j]];
                        if (!std::isnan(v)) neigh[n++] = v;
                    }
                }
                // compute number of neighbors to be used
                const int neigh_high = std::ceil(neigh_fraction * n);
                // mean and variance do not depend on the order, so only
                // the highest values are put first
                if (neigh_fraction < 1.0 && neigh_high > 0 && neigh_high < n)
                    std::nth_element(neigh.begin(),
                                     neigh.begin() + neigh_high - 1,
                                     neigh.begin() + n,
                                     std::greater<double>());
                // get the estimates for prior, as R mean() and var()
                // normal with mean m0 and variance s0
                long double sum = 0.0;
                for (int k = 0; k < neigh_high; ++k) sum += neigh[k];
                long double m = sum / neigh_high;
                long double err = 0.0;
                for (int k = 0; k < neigh_high; ++k) err += neigh[k] - m;
                const double m0 = m + err / neigh_high;
                long double ssq = 0.0;
                for (int k = 0; k < neigh_high; ++k)
                    ssq += (neigh[k] - m0) * (neigh[k] - m0);
                const double s0 = ssq / (neigh_high - 1);
                // get the current value
                const double x0 = values[i * ncols + j];
                if (std::isnan(x0) || s0 < 1e-04) {
                    out[i * ncols + j] = m0;
                } else {
                    // weight for Bayesian estimator
                    const double w = s0 / (s0 + smooth);
                    // apply Bayesian smoother
                    out[i * ncols + j] = w * x0 + (1 - w) * m0;
                }
            }
        }
    }
    return res;
}
