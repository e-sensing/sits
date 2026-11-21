//[[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <vector>

using namespace Rcpp;
using namespace std;

// A metric reads the n pairs (a[k], b[k]) of one window and one angle.
// The GLCM is symmetric: each pair counts as (a, b) and as (b, a), so
// P(i, j) = count(i, j) / (2 * n) and a sum over P is a mean over the pairs.
typedef double _glcm_fun(const int* a, const int* b, int n,
                         long long* codes, long long n_grey);

// mirror outside indices without the edge pixel, as torch "reflect" pads;
// it needs size > leg, checked in R by sits_texture()
std::vector<int> locus_neigh2(int size, int leg) {
    std::vector<int> res(size + 2 * leg);
    for (int i = 0; i < (int) res.size(); ++i) {
        if (i < leg)
            res[i] = leg - i;
        else if (i < size + leg)
            res[i] = i - leg;
        else
            res[i] = 2 * size + leg - i - 2;
    }
    return res;
}

// This code is inspired by some existing libraries: glcm and
// GLCMTextures R packages. Both are licensed by GPL (>= 3); and the
// Python library scikit-image is licensed by BSD-3-Clause.
template <_glcm_fun _fun>
arma::mat glcm_fn(const arma::vec& x,
                  const arma::vec& angles,
                  const arma::uword nrows,
                  const arma::uword ncols,
                  const arma::uword n_grey,
                  const arma::u8 window_size) {
    const int nr = nrows;
    const int nc = ncols;
    const int ws = window_size;
    const int leg = ws / 2;
    const int n_angles = angles.size();
    const long long ng = n_grey;
    const arma::uword n_pixels = x.size();
    // grey level of each pixel; -1 drops its pairs, as the old filter
    // v < n_grey on an unsigned value did
    std::vector<int> grey(n_pixels);
    for (arma::uword k = 0; k < n_pixels; ++k) {
        const double v = x(k);
        grey[k] = (v > -1.0 && v < (double) ng) ? (int) v : -1;
    }
    // compute locus mirror
    const std::vector<int> loci = locus_neigh2(nr, leg);
    const std::vector<int> locj = locus_neigh2(nc, leg);
    // signed, as the angle 3*pi/4 has a column offset of -1
    std::vector<int> off_row(n_angles), off_col(n_angles);
    for (int ang = 0; ang < n_angles; ++ang) {
        off_row[ang] = std::round(std::sin(angles(ang)));
        off_col[ang] = std::round(std::cos(angles(ang)));
    }
    // one window buffer and one pair buffer for every pixel
    std::vector<int> win(ws * ws);
    std::vector<int> pair_a(ws * ws), pair_b(ws * ws);
    std::vector<long long> codes(ws * ws);
    arma::mat res(n_pixels, 1);
    double* out = res.memptr();
    for (int i = 0; i < nr; ++i) {
        for (int j = 0; j < nc; ++j) {
            for (int wi = 0; wi < ws; ++wi) {
                const int row = loci[wi + i] * nc;
                for (int wj = 0; wj < ws; ++wj)
                    win[wi * ws + wj] = grey[row + locj[wj + j]];
            }
            double total = 0.0;
            for (int ang = 0; ang < n_angles; ++ang) {
                const int dr = off_row[ang];
                const int dc = off_col[ang];
                const int start_row = std::max(0, -dr);
                const int end_row = std::min(ws, ws - dr);
                const int start_col = std::max(0, -dc);
                const int end_col = std::min(ws, ws - dc);
                int n = 0;
                for (int r = start_row; r < end_row; ++r) {
                    for (int c = start_col; c < end_col; ++c) {
                        const int va = win[r * ws + c];
                        const int vb = win[(r + dr) * ws + c + dc];
                        if (va >= 0 && vb >= 0) {
                            pair_a[n] = va;
                            pair_b[n] = vb;
                            ++n;
                        }
                    }
                }
                total += _fun(pair_a.data(), pair_b.data(), n,
                              codes.data(), ng);
            }
            out[i * nc + j] = n_angles > 1 ? total / n_angles : total;
        }
    }
    return res;
}

inline double _glcm_contrast(const int* a, const int* b, int n,
                             long long* codes, long long n_grey) {
    if (n == 0) return 0.0;
    double res = 0;
    for (int k = 0; k < n; ++k) {
        const double d = a[k] - b[k];
        res += d * d;
    }
    return res / n;
}

inline double _glcm_dissimilarity(const int* a, const int* b, int n,
                                  long long* codes, long long n_grey) {
    if (n == 0) return 0.0;
    double res = 0;
    for (int k = 0; k < n; ++k)
        res += std::abs(a[k] - b[k]);
    return res / n;
}

inline double _glcm_homogeneity(const int* a, const int* b, int n,
                                long long* codes, long long n_grey) {
    if (n == 0) return 0.0;
    double res = 0;
    for (int k = 0; k < n; ++k) {
        const double d = a[k] - b[k];
        res += 1.0 / (1.0 + d * d);
    }
    return res / n;
}

inline double _glcm_asm(const int* a, const int* b, int n,
                        long long* codes, long long n_grey) {
    if (n == 0) return 0.0;
    // (a, b) and (b, a) share one code, so the sort is over n codes only
    for (int k = 0; k < n; ++k)
        codes[k] = (long long) std::min(a[k], b[k]) * n_grey +
            std::max(a[k], b[k]);
    std::sort(codes, codes + n);
    double res = 0;
    int k = 0;
    while (k < n) {
        int run = 1;
        while (k + run < n && codes[k + run] == codes[k]) ++run;
        const double r = run;
        // a diagonal cell holds both orders, so its count is 2 * run;
        // two cells off the diagonal hold run each
        res += (codes[k] / n_grey == codes[k] % n_grey) ?
            4.0 * r * r : 2.0 * r * r;
        k += run;
    }
    const double total = 2.0 * n;
    return res / (total * total);
}

inline double _glcm_energy(const int* a, const int* b, int n,
                           long long* codes, long long n_grey) {
    return std::sqrt(_glcm_asm(a, b, n, codes, n_grey));
}

inline double _glcm_mean(const int* a, const int* b, int n,
                         long long* codes, long long n_grey) {
    if (n == 0) return 0.0;
    double res = 0;
    for (int k = 0; k < n; ++k)
        res += a[k] + b[k];
    return res / (2.0 * n);
}

inline double _glcm_variance(const int* a, const int* b, int n,
                             long long* codes, long long n_grey) {
    if (n == 0) return 0.0;
    const double mean = _glcm_mean(a, b, n, codes, n_grey);
    double res = 0;
    for (int k = 0; k < n; ++k) {
        const double da = a[k] - mean;
        const double db = b[k] - mean;
        res += da * da + db * db;
    }
    return res / (2.0 * n);
}

inline double _glcm_std(const int* a, const int* b, int n,
                        long long* codes, long long n_grey) {
    return std::sqrt(_glcm_variance(a, b, n, codes, n_grey));
}

inline double _glcm_correlation(const int* a, const int* b, int n,
                                long long* codes, long long n_grey) {
    const double var = _glcm_variance(a, b, n, codes, n_grey);
    // handle the special case of standard deviations near zero
    // reference: skimage
    if (var < 1e-15) return 1.0;
    const double mean = _glcm_mean(a, b, n, codes, n_grey);
    double res = 0;
    for (int k = 0; k < n; ++k)
        res += (a[k] - mean) * (b[k] - mean);
    return res / n / var;
}

// [[Rcpp::export]]
arma::mat C_glcm_contrast(const arma::vec& x,
                          const arma::vec& angles,
                          const arma::uword nrows,
                          const arma::uword ncols,
                          const arma::uword n_grey,
                          const arma::u8 window_size) {
    return glcm_fn<_glcm_contrast>(x, angles, nrows, ncols, n_grey,
                                   window_size);
}

// [[Rcpp::export]]
arma::mat C_glcm_dissimilarity(const arma::vec& x,
                               const arma::vec& angles,
                               const arma::uword nrows,
                               const arma::uword ncols,
                               const arma::uword n_grey,
                               const arma::u8 window_size) {
    return glcm_fn<_glcm_dissimilarity>(x, angles, nrows, ncols, n_grey,
                                        window_size);
}

// [[Rcpp::export]]
arma::mat C_glcm_homogeneity(const arma::vec& x,
                             const arma::vec& angles,
                             const arma::uword nrows,
                             const arma::uword ncols,
                             const arma::uword n_grey,
                             const arma::u8 window_size) {
    return glcm_fn<_glcm_homogeneity>(x, angles, nrows, ncols, n_grey,
                                      window_size);
}

// [[Rcpp::export]]
arma::mat C_glcm_energy(const arma::vec& x,
                        const arma::vec& angles,
                        const arma::uword nrows,
                        const arma::uword ncols,
                        const arma::uword n_grey,
                        const arma::u8 window_size) {
    return glcm_fn<_glcm_energy>(x, angles, nrows, ncols, n_grey,
                                 window_size);
}

// [[Rcpp::export]]
arma::mat C_glcm_asm(const arma::vec& x,
                     const arma::vec& angles,
                     const arma::uword nrows,
                     const arma::uword ncols,
                     const arma::uword n_grey,
                     const arma::u8 window_size) {
    return glcm_fn<_glcm_asm>(x, angles, nrows, ncols, n_grey, window_size);
}

// [[Rcpp::export]]
arma::mat C_glcm_mean(const arma::vec& x,
                      const arma::vec& angles,
                      const arma::uword nrows,
                      const arma::uword ncols,
                      const arma::uword n_grey,
                      const arma::u8 window_size) {
    return glcm_fn<_glcm_mean>(x, angles, nrows, ncols, n_grey, window_size);
}

// [[Rcpp::export]]
arma::mat C_glcm_variance(const arma::vec& x,
                          const arma::vec& angles,
                          const arma::uword nrows,
                          const arma::uword ncols,
                          const arma::uword n_grey,
                          const arma::u8 window_size) {
    return glcm_fn<_glcm_variance>(x, angles, nrows, ncols, n_grey,
                                   window_size);
}

// [[Rcpp::export]]
arma::mat C_glcm_std(const arma::vec& x,
                     const arma::vec& angles,
                     const arma::uword nrows,
                     const arma::uword ncols,
                     const arma::uword n_grey,
                     const arma::u8 window_size) {
    return glcm_fn<_glcm_std>(x, angles, nrows, ncols, n_grey, window_size);
}

// [[Rcpp::export]]
arma::mat C_glcm_correlation(const arma::vec& x,
                             const arma::vec& angles,
                             const arma::uword nrows,
                             const arma::uword ncols,
                             const arma::uword n_grey,
                             const arma::u8 window_size) {
    return glcm_fn<_glcm_correlation>(x, angles, nrows, ncols, n_grey,
                                      window_size);
}
