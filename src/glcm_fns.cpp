//[[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <cmath>
#include <math.h>
#define ARMA_DONT_USE_WRAPPER
#define ARMA_USE_OPENMP

using namespace Rcpp;
using namespace std;

typedef double _glcm_fun(const arma::sp_mat&, const arma::mat&, const arma::mat&);

// compute outside indices of a vector as a mirror
IntegerVector locus_neigh2(int size, int leg) {
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

arma::mat glcm_fn(const arma::vec& x,
                  const arma::vec& angles,
                  const arma::uword& nrows,
                  const arma::uword& ncols,
                  const arma::uword& window_size,
                  _glcm_fun _fun) {
    // get the value of grey values
    int n_grey = x.max();
    // initialize sparse matrix to store co-occurrence values
    arma::sp_mat glcm_co(n_grey, n_grey);
    // initialize result matrix
    arma::mat res(x.size(), angles.size(), arma::fill::zeros);
    // initialize neighborhood matrix
    arma::mat neigh(window_size, window_size);

    // auxiliary variables
    double ang_v = 0;
    arma::u8 offset_row, offset_col = 1;
    arma::u16 row, col = 0;
    arma::uword start_row, end_row, start_col, end_col = 0;
    int v_i, v_j, sum = 0;

    // initialize auxiliary matrices needed in some metrics
    arma::mat i_aux(n_grey, n_grey);
    arma::mat j_aux(n_grey, n_grey);
    // fill auxiliary matrices with a sequence of 1 to n_grey levels
    i_aux = arma::repmat(
        arma::linspace<arma::vec>(0, n_grey - 1, n_grey), 1, n_grey
    );
    j_aux = arma::trans(i_aux);

    // compute window leg
    arma::u8 leg = window_size / 2;
    // compute locus mirror
    IntegerVector loci = locus_neigh2(nrows, leg);
    IntegerVector locj = locus_neigh2(ncols, leg);

    // compute values for each pixel
    for (arma::uword i = 0; i < nrows; ++i) {
        for (arma::uword j = 0; j < ncols; ++j) {
            // for all angles
            for (arma::uword ang = 0; ang < angles.size(); ++ang) {
                ang_v = angles(ang);
                // compute the neighborhood
                for (arma::uword wi = 0; wi < window_size; ++wi) {
                    for (arma::uword wj = 0; wj < window_size; ++wj) {
                        neigh(wi, wj) =
                            x(loci(wi + i) * ncols + locj(wj + j));
                    }
                }

                offset_row = std::round(std::sin(ang_v));
                offset_col = std::round(std::cos(ang_v));
                // row
                start_row = std::max(0, -offset_row);
                end_row = std::min(neigh.n_rows, neigh.n_rows - offset_row);
                // col
                start_col = std::max(0, -offset_col);
                end_col = std::min(neigh.n_cols, neigh.n_cols - offset_col);
                for (arma::uword r = start_row; r < end_row; r++) {
                    for (arma::uword c = start_col; c < end_col; c++) {
                        v_i = neigh(r,c);
                        row = r + offset_row;
                        col = c + offset_col;
                        v_j = neigh(row, col);
                        if (v_i < n_grey && v_j < n_grey) {
                            glcm_co(v_i, v_j) += 1;
                            glcm_co(v_j, v_i) += 1;
                        }
                    }
                }
                // calculate co-occurrence probabilities
                sum = arma::accu(glcm_co);
                glcm_co /= sum;

                // calculate glcm metric
                res(i * ncols + j, ang) = _fun(glcm_co, i_aux, j_aux);
                // clear and reset co-occurrence matrix
                glcm_co.clear();
                glcm_co.set_size(n_grey, n_grey);
            }
        }
    }
    if (angles.size() > 1) {
        res = arma::mean(res, 1);
    }
    return res;
}

inline double _glcm_contrast(const arma::sp_mat& x,
                             const arma::mat& i,
                             const arma::mat& j) {
    double res = 0;

    res = arma::accu(x % pow(i - j, 2));
    return(res);
}

inline double _glcm_dissimilarity(const arma::sp_mat& x,
                                  const arma::mat& i,
                                  const arma::mat& j) {
    double res = 0;
    res = arma::accu(x % arma::abs(i - j));
    return(res);
}

inline double _glcm_homogeneity(const arma::sp_mat& x,
                                const arma::mat& i,
                                const arma::mat& j) {
    double res = 0;

    res = arma::accu(x % (1 / (1 + pow(i - j, 2))));
    return(res);
}

inline double _glcm_energy(const arma::sp_mat& glcm,
                           const arma::mat& i,
                           const arma::mat& j) {
    double res = 0;
    res = std::sqrt(arma::accu(glcm % glcm));
    return(res);
}

inline double _glcm_asm(const arma::sp_mat& glcm,
                        const arma::mat& i,
                        const arma::mat& j) {
    double res = 0;

    res = arma::accu(glcm % glcm);
    return(res);
}

inline double _glcm_mean(const arma::sp_mat& glcm,
                         const arma::mat& i,
                         const arma::mat& j) {
    double res = 0;

    res = arma::accu(glcm % i);
    return(res);
}

inline double _glcm_variance(const arma::sp_mat& glcm,
                             const arma::mat& i,
                             const arma::mat& j) {
    double res = 0;

    double mean = arma::accu(glcm % i);

    res = arma::accu(glcm % pow(i - mean, 2));
    return(res);
}


inline double _glcm_std(const arma::sp_mat& glcm,
                        const arma::mat& i,
                        const arma::mat& j) {

    double res = _glcm_variance(glcm, i, j);

    res = sqrt(res);
    return(res);
}

inline double _glcm_correlation(const arma::sp_mat& glcm,
                                const arma::mat& i,
                                const arma::mat& j) {
    double res = 0;
    double mean = arma::accu(glcm % i);
    double var = _glcm_variance(glcm, i, j);

    res = arma::accu(glcm % (( (i-mean) % (j-mean) ) / (var)));

    return(res);
}

// [[Rcpp::export]]
arma::mat C_glcm_contrast(const arma::vec& x,
                          const arma::uword& nrows,
                          const arma::uword& ncols,
                          const arma::uword& window_size,
                          const arma::vec& angles) {

    return glcm_fn(x, angles, nrows, ncols, window_size, _glcm_contrast);
}

// [[Rcpp::export]]
arma::mat C_glcm_dissimilarity(const arma::vec& x,
                               const arma::uword& nrows,
                               const arma::uword& ncols,
                               const arma::uword& window_size,
                               const arma::vec& angles) {

    return glcm_fn(x, angles, nrows, ncols, window_size, _glcm_dissimilarity);
}

// [[Rcpp::export]]
arma::mat C_glcm_homogeneity(const arma::vec& x,
                             const arma::uword& nrows,
                             const arma::uword& ncols,
                             const arma::uword& window_size,
                             const arma::vec& angles) {

    return glcm_fn(x, angles, nrows, ncols, window_size, _glcm_homogeneity);
}

// [[Rcpp::export]]
arma::mat C_glcm_energy(const arma::vec& x,
                        const arma::uword& nrows,
                        const arma::uword& ncols,
                        const arma::uword& window_size,
                        const arma::vec& angles) {

    return glcm_fn(x, angles, nrows, ncols, window_size, _glcm_energy);
}

// [[Rcpp::export]]
arma::mat C_glcm_asm(const arma::vec& x,
                     const arma::uword& nrows,
                     const arma::uword& ncols,
                     const arma::uword& window_size,
                     const arma::vec& angles) {

    return glcm_fn(x, angles, nrows, ncols, window_size, _glcm_asm);
}

// [[Rcpp::export]]
arma::mat C_glcm_mean(const arma::vec& x,
                      const arma::uword& nrows,
                      const arma::uword& ncols,
                      const arma::uword& window_size,
                      const arma::vec& angles) {

    return glcm_fn(x, angles, nrows, ncols, window_size, _glcm_mean);
}

// [[Rcpp::export]]
arma::mat C_glcm_variance(const arma::vec& x,
                          const arma::uword& nrows,
                          const arma::uword& ncols,
                          const arma::uword& window_size,
                          const arma::vec& angles) {

    return glcm_fn(x, angles, nrows, ncols, window_size, _glcm_variance);
}

// [[Rcpp::export]]
arma::mat C_glcm_std(const arma::vec& x,
                     const arma::uword& nrows,
                     const arma::uword& ncols,
                     const arma::uword& window_size,
                     const arma::vec& angles) {

    return glcm_fn(x, angles, nrows, ncols, window_size, _glcm_std);
}

// [[Rcpp::export]]
arma::mat C_glcm_correlation(const arma::vec& x,
                             const arma::uword& nrows,
                             const arma::uword& ncols,
                             const arma::uword& window_size,
                             const arma::vec& angles) {

    return glcm_fn(x, angles, nrows, ncols, window_size, _glcm_correlation);
}

// double glcm_entropy(const arma::sp_mat& glcm,
//                     const arma::mat& i,
//                     const arma::mat& j) {
//     double res = 0;
//
//     arma::mat glcm_entropy = glcm % ((-1) * arma::logmat(glcm));
//     glcm_entropy.replace(arma::datum::nan, 0);
//
//     res = accu(glcm_entropy);
//     return(res);
// }

