//[[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <cmath>
#include <math.h>

using namespace Rcpp;
using namespace std;

//[[Rcpp::export]]
arma::mat glcm_tabulate(const arma::mat& x,
                        const arma::uword window_size,
                        const float& angle,
                        const int n_bits = 16) {


    arma::mat out(n_bits, n_bits, arma::fill::zeros);

    int nrows = x.n_rows;
    int ncols = x.n_cols;

    arma::uword start_row, end_row, start_col, end_col = 0;
    int offset_row, offset_col, v_i, v_j, row, col = 0;

    offset_row = std::round(std::sin(angle) * window_size);
    offset_col = std::round(std::cos(angle) * window_size);
    // row
    start_row = std::max(0, -offset_row);
    end_row = std::min(nrows, nrows - offset_row);
    // col
    start_col = std::max(0, -offset_col);
    end_col = std::min(ncols, ncols - offset_col);

    for (arma::uword r = start_row; r < end_row; r++) {
        for (arma::uword c = start_col; c < end_col; c++) {
            v_i = x(r,c);
            row = r + offset_row;
            col = c + offset_col;
            v_j = x(row, col);
            if (v_i < n_bits && v_j < n_bits) {
                out(v_i, v_j) += 1;
            }
        }
    }
    return out;
}

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

// [[Rcpp::export]]
NumericMatrix glcm_calc_new(const arma::vec& x,
                            const arma::uword& nrows,
                            const arma::uword& ncols,
                            const arma::uword& window_size,
                            const arma::vec& angles,
                            const arma::uword& n_bits = 16) {
    // initialize result values
    NumericMatrix res(nrows, ncols);
    arma::mat neigh(window_size, window_size);
    arma::mat glcm(n_bits, n_bits, arma::fill::zeros);
    double sum = 0;

    // compute window leg
    int leg = window_size / 2;
    // compute locus mirror
    IntegerVector loci = locus_neigh2(nrows, leg);
    IntegerVector locj = locus_neigh2(ncols, leg);
    // compute values for each pixel
    for (int i = 0; i < nrows; ++i) {
        for (int j = 0; j < ncols; ++j) {
            // for all bands
            for (int angle = 0; angle < angles.size(); ++angle) {
                // compute the neighborhood
                for (int wi = 0; wi < window_size; ++wi) {
                    for (int wj = 0; wj < window_size; ++wj) {
                        neigh(wi, wj) =
                            x(loci(wi + i) * ncols + locj(wj + j));
                    }
                }
                Rcpp::Rcout << neigh << "\n";

                glcm = glcm_tabulate(neigh, 1, 0, n_bits);
                sum = arma::accu(glcm);
                glcm /= sum;
                Rcpp::Rcout << glcm << "\n";
                // remove NA
                //NumericVector neigh2 = na_omit(neigh);
                res(i * ncols + j, angle) = 1;
            }
        }
    }
    return res;
}

// [[Rcpp::export]]
double glcm_contrast(const arma::mat glcm,
                     const arma::uword n_levels) {
    arma::mat j(n_levels, n_levels, arma::fill::zeros);
    arma::mat i(n_levels, n_levels, arma::fill::zeros);
    double res = 0;

    for (arma::uword r = 0; r < n_levels; r++) {
        for (arma::uword c = 0; c < n_levels; c++) {
            i(r, c) = r;
            j(r, c) = c;
        }
    }
    res = arma::accu(glcm % pow(i - j, 2));
    return(res);
}

// [[Rcpp::export]]
double glcm_dissimilarity(const arma::mat glcm,
                          const arma::uword n_levels) {
    arma::mat j(n_levels, n_levels, arma::fill::zeros);
    arma::mat i(n_levels, n_levels, arma::fill::zeros);
    double res = 0;

    for (arma::uword r = 0; r < n_levels; r++) {
        for (arma::uword c = 0; c < n_levels; c++) {
            i(r, c) = r;
            j(r, c) = c;
        }
    }
    res = arma::accu(glcm % abs(i - j));
    return(res);
}

// [[Rcpp::export]]
double glcm_homogeneity(const arma::mat glcm,
                        const arma::uword n_levels) {
    arma::mat j(n_levels, n_levels, arma::fill::zeros);
    arma::mat i(n_levels, n_levels, arma::fill::zeros);
    double res = 0;

    for (arma::uword r = 0; r < n_levels; r++) {
        for (arma::uword c = 0; c < n_levels; c++) {
            i(r, c) = r;
            j(r, c) = c;
        }
    }
    res = arma::accu(glcm / (1 + (pow(i - j, 2))));
    return(res);
}

// [[Rcpp::export]]
double glcm_energy(const arma::mat glcm,
                   const arma::uword n_levels) {
    double res = 0;

    res = std::sqrt(arma::accu(pow(glcm, 2)));
    return(res);
}

// [[Rcpp::export]]
double glcm_asm(const arma::mat glcm,
                const arma::uword n_levels) {
    double res = 0;

    res = arma::accu(pow(glcm, 2));
    return(res);
}

// [[Rcpp::export]]
double glcm_mean(const arma::mat glcm,
                 const arma::uword n_levels) {
    arma::mat i(n_levels, n_levels, arma::fill::zeros);
    double res = 0;

    for (arma::uword r = 0; r < n_levels; r++) {
        for (arma::uword c = 0; c < n_levels; c++) {
            i(r, c) = r;
        }
    }

    res = arma::accu(glcm % i);
    return(res);
}

// [[Rcpp::export]]
double glcm_variance(const arma::mat glcm,
                     const arma::uword n_levels) {
    arma::mat i(n_levels, n_levels, arma::fill::zeros);
    double res = 0;

    for (arma::uword r = 0; r < n_levels; r++) {
        for (arma::uword c = 0; c < n_levels; c++) {
            i(r, c) = r;
        }
    }
    res = arma::accu(glcm % i);

    res = arma::accu(glcm % pow(i - res, 2));
    return(res);
}

// [[Rcpp::export]]
double glcm_std(const arma::mat glcm,
                const arma::uword n_levels) {

    double res = glcm_variance(glcm, n_levels);

    res = sqrt(res);
    return(res);
}

// [[Rcpp::export]]
double glcm_entropy(const arma::mat glcm,
                    const arma::uword n_levels) {
    double res = 0;

    arma::mat glcm_entropy = glcm % ((-1) * log(glcm));
    glcm_entropy.replace(arma::datum::nan, 0);

    res = accu(glcm_entropy);
    return(res);
}

// [[Rcpp::export]]
double glcm_correlation(const arma::mat glcm,
                        const arma::uword n_levels) {
    double res = 0;
    double res_mean = glcm_mean(glcm, n_levels);
    double res_var = glcm_variance(glcm, n_levels);

    arma::mat j(n_levels, n_levels, arma::fill::zeros);
    arma::mat i(n_levels, n_levels, arma::fill::zeros);

    for (arma::uword r = 0; r < n_levels; r++) {
        for (arma::uword c = 0; c < n_levels; c++) {
            i(r, c) = r;
            j(r, c) = c;
        }
    }

    res = accu(glcm % (((i - res_mean) % (j - res_mean)) / (res_var)));
    return(res);
}

// [[Rcpp::export]]
arma::mat C_create_glcm_weights(const arma::uword n_levels) {
    arma::mat j(n_levels, n_levels);
    arma::mat i(n_levels, n_levels);

    for (arma::uword r = 0; r < n_levels; r++) {
        for (arma::uword c = 0; c < n_levels; c++) {
            i(r, c) = r;
            j(r, c) = c;
        }
    }
    Rcpp::Rcout << i << "\n";
    Rcpp::Rcout << j << "\n";

    return (i - j);
}
