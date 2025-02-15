//[[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <cmath>
#include <math.h>

using namespace Rcpp;
using namespace std;

typedef double _glcm_fun(const arma::mat&, const arma::mat&, const arma::mat&);

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
arma::mat glcm_tabulate(const arma::mat& x,
                        arma::mat glcm,
                        const float& angle,
                        const arma::uword& n_grey) {

    int pixels_to_move = 1;

    int nrows = x.n_rows;
    int ncols = x.n_cols;

    arma::uword start_row, end_row, start_col, end_col = 0;
    int offset_row, offset_col, v_i, v_j, row, col = 0;

    offset_row = std::round(std::sin(angle) * pixels_to_move);
    offset_col = std::round(std::cos(angle) * pixels_to_move);
    // row
    start_row = std::max(0, -offset_row);
    end_row = std::min(nrows, nrows - offset_row);
    // col
    start_col = std::max(0, -offset_col);
    end_col = std::min(ncols, ncols - offset_col);
    // for (arma::uword r = start_row; r < end_row; r++) {
    //     for (arma::uword c = start_col; c < end_col; c++) {
    //
    //         // v_i = x(r,c);
    //         // row = r + offset_row;
    //         // col = c + offset_col;
    //         // v_j = x(row, col);
    //         // //if (v_i < n_grey && v_j < n_grey) {
    //         //     //glcm(v_i, v_j) += 1;
    //         //     glcm(v_i, v_j) = 1;
    //         //}
    //     }
    // }
    return glcm;
}

arma::mat glcm_fn(const arma::vec& x,
                  const arma::vec& angles,
                  const arma::uword& nrows,
                  const arma::uword& ncols,
                  const arma::uword& window_size,
                  const arma::uword& n_grey,
                  _glcm_fun _fun) {
    // initialize output values
    arma::mat glcm_co(n_grey, n_grey, arma::fill::zeros);
    // initialize result matrix
    arma::mat res(x.size(), angles.size(), arma::fill::zeros);
    // initialize co-occurrence matrix
    arma::mat co_occur(n_grey, n_grey, arma::fill::zeros);
    // initialize neighborhood matrix
    arma::mat neigh(window_size, window_size);
    arma::mat pos_window(window_size, window_size, arma::fill::zeros);

    double sum = 0;

    arma::uword angle_ith = 0;

    // Initialize auxiliary matrices they are needed in some metrics
    arma::mat i_aux(n_grey, n_grey, arma::fill::zeros);
    arma::mat j_aux(n_grey, n_grey, arma::fill::zeros);

    i_aux = arma::repmat(
        arma::linspace<arma::vec>(1, n_grey, n_grey), 1, n_grey
    );
    j_aux = arma::trans(i_aux);
    arma::uvec a;


    // compute window leg
    int leg = window_size / 2;
    // compute locus mirror
    IntegerVector loci = locus_neigh2(nrows, leg);
    IntegerVector locj = locus_neigh2(ncols, leg);
    // compute values for each pixel
    for (arma::uword i = 0; i < nrows; ++i) {
        for (arma::uword j = 0; j < ncols; ++j) {
            // for all angles
            //for (arma::uword angle = 0; angle < angles.size(); ++angle) {
            // compute the neighborhood
            for (int wi = 0; wi < window_size; ++wi) {
                for (int wj = 0; wj < window_size; ++wj) {
                    neigh(wi, wj) =
                        x(loci(wi + i) * ncols + locj(wj + j));
                }
            }

            //Rcpp::Rcout << "test1" << "\n";
            //glcm_co = glcm_tabulate(neigh, glcm_co, angles(0), n_grey);
            //Rcpp::Rcout << "test2" << "\n";
            // calculate co-occurrence probabilities
            //sum = arma::accu(glcm_co);
            //glcm_co /= sum;

            int pixels_to_move = 1;


            arma::uword start_row, end_row, start_col, end_col = 0;
            int offset_row, offset_col, v_i, v_j, row, col = 0;

            offset_row = std::round(std::sin(0) * pixels_to_move);
            offset_col = std::round(std::cos(0) * pixels_to_move);
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
                    }
                }
            }

            // remove NA
            //NumericVector neigh2 = na_omit(neigh);
            //res(i * ncols + j, 0) = _fun(glcm_co, i_aux, j_aux);
            res(i * ncols + j, 0) = 1;


            //}
            //angle_ith++;
            //glcm_co.clear();
            //glcm_co = aux;
            //glcm_co = 0;
        }
    }
    return res;
}


inline double _glcm_contrast(const arma::mat& x,
                             const arma::mat& i,
                             const arma::mat& j) {
    double res = 0;

    res = arma::accu(x % pow(i - j, 2));
    return(res);
}

// [[Rcpp::export]]
arma::mat C_glcm_contrast(const arma::vec& x,
                          const arma::uword& nrows,
                          const arma::uword& ncols,
                          const arma::uword& window_size,
                          const arma::vec& angles,
                          const arma::uword& n_grey) {

    return glcm_fn(x, angles, nrows, ncols, window_size, n_grey, _glcm_contrast);
}


// // [[Rcpp::export]]
// double glcm_dissimilarity(const arma::mat glcm,
//                           const arma::uword n_levels) {
//     arma::mat j(n_levels, n_levels, arma::fill::zeros);
//     arma::mat i(n_levels, n_levels, arma::fill::zeros);
//     double res = 0;
//
//     for (arma::uword r = 0; r < n_levels; r++) {
//         for (arma::uword c = 0; c < n_levels; c++) {
//             i(r, c) = r;
//             j(r, c) = c;
//         }
//     }
//     res = arma::accu(glcm % abs(i - j));
//     return(res);
// }
//
// // [[Rcpp::export]]
// double glcm_homogeneity(const arma::mat glcm,
//                         const arma::uword n_levels) {
//     arma::mat j(n_levels, n_levels, arma::fill::zeros);
//     arma::mat i(n_levels, n_levels, arma::fill::zeros);
//     double res = 0;
//
//     for (arma::uword r = 0; r < n_levels; r++) {
//         for (arma::uword c = 0; c < n_levels; c++) {
//             i(r, c) = r;
//             j(r, c) = c;
//         }
//     }
//     res = arma::accu(glcm / (1 + (pow(i - j, 2))));
//     return(res);
// }
//
// // [[Rcpp::export]]
// double glcm_mean(const arma::mat glcm,
//                  const arma::uword n_levels) {
//     arma::mat i(n_levels, n_levels, arma::fill::zeros);
//     double res = 0;
//
//     for (arma::uword r = 0; r < n_levels; r++) {
//         for (arma::uword c = 0; c < n_levels; c++) {
//             i(r, c) = r;
//         }
//     }
//
//     res = arma::accu(glcm % i);
//     return(res);
// }
//
// // [[Rcpp::export]]
// double glcm_correlation(const arma::mat glcm,
//                         const arma::uword n_levels) {
//     double res = 0;
//     double res_mean = glcm_mean(glcm, n_levels);
//     double res_var = glcm_variance(glcm, n_levels);
//
//     arma::mat j(n_levels, n_levels, arma::fill::zeros);
//     arma::mat i(n_levels, n_levels, arma::fill::zeros);
//
//     for (arma::uword r = 0; r < n_levels; r++) {
//         for (arma::uword c = 0; c < n_levels; c++) {
//             i(r, c) = r;
//             j(r, c) = c;
//         }
//     }
//
//     res = accu(glcm % (((i - res_mean) % (j - res_mean)) / (res_var)));
//     return(res);
// }
//
// // [[Rcpp::export]]
// double glcm_variance(const arma::mat glcm,
//                      const arma::uword n_levels) {
//     arma::mat i(n_levels, n_levels, arma::fill::zeros);
//     double res = 0;
//
//     for (arma::uword r = 0; r < n_levels; r++) {
//         for (arma::uword c = 0; c < n_levels; c++) {
//             i(r, c) = r;
//         }
//     }
//     res = arma::accu(glcm % i);
//
//     res = arma::accu(glcm % pow(i - res, 2));
//     return(res);
// }
//
// // [[Rcpp::export]]
// double glcm_energy(const arma::mat glcm,
//                    const arma::uword n_levels) {
//     double res = 0;
//
//     res = std::sqrt(arma::accu(pow(glcm, 2)));
//     return(res);
// }
//
// // [[Rcpp::export]]
// double glcm_asm(const arma::mat glcm,
//                 const arma::uword n_levels) {
//     double res = 0;
//
//     res = arma::accu(pow(glcm, 2));
//     return(res);
// }
//
// // [[Rcpp::export]]
// double glcm_std(const arma::mat glcm,
//                 const arma::uword n_levels) {
//
//     double res = glcm_variance(glcm, n_levels);
//
//     res = sqrt(res);
//     return(res);
// }
//
// // [[Rcpp::export]]
// double glcm_entropy(const arma::mat glcm,
//                     const arma::uword n_levels) {
//     double res = 0;
//
//     arma::mat glcm_entropy = glcm % ((-1) * log(glcm));
//     glcm_entropy.replace(arma::datum::nan, 0);
//
//     res = accu(glcm_entropy);
//     return(res);
// }
//
// // [[Rcpp::export]]
// arma::mat C_create_glcm_weights(const arma::uword n_levels) {
//     arma::mat j(n_levels, n_levels);
//     arma::mat i(n_levels, n_levels);
//
//     for (arma::uword r = 0; r < n_levels; r++) {
//         for (arma::uword c = 0; c < n_levels; c++) {
//             i(r, c) = r;
//             j(r, c) = c;
//         }
//     }
//
//     return (i - j);
// }
