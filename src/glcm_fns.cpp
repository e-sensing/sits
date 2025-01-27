//[[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <math.h>

using namespace Rcpp;
using namespace std;

//[[Rcpp::export]]
arma::mat glcm_calc(const arma::mat& x,
                    const int window_size,
                    const arma::vec& angles,
                    const int n_bits = 16) {
    arma::mat out(n_bits, n_bits, arma::fill::zeros);

    int nrows = x.n_rows;
    int ncols = x.n_cols;
    int offset_row, offset_col, start_row, end_row, start_col, end_col,
    v_i, v_j, row, col = 0;

    // For each angle
    for (arma::uword i = 0; i < angles.size(); i++) {
        float angle = angles(i);
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
    }
    return out;
}
