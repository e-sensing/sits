#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
/*
 * This function implements the Savitzky-Golay smoother
 * It has been lifted from R package signal
 * to reduce the number of dependencies in sits
 *
 * sits is licensed as GPL-3
 *
 */

// Octave/Matlab-compatible filter function
arma::vec signal_filter(const arma::vec& filt,
                        const arma::vec& x) {

    arma::uword len_init = filt.n_rows - 1;
    arma::vec ext_x(len_init, arma::fill::zeros);
    arma::vec result(x.n_rows);
    // extend the input vector with left-side zeros
    ext_x = arma::join_cols(ext_x, x);

    for (arma::uword i = 0; i < x.n_rows; i++){
        double out = 0;
        for (arma::uword j = 0; j < filt.n_rows; j++){
            out +=  ext_x(i+j) * filt(filt.n_rows - (j + 1));
        }
        result(i) = out;
    }
    // x <- stats::na.omit(x1, filt, sides = 1)
    return(result);
}


// Savitzky-Golay  smoother: lifted from package signal
// [[Rcpp::export]]
arma::vec smooth_sg(const arma::vec& data,
                    const arma::mat& f_res,
                    const int& p,
                    const int& n) {

    int len = data.n_rows;
    int ncols = f_res.n_cols;
    int nrows = f_res.n_rows;
    int k = floor(n/2);

    arma::vec z(len);
    arma::vec y(len);
    arma::vec filt_coef(ncols);
    filt_coef = f_res.row(k).as_col();
    z = signal_filter(filt_coef, data);

    y = arma::join_cols(
        f_res.submat(0, 0, k - 1, ncols - 1) * data.subvec(0, n - 1),
        z.subvec(n - 1, len - 1),
        f_res.submat(k + 1, 0, nrows - 1, ncols - 1) * data.subvec(len - n, len - 1)
    );
    return(y);
}

// Savitzky-Golay  smoother for matrix lifted from package signal
// [[Rcpp::export]]
arma::mat smooth_sg_mtx(const arma::mat& data,
                        const arma::mat& f_res,
                        const int& p,
                        const int& n) {

    int nrows = data.n_rows;
    arma::mat result(data);

    for (int i = 0; i < nrows; i++){
        result.row(i) = smooth_sg(data.row(i).as_col(), f_res, p, n).as_row();
    }
    return(result);
}
