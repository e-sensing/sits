
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]


using namespace Rcpp;


// [[Rcpp::export]]
arma::mat neighborhood(const arma::mat& m,
                       const unsigned int m_nrow,
                       const unsigned int m_ncol,
                       const arma::mat& w,
                       int m_i, int m_j) {

    if (m_nrow * m_ncol != m.n_rows)
        throw std::invalid_argument("Invalid matriz size");

    if (w.n_cols % 2 == 0 || w.n_rows % 2 == 0)
        throw std::invalid_argument("Window size must be odd");

    if (m_i >= m_nrow || m_j >= m_ncol)
        throw std::invalid_argument("Index out of bounds");

    //  compute window intersection...
    unsigned int w_nrow = w.n_rows, w_ncol = w.n_cols;
    unsigned int w_legi = w.n_rows / 2, w_legj = w.n_cols / 2;

    // ...compute window size
    w_nrow = w.n_rows - (w_legi > m_i ? w_legi - m_i : 0)
        - (w_legi > m_nrow - m_i - 1 ? w_legi + m_i + 1 - m_nrow : 0);
    w_ncol = w.n_cols - (w_legj > m_j ? w_legj - m_j : 0)
        - (w_legj > m_ncol - m_j - 1 ? w_legj + m_j + 1 - m_ncol : 0);

    // ...offset matrix m
    m_i = m_i - (w_legi > m_i ? m_i : w_legi);
    m_j = m_j - (w_legj > m_j ? m_j : w_legj);

    // initialize result matrix
    arma::mat res(w_nrow * w_ncol, m.n_cols);

    // optimize navigation (col first, row later)
    for (int i = 0; i < w_nrow; ++i) {
        for (int j = 0; j < w_ncol; ++j) {
            res.row(j + i * w_ncol) =
                m.row((j + m_j) + (i + m_i) * m_ncol) * w(i, j);
        }
    }
    return res;
}

// [[Rcpp::export]]
arma::colvec post_mean_x(const arma::colvec& x,
                         const arma::mat& sigma,
                         const arma::colvec& mu0,
                         const arma::mat& sigma0) {

    // inverse sigma0
    arma::mat inv_sum_weights(arma::size(sigma0));
    inv_sum_weights = arma::inv(sigma + sigma0);

    return sigma * inv_sum_weights * mu0
        + sigma0 * inv_sum_weights * x;
}


// [[Rcpp::export]]
arma::mat bayes_multiv_smooth(const arma::mat& m,
                              const unsigned int m_nrow,
                              const unsigned int m_ncol,
                              const arma::mat& w,
                              const arma::mat& sigma,
                              bool covar) {

    if (m_nrow * m_ncol != m.n_rows)
        throw std::invalid_argument("Invalid matrix size");

    if (m.n_cols != sigma.n_rows || m.n_cols != sigma.n_cols)
        throw std::invalid_argument("Invalid sigma matrix size");

    // prior mean vector (neighborhood)
    arma::colvec mu0(m.n_cols, arma::fill::zeros);

    // prior co-variance matrix (neighborhood)
    arma::mat sigma0(arma::size(sigma), arma::fill::zeros);

    // neighborhood window values
    arma::mat neigh(arma::size(w), arma::fill::zeros);

    // initialize result matrix
    arma::mat res(arma::size(m), arma::fill::zeros);

    // optimize navigation
    for (int i = 0; i < m_nrow; ++i) {
        for (int j = 0; j < m_ncol; ++j) {

            neigh = neighborhood(m, m_nrow, m_ncol, w, i, j);

            mu0 = arma::mean(neigh).as_col();

            sigma0 = arma::cov(neigh);

            if (!covar) {
                // clear non main diagonal cells
                sigma0.elem(arma::trimatu_ind(
                        arma::size(sigma0), 1)).fill(0.0);
                sigma0.elem(arma::trimatl_ind(
                        arma::size(sigma0), -1)).fill(0.0);
            }

            // evaluate multivariate bayesian
            res.row(j + i * m_ncol) =
                post_mean_x(m.row(j + i * m_ncol).as_col(),
                            sigma, mu0, sigma0).as_row();
        }
    }
    return res;
}

