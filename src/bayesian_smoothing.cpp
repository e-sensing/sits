
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]


using namespace Rcpp;

inline void filter_finite(arma::mat& m) {

    arma::mat res(arma::size(m));
    arma::uword k = 0;
    for(arma::uword i = 0; i < m.n_rows; i++) {
        if (!m.row(i).has_inf() && !m.row(i).has_nan())
            res.row(k++) = m.row(i);
    }
    for(arma::uword i = 0; i < k; i++)
        m.row(i) = res.row(i);
    m.resize(k, m.n_cols);
}

void neighbours(arma::mat& res,
                const arma::mat& m,
                const arma::uword m_nrow,
                const arma::uword m_ncol,
                const arma::imat& w,
                const arma::uword m_i,
                const arma::uword m_j) {

    // compute half window size
    arma::uword w_legi = w.n_rows / 2, w_legj = w.n_cols / 2;

    // max(m_i - w_legi, 0)
    arma::uword r1 = m_i > w_legi ? m_i - w_legi : 0;

    // min(m_i + w_legi, m_nrow - 1)
    arma::uword r2 = m_i + w_legi < m_nrow ? m_i + w_legi : m_nrow - 1;

    // max(m_j - w_legj, 0)
    arma::uword c1 = m_j > w_legj ? m_j - w_legj : 0;

    // min(m_j + w_legj, m_ncol - 1)
    arma::uword c2 = m_j + w_legj < m_ncol ? m_j + w_legj : m_ncol - 1;

    // optimize navigation C standard
    arma::uword k = 0;
    for (arma::uword i = r1; i <= r2; ++i) {
        for (arma::uword j = c1; j <= c2; ++j) {
            if (w(i - m_i + w_legi, j - m_j + w_legj))
                res.row(k++) = m.row(j + i * m_ncol);
        }
    }
    res.resize(k, m.n_cols);
}

inline arma::colvec nm_post_mean_x(const arma::colvec& x,
                                   const arma::mat& sigma,
                                   const arma::colvec& mu0,
                                   const arma::mat& sigma0) {

    // inverse sigma0
    arma::mat inv_sum_weights(arma::size(sigma0));
    inv_sum_weights = arma::inv(sigma + sigma0);

    return sigma * inv_sum_weights * mu0 + sigma0 * inv_sum_weights * x;
}


// [[Rcpp::export]]
arma::mat bayes_multiv_smooth(const arma::mat& m,
                              const arma::uword m_nrow,
                              const arma::uword m_ncol,
                              const arma::imat& w,
                              const arma::mat& sigma,
                              bool covar) {

    if (m_nrow * m_ncol != m.n_rows)
        throw std::invalid_argument("Invalid matrix size");

    if (m.n_cols != sigma.n_rows || m.n_cols != sigma.n_cols)
        throw std::invalid_argument("Invalid sigma matrix size");

    if (w.n_rows % 2 == 0 || w.n_cols % 2 == 0)
        throw std::invalid_argument("Invalid window matrix size");

    // prior mean vector (neighborhood)
    arma::colvec mu0(m.n_cols, arma::fill::zeros);

    // prior co-variance matrix (neighborhood)
    arma::mat sigma0(arma::size(sigma), arma::fill::zeros);

    // neighborhood window values
    arma::mat neigh(w.n_rows * w.n_cols, m.n_cols, arma::fill::zeros);

    // initialize result matrix
    arma::mat res(arma::size(m), arma::fill::zeros);

    // optimize navigation
    for (arma::uword i = 0; i < m_nrow; ++i) {
        for (arma::uword j = 0; j < m_ncol; ++j) {

            neigh.set_size(w.n_rows * w.n_cols, m.n_cols);

            neighbours(neigh, m, m_nrow, m_ncol, w, i, j);

            filter_finite(neigh);

            if (neigh.n_rows == 0) continue;

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
                nm_post_mean_x(m.row(j + i * m_ncol).as_col(),
                               sigma, mu0, sigma0).as_row();
        }
    }
    return res;
}

