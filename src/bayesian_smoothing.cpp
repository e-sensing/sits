#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

struct _img {
    const arma::mat* data;
    arma::uword n_rows;
    arma::uword n_cols;
};

typedef _img img_t;

struct _kern {
    const arma::mat* data;
    arma::uword leg_i;
    arma::uword leg_j;
};

typedef _kern kern_t;

struct _region {
    arma::uword r1;
    arma::uword r2;
    arma::uword c1;
    arma::uword c2;
    arma::uword m_i;
    arma::uword m_j;
};

typedef _region region_t;

struct _neigh {
    arma::mat data;
    arma::colvec weights;
    arma::uword n_rows;
    _neigh(const img_t& m, const kern_t& w):
        data(w.data->n_elem, m.data->n_cols, arma::fill::zeros),
        weights(w.data->n_elem, arma::fill::zeros),
        n_rows(0) {}
};

typedef _neigh neigh_t;

inline double value(img_t m, arma::uword i, arma::uword j, arma::uword b) {
    return (*m.data)(j + i * m.n_cols, b);
}

inline arma::rowvec value(img_t m, arma::uword i, arma::uword j) {
    return m.data->row(j + i * m.n_cols);
}

inline double value(kern_t w, region_t reg, arma::uword i, arma::uword j) {
    return (*w.data)(i - reg.m_i + w.leg_i, j - reg.m_j + w.leg_j);
}

void region(region_t& reg,
            const img_t& m,
            const kern_t& w,
            const arma::uword m_i,
            const arma::uword m_j) {

    // store reference
    reg.m_i = m_i;
    reg.m_j = m_j;
    // compute window region
    reg.r1 = m_i > w.leg_i ? m_i - w.leg_i : 0;
    reg.r2 = m_i + w.leg_i < m.n_rows ? m_i + w.leg_i : m.n_rows - 1;
    reg.c1 = m_j > w.leg_j ? m_j - w.leg_j : 0;
    reg.c2 = m_j + w.leg_j < m.n_cols ? m_j + w.leg_j : m.n_cols - 1;
}

img_t image(const arma::mat* m,
            const arma::uword m_nrow,
            const arma::uword m_ncol) {

    if (m_nrow * m_ncol != m->n_rows)
        throw std::invalid_argument("Invalid matrix size");

    // initialize result
    img_t res;
    // store reference
    res.data = m;
    res.n_rows = m_nrow;
    res.n_cols = m_ncol;

    return res;
}

kern_t kernel(const arma::mat* w) {

    if (w->n_rows % 2 == 0 || w->n_cols % 2 == 0)
        throw std::invalid_argument("Invalid window matrix size");

    // initialize result
    kern_t res;

    // store reference
    res.data = w;
    res.leg_i = w->n_rows / 2;
    res.leg_j = w->n_cols / 2;

    return res;
}

void neigh_vec(neigh_t& n,
               const img_t& m,
               const region_t& reg,
               const arma::uword b) {

    // copy values
    arma::uword k = 0;
    for (arma::uword i = reg.r1; i <= reg.r2; ++i)
        for (arma::uword j = reg.c1; j <= reg.c2; ++j)
            if (arma::is_finite(value(m, i, j, 0)))
                n.data(k++, b) = value(m, i, j, b);
    n.n_rows = k;
}

void neigh_weights(neigh_t& n,
                   const img_t& m,
                   const kern_t& w,
                   const region_t& reg) {

    // copy values
    arma::uword k = 0;
    for (arma::uword i = reg.r1; i <= reg.r2; ++i)
        for (arma::uword j = reg.c1; j <= reg.c2; ++j)
            if (arma::is_finite(value(m, i, j, 0)))
                n.weights(k++) = value(w, reg, i, j);
}

void neighbours(neigh_t& neigh,
                const img_t& m,
                const kern_t& w,
                const region_t& reg) {

    // copy values
    for (arma::uword b = 0; b < m.data->n_cols; ++b)
        neigh_vec(neigh, m, reg, b);
    // copy weights
    neigh_weights(neigh, m, w, reg);
}

arma::colvec nm_post_mean_x(const arma::colvec& x,
                            const arma::mat& sigma,
                            const arma::colvec& mu0,
                            const arma::mat& sigma0) {

    // inverse sigma0
    arma::mat inv_sum_weights(arma::size(sigma0));
    inv_sum_weights = arma::inv(sigma + sigma0);

    return sigma * inv_sum_weights * mu0 + sigma0 * inv_sum_weights * x;
}

// [[Rcpp::export]]
arma::mat bayes_smoother(const arma::mat& m,
                         const arma::uword m_nrow,
                         const arma::uword m_ncol,
                         const arma::mat& w,
                         const arma::mat& sigma,
                         bool covar_sigma0) {

    if (m.n_cols != sigma.n_rows || m.n_cols != sigma.n_cols)
        throw std::invalid_argument("Invalid sigma matrix size");

    // initialize result matrix
    arma::mat res(arma::size(m), arma::fill::none);
    res.fill(arma::datum::nan);

    // prior mean vector (neighbourhood)
    arma::colvec mu0(m.n_cols, arma::fill::zeros);

    // prior co-variance matrix (neighbourhood)
    arma::mat sigma0(arma::size(sigma), arma::fill::zeros);

    // create image reference
    img_t img = image(&m, m_nrow, m_ncol);

    // create kernel reference
    kern_t krn = kernel(&w);

    // neighbourhood
    neigh_t neigh(img, krn);

    // region variable
    region_t reg;

    // compute values for each pixel
    for (arma::uword i = 0; i < img.n_rows; ++i) {
        for (arma::uword j = 0; j < img.n_cols; ++j) {

            // update region
            region(reg, img, krn, i, j);

            // fill neighbours values
            neighbours(neigh, img, krn, reg);

            if (neigh.n_rows == 0) continue;

            // compute prior mean
            mu0 = arma::mean(neigh.data, 0).as_col();

            // compute prior sigma
            sigma0 = arma::cov(neigh.data, 0);

            // prior sigma covariance
            if (!covar_sigma0) {

                // clear non main diagonal cells
                sigma0.elem(arma::trimatu_ind(
                        arma::size(sigma0), 1)).fill(0.0);
                sigma0.elem(arma::trimatl_ind(
                        arma::size(sigma0), -1)).fill(0.0);
            }

            // evaluate multivariate bayesian
            res.row(j + i * m_ncol) =
                nm_post_mean_x(value(img, i, j).as_col(),
                               sigma, mu0, sigma0).as_row();
        }
    }
    return res;
}

// [[Rcpp::export]]
arma::mat kernel_smoother(const arma::mat& m,
                          const arma::uword m_nrow,
                          const arma::uword m_ncol,
                          const arma::mat& w,
                          const bool normalised) {

    // initialize result matrix
    arma::mat res(arma::size(m), arma::fill::none);
    res.fill(arma::datum::nan);

    // create image reference
    img_t img = image(&m, m_nrow, m_ncol);

    // create kernel reference
    kern_t krn = kernel(&w);

    // neighbourhood
    neigh_t neigh(img, krn);

    // region variable
    region_t reg;

    // compute values for each pixel
    for (arma::uword b = 0; b < m.n_cols; ++b) {
        for (arma::uword i = 0; i < img.n_rows; ++i) {
            for (arma::uword j = 0; j < img.n_cols; ++j) {

                // update region
                region(reg, img, krn, i, j);

                // fill neighbours values
                neigh_vec(neigh, img, reg, b);

                if (neigh.n_rows == 0) continue;

                // fill neighbours weights
                neigh_weights(neigh, img, krn, reg);

                // normalise weight values
                if (normalised)
                    neigh.weights = neigh.weights / arma::sum(neigh.weights);

                // compute kernel neighbourhood weighted mean
                res(j + i * m_ncol, b) = arma::as_scalar(
                    neigh.weights.as_row() * neigh.data.col(b));
            }
        }
    }
    return res;
}

// [[Rcpp::export]]
arma::mat bilinear_smoother(const arma::mat& m,
                            const arma::uword m_nrow,
                            const arma::uword m_ncol,
                            const arma::mat& w,
                            double tau) {

    if (tau <= 0)
        throw std::invalid_argument("Invalid tau value");

    // initialize result matrix
    arma::mat res(arma::size(m), arma::fill::none);
    res.fill(arma::datum::nan);

    // create image reference
    img_t img = image(&m, m_nrow, m_ncol);

    // create kernel reference
    kern_t krn = kernel(&w);

    // neighbourhood
    neigh_t neigh(img, krn);

    // region variable
    region_t reg;

    // bilinear weights
    arma::colvec bln_weight;

    // compute values for each pixel
    for (arma::uword b = 0; b < m.n_cols; ++b) {
        for (arma::uword i = 0; i < img.n_rows; ++i) {
            for (arma::uword j = 0; j < img.n_cols; ++j) {

                // update region
                region(reg, img, krn, i, j);

                // fill neighbours values
                neigh_vec(neigh, img, reg, b);

                if (neigh.n_rows == 0) continue;

                // fill neighbours weights
                neigh_weights(neigh, img, krn, reg);

                // compute bilinear weight
                bln_weight = neigh.weights % arma::normpdf(
                    neigh.data.col(b) - value(img, i, j, b), 0, tau);

                // normalise weight values
                bln_weight = bln_weight / arma::sum(bln_weight);

                // compute kernel neighbourhood weighted mean
                res(j + i * m_ncol, b) = arma::as_scalar(bln_weight.as_row() *
                    neigh.data.col(b));
            }
        }
    }
    return res;
}
