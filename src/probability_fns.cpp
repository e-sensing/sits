#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

// [[Rcpp::export]]
arma::mat C_dnorm(const arma::mat& mtx,
                  const double mean = 0,
                  const double std = 1) {
    return arma::normpdf(mtx, mean, std);
}

arma::vec C_radd_calc_pcond(const arma::vec& p1,
                           const arma::vec& p2) {
    return p1 / (p1 + p2);
}

arma::vec C_radd_calc_pbayes(const arma::vec& prior,
                             const arma::vec& post) {
    return (prior * post) / ((prior * post) + ((1 - prior) * (1 - post)));
}

// [[Rcpp::export]]
arma::vec C_radd_calc_nf(const arma::mat& ts,
                         const arma::mat& mean,
                         const arma::mat& std) {

    arma::vec pnfor(ts.n_rows, arma::fill::zeros);
    arma::vec pfor(ts.n_rows, arma::fill::zeros);
    arma::vec pres(ts.n_rows, arma::fill::zeros);
    bool update_res = false;
    for (int i = 0; i < ts.n_cols; i++) {
        pfor = C_dnorm(ts.col(i), mean(0, i), std(0, i));
        pnfor = C_dnorm(ts.col(i), mean(1, i), std(1, i));

        pnfor.elem(arma::find(pnfor < 0.00001)).zeros();

        pnfor.elem(arma::find(pnfor > 0)) = C_radd_calc_pcond(
            pnfor.elem(arma::find(pnfor > 0)),
            pfor.elem(arma::find(pnfor > 0))
        );

        pnfor.elem(arma::find(pnfor < 0)).zeros();
        pnfor.elem(arma::find(pnfor > 1)).ones();

        if (update_res) {
            pnfor = C_radd_calc_pbayes(pnfor, pres);
        }
        update_res = true;
    }
    return pnfor;
}
