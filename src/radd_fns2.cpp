#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;



// [[Rcpp::export]]
arma::vec seq_int(const arma::uword& from,
                  const arma::uword& to,
                  const arma::uword& n = 1) {
    arma::vec aux = arma::vec(to - from, arma::fill::zeros);
    arma::uword t = 0;
    for (arma::uword i = from; i < to; i = i + n) {
        aux.at(t) = i;
        t++;
    }

    return aux;
}

arma::vec C_radd_calc_pcond(const arma::vec& p1, const arma::vec& p2) {
    return p1 / (p1 + p2);
}

arma::vec C_radd_calc_pbayes(const arma::vec& prior, const arma::vec& post) {
    return (prior % post) / ((prior % post) + ((1 - prior) % (1 - post)));
}

// [[Rcpp::export]]
arma::rowvec C_radd_calc_sub(const arma::mat& x, const arma::mat& y) {
    return x - y;
}

double C_radd_calc_pbayes(const double& prior, const double& post) {
    return (prior * post) / ((prior * post) + ((1 - prior) * (1 - post)));
}
