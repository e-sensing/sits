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
    return (prior % post) / ((prior % post) + ((1 - prior) % (1 - post)));
}

double C_radd_calc_pbayes(const double& prior,
                          const double& post) {
    return (prior * post) / ((prior * post) + ((1 - prior) * (1 - post)));
}

// [[Rcpp::export]]
arma::mat C_radd_calc_nf(const arma::mat& ts,
                         const arma::mat& mean,
                         const arma::mat& sd,
                         const arma::uword& n_times,
                         const double& threshold = 0.5) {

    arma::mat p_res(ts.n_rows, n_times, arma::fill::zeros);
    arma::mat p_flag(ts.n_rows, n_times, arma::fill::value(arma::datum::nan));
    arma::mat p_change(ts.n_rows, n_times, arma::fill::value(arma::datum::nan));

    // for each pixel
    for (int i = 0; i < ts.n_rows; i++) {
        arma::colvec p_for(n_times, arma::fill::zeros);
        arma::colvec p_nfor(n_times, arma::fill::zeros);
        arma::colvec p_nfor_past(n_times, arma::fill::zeros);

        arma::uword col_idx = 0;

        bool update_res = false;
        // for each band
        for (int c = 0; c < ts.n_cols; c = c + n_times) {
            p_for = C_dnorm(
                ts.submat(i, c, i, c + n_times - 1).t(),
                mean(0, col_idx),
                sd(0, col_idx)
            );
            p_nfor = C_dnorm(
                ts.submat(i, c, i, c + n_times - 1).t(),
                mean(1, col_idx),
                sd(1, col_idx)
            );
            p_nfor.elem(arma::find(p_nfor < 0.00001)).zeros();

            p_nfor.elem(arma::find(p_nfor > 0)) = C_radd_calc_pcond(
                p_nfor.elem(arma::find(p_nfor > 0)),
                p_for.elem(arma::find(p_nfor > 0))
            );

            p_nfor.elem(arma::find(p_nfor < 0)).zeros();
            p_nfor.elem(arma::find(p_nfor > 1)).ones();

            if (update_res) {
                p_nfor = C_radd_calc_pbayes(p_nfor, p_nfor_past);
            }

            update_res = true;
            p_nfor_past = p_nfor;
            col_idx++;
        }
        p_res.row(i) = p_nfor.t();
    }
    return p_res;
}

// [[Rcpp::export]]
arma::vec seq_int(const arma::uword& from,
                  const arma::uword& to,
                  const arma::uword& n = 1) {
    arma::vec aux = arma::vec(to - from + 1, arma::fill::zeros);
    arma::uword t = 0;
    for (arma::uword i = from; i <= to; i = i + n) {
        aux.at(t) = i;
        t++;
    }

    return aux;
}

// [[Rcpp::export]]
void C_radd_start_monitoring(const arma::mat& p_res,
                             const double& threshold = 0.5) {
    arma::mat p_flag(
            p_res.n_rows, p_res.n_cols, arma::fill::value(arma::datum::nan)
    );
    arma::mat p_change(
            p_res.n_rows, p_res.n_cols, arma::fill::value(arma::datum::nan)
    );
    arma::rowvec p_flag_aux(
            p_res.n_cols, arma::fill::value(arma::datum::nan)
    );
    arma::rowvec p_change_aux(
            p_res.n_cols, arma::fill::value(arma::datum::nan)
    );

    for (arma::uword i = 0; i < p_res.n_rows; i++) {
        p_flag_aux.fill(arma::datum::nan);
        // TODO: remove the first element dummy of this vector
        arma::uvec valid_idx = arma::find(p_res.row(i) >= threshold);
        p_flag_aux.elem(arma::find(p_res.row(i) < threshold)).zeros();
        p_flag.row(i) = p_flag_aux;
        for (arma::uword idx = 0; idx < valid_idx.size(); idx++) {
            arma::vec seq_idx = seq_int(valid_idx.at(idx), p_res.n_cols);
            for (arma::uword t = 0; t < seq_idx.size(); t++) {
                arma::uword t_value = seq_idx.at(t);
                // step 2.1: Update Flag and PChange for current time step (i)
                // (case 1) No confirmed or flagged change:
                int r;
                if (p_flag(i, t_value - 1) == 0 || p_flag(i, t_value - 1) == 2) {
                    r = 0;
                    double prior = p_res(i, t_value - 1);
                    double likelihood = p_res(i, t_value);

                    double posterior = C_radd_calc_pbayes(prior, likelihood);
                    p_flag(i, t_value) = 1;
                    p_change(i, t_value) = posterior;
                }

                if (p_flag(i, t_value - 1) == 1) {
                    double prior = p_change(i, t_value - 1);
                    double likelihood = p_res(i, t_value);
                    double posterior = C_radd_calc_pbayes(prior, likelihood);
                    p_flag(i, t_value) = 1;
                    p_change(i, t_value) = posterior;
                    r++;
                }

                if (p_flag(i, t_value) != arma::datum::nan &&
                    p_flag(i, t_value) == 1) {
                    if (r > 0) {
                        if (p_change(i, t_value) < 0.5) {
                            arma::vec ti = arma::linspace(t_value - r, t);
                            p_flag.row(i).cols(t_value - r, t_value) = 0;
                            p_flag(i, t_value - r) = 2;
                            break;
                        }
                    }
                }
                // TODO: add parameter chi
                if (p_change(i, t_value) != arma::datum::nan &&
                    p_change(i, t_value) >= 0.5) {

                    if (p_res(i, t_value) >= threshold) {
                        arma::uword min_idx = arma::find(p_flag.row(i) == 1).index_min();
                        p_flag.row(i).cols(t_value - r, t_value) = 0;
                        //return p_change;
                    }
                }
            }
        }
    }
    //return p_res;
}

