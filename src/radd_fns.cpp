#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

// [[Rcpp::export]]
arma::mat C_dnorm(const arma::mat& mtx,
                  const double mean = 0,
                  const double std = 1) {
    return arma::normpdf(mtx, mean, std);
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

// [[Rcpp::export]]
arma::mat C_radd_calc_nf(arma::mat& ts,
                         const arma::mat& mean,
                         const arma::mat& sd,
                         const arma::uword& n_times,
                         const arma::mat& quantile_values,
                         const arma::vec& bwf) {

    // Using the first element as dummy value
    arma::mat p_res(ts.n_rows, n_times + 1, arma::fill::value(0.5));
    // For each pixel
    for (arma::uword i = 0; i < ts.n_rows; i++) {
        // Probability to be a Forest
        arma::colvec p_for(n_times, arma::fill::zeros);
        // Probability to be a Non-Forest
        arma::colvec p_nfor(n_times, arma::fill::zeros);
        // Probability to be a Non-Forest in the past
        arma::colvec p_nfor_past(n_times, arma::fill::zeros);

        // Aux variables
        arma::uword col_idx = 0;
        bool update_res = false;

        // For each band
        for (arma::uword c = 0; c < ts.n_cols; c = c + n_times) {
            // Deseasonlize time series
            if (quantile_values.size() > 1) {
                ts.submat(i, c, i, c + n_times - 1) = C_radd_calc_sub(
                    ts.submat(i, c, i, c + n_times - 1),
                    quantile_values.submat(0, c, 0, c + n_times - 1)
                );
            }
            // Estimate a normal distribution based on Forest stats
            p_for = C_dnorm(
                ts.submat(i, c, i, c + n_times - 1).t(),
                mean(0, col_idx),
                sd(0, col_idx)
            );
            // Estimate a normal distribution based on Non-Forest stats
            p_nfor = C_dnorm(
                ts.submat(i, c, i, c + n_times - 1).t(),
                mean(1, col_idx),
                sd(1, col_idx)
            );
            // Clean values lower than 0.00001
            p_nfor.elem(arma::find(p_nfor < 0.00001)).zeros();
            // Estimate a conditional prob for each positive distribution value
            p_nfor.elem(arma::find(p_nfor > 0)) = C_radd_calc_pcond(
                p_nfor.elem(arma::find(p_nfor > 0)),
                p_for.elem(arma::find(p_nfor > 0))
            );
            // Fix the range of prob values
            p_nfor.elem(arma::find(p_nfor < bwf(0))).fill(bwf(0));
            p_nfor.elem(arma::find(p_nfor > bwf(1))).fill(bwf(1));

            // Update NF probabilities with a Bayesian approach
            if (update_res) {
                p_nfor = C_radd_calc_pbayes(p_nfor, p_nfor_past);
            }
            // Update Non-Forest probs
            p_nfor_past = p_nfor;
            update_res = true;
            col_idx++;
        }
        // Get the probs for NF values
        p_res.submat(i, 1, i, n_times) = p_nfor.t();
    }
    // Return the probs results
    return p_res;
}

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

arma::vec C_select_cols(const arma::mat& m,
                        const arma::uword row,
                        const arma::uvec idx) {
    arma::vec v(idx.size(), arma::fill::value(arma::datum::nan));

    for (arma::uword i = 0; i < idx.size(); i++) {
        v(i) = m(row, idx.at(i));
    }
    return v;
}

arma::vec C_vec_select_cols(const arma::vec& m,
                            const arma::uvec idx) {
    arma::vec v(idx.size(), arma::fill::value(arma::datum::nan));

    for (arma::uword i = 0; i < idx.size(); i++) {
        v(i) = m(idx.at(i));
    }
    return v;
}

// [[Rcpp::export]]
arma::mat C_radd_detect_changes(const arma::mat& p_res,
                                const arma::uword& start_detection,
                                const arma::uword& end_detection,
                                const double& threshold = 0.5,
                                const double& chi = 0.9) {
    arma::mat res(
            p_res.n_rows, 1, arma::fill::value(arma::datum::nan)
    );

    arma::uvec idx_value_res;
    arma::uword v;
    bool next_pixel;
    arma::uword first_idx;
    // for each pixel
    for (arma::uword i = 0; i < p_res.n_rows; i++) {
        // Filter non NA values
        arma::uvec valid_values = arma::find_finite(
            p_res.submat(i, 0, i, p_res.n_cols - 1)
        );
        // Only one valid is valid
        if (valid_values.size() == 1) {
            res.row(i) = 0;
            continue;
        }

        // Remove the dummy position from valid values
        arma::uvec idxs_to_filter = valid_values;
        if (start_detection > 0) {
            idxs_to_filter = valid_values.subvec(1, valid_values.size() - 1);
        }

        // Select columns that are not NA
        arma::vec v_res = C_select_cols(p_res, i, idxs_to_filter);

        // Vectors to store flag and change values
        arma::vec p_flag(v_res.size(), arma::fill::value(arma::datum::nan));
        arma::vec p_change(v_res.size(), arma::fill::value(arma::datum::nan));

        // Filter only values that are in valid timeline
        arma::uvec p_filt = arma::find(
            idxs_to_filter >= start_detection && idxs_to_filter <= end_detection
        );

        // Only one valid is valid
        if (p_filt.size() == 0) {
            res.row(i) = 0;
            continue;
        }

        // Add a zero to the first element in flag vector
        arma::uword start_idx = p_filt.min();
        if (start_idx > 0) {
            start_idx--;
        }
        p_flag(start_idx) = 0;
        // Add zeros in values that are lower than the threshold
        p_flag.elem(
            arma::find(v_res < threshold)
        ).zeros();

        // We need to remove the first dummy in case the start is zero
        first_idx = 0;
        if (start_detection == 0) {
            valid_values = valid_values.subvec(1, valid_values.size() - 1);
            first_idx = 1;
        } else {
            valid_values = idxs_to_filter;
        }

        // Update next_pixel variable
        next_pixel = false;

        // Remove the first column its a dummy value
        arma::uvec res_idx = arma::find(
            valid_values >= start_detection   &&
                valid_values <= end_detection &&
                v_res.subvec(first_idx, v_res.size() - 1) > threshold
        );

        for (arma::uword idx = 0; idx < res_idx.size(); idx++) {
            arma::vec seq_idx = seq_int(res_idx.at(idx), v_res.size());
            for (arma::uword t = 0; t < seq_idx.size(); t++) {
                arma::uword t_value = seq_idx.at(t);
                // step 2.1: Update Flag and PChange for current time step (i)
                // (case 1) No confirmed or flagged change:
                int r;
                if (t_value > 0) {
                    if (p_flag(t_value - 1) == 0 ||
                        p_flag(t_value - 1) == 254) {
                        r = 0;
                        double prior = v_res(t_value - 1);
                        double likelihood = v_res(t_value);
                        double posterior = C_radd_calc_pbayes(prior, likelihood);
                        p_flag(t_value) = 1;
                        p_change(t_value) = posterior;
                    }

                    if (p_flag(t_value - 1) == 1) {
                        double prior = p_change(t_value - 1);
                        double likelihood = v_res(t_value);
                        double posterior = C_radd_calc_pbayes(prior, likelihood);
                        p_flag(t_value) = 1;
                        p_change(t_value) = posterior;
                        r++;
                    }
                }
                if (p_flag(t_value) == 1) {
                    if (r > 0) {
                        if (p_change(t_value) < 0.5) {
                            p_flag.subvec(t_value - r, t_value).zeros();
                            p_flag(t_value - r) = 254;
                            break;
                        }
                    }
                }
                if (p_change(t_value) >= chi) {

                    if (v_res(t_value) >= 0.5) {
                        arma::uword min_idx = arma::find(p_flag == 1).min();
                        p_flag.subvec(min_idx, t_value).fill(2);
                        next_pixel = true;
                        break;
                    }
                }
            }
            if (next_pixel) {
                break;
            }
        }

        idx_value_res = arma::find(p_flag == 2);
        v = 0;
        if (idx_value_res.size() > 0) {
            v = idxs_to_filter(arma::find(p_flag == 2).max());
        }
        res.row(i) = v;
    }
    return res;
}
