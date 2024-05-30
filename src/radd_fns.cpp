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
                         const arma::mat& deseasonlize_values) {

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
            if (deseasonlize_values.size() > 1) {
                ts.submat(i, c, i, c + n_times - 1) = C_radd_calc_sub(
                    ts.submat(i, c, i, c + n_times - 1),
                    deseasonlize_values.submat(0, c, 0, c + n_times - 1)
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
            // Fix the range of prob values between 0 and 1
            // TODO: use parameter bwf
            p_nfor.elem(arma::find(p_nfor < 0.1)).fill(0.1);
            p_nfor.elem(arma::find(p_nfor > 0.9)).fill(0.9);

            // Update NF prob with a Bayesian approach
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

// [[Rcpp::export]]
arma::mat C_radd_detect_changes(const arma::mat& p_res,
                                const arma::uword& start,
                                const arma::uword& end,
                                const double& threshold = 0.5,
                                const double& chi = 0.9) {

    arma::mat res(
            p_res.n_rows, 1, arma::fill::value(arma::datum::nan)
    );
    arma::mat p_flag(
            p_res.n_rows, p_res.n_cols, arma::fill::value(arma::datum::nan)
    );
    arma::mat p_change(
            p_res.n_rows, p_res.n_cols, arma::fill::value(arma::datum::nan)
    );
    arma::rowvec p_flag_aux(
            p_res.n_cols, arma::fill::value(arma::datum::nan)
    );
    arma::uvec idx_value_res;
    arma::uword v;
    bool next_pixel;
    for (arma::uword i = 0; i < p_res.n_rows; i++) {
        // create an auxiliary matrix
        p_flag_aux.fill(arma::datum::nan);
        // set to zero in the past time
        p_flag_aux.row(0).col(start - 1) = 0;
        p_flag_aux.elem(
            arma::find(p_res.submat(i, 0, i, p_res.n_cols - 1) < threshold)
        ).zeros();
        p_flag.row(i) = p_flag_aux;

        // remove the first column its a dummy value
        arma::uvec valid_idx = arma::find(
            p_res.submat(i, 1, i, p_res.n_cols - 1) >= threshold
        ) + 1;

        arma::uvec valid_filt = arma::find(valid_idx >= start && valid_idx <= end);
        valid_idx = valid_idx(valid_filt);
        next_pixel = false;
        for (arma::uword idx = 0; idx < valid_idx.size(); idx++) {
            arma::vec seq_idx = seq_int(valid_idx.at(idx), p_res.n_cols);
            for (arma::uword t = 0; t < seq_idx.size(); t++) {
                arma::uword t_value = seq_idx.at(t);
                // step 2.1: Update Flag and PChange for current time step (i)
                // (case 1) No confirmed or flagged change:
                int r;
                if (t_value > 0) {
                    if (p_flag(i, t_value - 1) == 0 ||
                        p_flag(i, t_value - 1) == 254) {
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
                }

                if (p_flag(i, t_value) != arma::datum::nan &&
                    p_flag(i, t_value) == 1) {
                    if (r > 0) {
                        if (p_change(i, t_value) < 0.5) {
                            p_flag.submat(i, t_value - r, i, t_value).zeros();
                            p_flag(i, t_value - r) = 254;
                            break;
                        }
                    }
                }
                if (p_change(i, t_value) != arma::datum::nan &&
                    p_change(i, t_value) >= chi) {

                    if (p_res(i, t_value) >= threshold) {
                        arma::uword min_idx = arma::find(p_flag.row(i) == 1).min();
                        p_flag.submat(i, min_idx, i, t_value).ones();
                        next_pixel = true;
                        break;
                    }
                }
            }
            if (next_pixel) {
                break;
            }
        }
        idx_value_res = arma::find(p_flag.row(i) == 1);
        v = 0;
        if (idx_value_res.size() > 0) {
            v = arma::find(p_flag.row(i) == 1).max();
        }
        res.row(i) = v;
    }
    return res;
}

// [[Rcpp::export]]
arma::vec C_select_cols(const arma::mat& m,
                        const arma::uword row,
                        const arma::uvec idx) {
    arma::vec v(idx.size(), arma::fill::value(arma::datum::nan));

    for (arma::uword i = 0; i < idx.size(); i++) {
        v(i) = m(row, idx.at(i));
    }
    return v;
}

// [[Rcpp::export]]
arma::vec C_vec_select_cols(const arma::vec& m,
                            const arma::uvec idx) {
    arma::vec v(idx.size(), arma::fill::value(arma::datum::nan));

    for (arma::uword i = 0; i < idx.size(); i++) {
        v(i) = m(idx.at(i));
    }
    return v;
}

// [[Rcpp::export]]
arma::mat C_radd_detect_changes_2(const arma::mat& p_res,
                                  arma::uword& start,
                                  arma::uword& end,
                                  const double& threshold = 0.5,
                                  const double& chi = 0.9) {
    arma::mat res(
            p_res.n_rows, 1, arma::fill::value(arma::datum::nan)
    );

    // Reduce one to be equivalent to cpp indexes
    start--;

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

        // Remove the dummy position from valid values
        arma::uvec idxs_to_filter = valid_values;
        if (start > 0) {
            idxs_to_filter = valid_values.subvec(1, valid_values.size() - 1);
        }

        // Select columns that are not NA
        arma::vec v_res = C_select_cols(p_res, i, idxs_to_filter);

        // Vectors to store flag and change values
        arma::vec p_flag(v_res.size(), arma::fill::value(arma::datum::nan));
        arma::vec p_change(v_res.size(), arma::fill::value(arma::datum::nan));

        // Filter only values that are in valid timeline
        arma::uvec p_filt = arma::find(
            idxs_to_filter >= start && idxs_to_filter <= end
        );
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
        if (start == 0) {
            valid_values = valid_values.subvec(1, valid_values.size() - 1);
            first_idx = 1;
        } else {
            valid_values = idxs_to_filter;
        }

        // Update next_pixel variable
        next_pixel = false;

        // Remove the first column its a dummy value
        arma::uvec res_idx = arma::find(
            valid_values >= start &&
                valid_values <= end &&
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
                        p_flag.subvec(min_idx, t_value).ones();
                        next_pixel = true;
                        break;
                    }
                }
            }
            if (next_pixel) {
                break;
            }
        }

        idx_value_res = arma::find(p_flag == 1);
        v = 0;
        if (idx_value_res.size() > 0) {
            v = idxs_to_filter(arma::find(p_flag == 1).max());
        }
        res.row(i) = v;
    }
    return res;
}
