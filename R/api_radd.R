.radd_detect_events <- function(data,
                                threshold = 0.5,
                                start_date = NULL,
                                end_date = NULL) {
    data <- .radd_filter_changes(
        data = data, threshold = threshold, start_date = start_date,
        end_date = end_date
    )
    data <- .radd_add_dummy(data)

    data <- .radd_start_monitoring(data, threshold)
}

.radd_start_monitoring <- function(data, threshold, chi = 0.9) {
    prob_nf <- tidyr::unnest(data, "prob_nf")
    prob_nf <- dplyr::select(
        prob_nf, dplyr::all_of(c("sample_id", "NF", "Index", "Flag", "PChange"))
    )
    prob_nf <- dplyr::group_by(prob_nf, .data[["sample_id"]])
    prob_nf[prob_nf$NF < threshold, "Flag"] <- "0"
    prob_nf <- dplyr::group_modify(prob_nf, ~ {
        # Filter observations to monitoring and remove first dummy data
        valid_idxs <- which(.x$NF >= threshold)[-1] - 1
        for (r in seq_len(length(valid_idxs))) {
            for (t in seq(valid_idxs[r], nrow(.x))) {
                # step 2.1: Update Flag and PChange for current time step (i)
                # (case 1) No confirmed or flagged change:
                if (nrow(.x[t - 1, "Flag"]) > 0 && !is.na(.x[t - 1, "Flag"])[[1]]) {
                    if (.x[t - 1, "Flag"] == "0" || .x[t - 1, "Flag"] == "oldFlag") {
                        i <- 0
                        prior <- .x[t - 1, "NF"]
                        likelihood <- .x[t, "NF"]
                        posterior <- .radd_calc_bayes(prior, likelihood)
                        .x[t, "Flag"] <- "Flag"
                        .x[t, "PChange"] <- posterior
                    }
                    # (case 2) Flagged change at previous time step: update PChange
                    if (.x[t - 1, "Flag"] == "Flag") {
                        prior <- .x[t - 1, "PChange"]
                        likelihood <- .x[t, "NF"]
                        posterior <- .radd_calc_bayes(prior, likelihood)
                        .x[t, "Flag"] <- "Flag"
                        .x[t, "PChange"] <- posterior
                        i <- i + 1
                    }
                }
                # step 2.2: Confirm and reject flagged changes
                if (nrow(.x[t - 1, "Flag"]) > 0 && !is.na(.x[t, "Flag"]) && .x[t, "Flag"] == "Flag") {
                    if ((i > 0)) {
                        if (.x[t, "PChange"] < 0.5) {
                            .x[(t - i):t, "Flag"] <- "0"
                            .x[(t - i), "Flag"] <- "oldFlag"
                            break
                        }
                    }
                }
                # confirm change in case PChange >= chi
                if (nrow(.x[t - 1, "Flag"]) > 0 &&
                    !is.na(.x[t, "PChange"]) &&
                    .x[t, "PChange"] >= chi) {
                    if (.x[t, "NF"] >= threshold) {
                        min_idx <- min(which(.x$Flag == "Flag"))
                        .x[min_idx:t, "Flag"] <- "Change"
                        return(.x)
                    }
                }
            }
        }
        return(.x)
    })
    prob_nf[["#.."]] <- prob_nf[["sample_id"]]
    prob_nf <-  tidyr::nest(
        prob_nf, prob_nf = -"#.."
    )
    data[["prob_nf"]] <- prob_nf[["prob_nf"]]
    data
}

.radd_add_dummy <- function(data) {
    prob_nf <- tidyr::unnest(data, "prob_nf")
    prob_nf <- dplyr::select(
        prob_nf, dplyr::all_of(c("sample_id", "NF", "Index", "Flag", "PChange"))
    )
    prob_nf <- dplyr::group_by(prob_nf, .data[["sample_id"]])
    prob_nf <- dplyr::group_modify(prob_nf, ~ {
        tibble::add_row(
            .data = .x,
            NF = 0.5,
            Index = min(.x$Index) - 1,
            Flag = "0",
            PChange = NA,
            .before = 1
        )
    })
    prob_nf[["#.."]] <- prob_nf[["sample_id"]]
    prob_nf <-  tidyr::nest(
        prob_nf, prob_nf = -"#.."
    )
    data[["prob_nf"]] <- prob_nf[["prob_nf"]]
    data
}

.radd_filter_changes <- function(data, threshold, start_date, end_date) {
    prob_nf <- tidyr::unnest(data, "prob_nf")
    prob_nf <- dplyr::select(
        prob_nf, dplyr::all_of(c("sample_id", "NF", "Index", "Flag", "PChange"))
    )
    data[["sample_id"]] <- unique(prob_nf[["sample_id"]])
    if (!.has(start_date)) {
        start_date <- .ts_start_date(.ts(data))
    }
    if (!.has(end_date)) {
        end_date <- .ts_end_date(.ts(data))
    }
    prob_nf <- dplyr::filter(
        prob_nf, Index >= start_date & Index <= end_date
    )
    prob_nf[["#.."]] <- prob_nf[["sample_id"]]
    prob_nf <- tidyr::nest(
        prob_nf, prob_nf = -"#.."
    )
    data <- data[which(data[["sample_id"]] %in% prob_nf[["#.."]]), ]
    data[["sample_id"]] <- NULL
    data[["prob_nf"]] <- prob_nf[["prob_nf"]]
    data
}

.radd_calc_pnf <- function(data, pdf_fn, stats_layer) {
    samples_labels <- stats_layer[["label"]]
    bands <- .samples_bands(data)
    # We need to calculate for the first to update others
    band <- bands[[1]]
    prob_nf <- .radd_calc_pnf_band(
        data = data,
        pdf_fn = pdf_fn,
        stats_layer = stats_layer,
        band = band,
        labels = samples_labels
    )
    # We need to update de probability of non-forest
    for (b in setdiff(bands, band)) {
        prob_nf <<- .radd_calc_pnf_band(
            data = data,
            pdf_fn = pdf_fn,
            stats_layer = stats_layer,
            band = b,
            labels = samples_labels,
            pnf = prob_nf
        )
    }
    # Add Flag and Pchange columns
    prob_nf[, c("Flag", "PChange")] <- NA
    # Nest each NF probability
    prob_nf[["#.."]] <- prob_nf[["sample_id"]]
    prob_nf <- tidyr::nest(prob_nf, prob_nf = -"#..")
    data$prob_nf <- prob_nf$prob_nf
    # Return the probability of NF updated
    return(data)
}

.radd_calc_pnf_band <- function(data, pdf_fn, stats_layer, band, labels, pnf = NULL) {
    ts_band <- .ts_select_bands(.ts(data), bands = band)
    ts_band <- dplyr::group_by(ts_band, .data[["sample_id"]])
    prob_nf <- dplyr::group_modify(ts_band, ~ {
        # Estimate pdf for each samples labels
        # TODO: remove map and add two vectors
        pdf <- purrr::map_dfc(labels, function(label) {
            label_pdf <- pdf_fn(
                .x[[band]],
                mean = .radd_select_stats(stats_layer, label, band, "mean"),
                sd = .radd_select_stats(stats_layer, label, band, "sd")
            )
            tibble::tibble(label_pdf, .name_repair = ~ label)
        })
        pdf[pdf[["NF"]] < 1e-10000, "NF"] <- 0
        # Calculate conditional probability for NF
        pdf[pdf[["NF"]] > 0, "NF"] <- .radd_calc_prob(
            p1 = pdf[pdf[["NF"]] > 0, "NF"],
            p2 = pdf[pdf[["NF"]] > 0, "F"]
        )
        # Apply body weight function
        pdf <- .radd_apply_bwf(pdf)
        if (.has(pnf)) {
            pnf <- dplyr::filter(pnf, sample_id == .y$sample_id)
            pdf[, "NF"] <- .radd_calc_bayes(pdf[, "NF"], pnf[, "NF"])
        }
        # Return NF conditional probability
        pdf[, "NF"]
    })
    # Add Index column to probability of NF
    prob_nf[["Index"]] <- ts_band[["Index"]]
    prob_nf
}

.radd_create_stats <- function(data) {
    bands <- .samples_bands(data)
    data <- dplyr::group_by(.ts(data), .data[["label"]])
    dplyr::summarise(data, dplyr::across(
        dplyr::matches(bands), list(mean = mean, sd = sd))
    )
}

.radd_calc_prob <- function(p1, p2) {
    p1 / (p1 + p2)
}

.radd_calc_bayes <- function(prior, post){
    return((prior * post) / ((prior * post) + ((1 - prior) * (1 - post))))
}

.radd_apply_bwf <- function(tbl) {
    tbl[tbl[["NF"]] < 0, "NF"] <- 0
    tbl[tbl[["NF"]] > 1, "NF"] <- 1
    tbl
}

.radd_select_stats <- function(stats_layer, label, band, stats) {
    stats_layer <- dplyr::filter(stats_layer, label == !!label)
    band_name <- paste(band, stats, sep = "_")
    .as_dbl(dplyr::select(stats_layer, dplyr::matches(band_name)))
}

.pdf_fun <- function(dist_name) {
    switch(
        dist_name,
        "gaussian" = dnorm,
        "weibull" = dweibull
    )
}

# .radd_calc_pnf <- function(data, pdf_fn, stats_layer) {
#     samples_labels <- stats_layer[["label"]]
#     bands <- .samples_bands(data)
#     # We need to calculate for the first to update others
#     band <- bands[[1]]
#     prob_nf <- .radd_calc_pnf_band(
#         data = data,
#         pdf_fn = pdf_fn,
#         stats_layer = stats_layer,
#         band = band,
#         labels = samples_labels
#     )
#     # We need to update de probability of non-forest
#     for (b in setdiff(bands, band)) {
#         prob_nf <<- .radd_calc_pnf_band(
#             data = data,
#             pdf_fn = pdf_fn,
#             stats_layer = stats_layer,
#             band = b,
#             labels = samples_labels,
#             pnf = prob_nf
#         )
#     }
#     # Add Flag and Pchange columns
#     prob_nf[, c("Flag", "PChange")] <- NA
#     # Nest each NF probability
#     prob_nf[["#.."]] <- prob_nf[["sample_id"]]
#     prob_nf <- tidyr::nest(prob_nf, prob_nf = -"#..")
#     data$prob_nf <- prob_nf$prob_nf
#     # Return the probability of NF updated
#     return(data)
# }
