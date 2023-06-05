# ---- Predictors ----

.pred_cols <- c("sample_id", "label")

.predictors <- function(samples, ml_model = NULL) {
    # Prune samples time series
    samples <- .samples_prune(samples)
    # Get samples time series
    pred <- .ts(samples)
    # By default get bands as the same of first sample
    bands <- .samples_bands(samples)
    # Preprocess time series
    if (.has(ml_model)) {
        # If a model is informed, get predictors from model bands
        bands <- .ml_bands(ml_model)
        # Normalize values for old version model classifiers that
        #   do not normalize values itself
        # Models trained after version 1.2 do this automatically before
        #   classification
        stats <- .ml_stats_0(ml_model) # works for old models only!!
        if (.has(stats)) {
            # Read and preprocess values of each band
            pred[bands] <- purrr::imap_dfc(pred[bands], function(values, band) {
                # Get old stats parameters
                q02 <- .stats_0_q02(stats, band)
                q98 <- .stats_0_q98(stats, band)
                if (.has(q02) && .has(q98)) {
                    # Use C_normalize_data_0 to process old version of
                    #   normalization
                    values <- C_normalize_data_0(
                        data = as.matrix(values), min = q02, max = q98
                    )
                    # Convert from matrix to vector and return
                    unlist(values)
                }
                # Return updated values
                values
            })
        }
    }
    # Create predictors...
    pred <- pred[c(.pred_cols, bands)]
    # Add sequence 'index' column grouped by 'sample_id'
    pred <- pred |>
        dplyr::select("sample_id", "label", dplyr::all_of(bands)) |>
        dplyr::group_by(.data[["sample_id"]]) |>
        dplyr::mutate(index = seq_len(dplyr::n())) |>
        dplyr::ungroup()
    # Rearrange data to create predictors
    pred <- tidyr::pivot_wider(
        data = pred, names_from = "index", values_from = dplyr::all_of(bands),
        names_prefix = if (length(bands) == 1) bands else "",
        names_sep = ""
    )
    # Return predictors
    pred
}

.pred_features <- function(pred) {
    if (all(.pred_cols %in% names(pred))) {
        pred[, -2:0]
    } else {
        pred
    }
}

`.pred_features<-` <- function(pred, value) {
    if (all(.pred_cols %in% names(pred))) {
        pred[, seq_len(ncol(pred) - 2) + 2] <- value
    } else {
        pred[, ] <- value
    }
    pred
}

.pred_references <- function(pred) {
    if (all(.pred_cols %in% names(pred))) .as_chr(pred[["label"]]) else NULL
}

.pred_normalize <- function(pred, stats) {
    values <- as.matrix(.pred_features(pred))
    values <- C_normalize_data(
        data = values, min = .stats_q02(stats), max = .stats_q98(stats)
    )
    .pred_features(pred) <- values
    # Return predictors
    pred
}

.pred_create_partition <- function(pred, partitions) {
    pred[["part_id"]] <- .partitions(x = seq_len(nrow(pred)), n = partitions)
    tidyr::nest(pred, predictors = -"part_id")
}

.pred_sample <- function(pred, frac) {
    pred <- dplyr::group_by(pred, .data[["label"]])
    dplyr::slice_sample(pred, prop = frac)
}
# ---- Partitions ----

.pred_part <- function(part) {
    .default(part[["predictors"]][[1]])
}
