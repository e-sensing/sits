utils::globalVariables(c(
    "time_series",
    "masking_result",
    "mask_timeseries_random",
    "mask_timeseries_contiguous"
))
#' @title Apply time series masking to sits samples
#'
#' @description
#' Internal helper to apply timestep masking to every element of the
#' \code{time_series} list-column in a sits tibble. The masking strategy
#' can be random, contiguous, or mixed (random/contiguous chosen per call).
#'
#' This function returns the original \code{samples} with two additional
#' list-columns: \code{time_series_masked} containing the masked time
#' series, and \code{mask_vector} containing a binary mask vector aligned
#' with timesteps (1 = kept/visible, 0 = masked). These outputs are
#' typically used by MAE-style pretraining routines.
#'
#' @param samples A sits tibble with a \code{time_series} list-column.
#'   Each element must be a time series object accepted by the selected
#'   masking function (see Details).
#' @param mask_ratio A numeric value in (0, 1), specifying the fraction of
#'   timesteps to mask. Higher values mask more timesteps.
#' @param method A character string specifying the masking method. Options
#'   are \code{"random"}, \code{"contiguous"}, or \code{"mixed"}.
#' @param mask_value A numeric value specifying the fill value used for
#'   masked timesteps. Default is \code{0}.
#' @param bands A character vector specifying which bands will be masked.
#'   If \code{NULL} (default), all bands are masked.
#'
#' @details
#' The masking implementation is delegated to one of:
#' \itemize{
#'   \item \code{.mae_mask_ts_random}: masks a random subset of timesteps.
#'   \item \code{.mae_mask_ts_contiguous}: masks a contiguous block of
#'     timesteps.
#'   \item \code{method = "mixed"}: selects random or contiguous masking
#'     with probability 0.5 for the whole call.
#' }
#'
#' Each masking function must return a list with at least:
#' \itemize{
#'   \item \code{masked_ts}: the masked version of the input time series.
#'   \item \code{mask_vector}: a binary vector aligned with timesteps.
#' }
#'
#' @return The input \code{samples} tibble with two new columns:
#' \code{time_series_masked} (masked time series) and \code{mask_vector}
#' (binary mask vector per sample).
#'
#' @author Alexandre Assuncao \email{alexcarssuncao@@gmail.com}
#'
#' @keywords internal
#' @noRd
.mae_mask_samples <- function(samples,
                              mask_ratio = 0.5,
                              method = "random",
                              mask_value = 0,
                              bands = NULL) {
    time_series <- NULL
    masking_result <- NULL

    # Choose masking function
    masking_fun <- switch(method,
        random = .mae_mask_ts_random,
        contiguous = .mae_mask_ts_contiguous,
        mixed = if (runif(1) <= 0.5) {
            .mae_mask_ts_random
        } else {
            .mae_mask_ts_contiguous
        }
    )

    samples <- samples |>
        dplyr::mutate(
            masking_result = purrr::map(
                time_series,
                ~ masking_fun(.x,
                    mask_ratio = mask_ratio,
                    mask_value = mask_value,
                    bands      = bands
                )
            ),
            time_series_masked = purrr::map(masking_result, "masked_ts"),
            mask_vector = purrr::map(masking_result, "mask_vector")
        ) |>
        dplyr::select(-masking_result)

    return(samples)
}
#' @title Random time step masking for a single time series
#'
#' @description
#' Internal helper that applies random, scattered masking to a single
#' time series. A fraction of timesteps, defined by \code{mask_ratio}, is
#' sampled uniformly at random, and the selected timesteps are masked by
#' replacing their values with \code{mask_value}.
#'
#' Masking is applied simultaneously across all selected bands at each
#' chosen timestep. The function is designed to support MAE-style
#' pretraining workflows, where temporal sparsity is introduced without
#' enforcing temporal contiguity.
#'
#' @param ts A tibble representing a single time series. The first column
#'   must correspond to time (typically a date), followed by one or more
#'   numeric band columns.
#' @param mask_ratio A numeric value in (0, 1) specifying the fraction of
#'   timesteps to mask. The number of masked timesteps is computed as
#'   \code{ceiling(mask_ratio * nrow(ts))}.
#' @param mask_value A numeric value used to replace masked observations.
#'   Default is \code{0}.
#' @param bands A character vector specifying which band columns should be
#'   masked. If \code{NULL} (default), all band columns are masked.
#'
#' @details
#' Timesteps are selected without replacement using uniform random
#' sampling. If \code{bands} is provided, only the intersection between
#' \code{bands} and the band columns present in \code{ts} is masked.
#'
#' The returned mask vector is aligned with the rows of \code{ts} and
#' indicates which timesteps were masked.
#'
#' @return A list with two elements:
#' \itemize{
#'   \item \code{masked_ts}: A tibble with the same structure as \code{ts},
#'     where selected timesteps and bands have been replaced by
#'     \code{mask_value}.
#'   \item \code{mask_vector}: An integer vector of length equal to the
#'     number of timesteps, with \code{1} indicating masked timesteps and
#'     \code{0} indicating unmasked ones.
#' }
#'
#' @author Alexandre Assuncao \email{alexcarssuncao@@gmail.com}
#'
#' @keywords internal
#' @noRd
.mae_mask_ts_random <- function(ts,
                                mask_ratio = 0.5,
                                mask_value = 0,
                                bands = NULL) {
    n_time_steps <- nrow(ts)
    n_masked <- ceiling(mask_ratio * n_time_steps)

    ts_bands <- colnames(ts)[-1]

    if (is.null(bands)) {
        masked_bands <- ts_bands
    } else {
        masked_bands <- intersect(bands, ts_bands)
    }

    mask_idx <- sample(seq_len(n_time_steps), n_masked, replace = FALSE)

    masked_ts <- ts
    masked_ts[mask_idx, masked_bands] <- mask_value

    mask_vector <- rep(0, n_time_steps)
    mask_vector[mask_idx] <- 1

    list(
        masked_ts = masked_ts,
        mask_vector = mask_vector
    )
}

#' @title Contiguous block masking for a single time series
#'
#' @description
#' Internal helper that applies contiguous block masking to a single time
#' series. A single continuous block of timesteps is selected at random,
#' with block length determined by \code{mask_ratio}, and all selected
#' timesteps are masked by replacing their values with \code{mask_value}.
#'
#' Masking is applied simultaneously across all selected bands at each
#' timestep in the block. This strategy preserves local temporal structure
#' outside the masked region and is commonly used in MAE-style temporal
#' pretraining to encourage reconstruction from surrounding context.
#'
#' @param ts A tibble representing a single time series. The first column
#' must correspond to time (typically a date), followed by one or more
#' numeric band columns.
#' @param mask_ratio A numeric value in (0, 1) specifying the fraction of
#' timesteps to mask. The number of masked timesteps is computed as
#' \code{ceiling(mask_ratio * nrow(ts))}.
#' @param mask_value A numeric value used to replace masked observations.
#' Default is \code{0}.
#' @param bands A character vector specifying which band columns should be
#' masked. If \code{NULL} (default), all band columns are masked.
#'
#' @details
#' A contiguous block of timesteps is selected by uniformly sampling a
#' valid starting index such that the full block fits within the time
#' series length. If the requested \code{mask_ratio} is too large to allow
#' a contiguous block, the function signals an error.
#'
#' If \code{bands} is provided, only the intersection between \code{bands}
#' and the band columns present in \code{ts} is masked.
#'
#' The returned mask vector is aligned with the rows of \code{ts} and
#' indicates which timesteps belong to the masked block.
#'
#' @return A list with two elements:
#' \itemize{
#' \item \code{masked_ts}: A tibble with the same structure as \code{ts},
#' where a contiguous block of timesteps and selected bands has been
#' replaced by \code{mask_value}.
#' \item \code{mask_vector}: An integer vector of length equal to the
#' number of timesteps, with \code{1} indicating masked timesteps and
#' \code{0} indicating unmasked ones.
#' }
#'
#' @author Alexandre Assuncao \email{alexcarssuncao@@gmail.com}
#'
#' @keywords internal
#' @noRd
.mae_mask_ts_contiguous <- function(ts,
                                    mask_ratio = 0.5,
                                    mask_value = 0,
                                    bands = NULL) {
    n_time_steps <- nrow(ts)
    n_masked <- ceiling(mask_ratio * n_time_steps)

    max_start <- n_time_steps - n_masked + 1
    if (max_start < 1) {
        stop(.conf("messages", ".mae_mask_ratio_too_high"))
    }

    block_start <- sample(seq_len(max_start), 1)
    block_end <- block_start + n_masked - 1

    ts_bands <- colnames(ts)[-1]

    if (is.null(bands)) {
        masked_bands <- ts_bands
    } else {
        masked_bands <- intersect(bands, ts_bands)
    }

    masked_ts <- ts
    masked_ts[block_start:block_end, bands] <- mask_value

    mask_vector <- rep(0, n_time_steps)
    mask_vector[block_start:block_end] <- 1

    list(
        masked_ts = masked_ts,
        mask_vector = mask_vector
    )
}
#' @title Split and prepare masked training and validation datasets
#' @name .mae_data_split_train_val
#'
#' @description
#' Internal helper to prepare masked autoencoder (MAE) inputs from SITS
#' samples. The function applies a masking strategy to each sample time
#' series, computes per-band normalization parameters from the original
#' samples, splits the masked samples into training and validation
#' partitions, and converts inputs, targets, and masks into dense arrays
#' suitable for torch training.
#'
#' The returned inputs (\code{train_x}, \code{val_x}) contain masked time
#' series, while the targets (\code{train_y}, \code{val_y}) contain the
#' original (unmasked) time series. Both are normalized using quantile
#' statistics derived from the original samples.
#'
#' @param samples A \code{tibble} containing SITS samples with a
#' \code{time_series} list-column.
#' @param mask_ratio A \code{numeric} in (0, 1) indicating the fraction of
#' timesteps to mask.
#' @param masking_method A \code{character} string indicating the masking
#' strategy passed to \code{.mae_mask_samples}. Typical values include
#' \code{"random"}, \code{"contiguous"}, or \code{"mixed"}.
#' @param mask_value A numeric value used to replace masked observations.
#' @param bands A \code{character} vector specifying which bands will be
#' masked. If \code{NULL}, all available bands are masked.
#' @param validation_split A \code{numeric} in (0, 1) indicating the
#' fraction of samples assigned to the validation set.
#'
#' @details
#' The procedure follows these steps:
#' \enumerate{
#' \item Apply the masking strategy to create \code{time_series_masked}
#' and \code{mask_vector} per sample.
#' \item Compute normalization statistics (\code{q02}, \code{q98}) from
#' the original \code{samples} and reduce them to one value per band.
#' \item Normalize inputs and targets by per-band min-max scaling using
#' \code{q02} and \code{q98}, with clipping to [0, 1].
#' \item Randomly split samples into training and validation indices.
#' \item Pack the normalized matrices into arrays with shape
#' \code{[n_samples, n_times, n_bands]} and masks into
#' \code{[n_samples, n_times, 1]}.
#' }
#'
#' The quantiles returned (\code{q02}, \code{q98}) are intended to support
#' later denormalization or consistent scaling across training steps.
#'
#' @return A \code{list} with the following named elements:
#' \describe{
#' \item{train_x}{Masked time series for training, as an array of shape
#' \code{[n_train, n_times, n_bands]}.}
#' \item{val_x}{Masked time series for validation, same shape as
#' \code{train_x}.}
#' \item{train_y}{Original time series targets for training, array of
#' shape \code{[n_train, n_times, n_bands]}.}
#' \item{val_y}{Original time series targets for validation, array of
#' shape \code{[n_val, n_times, n_bands]}.}
#' \item{train_mask}{Binary mask for training inputs, array of shape
#' \code{[n_train, n_times, 1]}, where \code{1} indicates masked
#' timesteps and \code{0} indicates visible timesteps.}
#' \item{val_mask}{Binary mask for validation inputs, array of shape
#' \code{[n_val, n_times, 1]}.}
#' \item{q02}{Numeric vector of lower quantiles used in normalization,
#' typically one value per band-time entry as produced by
#' \code{.samples_stats}.}
#' \item{q98}{Numeric vector of upper quantiles used in normalization,
#' typically one value per band-time entry as produced by
#' \code{.samples_stats}.}
#' }
#'
#' @author Alexandre Assuncao \email{alexcarssuncao@@gmail.com}
#' @keywords internal
#' @noRd
.mae_data_split_train_val <- function(samples,
                                      mask_ratio,
                                      masking_method,
                                      mask_value,
                                      masked_bands,
                                      validation_split) {
    bands <- .samples_bands(samples)
    n_bands <- length(bands)
    n_times <- .samples_ntimes(samples)

    # Apply mask to samples
    masked_samples <- .mae_mask_samples(
        samples    = samples,
        mask_ratio = mask_ratio,
        method     = masking_method,
        mask_value = mask_value,
        bands      = masked_bands
    )

    # Data normalization
    ml_stats <- .samples_stats(samples)
    q02 <- as.numeric(ml_stats$q02)
    q98 <- as.numeric(ml_stats$q98)

    q02_band <- q02[seq(1, length(q02), by = n_times)]
    q98_band <- q98[seq(1, length(q98), by = n_times)]

    # Sanity check:
    stopifnot(length(q02_band) == n_bands)
    stopifnot(length(q98_band) == n_bands)

    # Normalize that uses one value per column
    normalize <- function(mat) {
        # mat is (n_samples*n_times) × n_bands
        centered <- sweep(mat, 2, q02_band, `-`) # mat - q02
        scaled <- sweep(centered, 2, q98_band - q02_band, `/`) # / (q98 - q02)
        pmax(pmin(scaled, 1), 0)
    }

    # Train/test split
    all_idx <- seq_len(nrow(masked_samples))
    n_val <- floor(length(all_idx) * validation_split)
    val_idx <- sample(all_idx, size = n_val)
    train_idx <- setdiff(all_idx, val_idx)

    train_samples <- masked_samples[train_idx, , drop = FALSE]
    val_samples <- masked_samples[val_idx, , drop = FALSE]
    train_targets <- samples[train_idx, , drop = FALSE]
    val_targets <- samples[val_idx, , drop = FALSE]

    n_samples_train <- length(train_idx)
    n_samples_val <- length(val_idx)

    # Helper: convert ts -> matrix without index
    strip_index <- function(ts) {
        as.matrix(ts[, -1, drop = FALSE])
    }

    train_x_mat <- do.call(
        rbind,
        lapply(train_samples$time_series_masked, strip_index)
    )
    train_y_mat <- do.call(
        rbind,
        lapply(train_targets$time_series, strip_index)
    )

    val_x_mat <- do.call(
        rbind,
        lapply(val_samples$time_series_masked, strip_index)
    )
    val_y_mat <- do.call(
        rbind,
        lapply(val_targets$time_series, strip_index)
    )

    pack <- function(mat, ns, nb, nt) {
        # mat: (ns * nt) × nb
        stopifnot(nrow(mat) == ns * nt)
        stopifnot(ncol(mat) == nb)

        # Reshape each band separately and stack them
        arr <- array(NA_real_, dim = c(ns, nt, nb))
        for (b in seq_len(nb)) {
            # Extract column for band b
            full_band_vec <- mat[, b]
            # Reshape as [samples × time]
            arr[, , b] <- matrix(
                full_band_vec,
                nrow = ns,
                ncol = nt,
                byrow = TRUE
            )
        }
        arr
    }

    train_x <- pack(
        mat = normalize(train_x_mat),
        ns = n_samples_train,
        nb = n_bands,
        nt = n_times
    )
    train_y <- pack(
        mat = normalize(train_y_mat),
        ns = n_samples_train,
        nb = n_bands,
        nt = n_times
    )
    val_x <- pack(
        mat = normalize(val_x_mat),
        ns = n_samples_val,
        nb = n_bands,
        nt = n_times
    )
    val_y <- pack(
        mat = normalize(val_y_mat),
        ns = n_samples_val,
        nb = n_bands,
        nt = n_times
    )

    # Mask vectors
    train_mask <- array(
        data = do.call(rbind, lapply(train_samples$mask_vector, as.numeric)),
        dim  = c(n_samples_train, n_times, 1)
    )

    val_mask <- array(
        data = do.call(rbind, lapply(val_samples$mask_vector, as.numeric)),
        dim  = c(n_samples_val, n_times, 1)
    )

    return(list(
        train_x = train_x,
        val_x = val_x,
        train_y = train_y,
        val_y = val_y,
        train_mask = train_mask,
        val_mask = val_mask,
        q02 = q02,
        q98 = q98
    ))
}
#' @title Masked Autoencoder Time Series Dataset
#' @name .mae_dataset
#'
#' @description
#' Internal Torch dataset for masked autoencoder (MAE) training on time
#' series. Each item returns a masked input tensor and a target structure
#' containing the original (unmasked) time series and the associated mask.
#'
#' The dataset is intended for self-supervised reconstruction: the model
#' receives \code{x} (masked input) and learns to reconstruct the original
#' signal from \code{y$y}, using \code{y$mask} to identify masked positions
#' for loss computation.
#'
#' @param masked A numeric 3D array of shape
#' \code{[n_samples, n_times, n_bands]} containing masked time series.
#' @param original A numeric 3D array of shape
#' \code{[n_samples, n_times, n_bands]} containing the corresponding
#' original (unmasked) time series.
#' @param mask A numeric 3D array of shape \code{[n_samples, n_times, 1]}
#' containing binary indicators for masked positions, where \code{1}
#' indicates masked and \code{0} indicates visible.
#'
#' @details
#' The dataset stores the provided arrays and converts slices to
#' \code{torch_tensor}s on demand in \code{.getitem}. Returned tensors are
#' reshaped to remove the leading singleton dimension introduced by
#' \code{drop = FALSE}, resulting in:
#' \itemize{
#' \item \code{x}: \code{[n_times, n_bands]}
#' \item \code{y$y}: \code{[n_times, n_bands]}
#' \item \code{y$mask}: \code{[n_times, 1]}
#' }
#'
#' The nested target structure (\code{y = list(y = ..., mask = ...)}) is
#' designed to work with dataloaders and training loops where the loss
#' function expects both the reconstruction target and the mask.
#'
#' @return A Torch dataset object compatible with \code{torch::dataloader}.
#' Calling \code{.getitem(i)} returns a list with:
#' \describe{
#' \item{x}{Masked input tensor of shape \code{[n_times, n_bands]}.}
#' \item{y}{A list with two tensors: \code{y} (original time series,
#' \code{[n_times, n_bands]}) and \code{mask} (binary mask,
#' \code{[n_times, 1]}).}
#' }
#'
#' @author Alexandre Assuncao \email{alexcarssuncao@@gmail.com}
#'
#' @keywords internal
#' @noRd
.mae_dataset <- torch::dataset(
    name = ".MaeUnlabelledDataset",
    initialize = function(masked, original, mask) {
        self$masked <- masked
        self$original <- original
        self$mask <- mask
    },
    .getitem = function(i) {
        x <- torch::torch_tensor(
            self$masked[i, , , drop = FALSE],
            dtype = torch::torch_float()
        )$view(
            c(dim(self$masked)[2], dim(self$masked)[3])
        )
        y <- list(
            y = torch::torch_tensor(
                self$original[i, , , drop = FALSE],
                dtype = torch::torch_float()
            )$view(
                c(dim(self$original)[2], dim(self$original)[3])
            ),
            mask = torch::torch_tensor(
                self$mask[i, , , drop = FALSE],
                dtype = torch::torch_float()
            )$view(c(dim(self$mask)[2], dim(self$mask)[3]))
        )
        list(x = x, y = y)
    },
    .length = function() {
        dim(self$masked)[1]
    }
)
