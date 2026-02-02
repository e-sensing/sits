utils::globalVariables(c(
    "time_series",
    "masking_result",
    "mask_timeseries_random",
    "mask_timeseries_contiguous"
))
#' @title Apply time series masking to sits samples
#'
#' @author Alexandre Assuncao \email{alexcarssuncao@@gmail.com}
#'
#' Internal function to apply masking to all samples in a sits tibble.
#' Supports different masking methods (e.g., random, contiguous).
#'
#' @param samples A sits tibble with a time_series list-column.
#' @param mask_ratio A numeric value in (0,1), specifying the fraction of timesteps to mask.
#' @param method A character string specifying the masking method.
#'   Options are \code{"random"}, \code{"contiguous"}, or \code{"mixed"}.
#' @param mask_value A numeric value specifying the values of masked samples. Default is 0.
#' @param bands      A character vector specifying which bands will be masked. In the default case, bands = NULL,
#'                   all bands are masked.
#'
#' @return The input samples tibble with two new columns:
#'   \code{time_series_masked} (masked time series) and
#'   \code{mask_vector} (binary mask vector per sample).
#'
#' @keywords internal
#' @importFrom dplyr mutate select
#' @importFrom purrr map
.mae_mask_samples <- function(samples,
                              mask_ratio = 0.5,
                              method     = "random",
                              mask_value = 0,
                              bands = NULL) {

    time_series    <- NULL
    masking_result <- NULL

    # Choose masking function
    masking_fun <- switch(
        method,
        random     = .mae_mask_ts_random,
        contiguous = .mae_mask_ts_contiguous,
        mixed      = if (runif(1) <= 0.5) {
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
                              bands      = bands)
            ),
            time_series_masked = purrr::map(masking_result, "masked_ts"),
            mask_vector        = purrr::map(masking_result, "mask_vector")
        ) |>
        dplyr::select(-masking_result)

    return(samples)
}


#' @title Random time step masking for a single time series
#'
#' @author Alexandre Assuncao \email{alexcarssuncao@@gmail.com}
#'
#' @description Internal function that applies random scattered masking to a single time series.
#'              A given fraction of time steps is selected randomly and all bands at those time
#'              steps are set to zero.
#'
#' @param ts A tibble representing a single time series (with a date column followed by band columns).
#' @param mask_ratio A numeric value in (0,1), specifying the fraction of timesteps to mask.
#' @param mask_value A numeric value specifying the values of masked samples. Default is 0.
#' @param bands      A character vector specifying which bands will be masked. In the default case, bands = NULL,
#'                   all bands are masked.
#'
#' @return A list with two elements:
#'   \itemize{
#'     \item \code{masked_ts}: The masked time series (tibble).
#'     \item \code{mask_vector}: A binary vector indicating masked timesteps (1 = masked, 0 = unmasked).
#'   }
#'
#' @keywords internal
.mae_mask_ts_random <- function(ts,
                                mask_ratio = 0.5,
                                mask_value = 0,
                                bands = NULL) {

    n_time_steps <- nrow(ts)
    n_masked     <- ceiling(mask_ratio * n_time_steps)

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
        masked_ts  = masked_ts,
        mask_vector = mask_vector
    )
}

#' @title Contiguous block masking for a single time series
#'
#' @author Alexandre Assuncao \email{alexcarssuncao@@gmail.com}
#'
#' @description Internal function that applies contiguous block masking to a single time series.
#'              A single continuous block of time steps is randomly selected and all bands at those
#'              time steps are set to zero.
#'
#' @param ts A tibble representing a single time series (with a date column followed by band columns).
#' @param mask_ratio A numeric value in (0,1), specifying the fraction of timesteps to mask.
#' @param mask_value A numeric value specifying the values of masked samples.
#' @param bands      A character vector specifying which bands will be masked. In the default case, bands = NULL,
#'                   all bands are masked.
#'
#' @return A list with two elements:
#'   \itemize{
#'     \item \code{masked_ts}: The masked time series (tibble).
#'     \item \code{mask_vector}: A binary vector indicating masked timesteps (1 = masked, 0 = unmasked).
#'   }
#'
#' @keywords internal
.mae_mask_ts_contiguous <- function(ts,
                                    mask_ratio = 0.5,
                                    mask_value = 0,
                                    bands = NULL) {

    n_time_steps <- nrow(ts)
    n_masked     <- ceiling(mask_ratio * n_time_steps)

    max_start <- n_time_steps - n_masked + 1
    if (max_start < 1) {
        stop("Mask ratio too high for the length of the time series.")
    }

    block_start <- sample(seq_len(max_start), 1)
    block_end   <- block_start + n_masked - 1

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
        masked_ts  = masked_ts,
        mask_vector = mask_vector
    )
}


#' @title Split and prepare masked training and validation datasets
#' @name .mae_data_split_train_val
#'
#' @description
#' Internal helper function that takes a set of SITS samples and:
#' 1. Applies a masking strategy to each time series;
#' 2. Normalizes the resulting samples;
#' 3. Randomly splits them into training and validation sets;
#' 4. Converts masked time series, targets, and masks into torch-ready arrays.
#'kll
#' This function is used to prepare data for masked autoencoder training,
#' where the input consists of masked time series, and the target is the original,
#' unmasked version.
#'
#' @param samples A `tibble` containing SITS samples with `time_series` columns.
#' @param mask_ratio A `numeric` between 0 and 1 indicating the fraction of timesteps to mask.
#' @param masking_method A `character` string indicating the masking strategy (e.g., "random", "contiguous").
#' @param mask_value A numeric value specifying the values of masked samples.
#' @param bands      A `character` vector specifying which bands will be masked. In the default case, bands = NULL,
#'                   all bands are masked.
#' @param validation_split A `numeric` between 0 and 1 indicating the fraction of samples used for validation.
#'
#' @return A `list` with the following named elements:
#' \describe{
#'   \item{train_x}{Masked time series for training (array of shape `[n_train, n_times, n_bands]`).}
#'   \item{val_x}{Masked time series for validation (same shape as train_x).}
#'   \item{train_y}{Original time series (targets) for training.}
#'   \item{val_y}{Original time series (targets) for validation.}
#'   \item{train_mask}{Binary mask for training inputs (1 = masked, 0 = visible).}
#'   \item{val_mask}{Binary mask for validation inputs.}
#'   \item{q02}{Quantile stat for denormalization.}
#'   \item{q98}{Quantile stat for denormalization.}
#' }
#'
#' @keywords internal
#' @noRd
.mae_data_split_train_val <- function(samples, mask_ratio, masking_method, mask_value, masked_bands, validation_split) {

    bands   <- .samples_bands(samples)
    n_bands <- length(bands)
    n_times <- .samples_ntimes(samples)

    # 1) Apply mask to samples
    masked_samples <- .mae_mask_samples(
        samples    = samples,
        mask_ratio = mask_ratio,
        method     = masking_method,
        mask_value = mask_value,
        bands      = masked_bands
    )

    # Data normalization
    ml_stats <- .samples_stats(samples)
    q02      <- as.numeric(ml_stats$q02)
    q98      <- as.numeric(ml_stats$q98)

    q02_band <- q02[seq(1, length(q02), by = n_times)]
    q98_band <- q98[seq(1, length(q98), by = n_times)]

    # Sanity check:
    stopifnot(length(q02_band) == n_bands)
    stopifnot(length(q98_band) == n_bands)

    # Normalize that uses one value per column
    normalize <- function(mat) {
        # mat is (n_samples*n_times) × n_bands
        centered <- sweep(mat, 2, q02_band, `-`)                  # mat - q02
        scaled   <- sweep(centered, 2, q98_band - q02_band, `/`)  # / (q98 - q02)
        pmax(pmin(scaled, 1), 0)
    }

    # Train/test split
    all_idx     <- seq_len(nrow(masked_samples))
    n_val       <- floor(length(all_idx) * validation_split)
    val_idx     <- sample(all_idx, size = n_val)
    train_idx   <- setdiff(all_idx, val_idx)

    train_samples <- masked_samples[train_idx, , drop = FALSE]
    val_samples   <- masked_samples[val_idx, , drop = FALSE]
    train_targets <- samples[train_idx, , drop = FALSE]
    val_targets   <- samples[val_idx, , drop = FALSE]

    n_samples_train <- length(train_idx)
    n_samples_val   <- length(val_idx)

    # Helper: convert ts -> matrix without index
    strip_index <- function(ts) {
        as.matrix(ts[, -1, drop = FALSE])
    }

    train_x_mat <- do.call(rbind, lapply(train_samples$time_series_masked, strip_index))
    train_y_mat <- do.call(rbind, lapply(train_targets$time_series,        strip_index))

    val_x_mat   <- do.call(rbind, lapply(val_samples$time_series_masked,   strip_index))
    val_y_mat   <- do.call(rbind, lapply(val_targets$time_series,          strip_index))

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
            arr[,,b] <- matrix(full_band_vec, nrow = ns, ncol = nt, byrow = TRUE)
        }
        arr
    }

    train_x       <- pack(normalize(train_x_mat), ns = n_samples_train, nb = n_bands, nt = n_times)
    train_y       <- pack(normalize(train_y_mat), ns = n_samples_train, nb = n_bands, nt = n_times)
    val_x         <- pack(normalize(val_x_mat),   ns = n_samples_val,   nb = n_bands, nt = n_times)
    val_y         <- pack(normalize(val_y_mat),   ns = n_samples_val,   nb = n_bands, nt = n_times)

    # ---- Mask vectors ----
    train_mask <- array(
        data = do.call(rbind, lapply(train_samples$mask_vector, as.numeric)),
        dim  = c(n_samples_train, n_times, 1)
    )

    val_mask <- array(
        data = do.call(rbind, lapply(val_samples$mask_vector, as.numeric)),
        dim  = c(n_samples_val, n_times, 1)
    )

    return(list(
        train_x    = train_x,
        val_x      = val_x,
        train_y    = train_y,
        val_y      = val_y,
        train_mask = train_mask,
        val_mask   = val_mask,
        q02 = q02,
        q98 = q98
    ))
}



#' @title Masked Autoencoder Time Series Dataset
#' @name .mae_dataset
#'
#' @description
#' A custom Torch dataset for training masked autoencoders on time series data.
#' It returns, for each index:
#' - the masked input time series (`x`)
#' - the original unmasked time series (`y`)
#' - a binary mask indicating which positions were masked (`mask`)
#'
#' This dataset is designed for use with self-supervised learning workflows,
#' where the model learns to reconstruct the masked portions of the time series.
#'
#' @param masked A 3D array of shape `[n_samples, n_times, n_bands]` with masked time series.
#' @param original A 3D array of shape `[n_samples, n_times, n_bands]` with original unmasked time series.
#' @param mask A 3D array of shape `[n_samples, n_times, 1]` with binary indicators for masked positions.
#'
#' @return A torch dataset object with `.getitem(i)` returning a list with:
#' - `x`: masked input `[n_times, n_bands]`
#' - `y`: target full input `[n_times, n_bands]`
#' - `mask`: binary mask `[n_times, 1]`
#'
#' @examples
#' \dontrun{
#' ds <- .mae_dataset(masked_array, original_array, mask_array)
#' sample <- ds$.getitem(1)
#' str(sample$x)  # input
#' str(sample$y)  # target
#' str(sample$mask)  # mask positions
#' }
#'
#' @keywords internal
#' @noRd
.mae_dataset <- torch::dataset(
    name = ".MaeUnlabelledDataset",
    initialize = function(masked, original, mask) {
        self$masked   <- masked
        self$original <- original
        self$mask     <- mask
    },

    .getitem = function(i) {
        x <- torch::torch_tensor(self$masked[i, , , drop = FALSE],   dtype = torch::torch_float())$view(c(dim(self$masked)[2],   dim(self$masked)[3]))
        y <- list(y = torch::torch_tensor(self$original[i, , , drop = FALSE], dtype = torch::torch_float())$view(c(dim(self$original)[2], dim(self$original)[3])),
                  mask = torch::torch_tensor(self$mask[i, , , drop = FALSE],  dtype = torch::torch_float())$view(c(dim(self$mask)[2],     dim(self$mask)[3])))
        list(x = x, y = y)
    },

    .length = function() {
        dim(self$masked)[1]
    }
)

