#' @title Merge fraction bands (mixture models)
#' @noRd
#' @param samples Original samples
#' @param values  Values from time series of fraction bands
#' @return merge set of training samples
.samples_merge_fracs <- function(samples, values) {
    # Bind samples time series and fractions columns
    values <- dplyr::bind_cols(.ts(samples), values)
    # Transform time series into a list of time instances
    values <- tidyr::nest(values, time_series = c(-"sample_id", -"label"))
    # Assign the fractions and bands time series to samples
    samples[["time_series"]] <- values[["time_series"]]
    # Return a sits tibble
    samples
}
#' @title Split samples in groups
#' @noRd
#' @param samples Original samples
#' @param multicores  Number of cores
#' @return Split samples by ID
.samples_split_groups <- function(samples, multicores) {
    # Change multicores value in case multicores is greater than samples nrows
    multicores <- if (multicores > nrow(samples)) nrow(samples) else multicores
    # Create a new column to each group id
    samples[["group"]] <- rep(
        seq_len(multicores),
        each = ceiling(nrow(samples) / multicores)
    )[seq_len(nrow(samples))]
    # Split each group by an id
    dplyr::group_split(
        dplyr::group_by(samples, .data[["group"]]),
        .keep = FALSE
    )
}
#' @title Merge samples
#' @noRd
#' @param samples_lst List of samples
#' @return Training samples data.frame
.samples_merge_groups <- function(samples_lst) {
    # Binding the list items into a tibble
    samples <- dplyr::bind_rows(samples_lst)
    # add sits class to the tibble structure
    class(samples) <- c("sits", class(samples))
    # Return sits tibble
    samples
}
#' @title Create partitions of a data set
#' @name  .samples_create_folds
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Alexandre Ywata, \email{alexandre.ywata@@ipea.gov.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Split a sits tibble into k groups, based on the label.
#' @keywords internal
#' @noRd
#' @param data   A sits tibble to be partitioned.
#' @param folds  Number of folds
#'
#' @return A list of row position integers corresponding to the training data.
#'
.samples_create_folds <- function(data, folds = 5) {
    # verify if data exists
    # splits the data into k groups
    data[["folds"]] <- caret::createFolds(data[["label"]],
        k = folds,
        returnTrain = FALSE, list = FALSE
    )
    return(data)
}
#' @title Extract time series from samples
#' @noRd
#' @param samples Data.frame with samples
#' @return Time series for the first sample
.samples_ts <- function(samples) {
    # Check time_series column
    if (!.has_ts(samples)) {
        stop("time_series column not found")
    }
    # Return time series of the first sample
    samples[["time_series"]][[1]]
}
#' @title Get number of temporal intervals in time series samples
#' @noRd
#' @param samples Data.frame with samples
#' @return Number of temporal intervals for the first sample
.samples_ntimes <- function(samples) {
    # Number of observations of the first sample governs whole samples data
    nrow(.samples_ts(samples))
}
#' @title Get bands of time series samples
#' @noRd
#' @param samples Data.frame with samples
#' @return Bands for the first sample
.samples_bands <- function(samples) {
    # Bands of the first sample governs whole samples data
    setdiff(names(.samples_ts(samples)), "Index")
}

#' @title Get timeline of time series samples
#' @noRd
#' @param samples Data.frame with samples
#' @return Timeline of the first sample
.samples_timeline <- function(samples) {
    as.Date(samples[["time_series"]][[1]][["Index"]])
}

#' @title Select bands of time series samples
#' @noRd
#' @param samples Data.frame with samples
#' @param bands   Bands to be selected
#' @return Time series samples with the selected bands
.samples_select_bands <- function(samples, bands) {
    # Filter samples
    .ts(samples) <- .ts_select_bands(ts = .ts(samples), bands = bands)
    # Return samples
    samples
}
#' @title Select time series samples based on a temporal interval
#' @noRd
#' @param samples      Data.frame with samples
#' @param start_date   First date of the interval
#' @param end_date     Last date of the interval
#' @return Time series samples filter by interval
.samples_filter_interval <- function(samples, start_date, end_date) {
    # Filter interval
    .ts(samples) <- .ts_filter_interval(
        ts = .ts(samples), start_date = start_date, end_date = end_date
    )
    # Update start_date and end_date columns with new values
    samples[["start_date"]] <- .ts_start_date(.ts(samples))
    samples[["end_date"]] <- .ts_end_date(.ts(samples))
    # Return samples
    samples
}
#' @title Get labels of time series samples
#' @noRd
#' @param samples Data.frame with samples
#' @return vector with labels
.samples_labels <- function(samples) {
    sort(unique(samples[["label"]]), na.last = TRUE)
}
#' @title Apply function to time series samples
#' @noRd
#' @param samples Data.frame with samples
#' @param fn      Function to be applied to sample
#' @param ...     Additional parameters for function
#' @return samples with applied function
.samples_foreach_ts <- function(samples, fn, ...) {
    # Apply function to each time_series
    samples[["time_series"]] <- lapply(samples[["time_series"]], fn, ...)
    # Return samples
    samples
}
#' @title Prune samples
#' @noRd
#' @param samples Data.frame with samples
#' @return Samples with the same number of temporal intervals as the first
.samples_prune <- function(samples) {
    # Get the time series length for the first sample
    ntimes <- .samples_ntimes(samples)
    # Prune time series according to the first time series length and return
    .samples_foreach_ts(samples, function(ts) {
        if (nrow(ts) >= ntimes) {
            ts[seq_len(ntimes), ]
        } else {
            stop("time series length is smaller than the first sample")
        }
    })
}
#' @title Get sample statistics
#' @noRd
#' @param samples Data.frame with samples
#' @return List of Q02 and Q98 for normalization
.samples_stats <- function(samples) {
    # Get all time series
    preds <- .samples_ts(samples)
    # Select attributes
    preds <- preds[.samples_bands(samples)]
    # Compute stats
    q02 <- apply(preds, 2, stats::quantile, probs = 0.02, na.rm = TRUE)
    q98 <- apply(preds, 2, stats::quantile, probs = 0.98, na.rm = TRUE)
    # Number of observations
    ntimes <- .samples_ntimes(samples)
    # Replicate stats
    q02 <- rep(unname(q02), each = ntimes)
    q98 <- rep(unname(q98), each = ntimes)
    # Return stats object
    list(q02 = q02, q98 = q98)
}
#' @title Split samples
#' @noRd
#' @param samples Data.frame with samples
#' @param split_intervals   Intervals for samples to be split
#' @return Samples split by desired intervals
.samples_split <- function(samples, split_intervals) {
    slider::slide_dfr(samples, function(sample) {
        ts <- sample[["time_series"]][[1]]
        purrr::map_dfr(split_intervals, function(index) {
            new_sample <- sample
            start <- index[[1]]
            end <- index[[2]]
            new_sample[["time_series"]][[1]] <- ts[seq(start, end), ]
            new_sample[["start_date"]] <- ts[["Index"]][[start]]
            new_sample[["end_date"]] <- ts[["Index"]][[end]]
            new_sample
        })
    })
}
