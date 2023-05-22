#' @title Create a sits tibble to store the time series information
#' @name .tibble
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function returns an empty sits tibble.
#' Sits tibbles are the main structures of sits package.
#' They contain both the satellite image time series and its metadata.
#' A sits tibble is a tibble with pre-defined columns that
#' has the metadata and data for each time series. The columns are
#' <longitude, latitude, start_date, end_date, label, cube, time_series>.
#' Most functions of sits package get a sits tibble as input
#' (with additional parameters)
#' and return another sits tibble as output.
#' This allows chaining functions over sits tibbles.
#'
#' @return A sits tibble.
#'
.tibble <- function() {
    sits <- tibble::tibble(
        longitude = double(),
        latitude = double(),
        start_date = as.Date(character()),
        end_date = as.Date(character()),
        label = character(),
        cube = character(),
        time_series = list()
    )
    class(sits) <- c("sits", class(sits))
    return(sits)
}
#' @title Create an empty tibble to store the results of predictions
#' @name .tibble_prediction
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Create a tibble to store the results of predictions.
#' @param  data             Tibble with the input data.
#' @param  class_info       Tibble with the information on classification.
#' @param  prediction       Matrix with the result of the classification
#'                          (one class per column and one row per interval).
#' @return                  Tibble storing the predictions.
#'
.tibble_prediction <- function(data, class_info, prediction) {

    # this list is a global one and it is created based on the samples
    ref_dates_lst <- class_info$ref_dates[[1]]
    # retrieve the global timeline
    timeline_global <- class_info$timeline[[1]]

    # get the labels of the data
    labels <- class_info$labels[[1]]
    n_labels <- length(labels)
    # create a named vector with integers match the class labels
    int_labels <- c(1:n_labels)
    names(int_labels) <- labels

    # compute prediction vector
    pred_labels <- names(int_labels[max.col(prediction)])

    data_pred <- slider::slide2_dfr(
        data,
        seq_len(nrow(data)),
        function(row, row_n) {
            # get the timeline of the row
            timeline_row <- lubridate::as_date(row$time_series[[1]]$Index)
            # the timeline of the row may differ from the global timeline
            # when we are processing samples with different dates
            if (timeline_row[1] != timeline_global[1]) {
                # what are the reference dates to do the classification?
                ref_dates_lst <- .timeline_match(
                    timeline_data = timeline_row,
                    model_start_date = lubridate::as_date(row$start_date),
                    model_end_date = lubridate::as_date(row$end_date),
                    num_samples = nrow(row$time_series[[1]])
                )
            }
            idx_fst <- (row_n - 1) * (length(ref_dates_lst)) + 1
            idx_lst <- idx_fst + length(ref_dates_lst) - 1
            pred_row <- prediction[idx_fst:idx_lst, ]
            if (idx_lst == idx_fst)
                pred_row <- matrix(
                    pred_row,
                    nrow = 1,
                    dimnames = list(NULL, colnames(prediction)))
            pred_row_lab <- pred_labels[idx_fst:idx_lst]

            # store the classification results
            pred_sample <- purrr::map2_dfr(
                ref_dates_lst,
                seq_len(length(ref_dates_lst)),
                function(rd, idx) {
                    probs_date <- rbind.data.frame(pred_row[idx, ])
                    names(probs_date) <- names(pred_row[idx, ])
                    pred_date <- tibble::tibble(
                        from = as.Date(rd[1]),
                        to = as.Date(rd[2]),
                        class = pred_row_lab[idx]
                    )
                    pred_date <- dplyr::bind_cols(pred_date, probs_date)
                })
            row$predicted <- list(pred_sample)
            return(row)
        })

    return(data_pred)
}

#' @title Aligns dates of time series to a reference date
#' @name .tibble_align_dates
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Converts the time indexes of a set of sits
#' tibble to a single reference year.
#' This function is useful to join many time series from
#' different years to a single year,
#' which is required by methods that combine many time series,
#' such as clustering methods.
#' The reference year is taken from the date of the start of the time series
#' available in the data cube.
#'
#' @param  data          Input sits tibble (useful for chaining functions).
#' @param  ref_dates     Dates to align the time series.
#' @return               The converted sits tibble
#'
.tibble_align_dates <- function(data, ref_dates) {
    # function to shift a time series in time
    shift_ts <- function(d, k) {
        dplyr::bind_rows(
            utils::tail(d, k),
            utils::head(d, -k)
        )
    }
    # get the reference date
    start_date <- lubridate::as_date(ref_dates[1])
    # align the dates in the data
    data <- purrr::pmap_dfr(
        list(
            data$longitude,
            data$latitude,
            data$label,
            data$cube,
            data$time_series
        ),
        function(long, lat, lab, cb, ts) {
            # only rows that match  reference dates are kept
            if (length(ref_dates) == nrow(ts)) {
                # find the date of minimum distance to the reference date
                idx <- which.min(
                    abs((lubridate::as_date(ts$Index)
                    - lubridate::as_date(start_date))
                    / lubridate::ddays(1))
                )
                # shift the time series to match dates
                if (idx != 1) ts <- shift_ts(ts, - (idx - 1))
                # change the dates to the reference dates
                ts1 <- dplyr::mutate(ts, Index = !!ref_dates)
                # save the resulting row in the output tibble
                row <- tibble::tibble(
                    longitude = long,
                    latitude = lat,
                    start_date = lubridate::as_date(ref_dates[1]),
                    end_date = ref_dates[length(ref_dates)],
                    label = lab,
                    cube = cb,
                    time_series = list(ts1)
                )
            }
            return(row)
        }
    )
    return(data)
}
#'
#' @title Checks that the timeline of all time series of a data set are equal
#' @name .tibble_prune
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function tests if all time series in a sits tibble
#' have the same number of samples, and returns a time series whose indices
#' match the majority of the samples.
#'
#' @param  data  Either a sits tibble or a raster metadata.
#' @return A pruned sits tibble.
#'
.tibble_prune <- function(data) {
    # verify that tibble is correct
    .check_samples(data)

    n_samples <- data$time_series %>%
        purrr::map_int(function(t) {
            nrow(t)
        })

    # check if all time indices are equal to the median
    if (all(n_samples == stats::median(n_samples))) {
        message("Success!! All samples have the same number of time indices")
        return(data)
    } else {
        message("Some samples of time series do not have the same time indices
                as the majority of the data")
        # return the time series that have the same number of samples
        ind2 <- which(n_samples == stats::median(n_samples))
        return(data[ind2, ])
    }
}
#' @title Check that the requested bands exist in the samples
#' @name .tibble_bands_check
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param samples       Time series with the samples
#' @param bands         Requested bands of the data sample
#' @return              Checked bands (cube bands if bands are NULL).
#'
.tibble_bands_check <- function(samples, bands = NULL) {

    # set caller to show in errors
    .check_set_caller(".tibble_bands_check")
    # check the bands are available
    sp_bands <- sits_bands(samples)
    if (purrr::is_null(bands)) {
        bands <- toupper(sp_bands)
    } else {
        bands <- toupper(bands)
        .check_chr_within(
            x = bands,
            within = sp_bands,
            msg = "required bands are not available in the samples"
        )
    }
    return(bands)
}

#' @title Returns a time series
#' @name  .tibble_time_series
#' @noRd
#' @param data  a tibble with time series
#' @return  time series
.tibble_time_series <- function(data) {
    return(data$time_series[[1]])
}

#' @title Split a sits tibble
#' @name .tibble_samples_split
#' @keywords internal
#' @noRd
#' @description Add a column to sits tibble indicating if a sample is
#' training sample or not.
#'
#' @param data  A sits tibble.
#' @return Returns TRUE if data has data.
.tibble_samples_split <- function(samples, validation_split = 0.2) {
    result <-
        samples %>%
        dplyr::group_by(.data[["label"]]) %>%
        dplyr::mutate(
            train = sample(c(
                rep(TRUE, round(dplyr::n() * (1 - !!validation_split))),
                rep(FALSE, round(dplyr::n() * !!validation_split))
            ))
        ) %>%
        dplyr::ungroup()

    return(result)
}


#---- time_series ----

.ts_cols <- c("sample_id", "label")

.is_ts <- function(x) {
    "Index" %in% names(x) && is.data.frame(x)
}

.has_ts <- function(x) {
    "time_series" %in% names(x) && .is_ts(x[["time_series"]][[1]])
}

.ts <- function(x) {
    # Check time_series column
    if (!.has_ts(x)) {
        stop("time_series not found")
    }
    # Add sample_id column
    x[["sample_id"]] <- seq_along(x[["time_series"]])
    # Extract time_series from column
    ts <- tidyr::unnest(
        data = x[c(.ts_cols, "time_series")],
        cols = "time_series"
    )
    # Return time series
    ts
}

`.ts<-` <- function(x, value) {
    if (!.is_ts(value)) {
        stop("invalid time series value")
    }
    # Pack time series
    value <- tidyr::nest(value, time_series = -dplyr::all_of(.ts_cols))
    x <- x[.ts_sample_id(value), ]
    x[["time_series"]] <- value[["time_series"]]
    # Return samples
    x
}

.ts_index <- function(ts) {
    .as_date(ts[["Index"]])
}

.ts_sample_id <- function(ts) {
    ts[["sample_id"]]
}

.ts_bands <- function(ts) {
    setdiff(colnames(ts), c(.ts_cols, "Index"))
}

.ts_select_bands <- function(ts, bands) {
    # Check missing bands
    miss_bands <- bands[!bands %in% .ts_bands(ts)]
    if (.has(miss_bands)) {
        stop("band(s) ", .collapse("'", miss_bands, "'"), " not found")
    }
    # Select the same bands as in the first sample
    ts <- ts[unique(c(.ts_cols, "Index", bands))]
    # Return time series
    ts
}

.ts_start_date <- function(ts) {
    # TODO: create a utility function instead. See .by() function
    .as_date(unlist(unname(tapply(
        as.character(.ts_index(ts)), .ts_sample_id(ts), min, simplify = FALSE
    ))))
}

.ts_min_date <- function(ts) {
    min(.ts_index(ts))
}

.ts_end_date <- function(ts) {
    .as_date(unlist(unname(tapply(
        as.character(.ts_index(ts)), .ts_sample_id(ts), max, simplify = FALSE
    ))))
}

.ts_max_date <- function(ts) {
    max(.ts_index(ts))
}

.ts_filter_interval <- function(ts, start_date, end_date) {
    if (!.has(start_date)) {
        start_date <- .ts_min_date(ts)
    }
    if (!.has(end_date)) {
        end_date <- .ts_max_date(ts)
    }
    # Filter the interval period
    ts <- ts[.ts_index(ts) >= start_date & .ts_index(ts) <= end_date, ]
    # Return time series
    ts
}

.ts_values <- function(ts, bands = NULL) {
    # Get the time series of samples
    bands <- .default(bands, .ts_bands(ts))
    # Check missing bands
    miss_bands <- bands[!bands %in% .ts_bands(ts)]
    if (.has(miss_bands)) {
        stop("band(s) ", .collapse("'", miss_bands, "'"), " not found")
    }
    ts[bands]
}

#---- sits (samples) ----

.sits_ts <- function(samples) {
    # Check time_series column
    if (!.has_ts(samples)) {
        stop("time_series column not found")
    }
    # Return time series of the first sample
    samples[["time_series"]][[1]]
}

.sits_ntimes <- function(samples) {
    # Number of observations of the first sample governs whole samples data
    nrow(.sits_ts(samples))
}

.sits_bands <- function(samples) {
    # Bands of the first sample governs whole samples data
    setdiff(names(.sits_ts(samples)), "Index")
}

.sits_select_bands <- function(samples, bands) {
    # Filter samples
    .ts(samples) <- .ts_select_bands(ts = .ts(samples), bands = bands)
    # Return samples
    samples
}

.sits_filter_interval <- function(samples, start_date, end_date) {
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

.sits_labels <- function(samples) {
    sort(unique(samples[["label"]]), na.last = TRUE)
}

.sits_foreach_ts <- function(samples, fn, ...) {
    # Apply function to each time_series
    samples[["time_series"]] <- lapply(samples[["time_series"]], fn, ...)
    # Return samples
    samples
}

.sits_prune <- function(samples) {
    # Get the time series length for the first sample
    ntimes <- .sits_ntimes(samples)
    # Prune time series according to the first time series length and return
    .sits_foreach_ts(samples, function(ts) {
        if (nrow(ts) >= ntimes) {
            ts[seq_len(ntimes), ]
        } else {
            stop("time series length is smaller than the first sample")
        }
    })
}

.sits_stats <- function(samples) {
    # Get all time series
    preds <- .sits_ts(samples)
    # Select attributes
    preds <- preds[.sits_bands(samples)]
    # Compute stats
    q02 <- apply(preds, 2, stats::quantile, probs = 0.02, na.rm = TRUE)
    q98 <- apply(preds, 2, stats::quantile, probs = 0.98, na.rm = TRUE)
    # Number of observations
    ntimes <- .sits_ntimes(samples)
    # Replicate stats
    q02 <- rep(unname(q02), each = ntimes)
    q98 <- rep(unname(q98), each = ntimes)
    # Return stats object
    list(q02 = q02, q98 = q98)
}

.sits_split <- function(samples, split_intervals) {
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

# ---- Predictors ----

.pred_cols <- c("sample_id", "label")

.predictors <- function(samples, ml_model = NULL) {
    # Prune samples time series
    samples <- .sits_prune(samples)
    # Get samples time series
    pred <- .ts(samples)
    # By default get bands as the same of first sample
    bands <- .sits_bands(samples)
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
    pred <- pred %>%
        dplyr::select("sample_id", "label", dplyr::all_of(bands)) %>%
        dplyr::group_by(.data[["sample_id"]]) %>%
        dplyr::mutate(index = seq_len(dplyr::n())) %>%
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

.part_predictors <- function(part) {
    .default(part[["predictors"]][[1]])
}

# ---- expressions ----

.expr_names <- function(expr) {
    if (is.call(expr)) {
        unique(unlist(lapply(as.list(expr)[-1], .expr_names)))
    } else if (is.name(expr)) {
        .as_chr(expr)
    } else {
        character()
    }
}

.expr_calls <- function(expr) {
    if (is.call(expr)) {
        unique(c(
            paste0(expr[[1]]), unlist(lapply(as.list(expr)[-1], .expr_calls))
        ))
    } else {
        character()
    }
}
