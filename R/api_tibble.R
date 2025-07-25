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
    sits
}


#' @title Create an empty tibble to store the results of predictions
#' @name .tibble_prediction
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @description Create a tibble to store the results of predictions.
#' @param  data             Tibble with the input data.
#' @param  prediction       Matrix with the result of the classification
#'                          (one class per column and one row per interval).
#' @return                  Tibble storing the predictions.
#'
.tibble_prediction <- function(data, prediction) {
    # get the labels of the data
    labels <- names(prediction)
    n_labels <- length(labels)
    # create a named vector with integers match the class labels
    int_labels <- seq_len(n_labels)
    names(int_labels) <- labels

    # compute prediction vector
    pred_labels <- names(int_labels[max.col(prediction)])

    pred_date <- tibble::tibble(
        from = as.Date(data[["start_date"]]),
        to = as.Date(data[["end_date"]]),
        class = pred_labels
    )
    pred_tbl <- dplyr::bind_cols(pred_date, prediction)
    pred_tbl <- slider::slide(pred_tbl, identity)

    data[["predicted"]] <- pred_tbl

    data
}

#' @title Create an empty tibble to store the results of predictions
#' @name .tibble_prediction_multiyear
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @description Create a tibble to store the results of predictions.
#' @param  data             Tibble with the input data.
#' @param  class_info       Tibble with the information on classification.
#' @param  prediction       Matrix with the result of the classification
#'                          (one class per column and one row per interval).
#' @return                  Tibble storing the predictions.
#'
.tibble_prediction_multiyear <- function(data, class_info, prediction) {
    # retrieve the global timeline
    timeline_global <- class_info[["timeline"]][[1L]]

    # get the labels of the data
    labels <- class_info[["labels"]][[1L]]
    n_labels <- length(labels)
    # create a named vector with integers match the class labels
    int_labels <- seq_len(n_labels)
    names(int_labels) <- labels

    # compute prediction vector
    pred_labels <- names(int_labels[max.col(prediction)])

    slider::slide2_dfr(
        data,
        seq_len(nrow(data)),
        function(row, row_n) {
            # get the timeline of the row
            timeline_row <- lubridate::as_date(
                row[["time_series"]][[1L]][["Index"]]
            )
            # the timeline of the row may differ from the global timeline
            # when we are processing samples with different dates
            if (timeline_row[[1L]] != timeline_global[[1L]]) {
                # what are the reference dates to do the classification?
                ref_dates_lst <- .timeline_match(
                    timeline_data = timeline_row,
                    model_start_date = lubridate::as_date(row[["start_date"]]),
                    model_end_date = lubridate::as_date(row[["end_date"]]),
                    num_samples = nrow(row[["time_series"]][[1L]])
                )
            } else {
                # simplest case - timelines match
                ref_dates_lst <- class_info[["ref_dates"]][[1L]]
            }
            idx_fst <- (row_n - 1L) * (length(ref_dates_lst)) + 1L
            idx_lst <- idx_fst + length(ref_dates_lst) - 1L
            pred_row <- prediction[idx_fst:idx_lst, ]
            if (idx_lst == idx_fst) {
                pred_row <- matrix(
                    pred_row,
                    nrow = 1L,
                    dimnames = list(NULL, colnames(prediction))
                )
            }
            pred_row_lab <- pred_labels[idx_fst:idx_lst]

            # store the classification results
            pred_sample <- purrr::map2_dfr(
                ref_dates_lst,
                seq_len(length(ref_dates_lst)),
                function(rd, idx) {
                    probs_date <- rbind.data.frame(pred_row[idx, ])
                    names(probs_date) <- names(pred_row[idx, ])
                    pred_date <- tibble::tibble(
                        from = as.Date(rd[[1L]]),
                        to = as.Date(rd[[2L]]),
                        class = pred_row_lab[idx]
                    )
                    pred_date <- dplyr::bind_cols(pred_date, probs_date)
                }
            )
            row[["predicted"]] <- list(pred_sample)
            row
        }
    )
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
    start_date <- lubridate::as_date(ref_dates[[1L]])
    # align the dates in the data
    data <- purrr::pmap_dfr(
        list(
            data[["longitude"]],
            data[["latitude"]],
            data[["label"]],
            data[["cube"]],
            data[["time_series"]]
        ),
        function(long, lat, lab, cb, ts) {
            # only rows that match  reference dates are kept
            if (length(ref_dates) == nrow(ts)) {
                # find the date of minimum distance to the reference date
                idx <- which.min(
                    abs((lubridate::as_date(ts[["Index"]])
                    - lubridate::as_date(start_date))
                    / lubridate::ddays(1L))
                )
                # shift the time series to match dates
                if (idx != 1L) ts <- shift_ts(ts, -(idx - 1L))
                # change the dates to the reference dates
                ts1 <- dplyr::mutate(ts, Index = !!ref_dates)
                # save the resulting row in the output tibble
                row <- tibble::tibble(
                    longitude = long,
                    latitude = lat,
                    start_date = lubridate::as_date(ref_dates[[1L]]),
                    end_date = ref_dates[[length(ref_dates)]],
                    label = lab,
                    cube = cb,
                    time_series = list(ts1)
                )
            }
            row
        }
    )
    # set class and return
    .set_class(data, "sits", class(data))
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
    .check_samples_ts(data)
    # get a vector with the number of samples per time series
    n_samples <- purrr::map_int(data[["time_series"]], nrow)

    # check if all time indices are equal to the median
    if (all(n_samples == stats::median(n_samples))) {
        .conf("messages", ".tibble_prune_yes")
        data
    } else {
        .conf("messages", ".tibble_prune_no")
        # return the time series that have the same number of samples
        ind2 <- which(n_samples == stats::median(n_samples))
        data[ind2, ]
    }
}
#' @title Returns a time series
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @name  .tibble_time_series
#' @noRd
#' @param data  a tibble with time series
#' @return  time series
.tibble_time_series <- function(data) {
    data[["time_series"]][[1L]]
}

#' @title Split a sits tibble
#' @name .tibble_samples_split
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @keywords internal
#' @noRd
#' @description Add a column to sits tibble indicating if a sample is
#' training sample or not.
#'
#' @param data  A sits tibble.
#' @return Returns TRUE if data has data.
.tibble_samples_split <- function(samples, validation_split = 0.2) {
    result <-
        samples |>
        dplyr::group_by(.data[["label"]]) |>
        dplyr::mutate(
            train = sample(c(
                rep(TRUE, round(dplyr::n() * (1.0 - !!validation_split))),
                rep(FALSE, round(dplyr::n() * !!validation_split))
            ))
        ) |>
        dplyr::ungroup()
    class(result) <- c("sits", class(result))
    result
}
