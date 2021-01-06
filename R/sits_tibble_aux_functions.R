#' @title Create a sits tibble to store the time series information
#' @name .sits_tibble
#' @keywords internal
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
.sits_tibble <- function() {
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
#' @title Aligns dates of time series to a reference date
#' @name .sits_align_dates
#' @keywords internal
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
.sits_align_dates <- function(data, ref_dates) {
    # backward compatibility
    data <- .sits_tibble_rename(data)
    # verify that tibble is correct
    .sits_test_tibble(data)
    # function to shift a time series in time
    shift_ts <- function(d, k) {
        dplyr::bind_rows(
            utils::tail(d, k),
            utils::head(d, -k)
        )
    }

    # get the reference date
    start_date <- lubridate::as_date(ref_dates[1])
    # create an output tibble
    data1 <- .sits_tibble()

    rows <- purrr::pmap(
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
                idx <- which.min(abs((lubridate::as_date(ts$Index)
                - lubridate::as_date(start_date)) / lubridate::ddays(1)))
                # shift the time series to match dates
                if (idx != 1) ts <- shift_ts(ts, - (idx - 1))
                # change the dates to the reference dates
                ts1 <- dplyr::mutate(ts, Index = ref_dates)

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

    # solve issue when a names list is returned
    if (!is.null(names(rows))) rows <- unname(rows)

    data1 <- dplyr::bind_rows(data1, rows)
    return(data1)
}




#' @title Checks that the timeline of all time series of a data set are equal
#' @name .sits_prune
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function tests if all time series in a sits tibble
#' have the same number of samples, and returns a time series whose indices
#' match the majority of the samples.
#'
#' @param  data  Either a sits tibble or a raster metadata.
#' @return A pruned sits tibble.
#'
.sits_prune <- function(data) {

    # backward compatibility
    data <- .sits_tibble_rename(data)

    .sits_test_tibble(data)

    # create a vector to store the number of indices per time series
    n_samples <- vector()

    data$time_series %>%
        purrr::map(function(t) {
            n_samples[length(n_samples) + 1] <<- NROW(t)
        })

    # check if all time indices are equal to the median
    if (all(n_samples == stats::median(n_samples))) {
        message("Success!! All samples have the same number of time indices")
        return(data)
    }
    else {
        message("Some samples of time series do not have the same time indices
                as the majority of the data")

        # return the time series that have the same number of samples
        ind2 <- which(n_samples == stats::median(n_samples))

        return(data[ind2, ])
    }
}
#' @title Add new sits bands and drops existing.
#' @name .sits_transmute_bands
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Adds new bands and drops existing in the time series
#'               of a sits tibble using dplyr::transmute function.
#' @param data          A sits tibble.
#' @param ...           Pair expressions in the format `name = value`.
#'                      See \code{\link[dplyr]{mutate}} help for more details.
#' @return A sits tibble with same samples and the new bands.
#'
#'
.sits_transmute_bands <- function(data, ...) {
    # backward compatibility
    data <- .sits_tibble_rename(data)

    # verify if data is valid
    .sits_test_tibble(data)

    # tricky to include "Index" column and expand `...` arguments
    proc_fun <- function(..., Index = Index) {
        Index <- quote(Index)
        purrr::map(data$time_series, function(ts) {
            ts_computed <- dplyr::transmute(ts, !!(Index), ...)
            return(ts_computed)
        })
    }

    # compute transmute for each time_series tibble
    tryCatch({
            data$time_series <- proc_fun(...)
        },
        error = function(e) {
            msg <- paste0("Error - Are your band names all uppercase?")
            message(msg)
        }
    )
    return(data)
}

#' @title Create partitions of a data set
#' @name  .sits_create_folds
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Alexandre Ywata, \email{alexandre.ywata@@ipea.gov.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Split a sits tibble into k groups, based on the label.
#'
#' @param data   A sits tibble to be partitioned.
#' @param folds     Number of folds.
.sits_create_folds <- function(data, folds = 5) {
    # verify if data exists
    .sits_test_tibble(data)

    # splits the data into k groups
    data$folds <- caret::createFolds(data$label,
        k = folds,
        returnTrain = FALSE, list = FALSE
    )

    return(data)
}

#' @title Check that the requested bands exist in the samples
#' @name .sits_samples_bands_check
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param samples       Time series with the samples
#' @param bands         Requested bands of the data sample
#' @return              Checked bands (cube bands if bands are NULL)
#'
.sits_samples_bands_check <- function(samples, bands = NULL) {
    # check the bands are available
    sp_bands <- sits_bands(samples)
    if (purrr::is_null(bands)) {
          bands <- toupper(sp_bands)
      } else {
        bands <- toupper(bands)
        assertthat::assert_that(all(bands %in% sp_bands),
            msg = "required bands are not available in the samples"
        )
    }
    return(bands)
}

#' @title Tests if a sits tibble is valid
#' @name .sits_test_tibble
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Tests if a sits tibble exists or has data inside.
#'
#' @param data  A sits tibble.
#' @return Returns TRUE if data has data.
.sits_test_tibble <- function(data) {
    assertthat::assert_that(!purrr::is_null(data),
        msg = "input data not provided"
    )
    assertthat::assert_that(NROW(data) > 0,
        msg = "input data is empty"
    )

    names <- c(
        "longitude", "latitude", "start_date", "end_date",
        "label", "cube", "time_series"
    )

    assertthat::assert_that(all(names %in% colnames(data)),
        msg = "data input is not a valid sits tibble"
    )

    return(TRUE)
}

#' @title Rename a tibble to use "cube" instead of "coverage"
#' @name .sits_tibble_rename
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param data   A sits tibble.
.sits_tibble_rename <- function(data) {
    # is the input data a valid sits tibble?
    if ("coverage" %in% names(data)) {
          data <- data %>% dplyr::rename(cube = coverage)
      }

    return(data)
}
