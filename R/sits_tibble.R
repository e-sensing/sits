#' @title Get the time series for a row of a sits tibble
#' @name sits_time_series
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Returns the time series associated to a row of the a sits tibble
#'
#' @param data     A sits tibble with one or more time series.
#' @return A tibble in sits format with the time series.
#' @export
sits_time_series <- function(data) {
    .sits_tibble_test(data)

    return(data$time_series[[1]])
}
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
#' @name .sits_tibble_align_dates
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
.sits_tibble_align_dates <- function(data, ref_dates) {
    # verify that tibble is correct
    .sits_tibble_test(data)
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
                idx <- which.min(abs((lubridate::as_date(ts$Index)
                                      - lubridate::as_date(start_date)) / lubridate::ddays(1)))
                # shift the time series to match dates
                if (idx != 1) ts <- shift_ts(ts, -(idx - 1))
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
    return(data)
}
#'
#' @title Checks that the timeline of all time series of a data set are equal
#' @name .sits_tibble_prune
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
.sits_tibble_prune <- function(data) {
    # verify that tibble is correct
    .sits_tibble_test(data)

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
#' @name .sits_tibble_bands_check
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param samples       Time series with the samples
#' @param bands         Requested bands of the data sample
#' @return              Checked bands (cube bands if bands are NULL).
#'
.sits_tibble_bands_check <- function(samples, bands = NULL) {

    # set caller to show in errors
    .check_set_caller(".sits_tibble_bands_check")
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

#' @title Tests if a sits tibble is valid
#' @name .sits_tibble_test
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Tests if a sits tibble exists or has data inside.
#'
#' @param data  A sits tibble.
#' @return Returns TRUE if data has data.
.sits_tibble_test <- function(data) {

    # set caller to show in errors
    .check_set_caller(".sits_tibble_test")
    .check_null(x = data, "invalid data parameter")
    .check_num(
        x = nrow(data),
        min = 1, msg = "invalid number of rows"
    )
    .check_chr_contains(
        x = colnames(data),
        contains = .config_get("sits_tibble_cols"),
        discriminator = "all_of",
        msg = "Data is not a valid sits tibble"
    )
    return(TRUE)
}
#' @title Apply a function to one band of a time series
#' @name .sits_fast_apply
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param  data      Tibble.
#' @param  col       Column where function should be applied
#' @param  fn        Function to be applied.
#' @return           Tibble where function has been applied.
#'
.sits_fast_apply <- function(data, col, fn, ...) {

    # pre-condition
    .check_chr_within(col,
                      within = names(data),
                      msg = "invalid column name"
    )
    # select data do unpack
    x <- data[col]
    # prepare to unpack
    x[["#.."]] <- seq_len(nrow(data))
    # unpack
    x <- tidyr::unnest(x, cols = dplyr::all_of(col))
    x <- dplyr::group_by(x, .data[["#.."]])
    # apply user function
    x <- fn(x, ...)
    # pack
    x <- dplyr::ungroup(x)
    x <- tidyr::nest(x, `..unnest_col` = -dplyr::any_of("#.."))
    # remove garbage
    x[["#.."]] <- NULL
    names(x) <- col
    # prepare result
    data[[col]] <- x[[col]]
    return(data)
}

#' @keywords internal
.sits_rename_bands <- function(x, bands) {
    UseMethod(".sits_rename_bands", x)
}

#' @export
.sits_rename_bands.sits <- function(x, bands) {
    data_bands <- sits_bands(x)

    # pre-condition
    .check_chr(bands,
               allow_empty = FALSE, len_min = length(data_bands),
               len_max = length(data_bands),
               msg = "invalid 'bands' value"
    )

    .sits_fast_apply(x, col = "time_series", fn = function(x) {

        # create a conversor
        new_bands <- colnames(x)
        names(new_bands) <- new_bands

        # rename
        new_bands[data_bands] <- toupper(bands)
        colnames(x) <- unname(new_bands)

        return(x)
    })
}

#' @export
.sits_rename_bands.raster_cube <- function(x, bands) {
    data_bands <- sits_bands(x)
    # pre-condition
    .check_chr(bands,
               allow_empty = FALSE,
               len_min = length(data_bands),
               len_max = length(data_bands),
               msg = "invalid 'bands' value"
    )
    .sits_fast_apply(x, col = "file_info", fn = function(x) {
        x <- tidyr::pivot_wider(x,
                                names_from = "band",
                                values_from = "path"
        )

        # create a conversor
        new_bands <- colnames(x)
        names(new_bands) <- new_bands

        # rename
        new_bands[data_bands] <- toupper(bands)
        colnames(x) <- unname(new_bands)

        x <- tidyr::pivot_longer(x,
                                 cols = toupper(bands),
                                 names_to = "band",
                                 values_to = "path"
        )

        return(x)
    })
}
#' @title Split a sits tibble
#' @name .sits_samples_split
#' @keywords internal
#'
#' @description Add a column to sits tibble indicating if a sample is
#' training sample or not.
#'
#' @param data  A sits tibble.
#' @return Returns TRUE if data has data.
.sits_samples_split <- function(samples, validation_split = 0.2) {

    result <-
        samples %>%
        dplyr::group_by(.data[["label"]]) %>%
        dplyr::mutate(
            train = sample(
                c(rep(TRUE, round(dplyr::n() * (1 - validation_split))),
                  rep(FALSE, round(dplyr::n() * validation_split)))
            )
        ) %>%
        dplyr::ungroup()

    return(result)
}
