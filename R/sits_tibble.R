#' @title Apply a function on a set of time series
#' @name sits_apply
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description Apply a 1D generic function to a time series
#'  and specific methods for common tasks,
#'  such as missing values removal and smoothing.
#' `sits_apply()` returns a sits tibble with the same samples
#'  and new bands computed by `fun`, `fun_index` functions.
#'  These functions must be defined inline; they are called by `sits_apply`
#'  for each band, whose vector values is passed as the function argument.
#'  The `fun` function may either return a vector or a list of vectors.
#'  In the first case, the vector will be the new values
#'  of the corresponding band.
#'  In the second case, the returned list must have names,
#'  and each element vector will generate a new band which name composed
#'  by concatenating original band name and the corresponding list element name.
#'
#'  If a suffix is provided in `bands_suffix`, all resulting band
#'  names will end with provided suffix separated by a ".".
#'
#' @param data          Valid sits tibble
#' @param fun           Function with one parameter as input
#'                      and a vector or list of vectors as output.
#' @param fun_index     Function with one parameter as input
#'                      and a Date vector as output.
#' @param bands_suffix  String informing the suffix of the resulting bands.
#' @param multicores    Number of cores to be used
#' @return A sits tibble with same samples and the new bands.
#' @examples
#' # Get a time series
#' point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
#' # apply a normalization function
#' point2 <- sits_apply(point_ndvi,
#'     fun = function(x) {
#'         (x - min(x)) / (max(x) - min(x))
#'     }
#' )
#'
#'
#' @export
sits_apply <- function(data,
                       fun,
                       fun_index = function(index) {
                           return(index)
                       },
                       bands_suffix = "",
                       multicores = 1) {

    # verify if data is valid
    .sits_tibble_test(data)

    # computes fun and fun_index for all time series
    data$time_series <- data$time_series %>%
        purrr::map(function(ts) {
            ts_computed_lst <- dplyr::select(ts, -Index) %>%
                purrr::map(fun)

            # append bands names' suffixes
            if (nchar(bands_suffix) != 0) {
                  names(ts_computed_lst) <- paste0(
                      names(ts_computed_lst), ".",
                      bands_suffix
                  )
              }

            # unlist if there are more than one result from `fun`
            if (is.recursive(ts_computed_lst[[1]])) {
                  ts_computed_lst <- unlist(ts_computed_lst, recursive = FALSE)
              }

            # convert to tibble
            ts_computed <- tibble::as_tibble(ts_computed_lst)

            # compute Index column
            ts_computed <- dplyr::mutate(ts_computed,
                Index = fun_index(ts$Index)
            )

            # reorganizes time series tibble
            return(dplyr::select(ts_computed, Index, dplyr::everything()))
        })
    return(data)
}

#' @title Add new sits bands.
#' @name sits_mutate_bands
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description Adds new bands and preserves existing in the time series
#'              of a sits tibble using \code{dplyr::mutate} function.
#' @param data       Valid sits tibble.
#' @param ...        Expressions written as `name = value`.
#'                   See \code{dplyr::mutate()} help for more details.
#' @return           A sits tibble with same samples and the selected bands.
#' @examples
#' \donttest{
#' # Retrieve data for time series with label samples in Mato Grosso in Brazil
#' data(samples_modis_4bands)
#' # Generate a new image with the SAVI (Soil-adjusted vegetation index)
#' ndwi.tb <- sits_mutate_bands(samples_modis_4bands,
#'            NDWI = (1.5 * (NIR - MIR) / (NIR + MIR)))
#' }
#' @export
sits_mutate_bands <- function(data, ...) {

    # verify if data has values
    .sits_tibble_test(data)

    # compute mutate for each time_series tibble
    proc_fun <- function(...) {
        data$time_series <- data$time_series %>%
            purrr::map(function(ts) {
                ts_computed <- ts %>%
                    dplyr::mutate(...)
                return(ts_computed)
            })
    }

    # compute mutate for each time_series tibble
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



#' @title Sample a percentage of a time series
#' @name sits_sample
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Takes a sits tibble with different labels and
#'              returns a new tibble. For a given field as a group criterion,
#'              this new tibble contains a given number or percentage
#'              of the total number of samples per group.
#'              Parameter n: number of random samples with replacement.
#'              Parameter frac: a fraction of random samples without
#'              replacement. If frac > 1, no sampling is done.
#'
#' @param  data       Input sits tibble.
#' @param  n          Number of samples to pick from each group of data.
#' @param  frac       Percentage of samples to pick from each group of data.
#' @return            A sits tibble with a fixed quantity of samples.
#' @examples
#' # Retrieve a set of time series with 2 classes
#' data(cerrado_2classes)
#' # Print the labels of the resulting tibble
#' sits_labels(cerrado_2classes)
#' # Samples the data set
#' data <- sits_sample(cerrado_2classes, n = 10)
#' # Print the labels of the resulting tibble
#' sits_labels(data)
#' @export
sits_sample <- function(data, n = NULL, frac = NULL) {

  # set caller to show in errors
  .check_set_caller("sits_sample")

    # verify if data is valid
    .sits_tibble_test(data)

    # verify if either n or frac is informed
    .check_that(
        x = !(purrr::is_null(n) & purrr::is_null(frac)),
        msg = "neither n or frac parameters informed"
    )

    # prepare sampling function
    sampling_fun <- if (!purrr::is_null(n)) {
          function(tb) {
              if (nrow(tb) >= n) {
                  return(dplyr::sample_n(tb,
                      size = n,
                      replace = FALSE
                  ))
              } else {
                  return(tb)
              }
          }
      } else if (frac <= 1) {
          function(tb) tb %>% dplyr::sample_frac(size = frac, replace = FALSE)
      } else {
          function(tb) tb %>% dplyr::sample_frac(size = frac, replace = TRUE)
      }

    # compute sampling
    result <- .sits_tibble()
    labels <- sits_labels(data)
    result <- purrr::map_dfr(labels,
        function(l) {
            tb_l <- dplyr::filter(data, label == l)
            tb_s <- sampling_fun(tb_l)
        })

    return(result)
}

#' @title Get the time series for a row of a sits tibble
#' @name sits_time_series
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Returns the time series associated to a row of the a sits tibble
#'
#' @param data     A sits tibble with one or more time series.
#' @return A tibble in sits format with the time series.
#' @examples
#' # Retrieve a set of time series with 2 classes
#' data(cerrado_2classes)
#' # Retrieve the first time series
#' sits_time_series(cerrado_2classes)
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
#' @title Rename a tibble to use "cube" instead of "coverage"
#' @name .sits_tibble_rename
#'
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param data   A sits tibble.
#'
.sits_tibble_rename <- function(data) {

  # is the input data a valid sits tibble?
  if ("coverage" %in% names(data)) {

    data <- dplyr::rename(data, cube = "coverage")
  }

  return(data)
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
    }
    else {
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
#' @return              Checked bands (cube bands if bands are NULL)
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

.sits_fast_apply <- function(data, col, fn, ...) {

    # pre-condition
    .check_chr_within(col, within = names(data),
                      msg = "invalid column name")

    # select data do unpack
    x <- data[col]

    # prepare to unpack
    x[["#.."]] <- seq_len(nrow(data))

    # unpack
    x <- tidyr::unnest(x, cols = col)
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

.sits_rename_bands <- function(x, bands) {

    data_bands <- sits_bands(x)

    # pre-condition
    .check_chr(bands, allow_empty = FALSE, len_min = length(data_bands),
               len_max = length(data_bands),
               msg = "invalid 'bands' value")

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
