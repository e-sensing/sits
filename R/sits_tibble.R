#' @title Apply a function over sits bands.
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
#' data(point_ndvi)
#' # apply a normalization function
#' point2 <- sits_apply(point_ndvi,
#'     fun = function(x) {
#'         (x - min(x)) / (max(x) - min(x))
#'     }
#' )
#' @export
sits_apply <- function(data,
                       fun,
                       fun_index = function(index) {
                           return(index)
                       },
                       bands_suffix = "",
                       multicores = 1) {

    # backward compatibility
    data <- .sits_tibble_rename(data)

    # verify if data is valid
    .sits_test_tibble(data)

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



#' @title Names of the bands of a time series
#' @name sits_rename
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Set the names of the bands of time series in a sits tibble.
#'
#' @param data      Valid sits tibble.
#' @param names        String vector with the names of the new bands.
#' @return A sits tibble with the new names for the bands.
#' @examples
#' # Retrieve a time series with one band
#' data(point_ndvi)
#' # Rename the band
#' ndvi1.tb <- sits_rename(point_ndvi, names = c("VEG_INDEX"))
#' # print the names of the new band
#' sits_bands(ndvi1.tb)
#' @export
sits_rename <- function(data, names) {
    # backward compatibility
    data <- .sits_tibble_rename(data)
    # verify if the number of bands informed is the same
    # as the actual number of bands in input data
    assertthat::assert_that(length(names) == length(sits_bands(data)),
        msg = "sits_bands: input bands and informed bands have different sizes."
    )
    # bands in SITS are uppercase
    names <- toupper(names)

    # rename and return
    data$time_series <- data$time_series %>%
        purrr::map(function(ts) {
            names(ts) <- c("Index", names)
            return(ts)
        })
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
#'              Parameter frac: a fraction of random samples without replacement.
#'              If frac > 1, no sampling is done.
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

    # backward compatibility
    data <- .sits_tibble_rename(data)

    # verify if data is valid
    .sits_test_tibble(data)

    # verify if either n or frac is informed
    assertthat::assert_that(!(purrr::is_null(n) & purrr::is_null(frac)),
        msg = "sits_sample: neither n or frac parameters informed"
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
    labels <- sits_labels(data)$label
    labels %>%
        purrr::map(function(l) {
            tb_l <- dplyr::filter(data, label == l)
            tb_s <- sampling_fun(tb_l)
            result <<- dplyr::bind_rows(result, tb_s)
        })

    return(result)
}
#' @title Retrieve the dates of time series for a row of a sits tibble
#' @name sits_time_series_dates
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Returns the dates of the time series associated to a sits tibble
#'
#' @param data         A sits tibble with one or more time series.
#' @return             A vector of dates
#' @examples
#' # Retrieve a set of time series with 2 classes
#' data(cerrado_2classes)
#' # Retrieve the dates of the first time series
#' sits_time_series_dates(cerrado_2classes)
#' @export
sits_time_series_dates <- function(data) {
    return(data$time_series[[1]]$Index)
}

#' @title Retrieve time series for a row of a sits tibble
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
    return(data$time_series[[1]])
}

#' @title Return the values of a given sits tibble as a list of matrices.
#' @name sits_values
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description This function returns only the values of a sits tibble
#' (according a specified format).
#' This function is useful to use packages such as ggplot2, dtwclust, or kohonen
#' that require values that are rowwise or colwise organised.
#'
#' @param  data       A sits tibble with time series for different bands.
#' @param  bands      A string with a group of bands whose
#'                    values are to be extracted. If no bands are informed
#'                    extract ALL bands.
#' @param  format     A string with either "cases_dates_bands"
#'                    or "bands_cases_dates" or "bands_dates_cases".
#'
#' @return A sits tibble with values.
#' @examples
#' # Retrieve a set of time series with 2 classes
#' data(cerrado_2classes)
#' # retrieve the values split by bands
#' sits_values(cerrado_2classes[1:2, ], format = "bands_dates_cases")
#' @export
sits_values <- function(data, bands = NULL, format = "cases_dates_bands") {
    assertthat::assert_that(format == "cases_dates_bands" ||
        format == "bands_cases_dates" ||
        format == "bands_dates_cases",
    msg = "sits_values: valid format parameter are
             'cases_dates_bands', 'bands_cases_dates', or 'bands_dates_cases'"
    )

    # backward compatibility
    data <- .sits_tibble_rename(data)

    if (purrr::is_null(bands)) {
          bands <- sits_bands(data)
      }

    # equivalent to former sits_values_rows()
    # used in sits_cluster input data
    # list elements: bands, matrix's rows: cases, matrix's cols: dates
    if (format == "cases_dates_bands") {
        # populates result
        values <- data$time_series %>%
            purrr::map(function(ts) {
                data.matrix(dplyr::select(ts, dplyr::one_of(bands)))
            })

        # another kind of sits_values_rows()
        # used in sits_kohonen input
        # list elements: bands, matrix's rows: cases, matrix's cols: dates
    } else if (format == "bands_cases_dates") {
        values <- bands %>% purrr::map(function(band) {
            data$time_series %>%
                purrr::map(function(ts) {
                    dplyr::select(ts, dplyr::one_of(band))
                }) %>%
                data.frame() %>%
                tibble::as_tibble() %>%
                as.matrix() %>%
                t()
        })

        names(values) <- bands
        # equivalent to former sits_values_cols()
        # list elements: bands, matrix's rows: dates, matrix's cols: cases
    } else if (format == "bands_dates_cases") {
        values <- bands %>% purrr::map(function(band) {
            data$time_series %>%
                purrr::map(function(ts) {
                    dplyr::select(ts, dplyr::one_of(band))
                }) %>%
                data.frame() %>%
                tibble::as_tibble() %>%
                as.matrix()
        })

        names(values) <- bands
    }

    return(values)
}
