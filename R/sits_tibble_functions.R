#' @title Aligns dates of time series to a reference date
#' @name sits_align
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Converts the time indexes of a set of sits tibble to a single reference year.
#' This function is useful to join many time series from different years to a single year,
#' which is required by methods that combine many time series, such as clustering methods.
#' The reference year is taken from the date of the start of the time series
#' available in the coverage.
#'
#' @param  data.tb       Input sits tibble (useful for chaining functions).
#' @param  ref_dates     Dates to align the time series.
#' @return A tibble with the converted sits tibble (useful for chaining functions).
#' @export
sits_align <- function(data.tb, ref_dates) {
    # function to shift a time series in time
    shift_ts <- function(d, k) dplyr::bind_rows(utils::tail(d,k), utils::head(d,-k))

    # get the reference date
    start_date <- lubridate::as_date(ref_dates[1])
    # create an output tibble
    data1.tb <- sits_tibble()

    purrr::pmap(list(data.tb$longitude, data.tb$latitude,
                     data.tb$label, data.tb$coverage, data.tb$time_series),
                function(long, lat, lab, cov, ts) {

                    # only rows that match the number of reference dates are kept
                    if (length(ref_dates) == nrow(ts)) {
                        # in what direction do we need to shift the time series?
                        sense <- lubridate::yday(lubridate::as_date(ts[1,]$Index)) - lubridate::yday(lubridate::as_date(start_date))
                        # find the date of minimum distance to the reference date
                        idx <- which.min(abs((lubridate::as_date(ts$Index) - lubridate::as_date(start_date))/lubridate::ddays(1)))
                        # do we shift time up or down?
                        if (sense < 0) shift <- -(idx - 1) else shift <- (idx - 1)
                        # shift the time series to match dates
                        if (idx != 1) ts <- shift_ts(ts, -(idx - 1))
                        # convert the time index to a reference year
                        first_date <- lubridate::as_date(ts[1,]$Index)
                        # change the dates to the reference dates
                        ts1 <- dplyr::mutate(ts, Index = ref_dates)

                        # save the resulting row in the output tibble
                        row <- tibble::tibble(longitude   = long,
                                              latitude    = lat,
                                              start_date  = lubridate::as_date(ref_dates[1]),
                                              end_date    = ref_dates[length(ref_dates)],
                                              label       = lab,
                                              coverage    = cov,
                                              time_series = list(ts1))
                    }
                    data1.tb <<- dplyr::bind_rows(data1.tb, row)
                })
    return(data1.tb)
}

#' @title Apply a function over sits bands.
#' @name sits_apply
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description Apply a 1D generic function to a time series and specific methods for
#' common tasks such as missing values removal and smoothing.
#' `sits_apply()` returns a sits tibble with the same samples points and new bands computed by `fun`,
#' `fun_index` functions. These functions must be defined inline and are called by `sits_apply` for each band,
#' whose vector values is passed as the function argument.
#' The `fun` function may either return a vector or a list of vectors. In the first case, the vector will be the new values
#' of the corresponding band. In the second case, the returned list must have names, and each element vector will
#' generate a new band which name composed by concatenating original band name and the corresponding list element name.
#'
#' If a suffix is provided in `bands_suffix`, all resulting bands names will end with provided suffix separated by a ".".
#'
#' @param data          Valid sits tibble or matrix.
#' @param fun           Function with one parameter as input and a vector or list of vectors as output.
#' @param fun_index     Function with one parameter as input and a Date vector as output.
#' @param bands_suffix  String informing the suffix of the resulting bands.
#' @return A sits tibble with same samples and the new bands.
#' @examples
#' # Get a time series
#' data(point_ndvi)
#' # apply a normalization function
#' point2 <- sits_apply (point_ndvi, fun = function (x) { (x - min (x))/(max(x) - min(x))} )
#' @export
sits_apply <- function(data, fun, fun_index = function(index){ return(index) }, bands_suffix = "") {
    if ("tbl" %in% class(data)) {
        # verify if data.tb has values
        .sits_test_tibble(data)

        # computes fun and fun_index for all time series and substitutes the original time series data
        data$time_series <- data$time_series %>%
            purrr::map(function(ts.tb) {
                ts_computed.lst <- dplyr::select(ts.tb, -Index) %>%
                    purrr::map(fun)

                # append bands names' suffixes
                if (nchar(bands_suffix) != 0)
                    names(ts_computed.lst) <- paste0(names(ts_computed.lst), ".", bands_suffix)

                # unlist if there are more than one result from `fun`
                if (is.recursive(ts_computed.lst[[1]]))
                    ts_computed.lst <- unlist(ts_computed.lst, recursive = FALSE)

                # convert to tibble
                ts_computed.tb <- tibble::as_tibble(ts_computed.lst)

                # compute Index column
                ts_computed.tb <- dplyr::mutate(ts_computed.tb, Index = fun_index(ts.tb$Index))

                # reorganizes time series tibble
                return(dplyr::select(ts_computed.tb, Index, dplyr::everything()))
            })
        return(data)
    }
    else if ("matrix" %in% class(data)) {
        multicores <- max(parallel::detectCores(logical = FALSE) - 1, 1)
        # auxiliary function to filter a block of data
        filter_block <- function(mat) {
            rows_block.lst <- lapply(seq_along(mat), function(i) fun(mat[i,]))
            mat_block.mx <- do.call(rbind, rows_block.lst)
        }
        chunk.lst <- .sits_split_data(data, multicores)
        rows.lst  <- parallel::mclapply(chunk.lst, filter_block, mc.cores = multicores)
        data <- do.call(rbind, rows.lst)

        return(data)
    }
}

#' @title Informs the names of the bands of a time series
#' @name sits_bands
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description  Finds the names of the bands of time series in a sits tibble
#'               or sets the names of the bands if a set of values is given.
#'
#' @param data.tb      Valid sits tibble.
#' @return A string vector with the names of the bands.
#'
#' @examples
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' data(samples_mt_9classes)
#' # print the bands
#' sits_bands(samples_mt_9classes)
#' @export
sits_bands <- function(data.tb) {
    result.vec <- data.tb[1,]$time_series[[1]] %>%
        colnames() %>% .[2:length(.)]
    return(result.vec)
}

#' @title Breaks a set of time series into equal intervals
#' @name sits_break
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function breaks a set of time series
#' into equal time intervals. This function is useful to produce
#' a set of time series with the same number of samples, which is
#' required for building a set of samples for classification.
#'
#' @param  data.tb    A sits tibble.
#' @param  timeline   Timeline associated with the coverage.
#' @param  start_date Starting date within an interval.
#' @param  end_date   Ending date within an interval.
#' @param  interval   Interval for breaking the series.
#' @return A sits tibble broken into equal intervals.
#' @examples
#' points.tb <- sits_break(point_ndvi, timeline_modis_392, "2000-08-28", "2016-08-12")
#' @export
sits_break <- function(data.tb, timeline, start_date, end_date, interval = "12 month"){
    # create a tibble to store the results
    newdata.tb <- sits_tibble()

    # get the dates
    subset_dates.lst <- sits_match_timeline(timeline, as.Date(start_date), as.Date(end_date), interval = interval)

    # break the data into intervals
    lapply(seq_len(nrow(data.tb)), function(i) {
        subset_dates.lst %>%
            purrr::map(function(date){
                point.tb <- .sits_extract(data.tb[i,], as.Date(date[1]), as.Date(date[2]))
                if (nrow(point.tb) > 0)
                    newdata.tb <<- dplyr::bind_rows(newdata.tb, point.tb)
            })

    })
    # prune the results to get the same number of samples
    newdata.tb <- sits_prune(newdata.tb)

    return(newdata.tb)
}

#' @title Return the dates of a sits tibble
#' @name sits_dates
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Returns a vector containing the dates of a sits tibble.
#'
#' @param  data.tb  A tibble in sits format with time series for different bands.
#' @return A tibble with values of time indexes.
#' @examples
#' # get a point and print its dates
#' sits_dates(point_mt_6bands)
#' @export
sits_dates <- function(data.tb) {
    values <- data.tb$time_series[[1]]$Index
    return(values)
}

#' @title Merge two satellite image time series
#' @name sits_merge
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function merges the time series of two sits tibbles.
#' To merge two series, we consider that they contain different
#' attributes but refer to the same coverage, and spatio-temporal location.
#' This function is useful to merge different bands of the same spatio-temporal locations.
#' For example, one may want to put the raw and smoothed bands for the same set of locations
#' in the same tibble.
#'
#' @param data1.tb      The first sits tibble to be merged.
#' @param data2.tb      The second sits tibble to be merged.
#' @return A merged sits tibble with a nested set of time series.
#' @examples
#' \donttest{
#' #' # Retrieve a time series with values of NDVI
#' data(point_ndvi)
#' # Filter the point using the whittaker smoother
#' point_ws.tb <- sits_whittaker(point_ndvi, lambda = 3.0)
#' # Plot the two points to see the smoothing effect
#' sits_plot(sits_merge(point_ndvi, point_ws.tb))
#' }
#' @export
sits_merge <-  function(data1.tb, data2.tb) {
    # are the names of the bands different?
    ensurer::ensure_that(data1.tb, !(any(sits_bands(.) %in% sits_bands(data2.tb)) | any(sits_bands(data2.tb) %in% sits_bands(.))),
                         err_desc = "sits_merge: cannot merge two sits tibbles with bands with the same names")

    # if some parameter is empty returns the another one
    if (NROW(data1.tb) == 0)
        return(data2.tb)
    if (NROW(data2.tb) == 0)
        return(data1.tb)

    # verify if data1.tb and data2.tb has the same number of rows
    ensurer::ensure_that(data1.tb, NROW(.) == NROW(data2.tb),
                         err_desc = "sits_merge: cannot merge two sits tibbles with different numbers of rows")

    # prepare result
    result.tb <- data1.tb

    # merge time series
    result.tb$time_series <- purrr::map2(data1.tb$time_series, data2.tb$time_series, function(ts1.tb, ts2.tb) {
        ts3.tb <- dplyr::bind_cols(ts1.tb, dplyr::select(ts2.tb, -Index))
        return(ts3.tb)
    })
    return(result.tb)
}

#' @title Add new sits bands.
#' @name sits_mutate_bands
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description Adds new bands and preserves existing in the time series of a sits tibble using \code{dplyr::mutate} function.
#' @param data.tb       Valid sits tibble.
#' @param ...           Expressions written as `name = value`. See \code{dplyr::mutate()} help for more details.
#' @examples
#' \donttest{
#' # Retrieve data for time series with label samples in Mato Grosso in Brazil
#' data (samples_mt_9classes)
#' # Generate a new image with the SAVI (Soil-adjusted vegetation index)
#' savi.tb <- sits_mutate_bands(samples_mt_9classes, savi = (1.5*(nir - red)/(nir + red + 0.5)))
#' }
#' @return A sits tibble with same samples and the selected bands.
#' @export
sits_mutate_bands <- function(data.tb, ...){
    # verify if data.tb has values
    .sits_test_tibble(data.tb)

    # compute mutate for each time_series tibble
    proc_fun <- function(...){
        data.tb$time_series <- data.tb$time_series %>%
            purrr::map(function(ts.tb) {
                ts_computed.tb <- ts.tb %>%
                    dplyr::mutate(...)
                return(ts_computed.tb)
            })
    }

    # compute mutate for each time_series tibble
    data.tb$time_series <- proc_fun(...)
    return(data.tb)
}

#' @title Checks that the timeline of all time series of a data set are equal
#' @name sits_prune
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function tests if all time series in a sits tibble
#' have the same number of samples, and returns a time series whose indices
#' match the majority of the samples.
#'
#' @param  data.tb  Either a sits tibble or a raster metadata.
#' @return A pruned sits tibble.
#' @export
sits_prune <- function(data.tb) {
    .sits_test_tibble(data.tb)

    # create a vector to store the number of indices per time series
    n_samples <- vector()

    data.tb$time_series %>%
        purrr::map(function(t) {
            n_samples[length(n_samples) + 1] <<- NROW(t)
        })

    # check if all time indices are equal to the median
    if (all(n_samples == stats::median(n_samples))) {
        message("Success!! All samples of time series have the same number of time indices")
        return(data.tb)
    }
    else{
        message("Some samples of time series do not have the same number of time indices
                as the majority of the data - see log file")

        # save the wrong data in a log file
        ind1 <- which(n_samples != stats::median(n_samples))
        msg_log <- paste0("Lines with wrong number of samples are ",ind1)
        .sits_log_error(msg_log)
        data_err.tb <- data.tb[ind1, ]
        .sits_log_csv(data_err.tb)

        # return the time series that have the same number of samples
        ind2 <- which(n_samples == stats::median(n_samples))

        return(data.tb[ind2, ])
    }
}

#' @title Names of the bands of a time series
#' @name sits_rename
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Set the names of the bands of time series in a sits tibble.
#'
#' @param data.tb      Valid sits tibble.
#' @param names        String vector with the names of the new bands.
#' @return A sits tibble with the new names for the bands.
#' @examples
#' # Retrieve a time series with one band
#' data(point_ndvi)
#' # Rename the band
#' ndvi1.tb <- sits_rename (point_ndvi, names = c("veg_index"))
#' # print the names of the new band
#' sits_bands(ndvi1.tb)
#' @export
sits_rename <- function(data.tb, names){
    # verify if the number of bands informed is the same as the actual number of bands in input data
    ensurer::ensure_that(names, length(.) == length(sits_bands(data.tb)),
                         err_desc = "sits_bands: bands in data input and informed band names have different lengths.")

    # proceed rename and return invisible
    data.tb$time_series <- data.tb$time_series %>%
        purrr::map(function(ts){
            names(ts) <- c("Index", names)
            return(ts)
        })
    return(data.tb)
}

#' @title Sample a percentage of a time series
#' @name sits_sample
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Takes a sits tibble with different labels and
#'              returns a new tibble. For a given field as a group criterion, this new tibble contains a given number or percentage
#'              of the total number of samples per group. Parameter n indicantes the number of random samples with reposition.
#'              Parameter frac indicates a fraction of random samples without reposition. If frac > 1, no sampling is done.
#'
#' @param  data.tb    Input sits tibble.
#' @param  n          Number of samples to pick from each group of data.
#' @param  frac       Percentage of samples to pick from each group of data.
#' @return A sits tibble with a fixed quantity of samples of informed labels and all other.
#' @examples
#' # Retrieve a set of time series with 2 classes
#' data(cerrado_2classes)
#' # Print the labels of the resulting tibble
#' sits_labels(cerrado_2classes)
#' # Samples the data set
#' data.tb <- sits_sample(cerrado_2classes, n = 10)
#' # Print the labels of the resulting tibble
#' sits_labels(data.tb)
#' @export
sits_sample <- function(data.tb, n = NULL, frac = NULL){
    # verify if data.tb is empty
    .sits_test_tibble(data.tb)

    # verify if either n or frac is informed
    ensurer::ensure_that(n, !(base::is.null(.) & base::is.null(frac)),
                         err_desc = "sits_sample: neither n or frac parameters informed")

    # prepare sampling function
    sampling_fun <- if (!base::is.null(n))
        function(tb) {
            if (nrow(tb) >= n) return(dplyr::sample_n(tb, size = n, replace = FALSE))
            else return(tb)
        }
    else if (frac <= 1)
        function(tb) tb %>% dplyr::sample_frac(size = frac, replace = FALSE)
    else
        function(tb) tb %>% dplyr::sample_frac(size = frac, replace = TRUE)

    # compute sampling
    result.tb <- sits_tibble()
    labels <- sits_labels(data.tb)$label
    labels %>%
        purrr::map(function(l){
            tb_l <- dplyr::filter (data.tb, label == l)
            tb_s <- sampling_fun(tb_l)
            result.tb <<- dplyr::bind_rows(result.tb, tb_s)
        })

    return(result.tb)
}

#' @title General selection criteria for subsetting a sits tibble
#' @name sits_select
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This is a general selection function for extracting any subset of a sits tibble
#'              It supports spatial selection by lat/long bounding boxes, names of labels,
#'              start and end dates, and subset of bands.
#'
#' @param data.tb      A sits tibble with the time series.
#' @param ...          Logical expressions in the format `name == value`, where `name` is any sits column name
#'                     or `bands = c(band names)` for selection of a subset of bands.
#'                     See `dplyr::filter()` help for more details.
#' @return A tibble in sits format with the selected bands.
#' @examples
#' # Retrieve a set of time series with 2 classes
#' data(cerrado_2classes)
#' # Select only the time series with the "Cerrado" label
#' data.tb <- sits_select(cerrado_2classes, label == "Cerrado")
#' # Print the labels of the resulting tibble
#' sits_labels(data.tb)
#' @export
sits_select <- function(data.tb, ...) {
    # store the dots in a list
    dots <- match.call(expand.dots = TRUE)

    # remove the bands argument from the list (this will be processed later)
    bands <- dots$bands
    if (!purrr::is_null(bands))
        dots$bands <- NULL

    # apply the dplyr filter to select a subset of the tibble
    # this works for arguments like "label", "start_date", "end_date"
    if (length(dots) > 2)
        for (i in 3:length(dots))
            data.tb <- dplyr::filter_(data.tb, toString(dots[i]))

    # retrieve only the chosen bands (if the bands argument is used)
    if (!purrr::is_null(bands)) {
        b1 <- as.character(bands)
        data.tb <- sits_select_bands_(data.tb, b1[-1])
    }

    return(data.tb)
}

#' @title Filter bands on a sits tibble
#' @name sits_select_bands
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Returns a sits tibble with the selected bands.
#'
#' @param data.tb      A sits tibble metadata and data on time series.
#' @param ...          Names of the selected bands.
#' @return A tibble in sits format with the selected bands.
#' @examples
#' # Retrieve a set of time series with 2 classes
#' data(cerrado_2classes)
#' # Print the original bands
#' sits_bands(cerrado_2classes)
#' # Select only the "ndvi" band
#' data.tb <- sits_select_bands(cerrado_2classes, ndvi)
#' # Print the labels of the resulting tibble
#' sits_bands(data.tb)
#' @export
sits_select_bands <- function(data.tb, ...) {
    bands <-  paste(substitute(list(...)))[-1]

    # verify if bands exists in data.tb
    ensurer::ensure_that(data.tb, all(bands %in% sits_bands(.)),
                         err_desc = "sits_select: some band(s) not found in input data")

    # prepare result sits tibble
    result.tb <- data.tb

    # select the chosen bands for the time series
    result.tb$time_series <- data.tb$time_series %>%
        purrr::map(function(ts) ts[, c("Index", bands)])

    # return the result
    return(result.tb)
}

#' @title Filter bands on a sits tibble
#' @name sits_select_bands_
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Returns a sits tibble with the selected bands.
#'
#' @param data.tb      A sits tibble metadata and data on time series.
#' @param bands        The selcted bands.
#' @return A tibble in sits format with the selected bands.
#' @examples
#' # Retrieve a set of time series with 2 classes
#' data(cerrado_2classes)
#' # Print the original bands
#' sits_bands(cerrado_2classes)
#' # Select only the "ndvi" band
#' band_ndvi <- "ndvi"
#' data.tb <- sits_select_bands_(cerrado_2classes, bands = band_ndvi)
#' # Print the labels of the resulting table
#' sits_bands(data.tb)
#' @export
sits_select_bands_ <- function(data.tb, bands) {
    # verify if bands exists in data.tb
    ensurer::ensure_that(data.tb, all(bands %in% sits_bands(.)),
                         err_desc = "sits_select: some band(s) not found in input data")

    # prepare result sits tibble
    result.tb <- data.tb

    # select the chosen bands for the time series
    result.tb$time_series <- data.tb$time_series %>%
        purrr::map(function(ts) ts[, c("Index", bands)])

    # return the result
    return(result.tb)
}

#' @title Add new sits bands and drops existing.
#' @name sits_transmute_bands
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Adds new bands and drops existing in the time series of a sits tibble using dplyr::transmute function.
#' @param data.tb       A sits tibble.
#' @param ...           Pair expressions in the format `name = value`. See \code{\link[dplyr]{transmute}} help for more details.
#' @return A sits tibble with same samples and the new bands.
#' @examples
#' \donttest{
#' # Retrieve data for time series with label samples in Mato Grosso in Brazil
#' data(samples_mt_9classes)
#' # Generate a new image with the SAVI (Soil-adjusted vegetation index)
#' savi.tb <- sits_transmute_bands(samples_mt_9classes, savi = (1.5*(nir - red)/(nir + red + 0.5)))
#' }
#' @export
sits_transmute_bands <- function(data.tb, ...){
    # verify if data.tb has values
    .sits_test_tibble(data.tb)

    # tricky to include "Index" column and expand `...` arguments
    proc_fun <- function(..., Index = Index){
        Index <- quote(Index)
        purrr::map(data.tb$time_series, function(ts.tb) {
            ts_computed.tb <- dplyr::transmute(ts.tb, !!(Index), ...)
        })
    }

    # compute transmute for each time_series tibble
    data.tb$time_series <- proc_fun(...)
    return(data.tb)
}

#' @title Return the values of a given sits tibble as a list of matrices according to a specified format.
#' @name sits_values
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function returns only the values of a sits tibble (according a specified format).
#' This function is useful to use packages such as ggplot2, dtwclust, or kohonen that
#' require values that are rowwise or colwise organised.
#'
#' @param  data.tb    A sits tibble with time series for different bands.
#' @param  bands      A string with a group of bands whose values are to be extracted. If no bands is informed extract ALL bands.
#' @param  format     A string with either "cases_dates_bands" or "bands_cases_dates" or "bands_dates_cases".
#' @return A sits tibble with values.
#' @examples
#' # Retrieve a set of time series with 2 classes
#' data(cerrado_2classes)
#' # retrieve the values split by bands
#' sits_values(cerrado_2classes[1:2,], format = "bands_dates_cases")
#' @export
sits_values <- function(data.tb, bands = NULL, format = "cases_dates_bands"){
    ensurer::ensure_that(format, . == "cases_dates_bands" || . == "bands_cases_dates" || . == "bands_dates_cases",
                         err_desc = "sits_values: valid format parameter are 'cases_dates_bands', 'bands_cases_dates', or 'bands_dates_cases'")

    if (purrr::is_null(bands))
        bands <- sits_bands(data.tb)

    # equivalent to former sits_values_rows()
    # used in sits_cluster input data
    # list elements: bands, matrix's rows: cases, matrix's cols: dates
    if (format == "cases_dates_bands") {
        # populates result
        values.lst <- data.tb$time_series %>%
            purrr::map(function(ts) {
                data.matrix(dplyr::select(ts, dplyr::one_of(bands)))
            })

        # another kind of sits_values_rows()
        # used in sits_kohonen input
        # list elements: bands, matrix's rows: cases, matrix's cols: dates
    } else if (format == "bands_cases_dates") {
        values.lst <- bands %>% purrr::map(function(band) {
            data.tb$time_series %>%
                purrr::map(function(ts) {
                    dplyr::select(ts, dplyr::one_of(band))
                }) %>%
                data.frame() %>%
                tibble::as_tibble() %>%
                as.matrix() %>% t()
        })

        names(values.lst) <- bands
        # equivalent to former sits_values_cols()
        # list elements: bands, matrix's rows: dates, matrix's cols: cases
    } else if (format == "bands_dates_cases") {
        values.lst <- bands %>% purrr::map(function(band) {
            data.tb$time_series %>%
                purrr::map(function(ts) {
                    dplyr::select(ts, dplyr::one_of(band))
                }) %>%
                data.frame() %>%
                tibble::as_tibble() %>%
                as.matrix()
        })

        names(values.lst) <- bands
    }

    return(values.lst)
}
