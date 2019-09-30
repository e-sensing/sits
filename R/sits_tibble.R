#' @title Create a sits tibble to store the time series information
#' @name .sits_tibble
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function returns an empty sits tibble.
#' Sits tibbles are the main structures of sits package.
#' They contain both the satellite image time series and its metadata.
#' A sits tibble is a tibble with pre-defined columns that
#' has the metadata and data for each time series. The columns are
#' <longitude, latitude, start_date, end_date, label, cube, time_series>.
#' Most functions of sits package get a sits tibble as input (with additional parameters)
#' and return another sits tibble as output. This allows chaining functions over sits tibbles.
#'
#' @return A sits tibble.
#' @export
.sits_tibble <- function() {
    result <- tibble::tibble(longitude   = double(),
                                latitude    = double(),
                                start_date  = as.Date(character()),
                                end_date    = as.Date(character()),
                                label       = character(),
                                cube        = character(),
                                time_series = list()
    )
    class(result) <- append(class(result), "sits", after = 0)
    return(result)
}
#' @title Aligns dates of time series to a reference date
#' @name sits_align_dates
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Converts the time indexes of a set of sits tibble to a single reference year.
#' This function is useful to join many time series from different years to a single year,
#' which is required by methods that combine many time series, such as clustering methods.
#' The reference year is taken from the date of the start of the time series
#' available in the data cube.
#'
#' @param  data          Input sits tibble (useful for chaining functions).
#' @param  ref_dates     Dates to align the time series.
#' @return A tibble with the converted sits tibble (useful for chaining functions).
#' @export
sits_align_dates <- function(data, ref_dates) {
    # backward compatibility
    if ("coverage" %in% names(data))
        data <- .sits_tibble_rename(data)
    .sits_test_tibble(data)
    # function to shift a time series in time
    shift_ts <- function(d, k) dplyr::bind_rows(utils::tail(d,k), utils::head(d,-k))

    # get the reference date
    start_date <- lubridate::as_date(ref_dates[1])
    # create an output tibble
    data1.tb <- .sits_tibble()

    purrr::pmap(list(data$longitude, data$latitude,
                     data$label, data$cube, data$time_series),
                function(long, lat, lab, cb, ts) {

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
                                              cube        = cb,
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
#' @param data       Valid sits tibble or matrix.
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
        # backward compatibility
        if ("coverage" %in% names(data))
            data <- .sits_tibble_rename(data)
        # verify if data is valid
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
        chunk.lst <- .sits_raster_split_data(data, multicores)
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
#'               or in a metadata cube
#'
#' @param data      Valid sits tibble (time series or a cube)
#' @return A string vector with the names of the bands.
#'
#' @examples
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' # show the bands
#' sits_bands(samples_mt_6bands)
#' @export
sits_bands <- function(data) {
    # backward compatibility
    if ("coverage" %in% names(data))
        data <- .sits_tibble_rename(data)

    # is this a cube metadata?
    if ("timeline" %in% names(data))
        bands <- data$bands[[1]]

    # is this a sits tibble with the time series?
    if ("time_series" %in% names(data))
        bands <- sits_time_series(data) %>%
            colnames() %>% .[2:length(.)]

    return(bands)
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
#' @param  data    A sits tibble.
#' @param  timeline   Timeline associated with the data cube.
#' @param  start_date Starting date within an interval.
#' @param  end_date   Ending date within an interval.
#' @param  interval   Interval for breaking the series.
#' @return A sits tibble broken into equal intervals.
#' @examples
#' points.tb <- sits_break(point_ndvi, timeline_modis_392, "2000-08-28", "2016-08-12")
#' @export
sits_break <- function(data, timeline, start_date, end_date, interval = "12 month"){
    # backward compatibility
    if ("coverage" %in% names(data))
        data <- .sits_tibble_rename(data)

    # create a tibble to store the results
    newdata <- .sits_tibble()

    # get the dates
    subset_dates.lst <- sits_timeline_match(timeline, as.Date(start_date), as.Date(end_date), interval = interval)

    # break the data into intervals
    lapply(seq_len(nrow(data)), function(i) {
        subset_dates.lst %>%
            purrr::map(function(date){
                point.tb <- .sits_extract(data[i,], as.Date(date[1]), as.Date(date[2]))
                if (nrow(point.tb) > 0)
                    newdata <<- dplyr::bind_rows(newdata, point.tb)
            })

    })
    # prune the results to get the same number of samples
    newdata <- sits_prune(newdata)

    return(newdata)
}

#' @title Return the dates of a sits tibble
#' @name sits_dates
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Returns a vector containing the dates of a sits tibble.
#'
#' @param  data  A tibble in sits format with time series for different bands.
#' @return A tibble with values of time indexes.
#' @examples
#' # get a point and print its dates
#' sits_dates(point_mt_6bands)
#' @export
sits_dates <- function(data) {
    # backward compatibility
    if ("coverage" %in% names(data))
        data <- .sits_tibble_rename(data)
    return(sits_time_series_dates(data))
}

#' @title Merge two satellite image time series
#' @name sits_merge
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function merges the time series of two sits tibbles.
#' To merge two series, we consider that they contain different
#' attributes but refer to the same data cube, and spatio-temporal location.
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
#' plot(sits_merge(point_ndvi, point_ws.tb))
#' }
#' @export
sits_merge <-  function(data1.tb, data2.tb) {
    # backward compatibility
    if ("coverage" %in% names(data1.tb))
        data1.tb <- .sits_tibble_rename(data1.tb)
    if ("coverage" %in% names(data2.tb))
        data2.tb <- .sits_tibble_rename(data2.tb)
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
    result <- data1.tb

    # merge time series
    result$time_series <- purrr::map2(data1.tb$time_series, data2.tb$time_series, function(ts1.tb, ts2.tb) {
        ts3.tb <- dplyr::bind_cols(ts1.tb, dplyr::select(ts2.tb, -Index))
        return(ts3.tb)
    })
    return(result)
}

#' @title Add new sits bands.
#' @name sits_mutate_bands
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description Adds new bands and preserves existing in the time series of a sits tibble using \code{dplyr::mutate} function.
#' @param data       Valid sits tibble.
#' @param ...           Expressions written as `name = value`. See \code{dplyr::mutate()} help for more details.
#' @examples
#' \donttest{
#' # Retrieve data for time series with label samples in Mato Grosso in Brazil
#' data (samples_mt_6bands)
#' # Generate a new image with the SAVI (Soil-adjusted vegetation index)
#' savi.tb <- sits_mutate_bands(samples_mt_6bands, savi = (1.5*(nir - red)/(nir + red + 0.5)))
#' }
#' @return A sits tibble with same samples and the selected bands.
#' @export
sits_mutate_bands <- function(data, ...){
    # backward compatibility
    if ("coverage" %in% names(data))
        data <- .sits_tibble_rename(data)

    # verify if data has values
    .sits_test_tibble(data)

    # compute mutate for each time_series tibble
    proc_fun <- function(...){
        data$time_series <- data$time_series %>%
            purrr::map(function(ts.tb) {
                ts_computed.tb <- ts.tb %>%
                    dplyr::mutate(...)
                return(ts_computed.tb)
            })
    }

    # compute mutate for each time_series tibble
    data$time_series <- proc_fun(...)
    return(data)
}



#' @title Checks that the timeline of all time series of a data set are equal
#' @name sits_prune
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function tests if all time series in a sits tibble
#' have the same number of samples, and returns a time series whose indices
#' match the majority of the samples.
#'
#' @param  data  Either a sits tibble or a raster metadata.
#' @return A pruned sits tibble.
#' @export
sits_prune <- function(data) {

    # backward compatibility
    if ("coverage" %in% names(data))
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
        message("Success!! All samples of time series have the same number of time indices")
        return(data)
    }
    else{
        message("Some samples of time series do not have the same number of time indices
                as the majority of the data - see log file")

        # save the wrong data in a log file
        ind1 <- which(n_samples != stats::median(n_samples))
        msg_log <- paste0("Lines with wrong number of samples are ",ind1)
        .sits_log_error(msg_log)
        data_err.tb <- data[ind1, ]
        .sits_log_csv(data_err.tb)

        # return the time series that have the same number of samples
        ind2 <- which(n_samples == stats::median(n_samples))

        return(data[ind2, ])
    }
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
#' ndvi1.tb <- sits_rename (point_ndvi, names = c("veg_index"))
#' # print the names of the new band
#' sits_bands(ndvi1.tb)
#' @export
sits_rename <- function(data, names){
    # backward compatibility
    if ("coverage" %in% names(data))
        data <- .sits_tibble_rename(data)
    # verify if the number of bands informed is the same as the actual number of bands in input data
    ensurer::ensure_that(names, length(.) == length(sits_bands(data)),
                         err_desc = "sits_bands: bands in data input and informed band names have different lengths.")

    # proceed rename and return invisible
    data$time_series <- data$time_series %>%
        purrr::map(function(ts){
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
#'              returns a new tibble. For a given field as a group criterion, this new tibble contains a given number or percentage
#'              of the total number of samples per group. Parameter n indicantes the number of random samples with reposition.
#'              Parameter frac indicates a fraction of random samples without reposition. If frac > 1, no sampling is done.
#'
#' @param  data    Input sits tibble.
#' @param  n          Number of samples to pick from each group of data.
#' @param  frac       Percentage of samples to pick from each group of data.
#' @return A sits tibble with a fixed quantity of samples of informed labels and all other.
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
sits_sample <- function(data, n = NULL, frac = NULL){

    # backward compatibility
    if ("coverage" %in% names(data))
        data <- .sits_tibble_rename(data)

    # verify if data is valid
    .sits_test_tibble(data)

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
    result <- .sits_tibble()
    labels <- sits_labels(data)$label
    labels %>%
        purrr::map(function(l){
            tb_l <- dplyr::filter (data, label == l)
            tb_s <- sampling_fun(tb_l)
            result <<- dplyr::bind_rows(result, tb_s)
        })

    return(result)
}

#' @title Filter bands on a sits tibble
#' @name sits_select_bands
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Returns a sits tibble with the selected bands.
#'
#' @param data      A sits tibble metadata and data on time series.
#' @param ...          Names of the selected bands.
#' @return A tibble in sits format with the selected bands.
#' @examples
#' # Retrieve a set of time series with 2 classes
#' data(cerrado_2classes)
#' # Print the original bands
#' sits_bands(cerrado_2classes)
#' # Select only the "ndvi" band
#' data <- sits_select_bands(cerrado_2classes, ndvi)
#' # Print the labels of the resulting tibble
#' sits_bands(data)
#' @export
sits_select_bands <- function(data, ...) {
    # backward compatibility
    if ("coverage" %in% names(data))
        data <- .sits_tibble_rename(data)
    bands <-  paste(substitute(list(...)))[-1]

    ensurer::ensure_that(data, all(bands %in% sits_bands(.)),
                         err_desc = paste0("sits_select_bands: the following bands do not exist in the input data: ",
                                           paste(bands[!bands %in% sits_bands(data)], collapse = ", ")))

    # prepare result sits tibble
    result <- data

    # select the chosen bands for the time series
    result$time_series <- data$time_series %>%
        purrr::map(function(ts) ts[, c("Index", bands)])

    # return the result
    return(result)
}
#' @title Shows the predicted labels for a classified tibble
#' @name sits_show_prediction
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function takes a tibble wih time series that has been classified
#' by a machine learning method and display the result.
#'
#' @param  class.tb  A SITS tibble that has been classified
#' @return returns a tibble with the columns "from", "to", "class"
#' @export
sits_show_prediction <- function(class.tb) {
    .sits_test_tibble(class.tb)
    ensurer::ensure_that(class.tb$predicted[[1]], all(names(.) %in% c("from", "to", "class", "probs")),
                         err_desc = "sits_show_prediction: tibble has not been classified")
    return(dplyr::select(class.tb$predicted[[1]], c("from", "to", "class")))
}
#' @title Retrieve the dates of time series for a row of a sits tibble
#' @name sits_time_series_dates
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
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


#' @title Add new sits bands and drops existing.
#' @name sits_transmute_bands
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Adds new bands and drops existing in the time series of a sits tibble using dplyr::transmute function.
#' @param data       A sits tibble.
#' @param ...           Pair expressions in the format `name = value`. See \code{\link[dplyr]{mutate}} help for more details.
#' @return A sits tibble with same samples and the new bands.
#'
#' @examples
#' \donttest{
#' # Retrieve data for time series with label samples in Mato Grosso in Brazil
#' data(samples_mt_6bands)
#' # Generate a new image with the SAVI (Soil-adjusted vegetation index)
#' savi.tb <- sits_transmute_bands(samples_mt_6bands, savi = (1.5*(nir - red)/(nir + red + 0.5)))
#' }
#'
#' @export
sits_transmute_bands <- function(data, ...){
    # backward compatibility
    if ("coverage" %in% names(data))
        data <- .sits_tibble_rename(data)

    # verify if data is valid
    .sits_test_tibble(data)

    # tricky to include "Index" column and expand `...` arguments
    proc_fun <- function(..., Index = Index){
        Index <- quote(Index)
        purrr::map(data$time_series, function(ts.tb) {
            ts_computed.tb <- dplyr::transmute(ts.tb, !!(Index), ...)
        })
    }

    # compute transmute for each time_series tibble
    data$time_series <- proc_fun(...)
    return(data)
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
#' @param  data    A sits tibble with time series for different bands.
#' @param  bands      A string with a group of bands whose values are to be extracted. If no bands is informed extract ALL bands.
#' @param  format     A string with either "cases_dates_bands" or "bands_cases_dates" or "bands_dates_cases".
#' @return A sits tibble with values.
#' @examples
#' # Retrieve a set of time series with 2 classes
#' data(cerrado_2classes)
#' # retrieve the values split by bands
#' sits_values(cerrado_2classes[1:2,], format = "bands_dates_cases")
#' @export
sits_values <- function(data, bands = NULL, format = "cases_dates_bands"){
    ensurer::ensure_that(format, . == "cases_dates_bands" || . == "bands_cases_dates" || . == "bands_dates_cases",
                         err_desc = "sits_values: valid format parameter are 'cases_dates_bands', 'bands_cases_dates', or 'bands_dates_cases'")
    # backward compatibility
    if ("coverage" %in% names(data))
        data <- .sits_tibble_rename(data)

    if (purrr::is_null(bands))
        bands <- sits_bands(data)

    # equivalent to former sits_values_rows()
    # used in sits_cluster input data
    # list elements: bands, matrix's rows: cases, matrix's cols: dates
    if (format == "cases_dates_bands") {
        # populates result
        values.lst <- data$time_series %>%
            purrr::map(function(ts) {
                data.matrix(dplyr::select(ts, dplyr::one_of(bands)))
            })

        # another kind of sits_values_rows()
        # used in sits_kohonen input
        # list elements: bands, matrix's rows: cases, matrix's cols: dates
    } else if (format == "bands_cases_dates") {
        values.lst <- bands %>% purrr::map(function(band) {
            data$time_series %>%
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
            data$time_series %>%
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
#' @title Apply a function over a set of time series.
#' @name .sits_apply_ts
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description Apply a 1D generic function to a time series and specific methods for
#' common tasks such as missing values removal and smoothing.
#' `sits_apply_ts` returns a time series tibble with the same samples points and new bands computed by `fun`,
#' `fun_index` functions. These functions must be defined inline and are called by `sits_apply` for each band,
#' whose vector values is passed as the function argument.
#' The `fun` function may either return a vector or a list of vectors. In the first case, the vector will be the new values
#' of the corresponding band. In the second case, the returned list must have names, and each element vector will
#' generate a new band which name composed by concatenating original band name and the corresponding list element name.
#'
#' If a suffix is provided in `bands_suffix`, all resulting bands names will end with provided suffix separated by a ".".
#'
#' @param ts.tb         Tibble with a time series (one or more bands).
#' @param fun           Function with one parameter as input and a vector or list of vectors as output.
#' @param fun_index     Function with one parameter as input and a Date vector as output.
#' @param bands_suffix  String informing the resulting bands name's suffix.
#' @return A sits tibble with same samples and the new bands.
.sits_apply_ts <- function(ts.tb, fun, fun_index = function(index){ return(index) }, bands_suffix = "") {
    # computes fun and fun_index for all time series and substitutes the original time series data
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

    return(ts.tb)
}

#' @title Create partitions of a data set
#' @name  .sits_create_folds
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
    data$folds <- caret::createFolds(data$label, k = folds, returnTrain = FALSE, list = FALSE)

    return(data)
}

#' @title Extract a subset of the data based on dates
#' @name .sits_extract
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Returns a vector containing the dates of a sits tibble.
#'
#' @param  row.tb     A sits tibble.
#' @param  start_date Starting date of the time series segment.
#' @param  end_date   End date of the time series segment.
#' @return A tibble in sits format with the chosen subset.
.sits_extract <- function(row.tb, start_date, end_date) {
    # create a tibble to store the results
    subset.tb <- .sits_tibble()

    # filter the time series by start and end dates
    ts <- sits_time_series(row.tb)
    indexes <- dplyr::between(ts$Index, start_date, end_date)

    if (any(indexes)) {
        sub.ts <- ts[indexes, ]
        # store the subset of the time series in a list
        ts.lst <- tibble::lst()
        ts.lst[[1]] <- sub.ts
        # create a new row of the output tibble
        subset.tb <- tibble::add_row(subset.tb,
                                     longitude    = row.tb$longitude,
                                     latitude     = row.tb$latitude,
                                     start_date   = as.Date(start_date),
                                     end_date     = as.Date(end_date),
                                     label        = row.tb$label,
                                     cube         = row.tb$cube,
                                     time_series  = ts.lst)
    }
    return(subset.tb)
}

#' @title Group the contents of a sits tibble by different criteria
#' @name .sits_group_by
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Returns a sits tibble by compound the sits tibble apply a function to a grouped sits tibble.
#'
#' @param data      A sits tibble.
#' @param ...          One or more sits tibble field separated by commas that are used to group the data.
#'                     See `dplyr::group_by` help for more details.
#' @return A sits tibble with the selected bands.
.sits_group_by <- function(data, ...){
    # execute the group by function from dplyr
    result <- data %>%
        dplyr::group_by(...)

    # comply result with sits tibble format and return
    result <- dplyr::bind_rows(list(.sits_tibble(), result))
    return(result)
}

#' @title Filter bands on a sits tibble
#' @name .sits_select_bands_
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Returns a sits tibble with the selected bands.
#'
#' @param data      A sits tibble metadata and data on time series.
#' @param bands        The selected bands.
#' @return A tibble in sits format with the selected bands.
.sits_select_bands_ <- function(data, bands) {
    # backward compatibility
    if ("coverage" %in% names(data))
        data <- .sits_tibble_rename(data)
    # verify if bands exists in data
    ensurer::ensure_that(data, all(bands %in% sits_bands(.)),
                         err_desc = paste0(".sits_select_bands_: the following bands do not exist in the input data: ",
                                           paste(bands[!bands %in% sits_bands(data)], collapse = ", ")))

    # prepare result sits tibble
    result <- data

    # select the chosen bands for the time series
    result$time_series <- data$time_series %>%
        purrr::map(function(ts) ts[, c("Index", bands)])

    # return the result
    return(result)
}

#' @title Tests if a sits tibble is valid
#' @name .sits_test_tibble
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Tests if a sits tibble exists or has data inside.
#'
#' @param data  A sits tibble.
#' @return Returns TRUE if data has data.
.sits_test_tibble <- function(data) {
    ensurer::ensure_that(data, !purrr::is_null(.),
                         err_desc = "input data not provided")
    ensurer::ensure_that(data, NROW(.) > 0,
                         err_desc = "input data is empty")

    names <- c("longitude", "latitude", "start_date", "end_date",
               "label", "cube", "time_series")

    ensurer::ensure_that(data, all(names %in% colnames(.)),
                         err_desc = "data input is not a valid sits tibble")

    return(TRUE)
}

#' @title Create an empty tibble to store the results of CSV samples that could not be read
#' @name .sits_tibble_csv
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Create an empty tibble to store the results of classification.
#'
#' @return A tibble to store the result of classifications.
.sits_tibble_csv <- function() {
    result <- tibble::tibble(longitude   = double(),
                                latitude    = double(),
                                start_date  = as.Date(character()),
                                end_date    = as.Date(character()),
                                label       = character()
    )
    return(result)
}

#' @title Create an empty tibble to store the results of predictions
#' @name .sits_tibble_prediction
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Create a tibble to store the results of predictions.
#' @param  data         A tibble with the input data.
#' @param  class_info.tb   A tibble with the information on classification.
#' @param  pred.mtx        The result of the classification (one class per column and one row per interval).
#' @param  interval        The time interval between two classifications.
#' @return A tibble storing the predictions.
.sits_tibble_prediction <- function(data, class_info.tb, pred.mtx, interval) {
    # retrieve the list of reference dates
    # this list is a global one and it is created based on the samples
    ref_dates.lst   <- class_info.tb$ref_dates[[1]]

    # retrieve the global timeline
    timeline_global <- class_info.tb$timeline[[1]]

    # size of prediction tibble
    nrows <- length(ref_dates.lst)

    # get the labels of the data
    labels <- class_info.tb$labels[[1]]
    n_labels <- length(labels)
    # create a named vector with integers match the class labels
    int_labels <- c(1:n_labels)
    names(int_labels) <- labels

    # compute pred.vec
    pred.vec <-  names(int_labels[max.col(pred.mtx)])

    class_idx <-  1

    predicted.lst <- purrr::pmap(
        list(data$start_date, data$end_date, data$time_series),
        function(row_start_date, row_end_date, row_time_series) {
            # get the timeline of the row
            timeline_row <- lubridate::as_date(row_time_series$Index)
            # the timeline of the row may be different from the global timeline
            # this happens when we are processing samples with different dates
            if (timeline_row[1] != timeline_global[1]) {
                # what is the reference start date?
                ref_start_date <- lubridate::as_date(row_start_date)
                # what is the reference end date?
                ref_end_date <- lubridate::as_date(row_end_date)
                # what are the reference dates to do the classification?
                ref_dates.lst <- sits_timeline_match(timeline_row, ref_start_date, ref_end_date, interval)
            }

            # store the classification results
            pred_row.lst <- ref_dates.lst %>%
                purrr::map(function(rd){
                    pred_row <- tibble::tibble(
                        from      = as.Date(rd[1]),
                        to        = as.Date(rd[2]),
                        class     = pred.vec[class_idx],
                        probs     = list(pred.mtx[class_idx,])
                    )
                    class_idx  <<- class_idx + 1
                    return(pred_row)
                })
            # transform the list into a tibble
            predicted.tb <- dplyr::bind_rows(pred_row.lst)
            return(predicted.tb)
        })

    data$predicted <- predicted.lst
    class(data) <- append(class(data), "predicted", after = 0)

    return(data)
}
#' @title Rename a tibble to use "cube" instead of "coverage"
#' @name .sits_tibble_rename
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param data   Tibble with
.sits_tibble_rename <- function(data)
{
    # is the input data a valid sits tibble?
    ensurer::ensure_that(data, "coverage" %in% names(.),
                         err_desc = "sits_tibble_rename: input data does not contain a coverage column ")

    data <- data %>% dplyr::rename(cube = coverage)

    return(data)
}


