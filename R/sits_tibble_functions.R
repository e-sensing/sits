#' @title Apply a function over SITS bands.
#' @name sits_apply
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description Apply a 1D generic function to a time series and specific methods for
#' common tasks such as missing values removal and smoothing.
#' `sits_apply` returns a sits tibble with the same samples points and new bands computed by `fun`,
#' `fun_index` functions. These functions must be defined inline and are called by `sits_apply` for each band,
#' whose vector values is passed as the function argument.
#' The `fun` function may either return a vector or a list of vectors. In the first case, the vector will be the new values
#' of the corresponding band. In the second case, the returned list must have names, and each element vector will
#' generate a new band which name composed by concatenating original band name and the corresponding list element name.
#'
#' If a suffix is provided in `bands_suffix`, all resulting bands names will end with provided suffix separated by a ".".
#'
#' @param data.tb       valid sits table
#' @param fun           function with one parameter as input and a vector or list of vectors as output.
#' @param fun_index     function with one parameter as input and a Date vector as output.
#' @param bands_suffix  string informing the suffix of the resulting bands.
#' @return data.tb      sits tibble with same samples and the new bands
#' @examples
#' # Get a time series
#' data(point_ndvi)
#' # apply a normalization function
#' point2 <- sits_apply (point_ndvi, fun = function (x) { (x - min (x))/(max(x) - min(x))} )
#'
#' @export
sits_apply <- function(data.tb, fun, fun_index = function(index){ return(index) }, bands_suffix = "") {

    # verify if data.tb has values
    .sits_test_tibble(data.tb)

    # computes fun and fun_index for all time series and substitutes the original time series data
    data.tb$time_series <- data.tb$time_series %>%
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
    return(data.tb)
}
#' @title Informs the names of the bands of a time series
#' @name sits_bands
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description  finds the names of the bands of time series in a sits table
#'               or sets the names of the bands if a set of values is given
#'
#' @param data.tb      valid sits table
#' @return result.vec  string vector with the names of the bands
#'
#' @examples
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' data(samples_MT_9classes)
#' # print the bands
#' sits_bands(samples_MT_9classes)
#' @export
#'
sits_bands <- function(data.tb) {
    result.vec <- data.tb[1,]$time_series[[1]] %>%
        colnames() %>% .[2:length(.)]
    return(result.vec)
}

#' @title Return the dates of a sits table
#' @name sits_dates
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description returns a vector containing the dates of a sits table
#'
#' @param  data.tb  tibble in SITS format with time series for different bands
#' @return table    tibble  with values of time indexes
#' @examples
#' # get a point and print its dates
#' sits_dates (point_MT_6bands)
#' @export
sits_dates <- function(data.tb) {
    values <- data.tb$time_series[[1]]$Index
    return(values)
}

#' @title Merge two satellite image time series
#' @name sits_merge
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function merges the time series of two STIS tables.
#' To merge two series, we consider that they contain different
#' attributes but refer to the same coverage, and spatio-temporal location.
#' This function is useful to merge different bands of the same spatio-temporal locations.
#' For example, one may want to put the raw and smoothed bands for the same set of locations
#' in the same table.
#'
#' @param data1.tb      the first SITS table to be merged
#' @param data2.tb      the second SITS table to be merged
#' @return result.tb    a merged SITS tibble with a nested set of time series
#' @examples
#' \donttest{
#' #' # Retrieve a time series with values of NDVI
#' data(point_ndvi)
#' # Filter the point using the whittaker smoother
#' point_ws.tb <- sits_whittaker (point_ndvi, lambda = 3.0)
#' # Plot the two points to see the smoothing effect
#' sits_plot(sits_merge(point_ndvi, point_ws.tb))
#' }
#' @export
sits_merge <-  function(data1.tb, data2.tb) {

    # are the names of the bands different?
    ensurer::ensure_that(data1.tb, !(any(sits_bands(.) %in% sits_bands(data2.tb)) | any(sits_bands(data2.tb) %in% sits_bands(.))),
                         err_desc = "sits_merge: cannot merge two sits tables with bands with the same names")

    # if some parameter is empty returns the another one
    if (NROW(data1.tb) == 0)
        return(data2.tb)
    if (NROW(data2.tb) == 0)
        return(data1.tb)

    # verify if data1.tb and data2.tb has the same number of rows
    ensurer::ensure_that(data1.tb, NROW(.) == NROW(data2.tb),
                         err_desc = "sits_merge: cannot merge two sits tables with different numbers of rows")

    # prepare result
    result.tb <- data1.tb

    # merge time series
    result.tb$time_series <- purrr::map2(data1.tb$time_series, data2.tb$time_series, function(ts1.tb, ts2.tb) {
        ts3.tb <- dplyr::bind_cols(ts1.tb, dplyr::select(ts2.tb, -Index))
        return(ts3.tb)
    })
    return(result.tb)
}
#' @title Add new SITS bands.
#' @name sits_mutate
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Adds new bands and preserves existing in the time series of a sits tibble using dplyr::mutate function
#' @param data.tb       valid sits tibble
#' @param ...           `name=value` pairs expressions. See `dplyr::mutate` help for more details.
#' @examples
#' \donttest{
#' # Retrieve data for time series with label samples in Mato Grosso in Brazil
#' data (samples_MT_9classes)
#' # Generate a new image with the SAVI (Soil-adjusted vegetation index)
#' savi.tb <- sits_mutate (samples_MT_9classes, savi = (1.5*(nir - red)/(nir + red + 0.5)))
#' }
#' @return data.tb      a sits tibble with same samples and the new bands
#' @export
sits_mutate <- function(data.tb, ...){

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
#' @description This function tests if all time series in a SITS tibble
#' have the same number of samples, and returns a time series whose indices
#' match the majority of the samples
#'
#' @param  data.tb  sits tibble (either a SITS tibble or a raster metadata)
#' @return data.tb  pruned sits tibble
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
        ind1 <- which (n_samples != stats::median(n_samples))
        msg_log <- paste0("Lines with wrong number of samples are ",ind1)
        .sits_log_error(msg_log)
        data_err.tb <- data.tb[ind1, ]
        .sits_log_data(data_err.tb, file_name = "data_wrong_indices.rda")

        # return the time series that have the same number of samples
        ind2 <- which (n_samples == stats::median(n_samples))

        return(data.tb[ind2, ])
    }
}


#' @title names of the bands of a time series
#' @name sits_rename
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description  set the names of the bands of time series in a sits table
#'
#' @param data.tb      valid sits tibble
#' @param names        string vector with the names of the new bands
#' @return data.tb     sits tibble with the new names for the bands
#' @examples
#' # Retrieve a time series with one band
#' data(point_ndvi)
#' # Rename the band
#' ndvi1.tb <- sits_rename (point_ndvi, names = c("veg_index"))
#' # print the names of the new band
#' sits_bands (ndvi1.tb)
#' @export
#'
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
#' @description takes a sits table with different labels and
#'              returns a new table. For a given field as a group criterion, this new table contains a given number or percentage
#'              of the total number of samples per group. Parameter n indicantes the number of random samples with reposition.
#'              Parameter frac indicates a fraction of random samples without reposition. If frac > 1, no sampling is done.
#'
#' @param  data.tb    input SITS table
#' @param  n          number of samples to pick from a given group of data.
#' @param  frac       percentage of samples to pick from a given group of data.
#' @return result.tb  new SITS table with a fixed quantity of samples of informed labels and all other
#' @examples
#' # Retrieve a set of time series with 2 classes
#' data (cerrado_2classes)
#' # Print the labels of the resulting table
#' sits_labels (cerrado_2classes)
#' # Samples the data set
#' data.tb <- sits_sample (cerrado_2classes, n = 10)
#' # Print the labels of the resulting table
#' sits_labels (data.tb)
#'
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
#' @title General selection criteria for subsetting a SITS table
#' @name sits_select
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This is a general selection function for extracting any subset of a sits tibble
#'              It supports spatial selection by lat/long bounding boxes, names of labels,
#'              start and end dates, and subset of bands
#'
#' @param data.tb      sits tibble with the time series
#' @param ...          `name == value` logical expressions, where `name` is any SITS column name
#'                     or 'bands = c(band names)' for selection of a subset of bands
#'                     See `dplyr::filter` help for more details..
#' @return data.tb     a tibble in SITS format with the selected bands
#' @examples
#' #' # Retrieve a set of time series with 2 classes
#' data (cerrado_2classes)
#' # Select only the time series with the "Cerrado" label
#' data.tb <- sits_select (cerrado_2classes, label == "Cerrado")
#' # Print the labels of the resulting table
#' sits_labels (data.tb)
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

    #retrieve only the chosen bands (if the bands argument is used)
    if (!purrr::is_null(bands)) {
        b1 <- as.character(bands)
        data.tb <- sits_select_bands(data.tb, b1[-1])
    }

    return(data.tb)
}
#' @title Filter bands on a SITS table
#' @name sits_select_bands
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description returns a sits table with the selected bands
#'
#' @param data.tb      sits tibble metadata and data on time series
#' @param bands        vector of bands
#' @return result.tb   tibble in SITS format with the selected bands
#' @examples
#' # Retrieve a set of time series with 2 classes
#' data (cerrado_2classes)
#' # Print the original bands
#' sits_bands (cerrado_2classes)
#' # Select only the "ndvi" band
#' data.tb <- sits_select_bands (cerrado_2classes, bands = c("ndvi"))
#' # Print the labels of the resulting table
#' sits_bands (data.tb)
#' @export
sits_select_bands <- function(data.tb, bands) {

    # verify if bands exists in data.tb
    ensurer::ensure_that(data.tb, all(bands %in% sits_bands(.)),
                         err_desc = "sits_select: some band(s) not found in input data")

    # prepare result SITS table
    result.tb <- data.tb

    # select the chosen bands for the time series
    result.tb$time_series <- data.tb$time_series %>%
        purrr::map(function(ts) ts[, c("Index", bands)])

    # return the result
    return(result.tb)
}

#' @title Add new SITS bands and drops existing.
#' @name sits_transmute
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Adds new bands and drops existing in the time series of a sits tibble using dplyr::transmute function
#' @param data.tb       valid sits tibble
#' @param ...           `name=value` pairs expressions. See `dplyr::transmute` help for more details.
#' @return data.tb      sits tibble with same samples and the new bands
#' @examples
#' \donttest{
#' # Retrieve data for time series with label samples in Mato Grosso in Brazil
#' data (samples_MT_9classes)
#' # Generate a new image with the SAVI (Soil-adjusted vegetation index)
#' savi.tb <- sits_transmute (samples_MT_9classes, savi = (1.5*(nir - red)/(nir + red + 0.5)))
#' }
#' @export
sits_transmute <- function(data.tb, ...){

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
#' @title Return the values of a given SITS table as a list of matrices according to a specified format.
#' @name sits_values
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description this function returns only the values of a sits table (according a specified format).
#' This function is useful to use packages such as ggplot, dtwclust, or kohonen that
#' require values that are rowwise or colwise organised.
#'
#' @param  data.tb    tibble in SITS format with time series for different bands
#' @param  bands      string - a group of bands whose values are to be extracted. If no bands is informed extract ALL bands.
#' @param  format     string - either "cases_dates_bands" or "bands_cases_dates" or "bands_dates_cases"
#' @return table      tibble in SITS format with values
#' @examples
#' # Retrieve a set of time series with 2 classes
#' data (cerrado_2classes)
#' # retrieve the values split by bands
#' sits_values (cerrado_2classes[1:2,], format = "bands_dates_cases")
#'
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
