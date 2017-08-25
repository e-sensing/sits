# ---------------------------------------------------------------
#
#  This file contain a list of functions to work with SITS tables
#  SITS tables are the main structures of the "sits" package
#  They contain both the satellite image time series and its metadata
#
#  A sits table is a tibble with pre-defined columns that
#  has the metadata and data for each time series. The columns are
# <longitude, latitude, start_date, end_date, label, coverage, time_series>
#  Most functions on the sits package use a sits table as input (with additional parameters)
# and a sits table as output. This allows for chaining of operation on time series.
#  The package provides the generic method sits_apply to apply a
#  1D generic function to a time series and specific methods for
#  common tasks such as missing values removal and smoothing.
#
#  The functions on this file work with sits tables, but do not change
#  the values of time series. For 1D functions that change the values of
#  the image time series, please see the file "sits_filters".R
#
# ---------------------------------------------------------------

#' @title Create a sits table to store the time series information
#' @name sits_table
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description this function returns an empty sits table.
#' A sits table is a tibble with pre-defined columns that
#' has the metadata and data for each time series. The columns are
#' <longitude, latitude, start_date, end_date, label, coverage, time_series>
#' Most functions on the sits package use a sits table as input (with additional parameters)
#' and a sits table as output. This allows for chaining of operation on time series.
#'
#' @return result.tb  a tibble in SITS format
#' @export

sits_table <- function () {
    result.tb <- tibble::tibble(longitude   = double(),
                                latitude    = double (),
                                start_date  = as.Date(character()),
                                end_date    = as.Date(character()),
                                label       = character(),
                                coverage    = character(),
                                time_series = list()
    )
    class (result.tb) <- append (class(result.tb), "sits_table")
    return (result.tb)
}

#' @title Aligns dates of time series to a reference date
#' @name sits_align
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description converts the time indexes of a set of sits tables to a single reference year.
#' This function is useful to join many time series from different years to a single year,
#' which is required by methods that combine many time series, such as clustering methods.
#' The reference year is taken from the date of the start of the time series
#' available in the coverage.
#'
#' @param  data.tb       tibble - input SITS table (useful for chaining functions)
#' @param  ref_dates     the dates to align the time series
#' @return data1.tb      tibble - the converted SITS table (useful for chaining functions)
#' @export
#'
sits_align <- function (data.tb, ref_dates) {

    # function to shift a time series in time
    shift_ts <- function(d, k) dplyr::bind_rows(utils::tail(d,k), utils::head(d,-k))

    # get the reference date
    start_date <- lubridate::as_date(ref_dates[1])
    # create an output table
    data1.tb <- sits_table()

    # add a progress bar
    message("Aligning samples time series intervals...")
    progress_bar <- utils::txtProgressBar(min = 0, max = nrow(data.tb), style = 3)

    for (i in 1:nrow(data.tb)) {
        # extract the time series
        row <- data.tb[i,]
        ts <- row$time_series[[1]]
        # rows that do not match the number of reference dates are discarded
        if(length(ref_dates) != nrow(ts)) {
            next
        }
        # in what direction do we need to shift the time series?
        sense <- lubridate::yday(lubridate::as_date (ts[1,]$Index)) - lubridate::yday(lubridate::as_date(start_date))
        # find the date of minimum distance to the reference date
        idx <- which.min(abs((lubridate::as_date (ts$Index) - lubridate::as_date(start_date))/lubridate::ddays(1)))
        # do we shift time up or down?
        if (sense < 0) shift <- -(idx - 1) else shift <- (idx - 1)
        # shift the time series to match dates
        if (idx != 1) ts <- shift_ts(ts, -(idx - 1))
        # convert the time index to a reference year
        first_date <- lubridate::as_date(ts[1,]$Index)
        # change the dates to the reference dates
        ts1 <- dplyr::mutate (ts, Index = ref_dates)
        # save the resulting row in the output table
        row$time_series[[1]] <- ts1
        row$start_date <- lubridate::as_date(ref_dates[1])
        row$end_date   <- ref_dates[length(ref_dates)]
        data1.tb <- dplyr::bind_rows(data1.tb, row)

        # update progress bar
        utils::setTxtProgressBar(progress_bar, i)
    }

    close(progress_bar)
    return (data1.tb)
}

#' @title returns the names of the bands of a time series
#' @name sits_bands
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description  finds the names of the bands of time series in a sits table
#'
#' @param data.tb      a valid sits table
#' @return result.vec  a string vector with the names of the bands
#' @export
#'
sits_bands <- function (data.tb) {
    result.vec <- data.tb[1,]$time_series[[1]] %>%
        colnames() %>% .[2:length(.)]
    return (result.vec)
}

#' @title Return the dates of a sits table
#' @name sits_dates
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description returns a vector containing the dates of a sits table
#'
#' @param  data.tb a tibble in SITS format with time series for different bands
#' @return table   a tibble in SITS format with values of time indexes
#' @export
sits_dates <- function (data.tb) {
    values <- data.tb$time_series %>%
        data.frame() %>%
        tibble::as_tibble() %>%
        dplyr::select (dplyr::starts_with ("Index")) %>%
        t() %>%
        as.vector() %>%
        lubridate::as_date()
    return (values)
}

#' @title Group different time series for the same lat/long coordinate
#' @name sits_group_bylatlong
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a sits table in which different time references
#' for the same lat/long coordinate has been separated, and groups them together.
#' This function is useful por plotting together all time series associated to
#' the same location and is also useful to regroup series that have been split
#' to produce yearly samples that are used to define patterns
#'
#' @param    data.tb    tibble - input SITS table
#' @return   data1.tb   tibble - the converted SITS table with time series grouped by latlong
#' @export
#'
sits_group_bylatlong <- function (data.tb) {
    #create a sits table to store the output
    out.tb <- sits_table()
    #find out how many distinct lat/long locations exist in the data
    locs <- dplyr::distinct(data.tb, latitude, longitude)

    # process each lat/long location
    locs %>%
        purrrlyr::by_row ( function (loc) {
            long = as.double (loc$longitude) # select longitude
            lat  = as.double (loc$latitude)  # select latitude
            # filter only those rows with the same label
            rows <- dplyr::filter (data.tb, longitude == long, latitude == lat)

            # make an initial guess for the start and end dates
            start_date <- rows[1,]$start_date
            end_date   <- rows[1,]$end_date
            # get the first time series
            time_series <- rows[1,]$time_series[[1]]

            # are there more time series for the same location?
            if (nrow(rows) > 1) {
                rows %>%
                    utils::tail (n = -1) %>%
                    purrrlyr::by_row (function(row) {
                        # adjust the start and end dates
                        if (row$start_date < start_date) start_date <- row$start_date
                        if (row$end_date   > end_date)   end_date   <- row$end_date
                        # get the time series and join it with the previous ones
                        t <- row$time_series[[1]]
                        time_series <<- dplyr::bind_rows(time_series, t)
                    })
            }
            ts.lst <- tibble::lst()
            ts.lst[[1]] <- time_series
            out.tb <<- tibble::add_row (out.tb,
                                        longitude    = long,
                                        latitude     = lat,
                                        start_date   = as.Date(start_date),
                                        end_date     = as.Date(end_date),
                                        label        = "NoClass",
                                        coverage     = rows[1,]$coverage,
                                        time_series  = ts.lst)
        })
    return (out.tb)
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
#' @export
sits_merge <-  function(data1.tb, data2.tb) {

    # are the names of the bands different?
    ensurer::ensure_that(data1.tb, !(any(sits_bands(.) %in% sits_bands(data2.tb)) | any(sits_bands(data2.tb) %in% sits_bands(.))),
                         err_desc = "sits_merge: cannot merge two sits tables with bands with the same names")

    # if some parameter is empty returns the another one
    if (NROW(data1.tb) == 0)
        return (data2.tb)
    if (NROW(data2.tb) == 0)
        return (data1.tb)

    # verify if data1.tb and data2.tb has the same number of rows
    ensurer::ensure_that(data1.tb, NROW(.) == NROW(data2.tb),
                         err_desc = "sits_merge: cannot merge two sits tables with different numbers of rows")

    # prepare result
    result.tb <- data1.tb

    # merge time series
    result.tb$time_series <- purrr::map2 (data1.tb$time_series, data2.tb$time_series, function (ts1, ts2) {
        ts3 <- dplyr::bind_cols(ts1, dplyr::select(ts2, -Index))
        return (ts3)
    })

    return (result.tb)
}

#' @title Prunes dates of time series to fit an interval
#' @name sits_prune
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description prunes the times series contained a set of sits tables
#' to an interval. This function is useful to constrain the different samples
#' of land cover to the same interval (usually, one year)
#'
#' @param    data.tb      tibble - input SITS table
#' @param    min_interval a string describing the min interval (in days) bellow which the samples are discarded.
#' @param    max_interval a string describing the max interval (in days) above which the samples are proned.
#' @return   pruned.tb    tibble - the converted SITS table
#' @export
#'
sits_prune <- function (data.tb, min_interval = "349 days", max_interval = "365 days") {
    #does the input data exist?
    .sits_test_table (data.tb)

    pruned.tb <- sits_table()
    discarded.tb <- sits_table()

    #
    message("Processing...")

    # add a progress bar
    i <- 0
    progress_bar <- utils::txtProgressBar(min = 0, max = nrow(data.tb), style = 3)

    data.tb %>%
        purrrlyr::by_row (function (row) {
            ts <- row$time_series[[1]]
            row_interval <- lubridate::as_date(row$end_date) - lubridate::as_date(row$start_date)

            # data interval is greater than maximum interval. Trying to cut it.
            if ( row_interval >= lubridate::as.duration(max_interval)) {

                # extract the time series
                ts <- row$time_series[[1]]

                # find the first date which exceeds the required max_interval
                idx <- which.max (lubridate::as_date(ts$Index) - lubridate::as_date(row$start_date) >= lubridate::as.duration(max_interval))

                # prune the time series to fit inside the required max_interval
                ts1 <- ts[1:(idx - 1),]

                # save the pruned time series
                row$time_series[[1]] <- ts1

                # store the new end date
                row$end_date <- ts1[nrow(ts1),]$Index
            }

            # verifies if resulting time series satisfies min_interval requirement. If don't discard sample.
            # Else, stores the resulting row in the SITS table
            row_interval <- lubridate::as_date(row$end_date) - lubridate::as_date(row$start_date)
            if ( row_interval < lubridate::as.duration(min_interval))
                discarded.tb <<- dplyr::bind_rows(discarded.tb, row)
            else
                pruned.tb <<- dplyr::bind_rows(pruned.tb, row)

            # update progress bar
            i <<- i + 1
            utils::setTxtProgressBar(progress_bar, i)
        })

    close(progress_bar)

    if (nrow(discarded.tb) > 0){
        message("The following sample(s) has(have) been discarded:\n")
        print(tibble::as_tibble(discarded.tb))
    }
    return (pruned.tb)
}


#' @title Add new SITS bands.
#' @name sits_mutate
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Adds new bands and preserves existing in a sits_table's time series,
#' using dplyr::mutate function
#' @param data.tb       a valid sits table
#' @param ...           Name-value pairs of expressions. Use NULL to drop a variable.
#' @return result.tb    a sits_table with same samples and the new bands
#' @export
sits_mutate <- function(data.tb, ...){
    result.tb <- data.tb

    result.tb$time_series <- result.tb$time_series %>% purrr::map(function(ts.tb) {
        ts_computed.tb <- dplyr::mutate(ts.tb, ...)
        return(ts_computed.tb)
    })

    return(result.tb)
}
#' @title Rename bands of a sits table
#' @name sits_rename
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description replaces the names of the bands of a satellite image time series
#'
#' @param data.tb      a SITS table with a list of SITS time series
#' @param bands_new    a list of new band names
#' @return out.tb      a SITS table with a list of renamed bands for the time series
#' @export
sits_rename <-  function (data.tb, bands_new) {

    #does the input data exist?
    .sits_test_table (data.tb)

    ensurer::ensure_that(bands_new, !purrr::is_null(.), err_desc = "sits_rename: New band names should be provided")
    ensurer::ensure_that(data.tb, length(sits_bands(.)) == length (bands_new),
                         fail_with = function (e) stop(e),
                         err_desc = "sits_rename: Please provide names for all input bands")

    # rename the time series
    out.ts <- data.tb$time_series %>%
        purrr::map (function (ts) {
            ts_out <- ts
            colnames (ts_out) <- c("Index", bands_new)
            return (ts_out)
        })
    out.tb <- dplyr::select (data.tb, latitude, longitude, start_date, end_date, label, coverage)
    out.tb$time_series <- out.ts

    return (out.tb)
}
#' @title Filter bands on a SITS table
#' @name sits_select
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description returns a sits table with the selected bands
#'
#' @param data.tb      a sits table with the time series of the selected bands
#' @param bands        a vector of bands
#' @return result.tb   a tibble in SITS format with the selected bands
#' @export
sits_select <- function (data.tb, bands) {

    # verify if bands exists in data.tb
    ensurer::ensure_that(data.tb, all(bands %in% sits_bands(.)),
                         err_desc = "sits_select: some band(s) not found in input data")

    # prepare result SITS table
    result.tb <- data.tb

    # select the chosen bands for the time series
    result.tb$time_series <- data.tb$time_series %>%
        purrr::map (function (ts) ts[, c("Index", bands)])

    # return the result
    return (result.tb)
}
#' @title Add new SITS bands and drops existing.
#' @name sits_transmute
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Adds new bands and drops existing in a sits_table's time series,
#' using dplyr::transmute function
#' @param data.tb       a valid sits table
#' @param ...           Name-value pairs of expressions.
#' @return result.tb    a sits_table with same samples and the new bands
#' @export
sits_transmute <- function(data.tb, ...){
    result.tb <- data.tb

    result.tb$time_series <- result.tb$time_series %>% purrr::map(function(ts.tb) {
        ts_computed.tb <- dplyr::transmute(ts.tb, ...)
        if (!("Index" %in% colnames(ts_computed.tb)))
            ts_computed.tb <- dplyr::bind_cols(dplyr::select(ts.tb, Index), ts_computed.tb)
        return(ts_computed.tb)
    })

    return(result.tb)
}
#' @title Apply a function over SITS bands.
#' @name sits_apply
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  `sits_apply` returns a sits_table with the same samples points and new bands computed by `fun`,
#' `fun_index` functions. These functions must be defined inline and are called by `sits_ts_apply` for each band,
#' whose vector values is passed as the function argument.
#'
#' `fun` function may either return a vector or a list of vectors. In the first case, the vector will be the new values
#' of the corresponding band. In the second case, the returned list must have names, and each element vector will
#' generate a new band which name composed by concatenating original band name and the corresponding list element name.
#'
#' If a suffix is provided in `bands_suffix`, all resulting bands names will end with provided suffix separated by a ".".
#'
#' @param data.tb       a valid sits table
#' @param fun           a function with one parameter as input and a vector or list of vectors as output.
#' @param fun_index     a function with one parameter as input and a Date vector as output.
#' @param bands_suffix  a string informing the resulting bands name's suffix.
#' @return data.tb    a sits_table with same samples and the new bands
#' @export
sits_apply <- function(data.tb, fun, fun_index = function(index){ return(index) }, bands_suffix = "") {

    # veify if data.tb has values
    .sits_test_table(data.tb)

    # computes fun and fun_index for all time series and substitutes the original time series data
    data.tb$time_series <- data.tb$time_series %>%
        purrr::map(function(ts.tb) {
            ts_computed.lst <- dplyr::select(ts.tb, -Index) %>%
                purrr::map(fun)

            # append bands names' suffixes
            if (nchar(bands_suffix) != 0)
                names(ts_computed.lst) <- paste0(bands, ".", bands_suffix)

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
#' @title Return the values of a given SITS table as a list of matrices according to a specified format.
#' @name sits_values
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description this function returns only the values of a sits table (according a specified format).
#' This function is useful to use packages such as ggplot, dtwclust, or kohonen that
#' require values that are rowwise or colwise organised.
#'
#' @param  data.tb    a tibble in SITS format with time series for different bands
#' @param  bands      string - a group of bands whose values are to be extracted. If no bands is informed extract ALL bands.
#' @param  format     string - either "cases_dates_bands" or "bands_cases_dates" or "bands_dates_cases"
#' @return table   a tibble in SITS format with values
#' @family   STIS table functions
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
            purrr::map(function (ts) {
                data.matrix(dplyr::select(ts, dplyr::one_of(bands)))
            })

        # another kind of sits_values_rows()
        # used in sits_kohonen input
        # list elements: bands, matrix's rows: cases, matrix's cols: dates
    } else if (format == "bands_cases_dates") {
        values.lst <- bands %>% purrr::map(function (band) {
            data.tb$time_series %>%
                purrr::map(function (ts) {
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
        values.lst <- bands %>% purrr::map(function (band) {
            data.tb$time_series %>%
                purrr::map(function (ts) {
                    dplyr::select(ts, dplyr::one_of(band))
                }) %>%
                data.frame() %>%
                tibble::as_tibble() %>%
                as.matrix()
        })

        names(values.lst) <- bands
    }
    return (values.lst)
}

#' @title Spread matches from a sits matches tibble
#' @name sits_spread_matches
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Given a SITS tibble with a matches, returns a tibble whose columns have
#' the reference label and the TWDTW distances for each temporal pattern.
#'
#' @param  data.tb    a SITS matches tibble
#' @return result.tb  a tibble where whose columns have the reference label and the TWDTW distances for each temporal pattern
#' @export
sits_spread_matches <- function(data.tb){

    # Get best TWDTW aligniments for each class
    data.tb$matches <- data.tb$matches %>%
        purrr::map(function (data.tb){
            data.tb %>%
                dplyr::group_by(predicted) %>%
                dplyr::summarise(distance=min(distance))
        })

    # Select best match and spread pred to columns
    result.tb <- data.tb %>%
        dplyr::transmute(original_row = 1:NROW(.), reference = label, matches = matches) %>%
        tidyr::unnest(matches, .drop = FALSE) %>%
        tidyr::spread(key = predicted, value = distance)

    return(result.tb)
}

#' @title Spread matches from a sits matches tibble
#' @name .sits_test_table
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Tests if a SITS table exists or has data inside
#'
#' @param data.tb  a SITS table
#' @return returns TRUE if data.tb has data.
#'
.sits_test_table <- function (data.tb) {
    ensurer::ensure_that(data.tb, !purrr::is_null(.),
                         err_desc = "input data not provided")
    ensurer::ensure_that(data.tb, NROW(.) > 0,
                         err_desc = "input data is empty")
    return (TRUE)
}
#' @title Create an empty distance table to store the results of distance metrics
#' @name sits_distance_table
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Create an empty distance table to store the results of distance metrics
#'
#' @param patterns.tb     a SITS table with a set of patterns
#' @return distances.tb   a tibble to store the distances between a time series and a set of patterns
#' @export
#'
sits_distance_table <- function (patterns.tb) {

    distances.tb <- tibble::tibble(
        original_row = integer(),
        reference    = character())

    distances.tb <- tibble::as_tibble (distances.tb)

    labels <- (dplyr::distinct(patterns.tb, label))$label
    bands  <- sits_bands (patterns.tb)

    for (l in 1:length(labels))
        for (b in 1:length(bands)) {
                    measure <- paste0 (labels[l], ".", bands[b])
                    distances.tb [measure] = double()
        }
    return (distances.tb)
}
