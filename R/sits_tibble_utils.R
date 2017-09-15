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
    data1.tb <- sits_tibble()

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
#' @title Apply a function over SITS bands.
#' @name sits_apply
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description Apply a 1D generic function to a time series and specific methods for
#  common tasks such as missing values removal and smoothing.
#' `sits_apply` returns a sits tibble with the same samples points and new bands computed by `fun`,
#' `fun_index` functions. These functions must be defined inline and are called by `sits_apply` for each band,
#' whose vector values is passed as the function argument.
#' The `fun` function may either return a vector or a list of vectors. In the first case, the vector will be the new values
#' of the corresponding band. In the second case, the returned list must have names, and each element vector will
#' generate a new band which name composed by concatenating original band name and the corresponding list element name.
#'
#' If a suffix is provided in `bands_suffix`, all resulting bands names will end with provided suffix separated by a ".".
#'
#' @param data.tb       a valid sits table
#' @param fun           a function with one parameter as input and a vector or list of vectors as output.
#' @param fun_index     a function with one parameter as input and a Date vector as output.
#' @param bands_suffix  a string informing the resulting bands name's suffix.
#' @return data.tb      a sits tibble with same samples and the new bands
#' @export
sits_apply <- function(data.tb, fun, fun_index = function(index){ return(index) }, bands_suffix = "") {

    # verify if data.tb has values
    .sits_test_tibble (data.tb)

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
#' @title names of the bands of a time series
#' @name sits_bands
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description  finds the names of the bands of time series in a sits table
#'               or sets the names of the bands if a set of values is given
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
#' @title Extract a subset of the data based on dates
#' @name sits_extract
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description returns a vector containing the dates of a sits table
#'
#' @param  data.tb    a tibble in SITS format with time series for different bands
#' @param  start_date the starting date of the time series segment
#' @param  end_date   the end date of the time series segment
#' @return subset.tb  a tibble in SITS format with the chosen subset
#' @export
sits_extract <- function (data.tb, start_date = NULL, end_date = NULL) {

    .sits_test_tibble(data.tb)

    # ensure that a start and end date are provided
    ensurer::ensure_that(start_date, !purrr::is_null(.), err_desc = "sits_extract: start_date must be provided")
    ensurer::ensure_that(end_date, !purrr::is_null(.), err_desc = "sits_extract: end_date must be provided")

    # create a tibble to store the results
    subset.tb <- sits_tibble()

    # extract the subsets on a row-by-row basis
    data.tb %>%
        purrrlyr::by_row(function (r) {
            # filter the time series by start and end dates
            sub.ts <- r$time_series[[1]] %>%
                dplyr::filter (dplyr::between (.$Index, start_date, end_date))

            # store the subset of the time series in a list
            ts.lst <- tibble::lst()
            ts.lst[[1]] <- sub.ts
            # create a new row of the output tibble
             subset.tb <<- tibble::add_row (subset.tb,
                                longitude    = r$longitude,
                                latitude     = r$latitude,
                                start_date   = as.Date(start_date),
                                end_date     = as.Date(end_date),
                                label        = r$label,
                                coverage     = r$coverage,
                                time_series  = ts.lst)
        })
    return (subset.tb)
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
#' @title apply a function to a grouped SITS table
#' @name sits_foreach
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description returns a sits table by compound the sits tables apply a function to a grouped SITS table
#'
#' @param data.tb      a sits table with the time series of the selected bands
#' @param ...          one or more sits table field separated by commas that are used to group the data.
#'                     See `dplyr::group_by` help for more details.
#' @param fun          a function that receives as input an sits table and outputs an sits table
#' @return result.tb   a tibble in SITS format with the selected bands
#' @export
sits_foreach <- function (data.tb, ..., fun){

    # execute the foreach applying fun function to each group
    result.tb <- data.tb %>%
        dplyr::group_by(...) %>%
        dplyr::do(.data %>% fun())

    # comply result with sits table format and return
    result.tb <- dplyr::bind_rows(list(sits_tibble(), result.tb))
    return (result.tb)
}
#' @title Group the contents of a sits tibble by different criteria
#' @name sits_group_by
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description returns a sits table by compound the sits tables apply a function to a grouped SITS table
#'
#' @param data.tb      a sits table with the time series of the selected bands
#' @param ...          one or more sits table field separated by commas that are used to group the data.
#'                     See `dplyr::group_by` help for more details.
#' @return result.tb   a tibble in SITS format with the selected bands
#' @export
sits_group_by <- function (data.tb, ...){

    # execute the group by function from dplyr
    result.tb <- data.tb %>%
        dplyr::group_by(...)

    # comply result with sits table format and return
    result.tb <- dplyr::bind_rows(list(sits_tibble(), result.tb))
    return (result.tb)
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
    out.tb <- sits_tibble()
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
    result.tb$time_series <- purrr::map2 (data1.tb$time_series, data2.tb$time_series, function (ts1.tb, ts2.tb) {
        ts3.tb <- dplyr::bind_cols(ts1.tb, dplyr::select(ts2.tb, -Index))
        return (ts3.tb)
    })
    return (result.tb)
}
#' @title Add new SITS bands.
#' @name sits_mutate
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Adds new bands and preserves existing in the time series of a sits tibble using dplyr::mutate function
#' @param data.tb       a valid sits tibble
#' @param ...           `name=value` pairs expressions. See `dplyr::mutate` help for more details.
#' @return data.tb      a sits tibble with same samples and the new bands
#' @export
sits_mutate <- function(data.tb, ...){

    # verify if data.tb has values
    .sits_test_tibble (data.tb)

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
    .sits_test_tibble(data.tb)

    pruned.tb <- sits_tibble()
    discarded.tb <- sits_tibble()

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


#' @title names of the bands of a time series
#' @name sits_rename
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description  set the names of the bands of time series in a sits table
#'
#' @param data.tb      a valid sits tibble
#' @param names        string vector with the new bands' names
#' @return data.tb     the sits tibble with the new names for the bands
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
#'              Parameter frac indicates a fraction of random samples without reposition. If frac > 1, the sampling is taken with reposition.
#'
#' @param  data.tb    input SITS table
#' @param  n          the quantity of samples to pick from a given group of data.
#' @param  frac       the percentage of samples to pick from a given group of data.
#' @return result.tb  the new SITS table with a fixed quantity of samples of informed labels and all other
#' @export
sits_sample <- function (data.tb, n = NULL, frac = NULL){

    # verify if data.tb is empty
    .sits_test_tibble (data.tb)

    # verify if either n or frac is informed
    ensurer::ensure_that(n, !(base::is.null(.) & base::is.null(frac)),
                         err_desc = "sits_sample: neither n or frac parameters informed")

    # prepare sampling function
    sampling_fun <- if (!base::is.null(n))
        function(tb) tb %>% dplyr::sample_n(size = n, replace = TRUE)
    else if (frac <= 1)
        function(tb) tb %>% dplyr::sample_frac(size = frac, replace = FALSE)
    else
        function(tb) tb %>% dplyr::sample_frac(size = frac, replace = TRUE)

    # compute sampling
    result.tb <- sits_tibble()
    labels <- sits_labels (data.tb)$label
    labels %>%
        purrr::map (function (l){
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
#' @param data.tb      a sits tibble with the time series
#' @param ...          `name == value` logical expressions, where `name` is any SITS column name
#'                     or 'bands = c(band names)' for selection of a subset of bands
#'                     See `dplyr::filter` help for more details..
#' @return data.tb     a tibble in SITS format with the selected bands
#' @export
sits_select <- function (data.tb, ...) {

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
    if (!purrr::is_null(bands)){
        b1 <- as.character (bands)
        data.tb <- sits_select_bands(data.tb, b1[-1])
    }

    return (data.tb)
}
#' @title Filter bands on a SITS table
#' @name sits_select_bands
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description returns a sits table with the selected bands
#'
#' @param data.tb      a sits table with the time series of the selected bands
#' @param bands        a vector of bands
#' @return result.tb   a tibble in SITS format with the selected bands
#' @export
sits_select_bands <- function (data.tb, bands) {

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
#' @title returns the labels' count of a sits tibble
#' @name sits_summary
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description  returns the labels and its respective counting and frequency.
#'
#' @param data.tb     a valid sits table
#' @return result.tb  a tibble with the names of the labels and its absolute and relative frequency
#' @export
#'
sits_summary <- function (data.tb) {

    # get frequency table
    data.vec <- table(data.tb$label)

    # compose output tibble containing labels, count and relative frequency columns
    result.tb <- tibble::as_tibble(list(label = names(data.vec),
                                        count = as.integer(data.vec),
                                        freq  = as.numeric(prop.table(data.vec))))
    return (result.tb)
}
#' @title Add new SITS bands and drops existing.
#' @name sits_transmute
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Adds new bands and drops existing in the time series of a sits tibble using dplyr::transmute function
#' @param data.tb       a valid sits tibble
#' @param ...           `name=value` pairs expressions. See `dplyr::transmute` help for more details.
#' @return data.tb      a sits tibble with same samples and the new bands
#' @export
sits_transmute <- function(data.tb, ...){

    # verify if data.tb has values
    .sits_test_tibble (data.tb)

    # tricky to include "Index" column and expand `...` arguments
    proc_fun <- function(..., Index = Index){
        Index <- quote(Index)
        purrr::map(data.tb$time_series, function (ts.tb) {
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
#' @title Apply a function on each SITS tibble column element
#' @name sits_apply_on
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Computes new values for a given SITS tibble field and returns a tibble whose field column have
#' the new values.
#'
#' @param  data.tb    a SITS tibble
#' @param  field      a valid field of data.tb to apply `fun` function
#' @param  fun        a function to apply in field data. The function must have an input parameter to receive each
#'                    field data element at a time and output a new value.
#' @return result.tb  a tibble where columns have the reference label and the time series bands as distances
sits_apply_on <- function(data.tb, field, fun){

    .sits_test_tibble(data.tb)

    # prepare field to be used in dplyr
    field <- deparse(substitute(field))

    # define what to do after apply fun to field data...
    post_process_func <- function(value) {
        if (length(value) == 0)
            return(value)
        # ...if atomic, unlist vectors or other atomic values
        if (is.atomic(value[[1]]))
            return(unlist(value))
        # ...by default, does nothing in non atomic values (like tibbles and list)
        value
    }

    # apply function to field data and return
    data.tb[[field]]  <- post_process_func(purrr::map(data.tb[[field]], fun))

    return(data.tb)
}
