#' @title Aligns dates of time series to a reference date
#' @name .sits_align
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
#'
.sits_align <- function(data.tb, ref_dates) {

    # function to shift a time series in time
    shift_ts <- function(d, k) dplyr::bind_rows(utils::tail(d,k), utils::head(d,-k))

    # get the reference date
    start_date <- lubridate::as_date(ref_dates[1])
    # create an output table
    data1.tb <- sits_tibble()

    for (i in 1:nrow(data.tb)) {
        # extract the time series
        row <- data.tb[i,]
        ts <- row$time_series[[1]]
        # rows that do not match the number of reference dates are discarded
        if (length(ref_dates) != nrow(ts)) {
            next
        }
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
        # save the resulting row in the output table
        row$time_series[[1]] <- ts1
        row$start_date <- lubridate::as_date(ref_dates[1])
        row$end_date   <- ref_dates[length(ref_dates)]
        data1.tb <- dplyr::bind_rows(data1.tb, row)
    }
    return(data1.tb)
}


#' @title Apply a function on each SITS tibble column element
#' @name .sits_apply_on
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
.sits_apply_on <- function(data.tb, field, fun){

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
#' @title Apply a function over a set of time series.
#' @name .sits_apply_ts
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description Apply a 1D generic function to a time series and specific methods for
#  common tasks such as missing values removal and smoothing.
#' `sits_apply_ts` returns a time series tibble with the same samples points and new bands computed by `fun`,
#' `fun_index` functions. These functions must be defined inline and are called by `sits_apply` for each band,
#' whose vector values is passed as the function argument.
#' The `fun` function may either return a vector or a list of vectors. In the first case, the vector will be the new values
#' of the corresponding band. In the second case, the returned list must have names, and each element vector will
#' generate a new band which name composed by concatenating original band name and the corresponding list element name.
#'
#' If a suffix is provided in `bands_suffix`, all resulting bands names will end with provided suffix separated by a ".".
#'
#' @param ts.tb         a valid sits table
#' @param fun           a function with one parameter as input and a vector or list of vectors as output.
#' @param fun_index     a function with one parameter as input and a Date vector as output.
#' @param bands_suffix  a string informing the resulting bands name's suffix.
#' @return data.tb      a sits tibble with same samples and the new bands
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
#' @title Break a time series to match the time range of a set of patterns
#' @name .sits_break_ts
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description  Given a long time series, divide it into segments to match
#'               the time range of a set of patterns
#'
#' @param  ts             time series data
#' @param  ref_dates.lst  list with dates to break
#' @return ts.lst         list with the breaks of the time series data
#'
.sits_break_ts <-  function(ts, ref_dates.lst) {

    new_ts.lst <- list()
    ref_dates.lst %>%
        purrr::map(function(dates) {
            if (ts$Index[1] <= dates[1]) {
                if (ts$Index[length(ts$Index)] >= dates[2]) {
                    ts_b <- ts %>% dplyr::filter(dplyr::between(.$Index, dates[1], dates[2]))
                    new_ts.lst[[length(new_ts.lst) + 1 ]] <<- ts_b
                }
            }
        })

    return(new_ts.lst)
}


#' @title Create partitions of a data set
#' @name  .sits_create_folds
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Alexandre Ywata, \email{alexandre.ywata@@ipea.gov.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Split a SITS table into k groups, based on the label
#'
#' @param data.tb a SITS table to be partitioned
#' @param folds   number of folds
.sits_create_folds <- function(data.tb, folds = 5) {

    # verify if data.tb exists
    .sits_test_tibble(data.tb)

    # splits the data into k groups
    data.tb$folds <- caret::createFolds(data.tb$label, k = folds, returnTrain = FALSE, list = FALSE)

    return(data.tb)
}
#' @title Create a list to store time series by timeline
#' @name  .sits_create_ts_list
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Create a list of time series that share the same timeline
#'
#' @param timeline  the timeline (dates of the time series)
#' @param n         Number of time series to create
#' @return ts.lst   list with the time series that share the same timeline
#'
.sits_create_ts_list <- function(timeline, n) {

    ts.lst <- list()
    # create one time series per pixel
    # all time series share the same timeline
    for (i in 1:n) {
        ts.tb <- tibble::tibble(Index = timeline)
        ts.lst[[length(ts.lst) + 1 ]] <- ts.tb
        i <- i + 1
    }
    return(ts.lst)
}
#' @title Extract a subset of the data based on dates
#' @name .sits_extract
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description returns a vector containing the dates of a sits table
#'
#' @param  row.tb    a tibble in SITS format with time series for different bands
#' @param  start_date the starting date of the time series segment
#' @param  end_date   the end date of the time series segment
#' @return subset.tb  a tibble in SITS format with the chosen subset
.sits_extract <- function(row.tb, start_date, end_date) {

    # create a tibble to store the results
    subset.tb <- sits_tibble()

    # filter the time series by start and end dates
    sub.ts <- row.tb$time_series[[1]] %>%
        dplyr::filter (dplyr::between(.$Index, start_date, end_date))

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
                                 coverage     = row.tb$coverage,
                                 time_series  = ts.lst)
    return(subset.tb)
}

#' @title Extract a subset of time series tibble based on dates
#' @name .sits_extract_ts
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description returns a vector containing the dates of a sits table
#'
#' @param  ts.tb      a tibble  with time series for different bands
#' @param  start_index the starting date of the time series segment
#' @param  end_index   the end date of the time series segment
#' @return subset.tb  a tibble with time series with the chosen subset
.sits_extract_ts <- function(ts.tb, start_index, end_index) {


    # filter the time series by start and end dates
    subset.tb <- ts.tb %>%
        dplyr::filter(dplyr::between(.$Index, start_date, end_date))

    return(subset.tb)
}
#' @title apply a function to a grouped SITS table
#' @name .sits_foreach
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description returns a sits table by compound the sits tables apply a function to a grouped SITS table
#'
#' @param data.tb      a sits table with the time series of the selected bands
#' @param field        one sits table field that are used to group the data.
#'                     See `dplyr::group_by` help for more details.
#' @param fun          a function that receives as input an sits table and outputs an sits table
#' @return result.tb   a tibble in SITS format with the selected bands
.sits_foreach <- function(data.tb, field, fun){

    .sits_test_tibble(data.tb)

    field <- deparse(substitute(field))

    # execute the foreach applying fun function to each group
    result.tb <- data.tb %>%
        dplyr::group_by_(field) %>%
        dplyr::do(.data %>% fun())

    # comply result with sits table format and return
    result.tb <- dplyr::bind_rows(list(sits_tibble(), result.tb))
    return(result.tb)
}
#' @title Group the contents of a sits tibble by different criteria
#' @name .sits_group_by
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description returns a sits table by compound the sits tables apply a function to a grouped SITS table
#'
#' @param data.tb      a sits table with the time series of the selected bands
#' @param ...          one or more sits table field separated by commas that are used to group the data.
#'                     See `dplyr::group_by` help for more details.
#' @return result.tb   a tibble in SITS format with the selected bands
.sits_group_by <- function(data.tb, ...){

    # execute the group by function from dplyr
    result.tb <- data.tb %>%
        dplyr::group_by(...)

    # comply result with sits table format and return
    result.tb <- dplyr::bind_rows(list(sits_tibble(), result.tb))
    return(result.tb)
}
#' @title Add new SITS bands.
#' @name .sits_mutate
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Adds new bands and preserves existing in the time series of a sits tibble using dplyr::mutate function
#' @param data.tb       a valid sits tibble
#' @param ...           `name=value` pairs expressions. See `dplyr::mutate` help for more details.
#' @return data.tb      a sits tibble with same samples and the new bands
.sits_mutate <- function(data.tb, ...){

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
#' @title Tests if a sits tibble is valid
#' @name .sits_test_tibble
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Tests if a SITS tibble exists or has data inside
#'
#' @param data.tb  a SITS tibble
#' @return returns TRUE if data.tb has data.
#'
.sits_test_tibble <- function(data.tb) {
    ensurer::ensure_that(data.tb, !purrr::is_null(.),
                         err_desc = "input data not provided")
    ensurer::ensure_that(data.tb, NROW(.) > 0,
                         err_desc = "input data is empty")
    return(TRUE)
}


#' @title Create an empty tibble to store the results of classifications
#' @name .sits_tibble_classification
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Create an empty tibble to store the results of classification
#'
#' @return result.tb   a tibble to store the result of classifications
#'
.sits_tibble_classification <- function() {
    result.tb <- tibble::tibble(longitude   = double(),
                                latitude    = double(),
                                start_date  = as.Date(character()),
                                end_date    = as.Date(character()),
                                label       = character(),
                                coverage    = character(),
                                time_series = list(),
                                predicted   = list()
    )
    return(result.tb)
}

#' @title Create an empty tibble to store the results of CSV samples that coudl not be read
#' @name .sits_tibble_csv
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Create an empty tibble to store the results of classification
#'
#' @return result.tb   a tibble to store the result of classifications
#'
.sits_tibble_csv <- function() {
    result.tb <- tibble::tibble(longitude   = double(),
                                latitude    = double(),
                                start_date  = as.Date(character()),
                                end_date    = as.Date(character()),
                                label       = character()
    )
    return(result.tb)
}

#' @title Create an empty tibble to store the metadata of a coverage
#' @name .sits_tibble_coverage
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Create an empty tibble to store the metadata about a coverage
#'
#' @return coverage.tb   a tibble to store the metadata
#'
.sits_tibble_coverage <- function() {
    result.tb <- tibble::tibble(wtss.obj       = list(),
                                service        = character(),
                                name           = character(),
                                band_info      = list(),
                                start_date     = as.Date(character()),
                                end_date       = as.Date(character()),
                                timeline       = list(),
                                xmin           = double(),
                                xmax           = double(),
                                ymin           = double(),
                                ymax           = double(),
                                xres           = double(),
                                yres           = double(),
                                crs            = character()
    )
    class(result.tb) <- append(class(result.tb), "sits_tibble_coverage")
    return(result.tb)
}
#' @title Create an empty tibble to store the results of predictions
#' @name .sits_tibble_prediction
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Create a tibble to store the results of predictions
#' @param  data.tb         a tibble with the input data
#' @param  class_info.tb   a tibble with the information on classification
#' @param  pred.vec        the result of the classification (one class per row per interval)
#' @param  interval        the time interval between two classifications
#' @return predic.tb       a tibble to store the predictions
#'
.sits_tibble_prediction <- function(data.tb, class_info.tb, pred.vec, interval) {

    # retrieve the list of reference dates
    # this list is a global one and it is created based on the samples
    ref_dates.lst   <- class_info.tb$ref_dates[[1]]

    # retrieve the global timeline
    timeline_global <- class_info.tb$timeline[[1]]

    # size of prediction table
    nrows <- length(ref_dates.lst)

    predicted.lst <- list()

    class_idx <-  1

    data.tb %>%
        purrrlyr::by_row(function(row) {
            # get the timeline of the row
            timeline_row <- .sits_timeline(row)
            # the timeline of the row may be different from the global timeline
            # this happens when we are processing samples with different
            if (timeline_row[1] != timeline_global[1]) {
                # what is the reference start date?
                ref_start_date <- lubridate::as_date(row$start_date)
                # what is the reference end date?
                ref_end_date <- lubridate::as_date(row$end_date)
                # what are the reference dates to do the classification?
                ref_dates.lst <- .sits_match_timeline(timeline_row, ref_start_date, ref_end_date, interval)
            }
            pred_row.lst <- list()
            # store the classification results
            for (d in 1:length(ref_dates.lst)) {
                pred_row.lst[[d]] <- tibble::tibble(
                    from      = as.Date(ref_dates.lst[[d]][1]),
                    to        = as.Date(ref_dates.lst[[d]][2]),
                    distance  =  0.0,
                    class     = pred.vec[class_idx]
                    )
                class_idx  <<- class_idx + 1
            }

            predicted.lst[[length(predicted.lst) + 1]] <<- dplyr::bind_rows(pred_row.lst)
        })

    data.tb$predicted <- predicted.lst
    class(data.tb) <- append(class(data.tb), "sits_tibble_prediction")
    return(data.tb)
}
#' @title Create one line of metadata tibble to store the description of a spatio-temporal raster
#' @name .sits_tibble_raster
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description  This function creates one line of tibble containing the metadata for
#'               a set of spatio-temporal raster files.
#'
#' @param raster.obj     Valid Raster object (associated to filename)
#' @param band           Name of band (either raw or classified)
#' @param timeline       Timeline of data collection
#' @param scale_factor   Scale factor to correct data
#' @return raster.tb     A tibble for storing metadata about a spatio-temporal raster

.sits_tibble_raster <- function(raster.obj, band, timeline, scale_factor) {

    raster.tb <- tibble::tibble(
        r_obj           = list(raster.obj),
        ncols           = raster.obj@ncols,
        nrows           = raster.obj@nrows,
        band            = band,
        start_date      = lubridate::as_date(timeline[1]),
        end_date        = lubridate::as_date(timeline[length(timeline)]),
        timeline        = list(timeline),
        xmin            = raster.obj@extent@xmin,
        xmax            = raster.obj@extent@xmax,
        ymin            = raster.obj@extent@ymin,
        ymax            = raster.obj@extent@ymax,
        xres            = raster::xres(raster.obj),
        yres            = raster::yres(raster.obj),
        scale_factor    = scale_factor,
        crs             = raster.obj@crs@projargs,
        name            = raster.obj@file@name
    )
    class(raster.tb) <- append(class(raster.tb), "sits_tibble_raster")
    return(raster.tb)
}

