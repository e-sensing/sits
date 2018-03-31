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

    purrr::pmap(list(data.tb$longitude, data.tb$latitude,
                     data.tb$label, data.tb$coverage, data.tb$time_series),
        function (long, lat, lab, cov, ts) {

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

                # save the resulting row in the output table
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
#' @param ts.tb         tibble with a time series (one or more bands)
#' @param fun           function with one parameter as input and a vector or list of vectors as output.
#' @param fun_index     function with one parameter as input and a Date vector as output.
#' @param bands_suffix  string informing the resulting bands name's suffix.
#' @return data.tb      sits tibble with same samples and the new bands
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
#' @description Split a SITS table into k groups, based on the label
#'
#' @param data.tb   SITS tibble to be partitioned
#' @param folds     number of folds
.sits_create_folds <- function(data.tb, folds = 5) {

    # verify if data.tb exists
    .sits_test_tibble(data.tb)

    # splits the data into k groups
    data.tb$folds <- caret::createFolds(data.tb$label, k = folds, returnTrain = FALSE, list = FALSE)

    return(data.tb)
}
#' @title Extract a subset of the data based on dates
#' @name .sits_extract
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description returns a vector containing the dates of a sits table
#'
#' @param  row.tb     SITS tibble
#' @param  start_date starting date of the time series segment
#' @param  end_date   end date of the time series segment
#' @return subset.tb  tibble in SITS format with the chosen subset
.sits_extract <- function(row.tb, start_date, end_date) {

    # create a tibble to store the results
    subset.tb <- sits_tibble()

    # filter the time series by start and end dates
    ts <- row.tb$time_series[[1]]
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
                                     coverage     = row.tb$coverage,
                                     time_series  = ts.lst)
    }
    return(subset.tb)
}
#' @title Apply a function over SITS bands.
#' @name .sits_fast_apply
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
#' Differently from `sits_apply` function, this function merge all time series before applying `fun` function.
#' Functions that depend on time sequence must use `sits_apply` function.
#'
#' @param data.tb       sits tibble
#' @param fun           function with one parameter as input and a vector or list of vectors as output.
#' @param fun_index     function with one parameter as input and a Date vector as output.
#' @param bands_suffix  string informing the resulting bands name's suffix.
#' @return data.tb      sits tibble with same samples and the new bands
.sits_fast_apply <- function(data.tb, fun, fun_index = function(index){ return(index) }, bands_suffix = "") {

    # verify if data.tb has values
    .sits_test_tibble (data.tb)

    # save columns name from the first time series tibble
    ts_cols <- names(data.tb$time_series[[1]])

    # unnest tibble
    data.tb <-
        data.tb %>%
        tidyr::unnest()

    # computes fun for all time series fields and substitutes the original time series data
    data.tb[,ts_cols] <-
        data.tb[,ts_cols[-1:0]] %>%
        purrr::map(fun) %>%
        dplyr::bind_cols()

    # computes fun_index for all time series fields and substitutes the original time series data
    data.tb[,ts_cols[1]] <-
        data.tb[,ts_cols[1]] %>%
        purrr::map(fun_index) %>%
        dplyr::bind_cols()

    # verifies if there is a suffix and updates the columns names
    if (nchar(bands_suffix) != 0)
        ts_cols[-1:0] <- paste0(ts_cols[-1:0], ".", bands_suffix)

    # append bands names' suffixes
    names(data.tb) <- c(names(data.tb)[1:(length(names(data.tb))-length(ts_cols))], ts_cols)

    # nest result and return
    data.tb <-
        data.tb %>%
        tidyr::nest(ts_cols, .key = "time_series")
    return(data.tb)
}

#' @title Group the contents of a sits tibble by different criteria
#' @name .sits_group_by
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description returns a sits table by compound the sits tables apply a function to a grouped SITS table
#'
#' @param data.tb      sits tibble
#' @param ...          one or more sits table field separated by commas that are used to group the data.
#'                     See `dplyr::group_by` help for more details.
#' @return result.tb   sits tibble with the selected bands
.sits_group_by <- function(data.tb, ...){

    # execute the group by function from dplyr
    result.tb <- data.tb %>%
        dplyr::group_by(...)

    # comply result with sits table format and return
    result.tb <- dplyr::bind_rows(list(sits_tibble(), result.tb))
    return(result.tb)
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

    names <- c("longitude", "latitude", "start_date", "end_date",
               "label", "coverage", "time_series")

    ensurer::ensure_that(data.tb, all(names %in% colnames(.)),
                         err_desc = "data input is not a valid SITS tibble")

    return(TRUE)
}

#' @title Create an empty tibble to store the results of CSV samples that could not be read
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
    result.tb <- tibble::tibble(r_objs          = list(),
                                name           = character(),
                                service        = character(),
                                product        = character(),
                                bands          = list(),
                                start_date     = as.Date(character()),
                                end_date       = as.Date(character()),
                                timeline       = list(),
                                nrows          = integer(),
                                ncols          = integer(),
                                xmin           = double(),
                                xmax           = double(),
                                ymin           = double(),
                                ymax           = double(),
                                xres           = double(),
                                yres           = double(),
                                crs            = character(),
                                file           = character()
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

    class_idx <-  1

    predicted.lst <- purrr::pmap(
        list(data.tb$start_date, data.tb$end_date, data.tb$time_series),
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
                ref_dates.lst <- sits_match_timeline(timeline_row, ref_start_date, ref_end_date, interval)
            }

            # store the classification results
            pred_row.lst <- ref_dates.lst %>%
                purrr::map(function(rd){
                    pred_row <- tibble::tibble(
                        from      = as.Date(rd[1]),
                        to        = as.Date(rd[2]),
                        distance  =  0.0,
                        class     = pred.vec[class_idx]
                    )
                    class_idx  <<- class_idx + 1
                    return(pred_row)
                })
            # transform the list into a tibble
            predicted.tb <- dplyr::bind_rows(pred_row.lst)
            return(predicted.tb)
        })

    data.tb$predicted <- predicted.lst

    return(data.tb)
}
