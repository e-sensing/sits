#' @title Add a new row to a SITS tibble
#' @name .sits_add_row
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Adds a row to a tibble, with suitable defaults
#'
#' @param data.tb         A SITS tibble
#' @param longitude       Longitude of the chosen location
#' @param latitude        Latitude of the chosen location
#' @param start_date      Start of the period
#' @param end_date        End of the period
#' @param label           Label to attach to the time series (optional)
#' @param coverage        The name of the coverage
#' @param time_series     List containing a time series (Index and band values)
#' @return data.tb        An updated SITS tibble
#'
.sits_add_row <- function (data.tb = NULL, longitude = 0.0, latitude = 0.0,
                           start_date = "1970-01-01", end_date = "1970-01-01",
                           label = "NoClass", coverage = "image", time_series = list()) {


    data.tb <- tibble::add_row (data.tb,
                                longitude    = longitude,
                                latitude     = latitude,
                                start_date   = as.Date(start_date),
                                end_date     = as.Date(end_date),
                                label        = label,
                                coverage     = coverage,
                                time_series  = time_series)

    return (data.tb)
}

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
.sits_align <- function (data.tb, ref_dates) {

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
#' @export
#'
.sits_break_ts <-  function (ts, ref_dates.lst){

    new_ts.lst <- list()
    ref_dates.lst %>%
        purrr::map(function (dates) {
            if (ts$Index[1] <= dates[1]) {
                if (ts$Index[length(ts$Index)] >= dates[2]){
                    ts_b <- ts %>% dplyr::filter (dplyr::between (.$Index, dates[1], dates[2]))
                    new_ts.lst[[length(new_ts.lst) + 1 ]] <<- ts_b
                }
            }
        })

    return (new_ts.lst)
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
.sits_create_folds <- function (data.tb, folds = 5) {

    # verify if data.tb exists
    .sits_test_tibble (data.tb)

    # splits the data into k groups
    data.tb$folds <- caret::createFolds(data.tb$label, k = folds, returnTrain = FALSE, list = FALSE)

    return (data.tb)
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
.sits_create_ts_list <- function (timeline, n) {

    ts.lst <- list()
    # create one time series per pixel
    # all time series share the same timeline
    for (i in 1:n) {
        ts.tb <- tibble::tibble (Index = timeline)
        ts.lst[[length(ts.lst) + 1 ]] <- ts.tb
        i <- i + 1
    }
    return (ts.lst)
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
.sits_extract <- function (row.tb, start_date, end_date) {

    # create a tibble to store the results
    subset.tb <- sits_tibble()

    # filter the time series by start and end dates
    sub.ts <- row.tb$time_series[[1]] %>%
        dplyr::filter (dplyr::between (.$Index, start_date, end_date))


    # store the subset of the time series in a list
    ts.lst <- tibble::lst()
    ts.lst[[1]] <- sub.ts
    # create a new row of the output tibble
    subset.tb <- tibble::add_row (subset.tb,
                                   longitude    = row.tb$longitude,
                                   latitude     = row.tb$latitude,
                                   start_date   = as.Date(start_date),
                                   end_date     = as.Date(end_date),
                                   label        = row.tb$label,
                                   coverage     = row.tb$coverage,
                                   time_series  = ts.lst)
    return (subset.tb)
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
.sits_extract_ts <- function (ts.tb, start_index, end_index) {


    # filter the time series by start and end dates
    subset.tb <- ts.tb %>%
        dplyr::filter (dplyr::between (.$Index, start_date, end_date))

    return (subset.tb)
}



