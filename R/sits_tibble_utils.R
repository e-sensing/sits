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
#' @title Find out the cuts in a long time to break it into intervals
#' @name .sits_find_cuts
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function produces a list of start and end points to cut the
#'              time series into a set of intervals
#'
#' @param data.tb               Input data set
#' @param start_date            Starting date of the distance matching between time series and patterns
#' @param end_date              End date of the distance matching between time series and patternss
#' @param interval              Period to match the data to the patterns
#' @return subset_dates.lst            the breaks that will be applied to the input data set
#'
.sits_find_cuts <- function (data.tb, start_date, end_date, interval = "12 month"){

    # ensure the input values exist
    .sits_test_tibble(data.tb)

    ensurer::ensure_that(data.tb, nrow(.) == 1, err_desc = ".sits_find_cuts: input data has more than one row")

    #find out what are the input start and end dates
    input_start_date <- lubridate::as_date(data.tb[1,]$start_date)
    input_end_date   <- lubridate::as_date(data.tb[1,]$end_date)

    #adjust the years, if required
    if (lubridate::year(input_start_date) != lubridate::year(start_date)){
        st_jday    <- lubridate::yday(start_date)
        date0      <- lubridate::as_date(paste0(as.character(lubridate::year(input_start_date)),"-01-01"))
        start_date <- lubridate::as_date(date0 + st_jday - 1)
    }

    if (lubridate::year(input_end_date) != lubridate::year(end_date)){
        ed_jday    <- lubridate::yday(end_date)
        date1      <- lubridate::as_date(paste0(as.character(lubridate::year(input_end_date)),"-01-01"))
        end_date   <- lubridate::as_date(date1 + ed_jday - 1)
    }

    breaks <- seq (from = as.Date (start_date), to = as.Date (end_date), by = interval)

    # if the last year of data is not included, include it
    if (lubridate::year(end_date) != lubridate::year(breaks[length(breaks)]))
        breaks[length(breaks) + 1 ] <- end_date

    #obtain the subset dates to break the input data set
    subset_dates.lst <- list()
    for (i in 1:(length(breaks) - 1))
        subset_dates.lst [[length(subset_dates.lst) + 1 ]] <- c(breaks[i], breaks[i+1])

    return (subset_dates.lst)
}

#' @title Prunes dates of time series to fit an interval
#' @name .sits_prune
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
#'
.sits_prune <- function (data.tb, min_interval = "349 days", max_interval = "365 days") {
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




