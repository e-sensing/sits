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
#' @return table  a tibble in SITS format
#' @export

sits_table <- function () {
     df <- data.frame(longitude   = double(),
                      latitude    = double (),
                      start_date  = as.Date(character()),
                      end_date    = as.Date(character()),
                      label       = character(),
                      coverage    = character(),
                      stringsAsFactors = FALSE
     )
     tb <- tibble::as_tibble (df)
     tb <- tibble::add_column (tb, time_series = list())
     class (tb) <- append (class(tb), "sits_table")
     return (tb)
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
#' @param data.tb     a valid sits table
#' @return names      a string vector with the names of the bands
#' @export
#'
sits_bands <- function (data.tb) {
     names <- data.tb[1,]$time_series %>%
          data.frame() %>%
          colnames() %>%
          . [2:length(.)]
     return (names)
}

#' @title Cross join bands of two satellite image time series
#' @name sits_cross
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Cross-join two SITS tables with the same spatio-temporal references. To cross two series' bands,
#' we consider that they contain different attributes (bands) but refer to the same coverage.
#' We make no assumptions about their spatio-temporal location.
#' This function is useful to create different bands of clusters centroids time series.
#' For example, one may want to put evi and ndvi bands centroids together in an cross joined fashion in order to
#' generate patterns combinations.
#'
#' @param sits1.tb  the first SITS table in wich entries will be crossed with ts2 entries
#' @param sits2.tb  the second SITS table entries
#' @return new.tb    a cross-joined SITS tibble with a nested set of time series
#' @export
#'
sits_cross <-  function(sits1.tb, sits2.tb) {
     # are the names of the bands different?
     ensurer::ensure_that(sits1.tb, (length(intersect(sits_bands(.), sits_bands(sits2.tb))) == 0),
                          err_desc = "sits_cross: cannot cross join two sits tables with bands with the same names")

     # if some parameter is empty returns the another one
     if (nrow(sits1.tb) == 0)
          return (sits2.tb)
     if (nrow(sits2.tb) == 0)
          return (sits1.tb)

     #first, add `cross_join` field in order to proceed with dplyr::inner_join (see bellow)
     sits1.data <- sits1.tb %>%
          dplyr::mutate (cross_join = 0)

     # second, select only time_series data and add `cross_join` field
     sits2.data <- sits2.tb %>%
          dplyr::select(time_series) %>%
          dplyr::mutate(cross_join = 0)

     # third, from time_series' fields of second SITS table removes the Index (dates) (because we already have one Index in sits1.data)
     sits2.data$time_series <- sits2.data$time_series %>%
          purrr::map(function(ts) {
               ts %>% dplyr::select(-Index)
          })

     # fourth, do a cross join, removes `cross_join` field, and adds a `cluster_id` field to distinct each cluster
     # (this will be use to nest time_series in crossed.tb -- see bellow)
     crossed.tb <- sits1.data %>%
          dplyr::inner_join(sits2.data, by='cross_join') %>%
          dplyr::select(-cross_join) %>%
          dplyr::mutate(cluster_id = 1:nrow(.))

     # finally, proceeds with unnesting, nesting, removing `cluster_id` field, erasing latitude and longitude metadata, and update labels' values (subclass)
     crossed.tb <- tidyr::unnest(crossed.tb) %>%
          tidyr::nest(-latitude, -longitude, -start_date, -end_date, -label, -coverage, -cluster_id, .key = "time_series") %>%
          dplyr::select(-cluster_id) %>%
          dplyr::mutate(latitude = 0.0, longitude = 0.0, label = paste(.$label, ".", as.character(1:nrow(.)), sep = ""))

     # we have a sits tibble with all cross-joined bands centroids with subclass labels
     return(crossed.tb)
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

#' @title returns the labels' count of a sits table
#' @name sits_labels
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description  returns the labels and its respective counting and frequency.
#'
#' @param data.tb     a valid sits table
#' @return result.tb  a tibble with the names of the labels and its absolute and relative frequency
#' @export
#'
sits_labels <- function (data.tb) {

     # verify if there is original_label column. If not exists initialize it with empty string.
     if (!any("original_label" %in% names(data.tb)))
          data.tb$original_label <- ""

     # verify if there is n_members column. If not exists initialize it with ones.
     if (!any("n_members" %in% names(data.tb)))
          data.tb$n_members <- 1

     # compute frequency (absolute and relative)
     result.tb <- data.tb %>%
          dplyr::group_by(original_label, label) %>%
          dplyr::summarize(count = sum(n_members, na.rm = TRUE)) %>%
          dplyr::mutate(total = sum(count, na.rm = TRUE), frac = count / sum(count, na.rm = TRUE)) %>%
          dplyr::ungroup() %>%
          dplyr::select(label, count, original_label, total, frac)
     return (result.tb)
}

#' @title Sample a percentage of a time series
#' @name sits_labels_sample
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description takes a sits table with different labels and
#' returns a new table. For each label, this new table contains a percentage
#' of the total number of samples per label
#'
#' @param    data.tb    tibble - input SITS table
#' @param    frac       fraction (value between 0 and 1) of samples of each label to be saved.
#' @return   result.tb   tibble - the new SITS table with a fixed percentage of samples per class
#' @export
sits_labels_sample <- function (data.tb, frac = 0.1){

     result.tb <- sits_table()
     # how many different labels are there?
     labels <- dplyr::distinct (data.tb, label)$label

     labels %>%
          purrr::map(function (lb){
               # filter only those rows with the same label
               frac.tb <- data.tb %>%
                    dplyr::filter (label == lb) %>%
                    dplyr::sample_frac (size = frac)
               result.tb <<- dplyr::bind_rows(result.tb, frac.tb)
          })

     return (result.tb)
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
#' @param sits1.tb  the first SITS table to be merged
#' @param sits2.tb  the second SITS table to be merged
#' @return new.tb    a merged SITS tibble with a nested set of time series
#' @export
sits_merge <-  function(sits1.tb, sits2.tb) {

     # are the names of the bands different?
     ensurer::ensure_that(sits1.tb, !(TRUE %in% (sits_bands(.) %in% sits_bands(sits2.tb))),
                           err_desc = "sits_merge: cannot merge two sits tables with bands with the same names")

     # if some parameter is empty returns the another one
     if (nrow(sits1.tb) == 0)
          return (sits2.tb)
     if (nrow(sits2.tb) == 0)
          return (sits1.tb)

     # merge the time series
     merge_one <-  function (ts1, ts2) {
          ts3 <- dplyr::left_join (ts1, ts2, by = "Index")
     }
     # first, select the metadata columns
     merged.tb <- sits1.tb %>%
          dplyr::select (-time_series)

     # then merge the data sets
     sits1.data <- sits1.tb$time_series
     sits2.data <- sits2.tb$time_series
     # join the attributes and values using zoo merge
     merged.tb$time_series <- purrr::map2 (sits1.data, sits2.data, merge_one)

     return (merged.tb)
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
#' @return   proned.tb    tibble - the converted SITS table
#' @export
#'
sits_prune <- function (data.tb, min_interval = "349 days", max_interval = "365 days") {
     ensurer::ensure_that (data.tb, !purrr::is_null(.),
                           err_desc = "sits_prune: input data not provided")

     proned.tb <- sits_table()
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
                    proned.tb <<- dplyr::bind_rows(proned.tb, row)

               # update progress bar
               i <<- i + 1
               utils::setTxtProgressBar(progress_bar, i)
     })

     close(progress_bar)

     if (nrow(discarded.tb) > 0){
          message("The following sample(s) has(have) been discarded:\n")
          print(tibble::as_tibble(discarded.tb))
     }
     return (proned.tb)
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
#' @param data.tb    a sits table with the time series of the selected bands
#' @param bands      a vector of bands
#' @return table  a tibble in SITS format with the selected bands
#' @export
sits_select <- function (data.tb, bands) {
     # create a new table to store the result
     new.tb <- sits_table()
     # select the metadata attributes from the input table
     new.tb <- dplyr::select (data.tb, longitude, latitude, start_date, end_date, label, coverage)
     # select the chosen bands for the time series
     new.tb$time_series <- data.tb$time_series %>%
          purrr::map (function (ts) ts <- ts [, c("Index", bands)])
     # return the result
     return (new.tb)
}
#' @title Get the significant labeled time series among all others labels.
#' @name sits_significant_labels
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Given a SITS table, select only those whose time series
#' exceed a minimum percentage
#'
#' @param  data.tb        a SITS table with the data to be extracted
#' @param  min_label_frac a decimal between 0 and 1. The minimum percentagem of valid cluster members,
#' with reference to the total number of samples.
#' @return result.tb      a SITS table with the filtered data.
#' @export
sits_significant_labels <- function (data.tb, min_label_frac = 0.05) {
     # check valid min_clu_perc
     ensurer::ensure_that(min_label_frac, . >= 0.0 && . <= 1.0,
                          err_desc = "sits_significant_labels: invalid min_label_frac value. Value must be between 0 and 1.")

     sig_labels <- sits_labels(data.tb) %>%
          dplyr::filter(frac >= min_label_frac) %>% .$label

     result.tb <- data.tb %>%
          dplyr::filter(label %in% sig_labels)

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


#' @title Create a sits table to store the result of TWDTW classification
#' @name sits_table_result
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#'
#' @description A sits table is a tibble with pre-defined columns that
#' has the metadata and data for each time series.
#' To include the results of the classification, this basic structure is extended with two new
#' columns: distances (a list of distances to each class for each interval) and matches
#' (the output of the TWDTW classifier). The resulting columns will be
#' <longitude, latitude, start_date, end_date, label, coverage, time_series, distances, matches>.
#'
#' @references Please see the documentation of the dtwSat package
#'
#' @return table  a tibble in SITS format
#' @family   STIS table functions
#' @export

sits_table_result <- function () {
     df <- data.frame(longitude   = double(),
                      latitude    = double (),
                      start_date  = as.Date(character()),
                      end_date    = as.Date(character()),
                      label       = character(),
                      coverage    = character(),
                      stringsAsFactors = FALSE
     )
     tb <- tibble::as_tibble (df)
     tb <- tibble::add_column (tb, time_series = list())
     tb <- tibble::add_column (tb, matches    = list())
     class (tb) <- append (class(tb), "sits_table_result")
     return (tb)
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

