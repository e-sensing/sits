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
     tb <- tibble::add_column (tb, distances  = list())
     tb <- tibble::add_column (tb, matches    = list())
     class (tb) <- append (class(tb), "sits_table_result")
     return (tb)
}
#' @title Return the values of one band of a SITS table
#' @name sits_value_rows
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description this function returns only the values of a sits table (rowwise organized).
#' This function is useful to use packages such as ggplot and dtwclust that
#' require values that are rowwise organised
#'
#' @param  data.tb    a tibble in SITS format with time series for different bands
#' @param  bands      string - a group of bands whose values are to be extracted
#' @return table   a tibble in SITS format with values
#' @family   STIS table functions
#' @export

sits_values_rows <- function (data.tb, bands) {
     values.lst <- data.tb$time_series %>%
          purrr::map(function (ts) {
               data.matrix(dplyr::select(ts, dplyr::one_of(bands)))
          })
     bands_name <- paste0(bands, collapse = "$")
     names(values.lst) <- paste0(bands_name, ".", 0:(length(values.lst)-1))
     return (values.lst)
}

#' @title Return the values of one band of a SITS table colwise organised
#' @name sits_value_cols
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description returns a sits table with values only (colwise organised)
#'
#' @param  data.tb    a tibble in SITS format with time series for different bands
#' @param  band       string - a band whose values are to be extracted
#' @return table   a tibble  with values
#' @family STIS table functions
#' @export

sits_values_cols <- function (data.tb, band) {
     values <- data.tb$time_series %>%
          data.frame() %>%
          tibble::as_tibble() %>%
          dplyr::select (dplyr::starts_with (band))
     return (values)
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
#' @description returns a table containing the dates of a sits table (colwise organised)
#'
#'
#' @param  data.tb a tibble in SITS format with time series for different bands
#' @return table   a tibble in SITS format with values of time indexes
#' @export
sits_dates <- function (data.tb) {
     values <- data.tb$time_series %>%
          data.frame() %>%
          tibble::as_tibble() %>%
          dplyr::select (dplyr::starts_with ("Index"))
     return (values)
}

#' @title Aligns dates of time series to a reference date
#' @name sits_align
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description converts the time indexes of a sits table to a single reference year.
#' This function is useful to join many time series from different years to a single year,
#' which is required by methods that combine many time series, such as clustering methods.
#' The reference year is taken from the date of the start of the time series
#' available in the coverage.
#'
#' @param    data.tb    tibble - input SITS table (useful for chaining functions)
#' @param    ref_date   date   - a reference date where all series will start
#' @return   data1.tb   tibble - the converted SITS table (useful for chaining functions)
#' @export
#'
sits_align <- function (data.tb, ref_date) {
     ts <- data.tb$time_series
     # convert the time index to a reference year
     ts1 <- ts %>%
          purrr::map (function (t) {
               df <- as.data.frame(t)
               start_date <- as.Date(df[1,"Index"])
               if (abs (lubridate::yday(start_date) - lubridate::yday(ref_date)) > 2) {
                    print (start_date, ref_date)
                    message (paste ("sits_align: time series do not start at the same date"))
               }
               dplyr::mutate (t, Index = Index - lubridate::ymd(start_date) + lubridate::ymd (ref_date))
          })
     data.tb$time_series <- ts1
     return (data.tb)
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
     for (i in 1:nrow(locs)) {
          loc <- locs[i,]
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
                    for (i in 2:nrow(rows))  {
                         row <- rows[i,]
                         # adjust the start and end dates
                         if (row$start_date < start_date) start_date <- row$start_date
                         if (row$end_date   > end_date)   end_date   <- row$end_date
                         # get the time series and join it with the previous ones
                         t <- row$time_series[[1]]
                         time_series <- dplyr::bind_rows(time_series, t)
                    }
               }
               ts.lst <- tibble::lst()
               ts.lst[[1]] <- time_series
               out.tb <- tibble::add_row (out.tb,
                                  longitude    = long,
                                  latitude     = lat,
                                  start_date   = as.Date(start_date),
                                  end_date     = as.Date(end_date),
                                  label        = "NoClass",
                                  coverage     = rows[1,]$coverage,
                                  time_series  = ts.lst)
     }
     return (out.tb)
}

#' @title Sample a percentage of a time series
#' @name sits_label_perc
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description takes a sits table with different labels and
#' returns a new table. For each label, this new table contains a percentage
#' of the total number of samples per label
#'
#' @param    data.tb    tibble - input SITS table
#' @param    perc       percentagem of samples of each label to be saved
#' @return   data1.tb   tibble - the new SITS table with a fixed percentage of samples per class
#' @export
sits_label_perc <- function (data.tb, perc = 0.1){

     data1.tb <- sits_table()
     # how many different labels are there?
     labels <- dplyr::distinct (data.tb, label)

     for (i in 1:nrow(labels)) {
          # get the label name as a character
          lb <-  as.character (labels[i,1])

          # filter only those rows with the same label
          frac.tb <- data.tb %>%
               dplyr::filter (label == lb) %>%
               dplyr::sample_frac (size = perc)

          data1.tb <- dplyr::bind_rows(data1.tb, frac.tb)
     }
     return (data1.tb)
}


#' @title Sample a percentage of a time series
#' @name sits_time_interval
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description finds out the temporal interval of the time series of the data
#'
#' @param data.tb  a sits tibble
#' @return ndays   number of days covered by the time series
#' @export
sits_time_interval <-  function (data.tb){
     ensurer::ensure_that(data.tb, nrow(.) == 1,
                           err_dec = "sits_time_interval - works with one row at a time")
     ndays <-  (lubridate::as_date(data.tb[1,]$end_date) -
                     lubridate::as_date(data.tb[1,]$start_date))/lubridate::ddays(1)
     return (ndays)
}

