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

#' @title returns the labels' count of a sits table
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

    # get unique labels and returns
    result.vec <- base::unique(data.tb$label)

    return (result.vec)
}

#' @title names of the labels of sits table
#' @name `sits_labels<-`
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description  set the names of the labels of a given sits table
#'
#' @param data.tb      a valid sits table
#' @param value        string vector with the new label names
#' @return invisible(data.tb) the input data invisible
#' @export
#'
`sits_labels<-` <- function(data.tb, value){

    # get actual labels
    labels.vec <-  sits_labels(data.tb)

    # verify if the number of labels informed is the same as the actual number of labels in input data
    ensurer::ensure_that(value, length(.) == length(labels.vec),
                         err_desc = "sits_labels: labels in data input and informed label names have different lengths.")

    # substitute old values on labels vector
    ref_label <- value
    names(ref_label) <- labels.vec

    # proceed rename and return invisible
    data.tb$label <- ref_label[data.tb$label]
    invisible(data.tb)
}

#' @title Filter bands on a SITS table
#' @name sits_filter
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description returns a sits table with the filtered bands
#'
#' @param data.tb      a sits table with the time series of the selected bands
#' @param ...          `name=value` pairs expressions, where `name` is any SITS column name.
#'                     See `dplyr::filter` help for more details.
#' @return result.tb   a tibble in SITS format with the selected bands
#' @export
sits_filter <- function (data.tb, ...){

    # verify if data.tb has values
    .sits_test_table(data.tb)

    # select the chosen bands for the time series
    data.tb <- dplyr::filter(data.tb, ...)
    return (data.tb)
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
        dplyr::do(. %>% fun())

    # comply result with sits table format and return
    result.tb <- dplyr::bind_rows(list(sits_table(), result.tb))
    return (result.tb)
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
#'
#' @param  data.tb    input SITS table
#' @param  ...        one or more sits table field separated by commas that are used to group the data.
#'                    See `dplyr::group_by` help for more details.
#' @param  n          the quantity of samples to pick from a given group of data.
#' @param  frac       the percentage of samples to pick from a given group of data.
#' @return result.tb  the new SITS table with a fixed quantity of samples of informed labels and all other
#' @export
sits_sample <- function (data.tb, ..., n = NULL, frac = NULL){

    # verify if data.tb is empty
    .sits_test_table(data.tb)

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
    result.tb <- data.tb %>%
        sits_foreach(label, sampling_fun)
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
