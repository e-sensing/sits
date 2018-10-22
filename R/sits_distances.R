#' @title Use time series values from a sits tibble as distances for training patterns
#' @name sits_distances
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function allows using a set of labelled time series as
#' input to the machine learning models. The attributes used to train the model
#' are the series themselves. This function extracts the time series from a sits tibble
#' and "spreads" them in time to produce a tibble with distances. It needs an additional
#' value that adjusts the values of the time series to meet the criteria of
#' machine learning methods, since most ML methods do not allow for negative data.
#'
#' @param  data.tb       A tibble with time series data and metadata.
#' @return A data.table where columns have the reference label and the time series values as distances.
#'
#' @examples
#' # Retrieve the set of samples for the Cerrado and Pasture classes
#' data(cerrado_2classes)
#' # estimate distances from the data
#' distances <- sits_distances(cerrado_2classes)
#' @export
sits_distances <- function(data.tb) {
    # create a list with the time series transposed from columns to rows
    ts.lst <- data.tb$time_series %>%
        purrr::map(function(ts){
            as.data.frame(t(unlist(ts[-1])))
        })
    # bind the lists of time series together
    dist_DT <- data.table::rbindlist(ts.lst)
    # create a data frame with the first two columns for training
    distances_DT <- data.table::data.table("original_row" = 1:nrow(data.tb), "reference" = data.tb$label)
    # join the two references columns with the data values
    distances_DT <- data.table::as.data.table(cbind(distances_DT, dist_DT))

    return(distances_DT)
}

#' @title Sample a percentage of a time series distance matrix
#' @name .sits_sample_distances
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Takes a sits table with different labels and
#'              returns a new table. For a given field as a group criterion, this new table contains a given number or percentage
#'              of the total number of samples per group. Parameter n indicantes the number of random samples with reposition.
#'              Parameter frac indicates a fraction of random samples without reposition. If frac > 1, no sampling is done.
#'
#' @param  distances_DT    A data.table object with the distances associated to a time series.
#' @param  frac            Percentage of samples to pick from a given group of data.
#' @return A data.table with a fixed quantity of samples of informed labels and all other.
.sits_sample_distances <- function(distances_DT, frac){
    # compute sampling
    result_DT <- distances_DT[, .SD[sample(.N, round(frac*.N))], by = reference]

    return(result_DT)
}
