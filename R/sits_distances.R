#' @title Use time series values from a sits tibble as distances for training patterns
#' @name .sits_distances
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function allows using a set of labelled time series as
#' input to the machine learning models. The attributes used to train the model
#' are the series themselves. This function extracts the time series from a sits tibble
#' and "spreads" them in time to produce a tibble with distances.
#'
#' @param  data.tb       A tibble with time series data and metadata.
#' @return A data.table where columns have the reference label and the time series values as distances.
#'
.sits_distances <- function(data.tb) {
    # backward compatibility
    if ("coverage" %in% names(data.tb))
        data.tb <- .sits_tibble_rename(data.tb)

    # create a list with the time series transposed from columns to rows
    ts.lst <- data.tb$time_series %>%
        purrr::map(function(ts){
            as.data.frame(t(unlist(ts[-1])))
        })
    # bind the lists of time series together
    dist_DT <- data.table::rbindlist(ts.lst, use.names = FALSE)
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
#' @description Takes a sits tibble with different labels and
#'              returns a new tibble. For a given field as a group criterion, this new table contains a given number or percentage
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
