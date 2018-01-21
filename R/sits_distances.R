#' @title Use time series values from a sits tibble as distances for training patterns
#' @name sits_distances
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function allows using a set of labelled time series as
#' input to the machine learning models. The attributes used to train the model
#' are the series themselves. This function extracts the time series from a SITS tibble
#' and "spreads" them in time to produce a tibble with distances. It needs an additional
#' function that adjusts the values of the time series to meet the criteria of
#' machine learning methods, since most ML methods do not allow for negative data.
#'
#' @param  data.tb       SITS tibble with original data
#' @param  adj_fun       Adjustment function to be applied to the data
#' @return distances.tb  a tibble where columns have the reference label and the time series values as distances
#'
#' @examples
#' # Retrieve the set of samples for the Cerrado and Pasture classes
#' data(cerrado_2classes)
#' # estimate distances from the data
#' distances.tb <- sits_distances(cerrado_2classes)
#'
#' @export
sits_distances <- function(data.tb, adj_fun = sits_adjust()) {

    # create a list with the time series transposed from columns to rows
    ts.lst <- data.tb$time_series %>%
        purrr::map(function(ts){
            as.data.frame(t(unlist(ts[-1])))
        })
    # bind the lists of time series together
    dist.tb <- data.table::rbindlist(ts.lst)
    # apply a function to the data
    dist.tb <- adj_fun(dist.tb)
    # create a data frame with the first two columns for training
    distances.tb <- data.frame("original_row" = 1:nrow(data.tb), "reference" = data.tb$label)
    # join the two references columns with the data values
    distances.tb <- cbind(distances.tb, dist.tb)

    return(distances.tb)

}
#' @title Function for adjust time series distances used as training sets for machine learning
#' @name sits_adjust
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function provides the default adjustment of the values of the time series
#' used to train ML methods. Since most ML methods do not allow for negative data, the default
#' adjustment is to shift the data by a fixed value. The adjustment value is taken from
#' the configuration file (config.yml). See \code{\link[sits]{sits_config}} for more details
#' on how to change the configuration file.
#' This function should always be used in combination with \code{\link[sits]{sits_distances}}.
#'
#' @return adj_fun       Adjustment function to be applied to the data
#'
#' @examples
#' # get one time series
#' data(cerrado_2classes)
#' # compute the distance vector for the first time series
#' distances.tb <- sits_distances(cerrado_2classes[1,], adj_fun = sits_adjust())
#'
#' @export
sits_adjust <- function() {
    f1 <- function(x) {x + .sits_get_adjustment_shift()}
    return(f1)
}

#' @title Sample a percentage of a time series distance matrix
#' @name .sits_sample_distances
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description takes a sits table with different labels and
#'              returns a new table. For a given field as a group criterion, this new table contains a given number or percentage
#'              of the total number of samples per group. Parameter n indicantes the number of random samples with reposition.
#'              Parameter frac indicates a fraction of random samples without reposition. If frac > 1, no sampling is done.
#'
#' @param  distances.tb    data.table objects with the distances associated to a time series
#' @param  frac            ercentage of samples to pick from a given group of data.
#' @return result.tb       the new data.table with a fixed quantity of samples of informed labels and all other
.sits_sample_distances <- function(distances.tb, frac){


    # compute sampling
    result.tb <- data.table::data.table()
    references <- as.vector(unique(distances.tb$reference))
    references %>%
        purrr::map(function(r){
            tb_r <- dplyr::filter(distances.tb, reference == r)
            tb_s <- dplyr::sample_frac(tb_r, size = frac, replace = FALSE)
            result.tb <<- dplyr::bind_rows(result.tb, tb_s)
        })

    return(result.tb)
}
