#' @title Use time series values from a sits tibble as distances for training patterns
#' @name sits_distances
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function allows using a set of labelled time series as
#' input to the machine learning models. The values used as input for the training methods
#' are computed by the function defined by the parameter dist_method
#'
#' @param  data.tb        a SITS tibble with original data
#' @param  dist_method    method used to compute the distances
#' @return distances.tb  a tibble where columns have the reference label and the time series values as distances
#'
#' @examples
#' # Retrieve the set of samples for the Cerrado and Pasture classes
#' data(cerrado_2classes)
#' # estimate distances from the data
#' distances.tb <- sits_distances(cerrado_2classes)
#
#' @export
sits_distances <- function (data.tb, dist_method = sits_distances_from_data()){

    # is the input data a valid SITS tibble?
    .sits_test_tibble(data.tb)
    # is the train method a function?
    ensurer::ensure_that(dist_method, class(.) == "function", err_desc = "sits_distances: dist_method is not a valid function")

    # calculate distances
    distances.tb <- dist_method (data.tb)

    return (distances.tb)
}

#' @title Use time series values from a sits tibble as distances for training patterns
#' @name sits_distances_from_data
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function allows using a set of labelled time series as
#' input to the machine learning models. Instead of first estimating a set
#' of idealised patterns and then computing distances from these patterns,
#' the attributes used to train the model are the series themselves.
#' This function then extracts the time series from a SITS tibble and
#' "spreads" them in time to produce a tibble with distances.
#'
#' @param  data.tb        a SITS tibble with original data
#' @param  shift          Adjustment value to avoid negative pixel vales
#' @return distances.tb  a tibble where columns have the reference label and the time series values as distances
#'
#' @examples
#' # Retrieve the set of samples for the Cerrado and Pasture classes
#' data(cerrado_2classes)
#' # estimate distances from the data
#' distances.tb <- sits_distances_from_data(cerrado_2classes)
#'
#' @export
sits_distances_from_data <- function(data.tb = NULL, shift = 3.0){

    result_fun <- function(input.tb){

        # create a list with the time series transposed from columns to rows
        ts.lst <- input.tb$time_series %>%
            purrr::map (function (ts){
                as.data.frame(t(unlist(ts[-1])))
            })
        # bind the lists of time series together
        dist.tb <- data.table::rbindlist(ts.lst)
        # shift the values of the time series to avoid negative numbers
        dist.tb <- dist.tb + shift
        # create a data frame with the first two columns for training
        distances.tb <- data.frame("original_row" = 1:nrow(input.tb), "reference" = input.tb$label)
        # join the two references columns with the data values
        distances.tb <- cbind(distances.tb, dist.tb)
    }
    result <- .sits_factory_function (data.tb, result_fun)
}
#' @title Use time series values from a sits tibble as distances for training patterns
#' @name sits_distances_normalized
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function allows using a set of labelled time series as
#' input to the machine learning models. Instead of first estimating a set
#' of idealised patterns and then computing distances from these patterns,
#' the attributes used to train the model are the series themselves.
#' This function then extracts the time series from a SITS tibble and
#' "spreads" them in time to produce a tibble with distances.
#'
#' @param  data.tb        a SITS tibble with original data
#' @param  method         Normalizing method. Available are: "center": Subtract mean.
#'                        "scale": Divide by standard deviation. "standardize": Center and scale.
#'                        "range": Scale to a given range.
#' @param margin          1 = rows, 2 = cols
#' @return distances.tb  a tibble where columns have the reference label and the time series values as distances
#'
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Cerrado and Pasture classes
#' data(cerrado_2classes)
#' # estimate distances from the data
#' distances.tb <- sits_distances_normalized(cerrado_2classes)
#' }
#' @export
sits_distances_normalized <- function(data.tb = NULL, method = "center", margin = 2){

    result_fun <- function(input.tb){

        # create a list with the time series transposed from columns to rows
        ts.lst <- input.tb$time_series %>%
            purrr::map (function (ts){
                as.data.frame(t(unlist(ts[-1])))
            })
        # bind the lists of time series together
        dist.tb <- data.table::rbindlist(ts.lst)
        # shift the values of the time series to avoid negative numbers
        dist.tb <- BBmisc::normalize (dist.tb, method = method, margin = margin)
        # create a data frame with the first two columns for training
        distances.tb <- data.frame("original_row" = 1:nrow(input.tb), "reference" = input.tb$label)
        # join the two references columns with the data values
        distances.tb <- cbind(distances.tb, dist.tb)
    }
    result <- .sits_factory_function (data.tb, result_fun)
}

.normalize <- function (x){(x-min(x))/(max(x)-min(x))}
