#' @title Returns the information about labels  of a sits tibble
#' @name sits_labels
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Returns the labels and its respective counting and frequency.
#'
#' @param data.tb     A tibble with time series data and metadata.
#' @return A tibble with the names of the labels and their absolute and relative frequency.
#'
#' @examples
#' # read a tibble with 400 samples of Cerrado and 346 samples of Pasture
#' data(cerrado_2classes)
#' # print the labels
#' sits_labels(cerrado_2classes)
#' @export
sits_labels <- function (data.tb) {
    # get frequency table
    data.vec <- table(data.tb$label)

    # compose output tibble containing labels, count and relative frequency columns
    result.tb <- tibble::as_tibble(list(label = names(data.vec),
                                        count = as.integer(data.vec),
                                        freq  = as.numeric(prop.table(data.vec))))
    return (result.tb)
}

#' @title Relabels a sits tibble
#' @name sits_relabel
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Given a sits tibble with a set of labels, and a conversion list
#' between the original labels and new labels, returns a new sits tibble whose labels are changed.
#'
#' @param  data.tb        A sits tibble.
#' @param  conv.lst       A named list used to convert labels to a new value. Actual labels must be the names of the conv.lst elements.
#'                        an empty list produces no difference.
#' @return A new sits tibble with modified labels.
#'
#' @examples
#' \donttest{
#' # Read a set of time series with information on deforestation
#' data(prodes_226_064)
#' # Print the labels
#' sits_labels(prodes_226_064)
#' # Create a conversion list.
#' # Three classes will be converted to "NonForest".
#' conv.lst = list(Deforestation_2014 = "NonForest",
#'                 Deforestation_2015 = "NonForest",
#'                 Pasture = "NonForest")
#' # relabel the data
#' new_data.tb <- sits_relabel(prodes_226_064, conv.lst)
#' # show the new labels
#' sits_labels(new_data.tb)
#' }
#' @export
sits_relabel <- function(data.tb, conv.lst = list()){
    #does the input data exist?
    .sits_test_tibble (data.tb)

    ensurer::ensure_that(conv.lst, !purrr::is_null(.),
                         err_desc = "sits_relabel: conversion list not provided")

    # prepare result tibble
    result.tb <- data.tb

    if (length(conv.lst) > 0){
        # get those labels not in conv.lst names
        conv.lst <- .sits_labels_list(data.tb, conv.lst)

        # convert labels and return
        result.tb$label <- as.character(conv.lst[result.tb$label])
    }
    return (result.tb)
}

#' @title Sits labels processing function
#' @name .sits_labels_list
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Completes list.lst list as a named list (names are unique labels from data.tb) according
#'              to a given function that receives each label as an argument.
#'
#' @param  data.tb     A sits tibble.
#' @param  list.lst    Any named list whose names are unique labels from data input. Non-informed labels will be completed
#'                     according to fun_label function.
#' @param  fun_label   A function that will be executed for each label non listed in list.lst parameter. The result of
#'                     the function is used as list.lst value for the respective label.
#' @return A list whose values non informed in list.lst is filled by fun_label for each unique label in data.tb.
.sits_labels_list <- function(data.tb, list.lst = list(), fun_label = function(lb) lb) {
    # verify if data.tb has data
    .sits_test_tibble (data.tb)

    # get unique labels
    u_labels <- base::unique(data.tb$label)

    # prepare result
    result.lst <- list.lst

    # get non listed labels in list.lst
    non_listed_values <- !(u_labels %in% names(list.lst))

    # generate entries to those labels not listed in list.lst
    if (any(non_listed_values)){
        # call fun_label for each label as an argument
        identity.lst <- u_labels[non_listed_values] %>%
            purrr::map(fun_label)

        # name resulting list
        names(identity.lst) <- u_labels[non_listed_values]

        # concat with list.lst
        result.lst <- c(result.lst, identity.lst)
    }
    return(result.lst)
}
