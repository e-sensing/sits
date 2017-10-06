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

    # get frequency table
    data.vec <- table(data.tb$label)

    # compose output tibble containing labels, count and relative frequency columns
    result.tb <- tibble::as_tibble(list(label = names(data.vec),
                                        count = as.integer(data.vec),
                                        freq  = as.numeric(prop.table(data.vec))))
    return (result.tb)
}
#' @title relabels a sits tibble
#' @name sits_relabel
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Given a SITS tibble with a set of labels, and a conversion list
#' between the original labels and new labels, returns a new SITS tibble whose labels are changed.
#'
#' @param  data.tb        a SITS tibble
#' @param  conv.lst       a named list used to convert labels to a new value. Actual labels must be the names of the conv.lst elements.
#'                        An empty list produces no difference.
#' @return result.tb      an assessment of validation
#' @export
sits_relabel <- function (data.tb, conv.lst = list()){

    #does the input data exist?
    .sits_test_tibble (data.tb)

    ensurer::ensure_that(conv.lst, !purrr::is_null(.),
                         err_desc = "sits_relabel: conversion list not provided")

    # prepare result tibble
    result.tb <- data.tb

    if (length(conv.lst) > 0){

        # get those labels not in conv.lst names
        conv.lst <- sits_labels_list(data.tb, conv.lst)

        # convert labels and return
        result.tb$label <- as.character(conv.lst[result.tb$label])
    }
    return (result.tb)
}
#' @title SITS labels processing function
#' @name sits_labels_list
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description completes list.lst list as a named list (names are unique labels from data.tb) according
#'              to a given function that receives each label as an argument.
#'
#' @param  data.tb     a SITS tibble
#' @param  list.lst    any named list whose names are unique labels from data input. Non-informed labels will be completed
#'                     according to fun_label function.
#' @param  fun_label   a function that will be executed for each label non listed in list.lst parameter. The result of
#'                     the function is used as list.lst value for the respective label.
#' @return result.lst  a list whose values non informed in list.lst is filled by fun_label for each unique label in data.tb.
#' @export
sits_labels_list <- function(data.tb, list.lst = list(), fun_label = function(lb) lb) {

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
