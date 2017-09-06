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

#' @title Sample a percentage of a time series
#' @name sits_labels_sample
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description takes a sits table with different labels and
#' returns a new table. For each label, this new table contains a percentage
#' of the total number of samples per label
#'
#' @param    data.tb      input SITS table
#' @param    samples.lst  a named list with the nambber of samples to be randomly picked from data input.
#'                        The labels are the list names, and the numbers of samples the list values.
#'                        If the size of a sample is greater than the number of actual samples of that label in data input, then
#'                        sampling procedure will be realized with replacement. If no entry for a given label is informed,
#'                        all samples of that label will be returned
#' @return   result.tb    the new SITS table with a fixed quantity of samples of informed labels and all other
#' @export
sits_sample_label <- function (data.tb, samples.lst){

    # get unique labels
    u_labels <- base::unique(data.tb$label)

    # get those labels not in samples.lst names
    samples.lst <- sits_labels_list(data.tb, samples.lst, function(lb) sum(data.tb$label == lb))

    # compose the resulting tibble and returns
    result.tb <- u_labels %>%
        purrr::map(function (lb){

            # filter only those rows with the same label
            sampled.tb <- data.tb %>%
                dplyr::filter(label == lb)

            # if lb is one of labels parameter, call dplyr sample function
            if (samples.lst[[lb]] != NROW(sampled.tb))
                sampled.tb <- sampled.tb %>%
                    dplyr::sample_n(size = samples.lst[[lb]],
                                    replace = base::ifelse(samples.lst[[lb]] > NROW(.), TRUE, FALSE))
            return(sampled.tb)
        }) %>%
        dplyr::bind_rows()
    return (result.tb)
}

#' @title relabels a sits tibble
#' @name sits_relabel
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
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
    .sits_test_table (data.tb)

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
