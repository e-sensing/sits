#' @title Returns the information about labels of a data set (tibble or cube)
#'
#' @name sits_labels
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description  Finds labels in a sits tibble or data cube
#'
#' @param data      Valid sits tibble (time series or a cube)
#'
#' @return A string vector with the labels.
#'
#' @examples
#' # read a tibble with 400 samples of Cerrado and 346 samples of Pasture
#' data(cerrado_2classes)
#' # print the labels
#' sits_labels(cerrado_2classes)
#'
#' @export
#'
sits_labels <- function(data) {

    # get the meta-type (sits or cube)
    data <- .sits_config_data_meta_type(data)

    UseMethod("sits_labels", data)
}

#' @export
#'
sits_labels.sits <- function(data) {

    return(sort(unique(data$label)))
}

#' @export
#'
sits_labels.cube <- function(data) {

    return(data$labels[[1]])
}

#' @export
#'
sits_labels.predicted <- function(data) {

    return(sits_labels.sits(data))
}

#' @export
#'
sits_labels.patterns <- function(data) {

    return(data$label)
}

#' @title Relabels a sits tibble
#'
#' @name sits_relabel
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Given a sits tibble with a set of labels, and a conversion list
#' between the original labels and new labels,
#' returns a new sits tibble whose labels are changed.
#'
#' @param  data           A sits tibble.
#' @param  conv_lst       A named list used to convert labels to a new value.
#'                        Actual labels must be the names of the list elements.
#'                        An empty list produces no difference.
#' @return                A new sits tibble with modified labels.
#'
#' @examples
#' # Read a set of time series with information on deforestation
#' data(samples_mt_4bands)
#' # Print the labels
#' sits_labels(samples_mt_4bands)
#' # Create a conversion list.
#' # Three classes will be converted to "Cropland".
#' conv_lst <- list(
#'     Soy_Corn = "Cropland",
#'     Soy_Cotton = "Cropland",
#'     Soy_Fallow = "Cropland",
#'     Soy_Millet = "Cropland",
#'     Soy_Sunflower = "Cropland",
#'     Fallow_Cotton = "Cropland"
#' )
#' # relabel the data
#' new_data <- sits_relabel(samples_mt_4bands, conv_lst)
#' # show the new labels
#' sits_labels(new_data)
#'
#' @export
#'
sits_relabel <- function(data, conv_lst) {
    # backward compatibility
    data <- .sits_tibble_rename(data)

    # does the input data exist?
    .sits_test_tibble(data)

    # prepare result tibble
    result <- data

    if (length(conv_lst) > 0) {
        # get those labels not in conversion lst names
        conv_lst <- .sits_labels_list(data, conv_lst)

        # convert labels and return
        result$label <- as.character(conv_lst[result$label])
    }
    return(result)
}

#' @title Sits labels processing function
#' @name .sits_labels_list
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Completes list.lst list as a named list
#'              (names are unique labels from data) according
#'              to a given function that receives each label as an argument.
#'
#' @param  data        A sits tibble (or a pred_ref tibble)
#' @param  label_lst    Any named list whose names are unique labels
#'                     from data input. Non-informed labels will be completed
#'                     according to fun_label function.
#' @param  fun_label   A function that will be executed for each label
#'                     non listed in list.lst parameter. The result of
#'                      is used as list.lst value for the respective label.
#' @return             A list whose non informed values
#'                     are filled by fun_label for each unique label in data.
#'
.sits_labels_list <- function(data, label_lst,
                              fun_label = function(lb) lb) {

    # get labels for sits tibble
    if (inherits(data, "sits")) {
        # backward compatibility
        data <- .sits_tibble_rename(data)
        # verify if data is correct
        .sits_test_tibble(data)
        # get unique labels
        u_labels <- base::unique(data$label)
    }
    else {
        if (inherits(data, "pred_ref")) {
            # get labels for pred_ref tibble
            u_labels <- base::unique(data$reference)
        } else {
            stop("sits_labels_list - wrong class of input")
        }
    }

    # prepare result
    result <- label_lst

    # get non listed labels in list.lst
    non_listed_values <- !(u_labels %in% names(label_lst))

    # generate entries to those labels not listed in list.lst
    if (any(non_listed_values)) {
        # call fun_label for each label as an argument
        identity_lst <- u_labels[non_listed_values] %>%
            purrr::map(fun_label)

        # name resulting list
        names(identity_lst) <- u_labels[non_listed_values]

        # concat with results
        result <- c(result, identity_lst)
    }
    return(result)
}
