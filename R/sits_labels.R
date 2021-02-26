#' @title Returns the information about labels of a data set (tibble or cube)
#' @name sits_labels
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description  Finds labels in a sits tibble or data cube
#'               For details see:
#' \itemize{
#'  \item{"time series": }{see \code{\link{sits_labels.sits}}}
#'  \item{"data cube": }{see \code{\link{sits_labels.cube}}}
#' }
#'
#' @param data      Valid sits tibble (time series or a cube)
#' @return A string vector with the labels.
#'
#' @export
sits_labels <- function(data) {
    # get the meta-type (sits or cube)
    data <- .sits_config_data_meta_type(data)

    UseMethod("sits_labels", data)
}
#'
#' @title Returns the information about labels of a set of time series
#' @name sits_labels.sits
#' @description Returns the labels and its respective counting and frequency.
#'
#' @param data     A tibble with time series data and metadata.
#' @return         A tibble with the names of the labels and
#'                 their absolute and relative frequency.
#'
#' @examples
#' # read a tibble with 400 samples of Cerrado and 346 samples of Pasture
#' data(cerrado_2classes)
#' # print the labels
#' sits_labels(cerrado_2classes)
#' @export
sits_labels.sits <- function(data) {
    # backward compatibility
    data <- .sits_tibble_rename(data)

    # get frequency table
    data_labels <- table(data$label)

    # compose tibble containing labels, count and relative frequency columns
    result <- tibble::as_tibble(list(
        label = names(data_labels),
        count = as.integer(data_labels),
        prop = as.numeric(prop.table(data_labels))
    ))
    return(result)
}

#'
#' @title Returns the information about labels of a data cube
#' @name sits_labels.cube
#' @description Returns the labels and its respective counting and frequency.
#'
#' @param data     A data cube
#' @return         A list of labels
#'
#' @examples
#' \donttest{
#' # Classify a raster file with 23 instances for one year
#' ndvi_file <- c(system.file("extdata/raster/mod13q1/sinop-ndvi-2014.tif",
#' package = "sits"))
#'
#' # create a data cube based on the information about the files
#' sinop_2014 <- sits_cube(
#'     type = "BRICK",
#'     name = "sinop-2014",
#'     timeline = timeline_2013_2014,
#'     satellite = "TERRA",
#'     sensor = "MODIS",
#'     bands = c("ndvi"),
#'     files = c(ndvi_file)
#' )
#'
#' # select band "NDVI"
#' samples_ndvi <- sits_select(samples_mt_4bands, bands = "NDVI")
#'
#' # select a random forest model
#' rfor_model <- sits_train(samples_ndvi,
#'               ml_method = sits_rfor(num_trees = 300))
#'
#' # classify the raster image
#' sinop_probs <- sits_classify(sinop_2014,
#'     ml_model = rfor_model,
#'     output_dir = tempdir(),
#'     memsize = 4, multicores = 2
#' )
#'
#' # label the classified image
#' sinop_label <- sits_label_classification(sinop_probs, output_dir = tempdir())
#'
#' # return the labels
#' sits_labels(sinop_label)
#' }
#' @export
sits_labels.cube <- function(data) {
    return(data[1, ]$labels[[1]])
}

#'
#' @title Returns the information about labels of a predicted tibble
#' @name sits_labels.predicted
#' @description Returns the labels and its respective counting and frequency.
#'
#' @param data     A data cube
#' @return         A tibble with the names of the labels and
#'                 their absolute and relative frequency.
#'
#' @export
sits_labels.predicted <- function(data) {
    labels <- sits_labels.sits(data)
    return(labels)
}

#'
#' @title Returns the information about labels of patterns tibble
#' @name sits_labels.patterns
#' @description Returns the labels and its respective counting and frequency.
#'
#' @param data     A data cube
#' @return         A tibble with the names of the labels and
#'                 their absolute and relative frequency.
#'
#' @export
sits_labels.patterns <- function(data) {
    return(data$label)
}

#' @title Relabels a sits tibble
#' @name sits_relabel
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
#' @export
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
    if ("sits" %in% class(data)) {
        # backward compatibility
        data <- .sits_tibble_rename(data)
        # verify if data is correct
        .sits_test_tibble(data)
        # get unique labels
        u_labels <- base::unique(data$label)
    }
    else {
        if ("pred_ref" %in% class(data)) {
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
