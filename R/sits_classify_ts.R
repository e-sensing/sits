
#' @title Shows the predicted labels for a classified tibble
#' @name sits_show_prediction
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function takes a tibble with a classified time series
#' by a machine learning method and displays the result.
#'
#' @param  class    A SITS tibble that has been classified
#' @return returns a tibble with the columns "from", "to", "class"
#'
#' @examples
#' # Retrieve the samples for Mato Grosso
#' # select band "NDVI"
#' samples_ndvi <- sits_select(samples_modis_4bands, bands = "NDVI")
#' # select a random forest model
#' rfor_model <- sits_train(samples_ndvi, sits_rfor(num_trees = 200))
#' # classify the point
#' point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
#' point_class <- sits_classify(point_ndvi, rfor_model)
#' # show the prediction
#' sits_show_prediction(point_class)
#' @export
sits_show_prediction <- function(class) {

    # set caller to show in errors
    .check_set_caller("sits_show_prediction")

    .sits_tibble_test(class)

    .check_chr_within(
        x = .config_get("ts_predicted_cols"),
        within = names(class$predicted[[1]]),
        msg = "tibble has not been classified"
    )

    return(dplyr::select(
        dplyr::bind_rows(class$predicted),
        c("from", "to", "class")
    ))
}
#' @title Create an empty tibble to store the results of predictions
#' @name .sits_tibble_prediction
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Create a tibble to store the results of predictions.
#' @param  data             A tibble with the input data.
#' @param  class_info       A tibble with the information on classification.
#' @param  prediction       A matrix with the result of the classification
#'                          (one class per column and one row per interval).
#' @return A tibble storing the predictions.
.sits_tibble_prediction <- function(data, class_info, prediction) {

    # this list is a global one and it is created based on the samples
    ref_dates_lst <- class_info$ref_dates[[1]]
    # retrieve the global timeline
    timeline_global <- class_info$timeline[[1]]

    # get the labels of the data
    labels <- class_info$labels[[1]]
    n_labels <- length(labels)
    # create a named vector with integers match the class labels
    int_labels <- c(1:n_labels)
    names(int_labels) <- labels

    # compute prediction vector
    pred <- names(int_labels[max.col(prediction)])

    idx <- 1

    data_pred <- slider::slide_dfr(data, function(row) {

            # get the timeline of the row
            timeline_row <- lubridate::as_date(row$time_series[[1]]$Index)

            # the timeline of the row may differ from the global timeline
            # this happens when we are processing samples with different dates
            if (timeline_row[1] != timeline_global[1]) {
                # what are the reference dates to do the classification?
                ref_dates_lst <- .sits_timeline_match(
                    timeline = timeline_row,
                    ref_start_date = lubridate::as_date(row$start_date),
                    ref_end_date = lubridate::as_date(row$end_date),
                    num_samples = nrow(row$time_series[[1]])
                )
            }

            # store the classification results
            pred_sample <- ref_dates_lst %>%
                purrr::map_dfr(function(rd) {
                    probs_date <- rbind.data.frame(prediction[idx,])
                    names(probs_date) <- names(prediction[idx,])
                    pred_date <- tibble::tibble(
                        from = as.Date(rd[1]),
                        to = as.Date(rd[2]),
                        class = pred[idx]
                    )
                    idx <<- idx + 1
                    pred_date <- dplyr::bind_cols(pred_date, probs_date)
                })
            row$predicted <- list(pred_sample)
            return(row)
        }
    )

    return(data_pred)
}
