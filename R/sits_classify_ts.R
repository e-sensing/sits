#' @title Classify a distances tibble using machine learning models
#' @name .sits_classify_ts
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Returns a sits tibble with the results of the ML classifier.
#'
#' @param  samples    a tibble with sits samples
#' @param  ml_model   model trained by \code{\link[sits]{sits_train}}.
#' @param  filter_fn  Smoothing filter to be applied (if desired).
#' @param  multicores number of threads to process the time series.
#' @param  progress   Show progress bar?
#' @return A data.table with the predicted labels.
.sits_classify_ts <- function(samples,
                              ml_model,
                              filter_fn,
                              multicores,
                              progress) {
    # recover the samples from the model
    model_samples <- .ml_samples(ml_model)

    # check band order is the same
    model_bands <- .sits_bands(model_samples)
    bands <- .sits_bands(samples)

    # its used equals comparison instead IN because the order of the bands
    # must be the same
    if (!all(model_bands == bands)) {
        samples <- .sits_select(samples, model_bands)
    }

    # Apply filter
    if (!is.null(filter_fn)) {
        samples <- .apply_across(data = samples, fn = filter_fn)
    }

    # get normalization params
    stats <- .ml_stats(ml_model)
    # has the training data been normalized?
    if (!is.null(stats)) {
        samples <- .sits_normalize(samples = samples, stats = stats)
    }

    # calculate the breaks in the time for multi-year classification
    class_info <- .sits_timeline_class_info(
        data = samples,
        samples = model_samples
    )

    distances <- .sits_distances(samples)
    # post condition: is distance data valid?
    .check_distances(distances, samples)

    # define the column names
    attr_names <- names(.sits_distances(.sits_ml_model_samples(ml_model)[1, ]))

    # select the data table indexes for each time index
    selected_idx <- .sits_timeline_dist_indexes(
        class_info,
        ncol(distances)
    )

    # classify a block of data
    classify_block <- function(block) {
        block <- data.table::as.data.table(block)
        # create a list to store the data tables to be used for prediction
        rows <- purrr::map(selected_idx, function(sel_index) {
            block_sel <- block[, sel_index, with = FALSE]
            return(block_sel)
        })
        # create a set of distances to be classified
        pred_block <- data.table::rbindlist(rows, use.names = FALSE)
        # set the attribute names of the columns
        colnames(pred_block) <- attr_names

        # classify the subset data
        pred_block <- ml_model(pred_block[, -2:0])

        return(pred_block)
    }

    .sits_parallel_start(workers = multicores, log = FALSE)
    on.exit(.sits_parallel_stop(), add = TRUE)

    distances <- .sits_get_chunk_ts(distances, multicores)

    prediction <- .sits_parallel_map(
        x = distances,
        fn = classify_block,
        progress = progress
    )
    prediction  <- do.call(rbind, prediction)

    # Store the result in the input data
    prediction <- .sits_tibble_prediction(
        data = samples,
        class_info = class_info,
        prediction = prediction
    )
    class(prediction) <- c("predicted", class(samples))

    return(prediction)
}

#' @title Shows the predicted labels for a classified tibble
#' @name sits_show_prediction
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function takes a tibble with a classified time series
#' by a machine learning method and displays the result.
#'
#' @param  class    A SITS tibble that has been classified.
#' @return          Tibble with the columns "from", "to", "class"
#'
#' @examples
#' if (sits_run_examples()) {
#'     # Retrieve the samples for Mato Grosso
#'     # train a tempCNN model
#'     ml_model <- sits_train(samples_modis_4bands, ml_method = sits_tempcnn)
#'     # classify the point
#'     bands_model <- sits_bands(ml_model)
#'     point_4bands <- sits_select(point_mt_6bands, bands = bands_model)
#'     point_class <- sits_classify(point_4bands, ml_model)
#'     sits_show_prediction(point_class)
#' }
#'
#' @export
sits_show_prediction <- function(class) {

    # set caller to show in errors
    .check_set_caller("sits_show_prediction")
    .check_predicted(class)

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
#' @param  data             Tibble with the input data.
#' @param  class_info       Tibble with the information on classification.
#' @param  prediction       Matrix with the result of the classification
#'                          (one class per column and one row per interval).
#' @return                  Tibble storing the predictions.
#'
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
    pred_labels <- names(int_labels[max.col(prediction)])

    idx <- 1

    data_pred <- slider::slide2_dfr(data, seq_len(nrow(data)),
                                    function(row, row_n) {

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
        idx_fst <- (row_n - 1)*(length(ref_dates_lst)) + 1
        idx_lst <- idx_fst + length(ref_dates_lst) - 1
        pred_row <- prediction[idx_fst:idx_lst,]
        if (idx_lst == idx_fst)
            pred_row <- matrix(pred_row, nrow = 1,
                               dimnames = list(NULL, colnames(prediction)))
        pred_row_lab <- pred_labels[idx_fst:idx_lst]

        # store the classification results
        pred_sample <- purrr::map2_dfr(ref_dates_lst,
                                       seq_len(length(ref_dates_lst)),
            function(rd, idx) {
                probs_date <- rbind.data.frame(pred_row[idx, ])
                names(probs_date) <- names(pred_row[idx, ])
                pred_date <- tibble::tibble(
                    from = as.Date(rd[1]),
                    to = as.Date(rd[2]),
                    class = pred_row_lab[idx]
                )
                pred_date <- dplyr::bind_cols(pred_date, probs_date)
            })
        row$predicted <- list(pred_sample)
        return(row)
    })

    return(data_pred)
}
