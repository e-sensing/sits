#' @title Use time series values as distances for training patterns
#' @name .sits_distances
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function allows using a set of labelled time series as
#' input to the machine learning models. The attributes used to train the model
#' are the series themselves. It extracts the time series from a sits tibble
#' and "spreads" them in time to produce a tibble with distances.
#'
#' @param  data       A tibble with time series data and metadata.
#' @return            A data.table where columns have the reference label
#'                    and the time series values as distances.
#'
.sits_distances <- function(data) {
    # backward compatibility
    data <- .sits_tibble_rename(data)

    # check the sits tibble
    .sits_test_tibble(data)
    # get the number of samples
    n_rows_data <- nrow(data)

    # create a list with the time series transposed from columns to rows
    ts <- data$time_series %>%
      purrr::map(function(ts) {
        as.data.frame(t(unlist(ts[-1])))
      })
    # bind the lists of time series together
    dist <- data.table::rbindlist(ts, use.names = FALSE)
    # create a data frame with the first two columns for training
    distances <- data.table::data.table(
        "original_row" = 1:n_rows_data,
        "reference" = data$label
    )
    # join the two references columns with the data values
    distances <- data.table::as.data.table(cbind(distances, dist))

    return(distances)
}

#' @title Classify a distances tibble using machine learning models
#' @name .sits_distances_classify
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Returns a sits tibble with the results of the ML classifier.
#'
#' @param  distances       data.table with distances.
#' @param  class_info      tibble with classification information.
#' @param  ml_model        model trained by \code{\link[sits]{sits_train}}.
#' @param  multicores      number of threads to process the time series.
#' @return A data.table with the predicted labels.
.sits_distances_classify <- function(distances, class_info,
                                     ml_model, multicores) {

    # keras-based models run in single-core mode
    if ("keras_model" %in% class(ml_model) | "ranger_model" %in% class(ml_model)
    | "xgb_model" %in% class(ml_model)) {
          multicores <- 1
      }
    # define the column names
    attr_names <- names(.sits_distances(environment(ml_model)$data[1, ]))
    assertthat::assert_that(length(attr_names) > 0,
        msg = "sits_classify_distances: training data not available"
    )

    # select the data table indexes for each time index
    selected_idx <- .sits_timeline_dist_indexes(
        class_info,
        ncol(distances)
    )

    # classify a block of data
    classify_block <- function(block) {
        # create a list to store the data tables to be used for prediction
        rows <- purrr::map(selected_idx, function(sel_index) {
            block_sel <- block[, sel_index, with = FALSE]
            return(block_sel)
        })
        # create a set of distances to be classified
        dist_block <- data.table::rbindlist(rows, use.names = FALSE)
        # set the attribute names of the columns
        colnames(dist_block) <- attr_names

        # classify the subset data
        pred_block <- ml_model(dist_block)

        return(pred_block)
    }

    join_blocks <- function(blocks) {
        pred <- blocks %>%
            dplyr::bind_rows()
        return(pred)
    }
    n_rows_dist <- nrow(distances)
    if (multicores > 1) {
        blocks <- split.data.frame(
            distances,
            cut(1:n_rows_dist,
                multicores,
                labels = FALSE
            )
        )
        # apply parallel processing to the split data
        results <- parallel::mclapply(
            blocks,
            classify_block,
            mc.cores = multicores
        )

        # fix 'Error: Tibble columns must have compatible sizes'
        predicted <- do.call(rbind, results)

        # predicted <- join_blocks(results)
    }
    else {
          predicted <- classify_block(distances)
      }
    return(predicted)
}

#' @title Sample a percentage of a time series distance matrix
#' @name .sits_distances_sample
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Takes a sits tibble with different labels and
#'              returns a new tibble. For a given field as a group criterion,
#'              this new table contains a given number or percentage
#'              of the total number of samples per group. Parameter n indicates
#'              the number of random samples with reposition.
#'              Parameter frac indicates a fraction of random samples
#'              without reposition. If frac > 1, no sampling is done.
#'
#' @param  distances       Distances associated to a time series.
#' @param  frac            Percentage of samples to pick.
#' @return                 Data.table with a fixed quantity of samples
#'                         of informed labels and all other.
.sits_distances_sample <- function(distances, frac) {
    # compute sampling
    result <- distances[, .SD[sample(.N, round(frac * .N))], by = reference]

    return(result)
}
