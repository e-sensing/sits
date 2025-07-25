#' @title Assess classification accuracy
#' @name sits_accuracy
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description This function calculates the accuracy of the classification
#' result. The input is either a set of classified time series or a classified
#' data cube. Classified time series are produced
#' by \code{\link[sits]{sits_classify}}.
#' Classified images are generated using \code{\link[sits]{sits_classify}}
#' followed by \code{\link[sits]{sits_label_classification}}.
#'
#' For a set of time series, \code{sits_accuracy} creates a confusion matrix and
#' calculates the resulting statistics using package \code{caret}. For a
#' classified image, the function uses an area-weighted technique
#' proposed by Olofsson et al. according to references [1-3] to produce reliable
#' accuracy estimates at 95\% confidence level. In both cases, it provides
#' an accuracy assessment of the classified,
#' including Overall Accuracy, Kappa, User's Accuracy, Producer's Accuracy
#' and error matrix (confusion matrix).
#'
#' @references
#' [1] Olofsson, P., Foody, G.M., Stehman, S.V., Woodcock, C.E. (2013).
#' Making better use of accuracy data in land change studies: Estimating
#' accuracy and area and quantifying uncertainty using stratified estimation.
#' Remote Sensing of Environment, 129, pp.122-131.
#'
#' @references
#' [2] Olofsson, P., Foody G.M., Herold M., Stehman, S.V.,
#' Woodcock, C.E., Wulder, M.A. (2014)
#' Good practices for estimating area and assessing accuracy of land change.
#' Remote Sensing of Environment, 148, pp. 42-57.
#'
#' @references
#' [3] FAO, Map Accuracy Assessment and Area Estimation: A Practical Guide.
#' National forest monitoring assessment working paper No.46/E, 2016.
#'
#' @param data             Either a data cube with classified images or
#'                         a set of time series
#' @param \dots            Specific parameters
#' @param validation       Samples for validation (see below)
#'                         Only required when data is a raster class cube.
#' @param method           A character with 'olofsson' or 'pixel' to compute
#'                         accuracy (only for raster class cubes)
#' @param prediction_attr  Name of the column of the segments object
#'                         that contains the predicted values
#'                         (only for vector class cubes)
#' @param reference_attr   Name of the column of the segments object
#'                         that contains the reference values
#'                         (only for vector class cubes)
#'
#' @return
#' A list of lists: The error_matrix, the class_areas, the unbiased
#' estimated areas, the standard error areas, confidence interval 95% areas,
#' and the accuracy (user, producer, and overall), or NULL if the data is empty.
#' The result is assigned to class "sits_accuracy" and can be visualized
#' directly on the screen.
#
#' @note
#' The `validation` data needs to contain the following columns: "latitude",
#'  "longitude", "start_date", "end_date", and "label". It can be either a
#'  path to a CSV file, a sits tibble, a data frame, or an sf object.
#'
#' When `validation` is an sf object, the columns "latitude" and "longitude" are
#' not required as the locations are extracted from the geometry column. The
#' `centroid` is calculated before extracting the location values for any
#' geometry type.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # show accuracy for a set of samples
#'     train_data <- sits_sample(samples_modis_ndvi, frac = 0.5)
#'     test_data <- sits_sample(samples_modis_ndvi, frac = 0.5)
#'     rfor_model <- sits_train(train_data, sits_rfor())
#'     points_class <- sits_classify(
#'         data = test_data, ml_model = rfor_model
#'     )
#'     acc <- sits_accuracy(points_class)
#'
#'     # show accuracy for a data cube classification
#'     # create a random forest model
#'     rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'     # classify a data cube
#'     probs_cube <- sits_classify(
#'         data = cube, ml_model = rfor_model, output_dir = tempdir()
#'     )
#'     # label the probability cube
#'     label_cube <- sits_label_classification(
#'         probs_cube,
#'         output_dir = tempdir()
#'     )
#'     # obtain the ground truth for accuracy assessment
#'     ground_truth <- system.file("extdata/samples/samples_sinop_crop.csv",
#'         package = "sits"
#'     )
#'     # make accuracy assessment
#'     as <- sits_accuracy(label_cube, validation = ground_truth)
#' }
#' @export
sits_accuracy <- function(data, ...) {
    # Set caller to show in errors
    .check_set_caller("sits_accuracy")
    .check_na_null_parameter(data)
    UseMethod("sits_accuracy", data)
}
#' @rdname sits_accuracy
#' @export
#'
sits_accuracy.sits <- function(data, ...) {
    .check_set_caller("sits_accuracy_sits")
    # require package
    .check_require_packages("caret")
    # does the input data contain a set of predicted values?
    .check_predicted(data)
    # recover predicted and reference vectors from input
    # is the input the result of a sits_classify?
    if ("label" %in% names(data)) {
        pred_ref <- .accuracy_pred_ref(data)
        pred <- pred_ref[["predicted"]]
        ref <- pred_ref[["reference"]]
    } else {
        # is the input the result of the sits_kfold_validate?
        pred <- data[["predicted"]]
        ref <- data[["reference"]]
    }
    # create factor vectors for caret
    unique_ref <- unique(ref)
    pred_fac <- factor(pred, levels = unique_ref)
    ref_fac <- factor(ref, levels = unique_ref)

    # call caret package to the classification statistics
    acc <- caret::confusionMatrix(pred_fac, ref_fac)

    # assign class to result
    class(acc) <- c("sits_accuracy", class(acc))
    # return caret confusion matrix
    acc
}
#' @title Accuracy assessment for vector class cubes
#' @rdname sits_accuracy
#' @export
sits_accuracy.class_vector_cube <- function(data, ...,
                                            prediction_attr,
                                            reference_attr) {
    .check_set_caller("sits_accuracy_class_vector_cube")
    segments <- .segments_read_vec(data)
    .check_chr_contains(
        colnames(segments),
        c(prediction_attr, reference_attr)
    )

    # create prediction and reference data frames
    pred <- segments[[prediction_attr]]
    ref <- segments[[reference_attr]]
    # create factor vectors for caret
    unique_ref <- unique(ref)
    pred_fac <- factor(pred, levels = unique_ref)
    ref_fac <- factor(ref, levels = unique_ref)

    # call caret package to the classification statistics
    acc <- caret::confusionMatrix(pred_fac, ref_fac)

    # assign class to result
    class(acc) <- c("sits_accuracy", class(acc))
    # return caret confusion matrix
    acc
}
#' @title Area-weighted post-classification accuracy for data cubes
#' @rdname sits_accuracy
#' @export
sits_accuracy.class_cube <- function(data, ...,
                                     validation,
                                     method = "olofsson") {
    .check_set_caller("sits_accuracy_class_cube")
    # get the validation samples
    valid_samples <- .accuracy_get_validation(validation)

    # find the labels of the cube
    labels_cube <- .cube_labels(data)
    # create a list of (predicted, reference) values
    # consider all tiles of the data cube
    pred_ref_lst <- slider::slide(data, function(tile) {
        # find the labelled band
        labelled_band <- .tile_bands(tile)
        # is the labelled band unique?
        .check_that(length(labelled_band) == 1L)
        # get xy in cube projection
        xy_tb <- .proj_from_latlong(
            longitude = valid_samples[["longitude"]],
            latitude = valid_samples[["latitude"]],
            crs = .crs(tile)
        )
        # join samples with XY values in a single tibble
        points <- dplyr::bind_cols(valid_samples, xy_tb)
        # are there points to be retrieved from the cube?
        .check_content_data_frame(points)
        # filter the points inside the tile
        points_tile <- dplyr::filter(
            points,
            .data[["X"]] >= tile[["xmin"]],
            .data[["X"]] <= tile[["xmax"]],
            .data[["Y"]] >= tile[["ymin"]],
            .data[["Y"]] <= tile[["ymax"]]
        )
        # no points in the cube? Return an empty list
        if (nrow(points_tile) < 1L) {
            return(NULL)
        }

        # convert the tibble to a matrix
        xy <- matrix(c(points_tile[["X"]], points_tile[["Y"]]),
            nrow = nrow(points_tile),
            ncol = 2L
        )
        colnames(xy) <- c("X", "Y")
        # extract values from cube
        values <- .tile_extract(
            tile = tile,
            band = labelled_band,
            xy = xy
        )
        # transform to vector
        values <- unlist(values)
        # indexes of NA values
        idx_na <- !is.na(values)
        # remove NAs from values
        values <- values[idx_na]
        # get the predicted values
        predicted <- labels_cube[.as_chr(values)]
        # get reference classes and remove NAs
        reference <- points_tile[["label"]][idx_na]
        # does the number of predicted and reference values match?
        .check_pred_ref_match(reference, predicted)
        # Create a tibble to store the results
        tibble::tibble(
            predicted = predicted,
            reference = reference
        )
    })
    # retrieve predicted and reference vectors for all rows of the cube
    pred_ref <- do.call(rbind, pred_ref_lst)
    # is this data valid?
    .check_null_parameter(pred_ref)
    # get predicted and reference values
    pred <- pred_ref[["predicted"]]
    ref <- pred_ref[["reference"]]
    acc_area <- switch(method,
        "olofsson" = .accuracy_area_assess(data, pred, ref),
        "pixel" = .accuracy_pixel_assess(data, pred, ref)
    )
    return(acc_area)
}
#' @rdname sits_accuracy
#' @export
sits_accuracy.raster_cube <- function(data, ...) {
    stop(.conf("messages", "sits_accuracy_raster_cube"))
}
#' @rdname sits_accuracy
#' @export
sits_accuracy.derived_cube <- function(data, ...) {
    stop(.conf("messages", "sits_accuracy_raster_cube"))
}
#' @rdname sits_accuracy
#' @export
sits_accuracy.tbl_df <- function(data, ...) {
    data <- tibble::as_tibble(data)
    if (all(.conf("sits_cube_cols") %in% colnames(data))) {
        data <- .cube_find_class(data)
    } else if (all(.conf("sits_tibble_cols") %in% colnames(data))) {
        class(data) <- c("sits", class(data))
    } else {
        stop(.conf("messages", "sits_accuracy_tbl_df"))
    }
    sits_accuracy(data, ...)
}
#' @rdname sits_accuracy
#' @export
sits_accuracy.default <- function(data, ...) {
    data <- tibble::as_tibble(data)
    sits_accuracy(data, ...)
}
#' @title Print accuracy summary
#' @name sits_accuracy_summary
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#
#' @description Adaptation of the caret::print.confusionMatrix method
#'              for the more common usage in Earth Observation.
#'
#' @param x         Object of class \code{sits_accuracy}.
#' @param digits    Number of significant digits when printed.
#' @return          Called for side effects.
#'
#' @keywords internal
#' @export
sits_accuracy_summary <- function(x, digits = NULL) {
    # set caller to show in errors
    .check_set_caller("sits_accuracy_summary")
    # default value for digits
    digits <- .default(digits, max(3L, getOption("digits") - 3L))

    if (inherits(x, "sits_area_accuracy")) {
        print.sits_area_accuracy(x)
        return(invisible(TRUE))
    }
    # is data of class sits_accuracy
    .check_is_sits_accuracy(x)
    # round the data to the significant digits
    overall <- round(x[["overall"]], digits = digits)

    accuracy_ci <- paste(
        "(", toString(overall[c("AccuracyLower", "AccuracyUpper")]), ")"
    )
    overall_text <- c(
        paste(overall[["Accuracy"]]), accuracy_ci,
        paste(overall[["Kappa"]])
    )
    overall_names <- c("Accuracy", "95% CI", "Kappa")

    cat("Overall Statistics")
    overall_names <- ifelse(overall_names == "",
        "",
        paste(overall_names, ":")
    )
    out <- cbind(format(overall_names, justify = "right"), overall_text)
    colnames(out) <- rep("", ncol(out))
    rownames(out) <- rep("", nrow(out))

    print(out, quote = FALSE)
    return(invisible(x))
}
#' @title Print the values of a confusion matrix
#' @name print.sits_accuracy
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#
#' @description Adaptation of the caret::print.confusionMatrix method
#'              for the more common usage in Earth Observation.
#'
#' @param x         Object of class \code{confusionMatrix}.
#' @param \dots     Other parameters passed to the "print" function.
#' @param digits    Number of significant digits when printed.
#' @return          Called for side effects.
#'
#' @keywords internal
#' @export
print.sits_accuracy <- function(x, ..., digits = NULL) {
    # default value for digits
    digits <- .default(digits, max(3L, getOption("digits") - 3L))
    # rename confusion matrix names
    names(x) <- c("positive", "table", "overall", "by_class", "mode", "dots")
    cat("Confusion Matrix and Statistics\n\n")
    print(x[["table"]])

    # Round the data to the significant digits
    overall <- round(x[["overall"]], digits = digits)
    # Format accuracy
    accuracy_ci <- paste(
        "(", toString(overall[c("AccuracyLower", "AccuracyUpper")]), ")"
    )
    overall_text <- c(
        paste(overall[["Accuracy"]]), accuracy_ci, "",
        paste(overall[["Kappa"]])
    )

    overall_names <- c("Accuracy", "95% CI", "", "Kappa")

    if (dim(x[["table"]])[[1L]] > 2L) {
        # multiclass case
        # names in caret are different from usual names in Earth observation
        cat("\nOverall Statistics\n")
        overall_names <- ifelse(overall_names == "",
            "",
            paste(overall_names, ":")
        )
        out <- cbind(format(overall_names, justify = "right"), overall_text)
        colnames(out) <- rep("", ncol(out))
        rownames(out) <- rep("", nrow(out))

        print(out, quote = FALSE)

        cat("\nStatistics by Class:\n\n")
        pattern_format <- paste(
            c(
                "(Sensitivity)",
                "(Pos Pred Value)",
                "(F1)"
            ),
            collapse = "|"
        )
        x[["by_class"]] <- x[["by_class"]][
            ,
            grepl(pattern_format, colnames(x[["by_class"]]))
        ]
        measures <- t(x[["by_class"]])
        rownames(measures) <- c(
            "Prod Acc (Recall)",
            "User Acc (Precision)",
            "F1 score"
        )
        print(measures, digits = digits)
    } else {
        # two class case
        # names in caret are different from usual names in Earth observation
        pattern_format <- paste(
            c(
                "(Sensitivity)",
                "(Specificity)",
                "(Pos Pred Value)",
                "(Neg Pred Value)"
            ),
            collapse = "|"
        )
        x[["by_class"]] <- x[["by_class"]][
            grepl(pattern_format, names(x[["by_class"]]))
        ]
        # names of the two classes
        names_classes <- row.names(x[["table"]])
        # first class is called the "positive" class by caret
        c1 <- x[["positive"]]
        # second class
        c2 <- names_classes[(names_classes != x[["positive"]])]
        # values of UA and PA for the two classes
        pa1 <- paste("Prod Acc ", c1)
        pa2 <- paste("Prod Acc ", c2)
        ua1 <- paste("User Acc ", c1)
        ua2 <- paste("User Acc ", c2)
        names(x[["by_class"]]) <- c(pa1, pa2, ua1, ua2)

        overall_text <- c(
            overall_text,
            "",
            format(x[["by_class"]], digits = digits)
        )
        overall_names <- c(overall_names, "", names(x[["by_class"]]))
        overall_names <- ifelse(overall_names == "", "",
            paste(overall_names, ":")
        )

        out <- cbind(format(overall_names, justify = "right"), overall_text)
        colnames(out) <- rep("", ncol(out))
        rownames(out) <- rep("", nrow(out))

        out <- rbind(out, rep("", 2L))

        print(out, quote = FALSE)
    }
    return(invisible(x))
}
#' @title Print the area-weighted accuracy
#' @name print.sits_area_accuracy
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#
#' @description Adaptation of the caret::print.confusionMatrix method
#'              for the more common usage in Earth Observation.
#'
#' @param x         An object of class \code{sits_area_accuracy}.
#' @param \dots     Other parameters passed to the "print" function
#' @param digits    Significant digits
#' @return          Called for side effects.
#'
#' @keywords internal
#' @export
print.sits_area_accuracy <- function(x, ..., digits = 2L) {
    # round the data to the significant digits
    overall <- round(x[["accuracy"]][["overall"]], digits = digits)

    cat("Area Weighted Statistics\n")
    cat(paste0("Overall Accuracy = ", overall, "\n"))

    acc_user <- round(x[["accuracy"]][["user"]], digits = digits)
    acc_prod <- round(x[["accuracy"]][["producer"]], digits = digits)

    # print accuracy values
    tb <- t(dplyr::bind_rows(acc_user, acc_prod))
    colnames(tb) <- c("User", "Producer")

    cat("\nArea-Weighted Users and Producers Accuracy\n")

    print(tb)

    area_pix <- round(x[["area_pixels"]], digits = digits)
    area_adj <- round(x[["error_ajusted_area"]], digits = digits)
    conf_int <- round(x[["conf_interval"]], digits = digits)

    tb1 <- t(dplyr::bind_rows(area_pix, area_adj, conf_int))
    colnames(tb1) <- c(
        "Mapped Area (ha)",
        "Error-Adjusted Area (ha)",
        "Conf Interval (ha)"
    )

    cat("\nMapped Area x Estimated Area (ha)\n")
    print(tb1)
    return(invisible(x))
}
