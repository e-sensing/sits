#' @title Assess classification accuracy (area-weighted method)
#' @name sits_accuracy
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description This function calculates the accuracy of the classification
#' result. For a set of time series, it creates a confusion matrix and then
#' calculates the resulting statistics using package \code{caret}. The time
#' series needs to be classified using \code{\link[sits]{sits_classify}}.
#'
#' Classified images are generated using \code{\link[sits]{sits_classify}}
#' followed by \code{\link[sits]{sits_label_classification}}.
#' For a classified image, the function uses an area-weighted technique
#' proposed by Olofsson et al. according to [1-3] to produce more reliable
#' accuracy estimates at 95% confidence level.
#'
#' In both cases, it provides an accuracy assessment of the classified,
#' including Overall Accuracy, Kappa, User's Accuracy, Producer's Accuracy
#' and error matrix (confusion matrix)
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
#'                         Only required when data is a class cube.
#'
#' @return
#' A list of lists: The error_matrix, the class_areas, the unbiased
#' estimated areas, the standard error areas, confidence interval 95% areas,
#' and the accuracy (user, producer, and overall), or NULL if the data is empty.
#' A confusion matrix assessment produced by the caret package.
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
#'     test_data  <- sits_sample(samples_modis_ndvi, frac = 0.5)
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
sits_accuracy.sits <- function(data, ...) {
    .check_set_caller("sits_accuracy_sits")
    # Require package
    .check_require_packages("caret")
    # Does the input data contain a set of predicted values?
    .check_predicted(data)
    # Recover predicted and reference vectors from input
    # Is the input the result of a sits_classify?
    if ("label" %in% names(data)) {
        pred_ref <- .accuracy_pred_ref(data)
        pred <- pred_ref[["predicted"]]
        ref <- pred_ref[["reference"]]
    } else {
        # is the input the result of the sits_kfold_validate?
        pred <- data[["predicted"]]
        ref <- data[["reference"]]
    }
    # Create factor vectors for caret
    unique_ref <- unique(ref)
    pred_fac <- factor(pred, levels = unique_ref)
    ref_fac <- factor(ref, levels = unique_ref)

    # Call caret package to the classification statistics
    acc <- caret::confusionMatrix(pred_fac, ref_fac)

    # Assign class to result
    class(acc) <- c("sits_accuracy", class(acc))
    # return caret confusion matrix
    return(acc)
}
#' @title Area-weighted post-classification accuracy for data cubes
#' @rdname sits_accuracy
#' @export
sits_accuracy.class_cube <- function(data, ..., validation) {
    .check_set_caller("sits_accuracy_class_cube")
    # get the validation samples
    valid_samples <- .accuracy_get_validation(validation)

    # Find the labels of the cube
    labels_cube <- .cube_labels(data)
    # Create a list of (predicted, reference) values
    # Consider all tiles of the data cube
    pred_ref_lst <- slider::slide(data, function(tile) {
        # Find the labelled band
        labelled_band <- .tile_bands(tile)
        # Is the labelled band unique?
        .check_that(length(labelled_band) == 1)
        # get xy in cube projection
        xy_tb <- .proj_from_latlong(
            longitude = valid_samples[["longitude"]],
            latitude = valid_samples[["latitude"]],
            crs = .crs(tile)
        )
        # join samples with XY values in a single tibble
        points <- dplyr::bind_cols(valid_samples, xy_tb)
        # are there points to be retrieved from the cube?
        .check_that(nrow(points) != 0)
        # Filter the points inside the tile
        points_tile <- dplyr::filter(
            points,
            .data[["X"]] >= tile[["xmin"]],
            .data[["X"]] <= tile[["xmax"]],
            .data[["Y"]] >= tile[["ymin"]],
            .data[["Y"]] <= tile[["ymax"]]
        )
        # No points in the cube? Return an empty list
        if (nrow(points_tile) < 1)
            return(NULL)

        # Convert the tibble to a matrix
        xy <- matrix(c(points_tile[["X"]], points_tile[["Y"]]),
                     nrow = nrow(points_tile),
                     ncol = 2
        )
        colnames(xy) <- c("X", "Y")
        # Extract values from cube
        values <- .tile_extract(
            tile = tile,
            band = labelled_band,
            xy = xy
        )
        # Transform to vector
        values <- unlist(values)
        # Indexes of NA values
        idx_na <- !is.na(values)
        # Remove NAs from values
        values <- values[idx_na]
        # Get the predicted values
        predicted <- labels_cube[.as_chr(values)]
        # Get reference classes and remove NAs
        reference <- points_tile[["label"]][idx_na]
        # Does the number of predicted and reference values match?
        .check_pred_ref_match(reference, predicted)
        # Create a tibble to store the results
        tb <- tibble::tibble(
            predicted = predicted,
            reference = reference
        )
        # Return the list
        return(tb)
    })
    # Retrieve predicted and reference vectors for all rows of the cube
    pred_ref <- do.call(rbind, pred_ref_lst)
    # is this data valid?
    .check_null_parameter(pred_ref)
    # Create the error matrix
    error_matrix <- table(
        factor(pred_ref[["predicted"]],
               levels = labels_cube,
               labels = labels_cube
        ),
        factor(pred_ref[["reference"]],
               levels = labels_cube,
               labels = labels_cube
        )
    )
    # Get area for each class of the cube
    class_areas <- .cube_class_areas(cube = data)
    # Compute accuracy metrics
    acc_area <- .accuracy_area_assess(
        cube = data,
        error_matrix = error_matrix,
        area = class_areas
    )
    class(acc_area) <- c("sits_area_accuracy", class(acc_area))
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
    acc <- sits_accuracy(data, ...)
    return(acc)
}
#' @rdname sits_accuracy
#' @export
sits_accuracy.default <- function(data, ...) {
    data <- tibble::as_tibble(data)
    acc <- sits_accuracy(data, ...)
    return(acc)
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
    digits <- .default(digits, max(3, getOption("digits") - 3))

    if (inherits(x, "sits_area_accuracy")) {
        print.sits_area_accuracy(x)
        return(invisible(TRUE))
    }
    # is data of class sits_accuracy
    .check_is_sits_accuracy(x)
    # round the data to the significant digits
    overall <- round(x[["overall"]], digits = digits)

    accuracy_ci <- paste0(
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
    digits <- .default(digits, max(3, getOption("digits") - 3))
    # rename confusion matrix names
    names(x) <- c("positive", "table", "overall", "by_class", "mode", "dots")
    cat("Confusion Matrix and Statistics\n\n")
    print(x[["table"]])

    # Round the data to the significant digits
    overall <- round(x[["overall"]], digits = digits)
    # Format accuracy
    accuracy_ci <- paste(
        "(",
        paste(overall[c("AccuracyLower", "AccuracyUpper")], collapse = ", "),
        ")", sep = ""
    )
    overall_text <- c(
        paste(overall[["Accuracy"]]), accuracy_ci, "",
        paste(overall[["Kappa"]])
    )

    overall_names <- c("Accuracy", "95% CI", "", "Kappa")

    if (dim(x[["table"]])[[1]] > 2) {
        # Multiclass case
        # Names in caret are different from usual names in Earth observation
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
                "(Specificity)",
                "(Pos Pred Value)",
                "(Neg Pred Value)",
                "(F1)"
            ),
            collapse = "|"
        )
        x[["by_class"]] <- x[["by_class"]][,
                                grepl(pattern_format, colnames(x[["by_class"]]))
        ]
        measures <- t(x[["by_class"]])
        rownames(measures) <- c(
            "Prod Acc (Sensitivity)", "Specificity",
            "User Acc (Pos Pred Value)", "Neg Pred Value", "F1 score"
        )
        print(measures, digits = digits)
    } else {
        # Two class case
        # Names in caret are different from usual names in Earth observation
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
        # Names of the two classes
        names_classes <- row.names(x[["table"]])
        # First class is called the "positive" class by caret
        c1 <- x[["positive"]]
        # Second class
        c2 <- names_classes[!(names_classes == x[["positive"]])]
        # Values of UA and PA for the two classes
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

        out <- rbind(out, rep("", 2))

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
print.sits_area_accuracy <- function(x, ..., digits = 2) {
    # round the data to the significant digits
    overall <- round(x[["accuracy"]][["overall"]], digits = digits)

    cat("Area Weighted Statistics\n")
    cat(paste0("Overall Accuracy = ", overall, "\n"))

    acc_user <- round(x[["accuracy"]][["user"]], digits = digits)
    acc_prod <- round(x[["accuracy"]][["producer"]], digits = digits)

    # Print accuracy values
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
