#' @title Obtains the predicted value of a reference set
#' @name .accuracy_pred_ref
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#
#' @description Obtains a tibble of predicted and reference values
#' from a classified data set.
#'
#' @param  class     Tibble with classified samples whose labels are known.
#' @return           A tibble with predicted and reference values.
.accuracy_pred_ref <- function(class) {
    # retrieve the predicted values
    pred <- unlist(purrr::map(class[["predicted"]], function(r) r[["class"]]))
    # retrieve the reference labels
    ref <- class[["label"]]
    # does the input data contains valid reference labels?
    .check_labels(ref)
    # build the tibble
    pred_ref <- tibble::tibble(predicted = pred, reference = ref)
    return(pred_ref)
}

#' @title Support for Area-weighted post-classification accuracy
#' @name .accuracy_area_assess
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @keywords internal
#' @noRd
#' @param cube         Data cube.
#' @param pred         Integer vector with predicted values.
#' @param ref          Integer vector with reference values.
#'
#' @references
#' Olofsson, P., Foody G.M., Herold M., Stehman, S.V.,
#' Woodcock, C.E., Wulder, M.A. (2014)
#' Good practices for estimating area and assessing accuracy of land change.
#' Remote Sensing of Environment, 148, pp. 42-57.
#'
#' @return
#' A list of lists: The error_matrix, the class_areas, the unbiased
#' estimated areas, the standard error areas, confidence interval 95% areas,
#' and the accuracy (user, producer, and overall).
.accuracy_area_assess <- function(cube, pred, ref) {
    # set caller to show in errors
    .check_set_caller(".accuracy_area_assess")
    # check if cube has the right type
    .check_is_class_cube(cube)
    labels_cube <- .cube_labels(cube)
    # Create the error matrix
    error_matrix <- table(
        factor(pred,
               levels = labels_cube,
               labels = labels_cube
        ),
        factor(ref,
               levels = labels_cube,
               labels = labels_cube
        )
    )
    # Get area for each class of the cube
    area <- .cube_class_areas(cube)

    # In the case where some classes are not in the classified cube, but
    # are in the validation file
    diff_classes <- setdiff(rownames(error_matrix), names(area))
    if (length(diff_classes) > 0 &&
        length(diff_classes) < length(rownames(error_matrix))) {
        warning(.conf("messages", ".accuracy_area_assess"),
                call. = FALSE
        )
        # Create a numeric vector with zeros
        vec_areas <- rep(0, length(diff_classes))
        names(vec_areas) <- diff_classes
        # Join with all area classes
        area <- c(area, vec_areas)
        area <- area[sort(names(area))]

    }
    # check error matrix
    .check_error_matrix_area(error_matrix, area)

    # reorder the area based on the error matrix
    area <- area[colnames(error_matrix)]

    # calculate class areas
    weight <- area / sum(area)
    class_areas <- rowSums(error_matrix)

    # proportion of area derived from the reference classification
    # weighted by the area of the classes
    # cf equation (1) of Olofsson et al (2013)
    prop <- weight * error_matrix / class_areas
    prop[is.na(prop)] <- 0

    # unbiased estimator of the total area
    # based on the reference classification
    # cf equation (2) of Olofsson et al (2013)
    error_adjusted_area <- colSums(prop) * sum(area)

    # Estimated standard error of the estimated area proportion
    # cf equation (3) of Olofsson et al (2013)
    stderr_prop <- sqrt(colSums((weight * prop - prop**2) / (class_areas - 1)))

    # standard error of the error-adjusted estimated area
    # cf equation (4) of Olofsson et al (2013)
    stderr_area <- sum(area) * stderr_prop

    # area-weighted user's accuracy
    # cf equation (6) of Olofsson et al (2013)
    user_acc <- diag(prop) / rowSums(prop)

    # area-weigthed producer's accuracy
    # cf equation (7) of Olofsson et al (2013)
    prod_acc <- diag(prop) / colSums(prop)

    # overall area-weighted accuracy
    over_acc <- sum(diag(prop))

    acc_area <- list(
        error_matrix = error_matrix,
        area_pixels = area,
        error_ajusted_area = error_adjusted_area,
        stderr_prop = stderr_prop,
        stderr_area = stderr_area,
        conf_interval = 1.96 * stderr_area,
        accuracy = list(
            user = user_acc,
            producer = prod_acc,
            overall = over_acc
        )
    )
    class(acc_area) <- c("sits_area_accuracy", class(acc_area))
    return(acc_area)
}

.accuracy_pixel_assess <- function(cube, pred, ref) {
    # Create factor vectors for caret
    unique_ref <- unique(ref)
    pred_fac <- factor(pred, levels = unique_ref)
    ref_fac <- factor(ref, levels = unique_ref)

    # Call caret package to the classification statistics
    acc <- caret::confusionMatrix(pred_fac, ref_fac)
    class(acc) <- c("sits_accuracy", class(acc))
    return(acc)
}
#' @title    Get validation samples
#' @name .accuracy_get_validation
#' @description Retrieves and checks validation data
#' @keywords internal
#' @noRd
#' @param validation  validation (CSV file, SHP file, SF object, data.frame)
#' @return samples for validation
#'
.accuracy_get_validation <- function(validation){
    # handle validation data as files
    if (is.character(validation)) {
        val_class <- tolower(.file_ext(validation))
        class(validation) <- c(val_class, validation)
    }
    UseMethod(".accuracy_get_validation", validation)
}
#' @export
.accuracy_get_validation.csv <- function(validation){
    # Read sample information from CSV file and put it in a tibble
    valid_samples <- .csv_get_validation_samples(validation)
    return(valid_samples)
}
#' @export
.accuracy_get_validation.shp <- function(validation){
    validation_sf <- sf::st_read(validation)
    .check_that(all(sf::st_geometry_type(validation_sf) == "POINT"))
    valid_samples <- .accuracy_get_validation(validation_sf)
    return(valid_samples)
}
#' @export
.accuracy_get_validation.gpkg <- function(validation){
    validation_sf <- sf::st_read(validation)
    .check_that(all(sf::st_geometry_type(validation_sf) == "POINT"))
    valid_samples <- .accuracy_get_validation(validation_sf)
    return(valid_samples)
}
#' @export
.accuracy_get_validation.sf <- function(validation){
    # Pre-condition - check for the required columns
    .check_chr_contains(colnames(validation), c("label"))
    # transform the `sf` object in a valid
    valid_samples <- validation |>
        dplyr::mutate(
            geom = sf::st_geometry(validation)
        ) |>
        dplyr::mutate(
            geom = sf::st_centroid(.data[["geom"]])
        ) |>
        dplyr::mutate(
            coords = sf::st_coordinates(.data[["geom"]])
        ) |>
        dplyr::mutate(
            longitude = .data[["coords"]][, 1],
            latitude  = .data[["coords"]][, 2]
        ) |>
        dplyr::select(
            "label", "longitude", "latitude"
        )
    return(valid_samples)
}
#' @export
`.accuracy_get_validation.data.frame` <- function(validation){
    # handle data frames
    .check_chr_contains(colnames(validation),
                        c("label", "longitude", "latitude")
    )
    return(validation)
}
