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
    pred <- unlist(purrr::map(class$predicted, function(r) r$class))

    # retrieve the reference labels
    ref <- class$label
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
#' @param error_matrix Matrix given in sample counts.
#'                     Columns represent the reference data and
#'                     rows the results of the classification
#' @param area         Named vector of the total area of each class on
#'                     the map
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

.accuracy_area_assess <- function(cube, error_matrix, area) {

    # set caller to show in errors
    .check_set_caller(".sits_accuracy_area_assess")
    # check if cube has the right type
    .check_cube_is_class_cube(cube)
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
    return(
        list(
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
    )
}
