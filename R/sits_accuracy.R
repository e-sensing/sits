#' @title Area-weighted classification accuracy assessment
#' @name sits_accuracy
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description To use this function the input table should be
#' a set of results containing
#' both the label assigned by the user and the classification result.
#' Accuracy assessment set us a confusion matrix to determine the accuracy
#' of your classified result.
#' This function uses an area-weighted technique proposed by Olofsson et al. to
#' produce more reliable accuracy estimates at 95% confidence level.
#'
#' This function performs an accuracy assessment of the classified, including
#' Overall Accuracy, User's Accuracy, Producer's Accuracy
#' and error matrix (confusion matrix) according to [1-2].
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
#' Remote Sensing of
#' Environment, 148, pp. 42-57.
#'
#' @param validation       A SITS tibble or CSV file with validation data
#'
#' @param area             A named vector of the total area of each class on
#'                         the map
#'
#' @export
sits_accuracy <- function(validation, area){

    # assertthat::assert_that("classified_image" %in% class(label_cube),
    #                         msg = "sits_accuracy requires a labelled cube")
    #
    #

    # backward compatibility
    class.tb <- .sits_tibble_rename(validation)

    # Get reference classes
    references <- class.tb$label

    # create a vector to store the result of the predictions
    mapped <-
        unlist(purrr::map(class.tb$predicted, function(r)
            r$class))

    # Get all labels
    classes   <- unique(c(references, mapped))

    # Create error matrix
    error_matrix <-
        table(
            factor(mapped,     levels = classes, labels = classes),
            factor(references, levels = classes, labels = classes)
        )

    # Get area
    if (purrr::is_null(area))
        area <- rowSums(error_matrix)

    # Compute accuracy metrics
    assessment <- .sits_assess_accuracy_area(error_matrix, area)

    return(assessment)
}


#' @title Support for Area-weighted post-classification accuracy
#' @name .sits_assess_accuracy_area
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @param error_matrix A matrix given in sample counts.
#'                     Columns represent the reference data and
#'                     rows the results of the classification
#' @param area         A named vector of the total area of each class on
#'                     the map
#'
#' @return             A list of lists: The error_matrix,
#'                     the class_areas,
#'                     confidence interval (confint95, a list of two numerics)
#'                     and the accuracy (accuracy, a list of three numerics:
#'                     overall, user, and producer)

.sits_assess_accuracy_area <- function(error_matrix, area){

    if (any(dim(error_matrix) == 0))
        stop("Invalid dimensions in error matrix.", call. = FALSE)
    if (length(unique(dim(error_matrix))) != 1)
        stop("The error matrix is not square.", call. = FALSE)
    if (!all(colnames(error_matrix) == rownames(error_matrix)))
        stop("Labels mismatch in error matrix.", call. = FALSE)
    if (unique(dim(error_matrix)) != length(area))
        stop("Mismatch between error matrix and area vector.",
             call. = FALSE)
    if (!all(names(area) %in% colnames(error_matrix)))
        stop("Label mismatch between error matrix and area vector.",
             call. = FALSE)

    # Re-order vector elements.
    area <- area[colnames(error_matrix)]

    W <- area/sum(area)
    n <- rowSums(error_matrix)

    if (any(n < 2))
        stop("Undefined accuracy: one pixel in a class (division by zero).",
             call. = FALSE)
    # n.mat <- matrix(rep(n, times = ncol(error_matrix)),
    #                 ncol = ncol(error_matrix))
    # p <- W * error_matrix / n.mat
    p <- W * error_matrix / n
    error_adjusted_area_estimate <- colSums(p) * sum(area)

    # Sphat_1 <- vapply(seq_len(ncol(error_matrix)), function(i){
    #     sqrt(sum(W^2 * error_matrix[, i]/n * (1 - error_matrix[, i]/n)/(n - 1)))
    # }, numeric(1))
    Sphat_1 <- sqrt(colSums((W * p - p ** 2) / (n - 1)))

    SAhat <- sum(area) * Sphat_1
    # Ahat_sup <- error_adjusted_area_estimate + 2 * SAhat
    Ahat_sup <- error_adjusted_area_estimate + 1.96 * SAhat
    # Ahat_inf <- error_adjusted_area_estimate - 2 * SAhat
    Ahat_inf <- error_adjusted_area_estimate - 1.96 * SAhat
    Ohat <- sum(diag(p))
    Uhat <- diag(p) / rowSums(p)
    Phat <- diag(p) / colSums(p)

    return(
        list(error_matrix = error_matrix,
             area = area, SE_area = SAhat,
             conf_95 = list(superior = Ahat_sup, inferior = Ahat_inf),
             accuracy = list(overall = Ohat, user = Uhat, producer = Phat))
    )
}


