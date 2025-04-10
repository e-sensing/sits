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
#'     # train an SVM model
#'     ml_model <- sits_train(samples_modis_ndvi, ml_method = sits_svm)
#'     # classify the point
#'     point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
#'     point_class <- sits_classify(
#'         data = point_ndvi, ml_model = ml_model
#'     )
#'     sits_show_prediction(point_class)
#' }
#'
#' @export
sits_show_prediction <- function(class) {
    # set caller to show in errors
    .check_set_caller("sits_show_prediction")
    .check_predicted(class)
    dplyr::select(
        dplyr::bind_rows(class[["predicted"]]),
        c("from", "to", "class")
    )
}

#' @title Informs if sits tests should run
#' @name sits_run_tests
#'
#' @description
#' To run the tests, set "SITS_RUN_TESTS" environment to "YES" using
#' Sys.setenv("SITS_RUN_TESTS" = "YES")
#' To come back to the default behaviour, please set
#' Sys.setenv("SITS_RUN_TESTS" = "NO")
#' @return TRUE/FALSE
#' @export
sits_run_tests <- function() {
    !Sys.getenv("SITS_RUN_TESTS") %in% c("", "NO", "FALSE", "OFF")
}
#' @title Informs if sits examples should run
#'
#' @name sits_run_examples
#'
#' @description
#' This function informs if sits examples should run.
#' To run the examples, set "SITS_RUN_EXAMPLES" to "YES" using
#' Sys.setenv("SITS_RUN_EXAMPLES" = "YES")
#' To come back to the default behaviour, please set
#' Sys.setenv("SITS_RUN_EXAMPLES" = "NO")
#'
#' @return A logical value
#' @export
sits_run_examples <- function() {
    !Sys.getenv("SITS_RUN_EXAMPLES") %in% c("", "NO", "FALSE", "OFF")
}
