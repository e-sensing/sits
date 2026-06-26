#' @title Pre-train deep learning models for sits
#'
#' @description
#' Runs self-supervised (or weakly supervised) pre-training for Earth
#' observation time series using \pkg{sits}. The function is a thin
#' wrapper that validates inputs and dispatches to a user-selected deep
#' learning pre-training method. The method is responsible for preparing
#' training data, fitting the model, and returning a \code{"sits_model"}
#' encoder that can be used later by \code{\link[sits]{sits_encode}}.
#'
#' Pre-training methods are created by factory functions such as
#' \code{\link[sits]{sits_mae}} (masked autoencoder) and
#' \code{\link[sits]{sits_barlow_twins}} (Barlow Twins), and
#' \code{\link[sits]{sits_multilabel_learning}} (contrastive
#' triplet-based encoder). These factories return a function (closure) that
#' implements the full pre-training procedure when called with \code{samples}.
#'
#' @param samples Time-series samples as a tibble of class \code{"sits"}.
#'   Labels are optional and may or may not be used depending on the
#'   selected pre-training method.
#' @param encoder_method A pre-training method created by a \pkg{sits} deep
#'   learning encoder factory (e.g., \code{sits_mae()},
#'   \code{sits_barlow_twins()}, or
#'   \code{sits_contrastive_learning()}). It must be a function that takes
#'   \code{samples} and returns a \code{"sits_encoder"} object.
#'
#' @return
#' A \code{"sits_encoder"} closure containing the pre-trained deep learning
#' encoder and the metadata required for subsequent encoding (e.g., band
#' order, feature naming, and normalization statistics, when applicable).
#'
#' @examples
#' if (sits_run_examples()) {
#'     mae_model <- sits_pre_train(
#'         samples = samples_modis_ndvi,
#'         encoder_method = sits_mae(
#'             encoder_model = sits_lighttae(),
#'             mask_ratio = 0.5
#'         )
#'     )
#' }
#'
#' @author Alexandre Assuncao \email{alexcarssuncao@@gmail.com}
#' @author Rolf Simoes \email{rolfsimoes@@gmail.com}
#'
#' @export
sits_pre_train <- function(samples, encoder_method) {
    # Set caller for internal error tracking
    .check_set_caller("sits_pre_train")
    # Validate samples
    .check_samples(samples)
    # Check if the method is a function
    .check_that(inherits(encoder_method, "function"),
        msg = .conf("messages", "sits_pre_train_method")
    )
    # pre conditions
    .check_is_encoder_method(encoder_method)
    # Run the pre-training method
    result <- encoder_method(samples)
    # post conditions
    .check_is_sits_encoder(result)
    # Return the pre-trained model
    return(result)
}
