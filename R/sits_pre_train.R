#' @title Pre-train deep learning models for sits
#' @name sits_pre_train
#'
#' @author Alexandre Assuncao \email{alexcarssuncao@@gmail.com}
#'
#' @description
#' Self-supervised pre-training for Earth observation time series.
#' Allows users to pre-train deep learning models using unlabeled or weakly labeled data.
#'
#' @param samples Time series samples (as a sits tibble). Labels are optional and
#'   may or may not be used depending on the pre-training method.
#'
#' @param deep_method A function defining the deep learning pre-training method.
#'
#' @return A \code{sits_model} object containing the pre-trained deep model.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # Example of MAE pre-training
#'     mae_model <- sits_pre_train(samples_modis_ndvi, sits_mae_pretrain(mask_ratio = 0.5))
#' }
#'
#' @export
sits_pre_train <- function(samples, deep_method) {
    # Set caller for internal error tracking
    .check_set_caller("sits_pre_train")
    # Validate samples
    .check_samples(samples)
    # Check if the deep_method is a function
    .check_that(inherits(deep_method, "function"),
                msg = "The deep_method parameter must be a function."
    )
    # Run the pre-training method
    result <- deep_method(samples)
    # Return the pre-trained model
    return(result)
}
