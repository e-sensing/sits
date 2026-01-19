#' @title Pre-train deep learning models to generate embeddings for sits
#' @name sits_embedding
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
#' @param method A function defining the deep learning pre-training method.
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
sits_embedding <- function(samples_pre_train,
                           samples_fine_tune = NULL,
                           method = sits_mae(
                                    encoder_model      = "lighttae",
                                    decoder_model      = "mlp",
                                    masking_method     = "contiguous",
                                    mask_ratio         = 0.6,
                                    epochs             = 150L,
                                    batch_size         = 128L,
                                    validation_split   = 0.2,
                                    optimizer_fn       = torch::optim_adamw,
                                    loss_fn            = torch::nnf_mse_loss,
                                    device             = if (torch::cuda_is_available()) torch::torch_device("cuda") else torch::torch_device("cpu"),
                                    lr                 = 0.001,
                                    verbose            = FALSE,
                                    seed               = 10L
                           )) {
    # Set caller for internal error tracking
    .check_set_caller("sits_embedding")
    # Validate samples
    .check_samples(samples_pre_train)
    if (.has(samples_fine_tune)) .check_samples(samples_fine_tune)
    # Check if the method is a function
    .check_that(inherits(method, "function"),
                msg = .conf("messages", "sits_pre_train_method")
    )
    ml_stats <- .samples_stats(samples_pre_train)
    samples  <- samples_fine_tune
    bands    <- .samples_bands(samples)
    # Run the pre-training method
    encoder <- method(samples_pre_train)
    if (!.has(samples_fine_tune)) return(encoder)

    encoder <- encoder$model$model

    encoder <- .sits_mae_fine_tune(
        samples            = samples_fine_tune,
        samples_validation = NULL,
        encoder            = encoder,
        epochs             = 20,
        batch_size         = 128L,
        validation_split   = 0.2,
        optimizer_fn       = torch::optim_adamw,
        loss_fn            = torch::nn_cross_entropy_loss(),
        device             = if (torch::cuda_is_available()) torch::torch_device("cuda") else torch::torch_device("cpu"),
        lr                 = 0.001,
        seed               = 10L,
        verbose            = FALSE
    )

    # Serialize model
    serialized_model <- .torch_serialize_model(encoder$model$model)

    # Function that predicts labels of input values
    predict_fun <- function(values) {
        # Verifies if torch package is installed
        .check_require_packages("torch")
        # Set torch threads to 1
        suppressWarnings(torch::torch_set_num_threads(1L))
        # Unserialize model
        encoder$model$model <- .torch_unserialize_model(serialized_model)
        # Transform input into a 3D tensor
        # Reshape the 2D matrix into a 3D array
        n_samples <- nrow(values)
        n_times <- .samples_ntimes(samples_pre_train)
        n_bands <- length(bands)
        # Performs data normalization
        values <- .pred_normalize(pred = values, stats = ml_stats)
        values <- array(
            data = as.matrix(values), dim = c(n_samples, n_times, n_bands)
        )
        # CPU or GPU classification?
        if (.torch_gpu_classification()) {
            # Get batch size
            batch_size <- sits_env[["batch_size"]]
            # transform the input array to a dataset
            values <- .torch_as_dataset(values)
            # Transform data set to dataloader to use the batch size
            values <- torch::dataloader(values, batch_size = batch_size)
            # GPU classification
            values <- .try(
                stats::predict(object = encoder, values),
                .msg_error = .conf("messages", ".check_gpu_memory_size")
            )
        } else {
            #  CPU classification
            values <- stats::predict(object = encoder, values)
        }
        # Convert from tensor to array
        values <- torch::as_array(values)
        # Update the columns names to labels
        colnames(values) <- paste0("EMB_", seq_len(ncol(values)))
        values
    }
    # Set model class
    predict_fun <- .set_class(
        predict_fun, "torch_model", "sits_model", class(predict_fun)
    )
    predict_fun
}
