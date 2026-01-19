#' @title Run t-SNE on sits torch models
#' @name sits_tsne
#' @description
#' Generic function that Applies t-SNE dimensionality reduction to the feature embeddings produced
#' by a sits torch model (e.g., LTAE, TempCNN) for a given set of samples.
#' Automatically handles duplicate embeddings by removing them before running t-SNE.
#'
#' @param model A sits torch model (output of sits_train()).
#' @param samples A sits tibble with time series samples.
#' @param ... Other parameters (passed to methods).
#'
#' @return A list with:
#' \itemize{
#'   \item \code{tsne}: The t-SNE result (object of class \code{"Rtsne"}).
#'   \item \code{labels}: A character vector of labels, aligned with rows in \code{tsne$Y}.
#' }
#'
#' @examples
#' if (sits_run_examples()) {
#'     # load samples
#'     samples <- samples_modis_ndvi
#'     # sits TAE
#'     model <- sits_train(
#'     samples   = samples,
#'     ml_method = sits_tae(epochs = 2)
#'     )
#'     # run tsne on a deep learning model
#'     tsne <- sits_tsne(model, samples, perplexity = 30, rounds = 100)
#'     plot(tsne)
#' }
#'
#' @export
sits_tsne <- function(model, samples, ...) {
    UseMethod("sits_tsne", model)
}

#' @title Run t-SNE on sits torch models
#' @name sits_tsne.torch_model_tae
#' @description
#' Applies t-SNE for TAE models using temporal encoder outputs as embeddings.
#' @inheritParams sits_tsne
#' @param remove_duplicates Logical. Should duplicated embeddings be removed before running t-SNE? Default is TRUE.
#' @param perplexity Numeric; Rtsne perplexity (default \code{30}). Will be clamped to
#'   \code{floor((N - 1) / 3)} based on the number of (possibly deduped) points.
#' @param rounds Integer; number of optimization iterations in t-SNE
#'   (maps to \code{Rtsne::Rtsne(max_iter=)}; default \code{1000}).
#' @importFrom Rtsne Rtsne
#' @export
sits_tsne.torch_model_tae <- function(model, samples, remove_duplicates = TRUE, perplexity = 30, rounds = 1000, ...) {
    # Check required packages
    .check_require_packages(c("torch", "Rtsne"))
    # Retrieve internal torch model
    internal_model <- .ml_model(model)[["model"]]
    internal_model$eval()
    # Prepare and normalize input samples
    pred <- .predictors(samples)
    ml_stats <- .samples_stats(samples)
    pred <- .pred_normalize(pred, ml_stats)
    # Convert to tensor
    n_samples <- nrow(pred)
    n_times <- .samples_ntimes(samples)
    n_bands <- length(.samples_bands(samples))
    x <- array(as.matrix(.pred_features(pred)), dim = c(n_samples, n_times, n_bands))
    x <- torch::torch_tensor(x, dtype = torch::torch_float())
    # Get embeddings
    embeddings_np <- NULL
    internal_model$eval()
    torch::with_no_grad({
        spatial_out <- internal_model$spatial_encoder(x)
        embeddings <- internal_model$temporal_attention_encoder(spatial_out)
        embeddings_np <- as.array(embeddings)
    })
    # Original labels
    labels <- samples[["label"]]
    # Handle duplicates
    if (remove_duplicates && any(duplicated(embeddings_np))) {
        dup_idx <- !duplicated(embeddings_np)
        embeddings_np <- embeddings_np[dup_idx, , drop = FALSE]
        labels <- labels[dup_idx]
        warning(.conf("messages", "sits_tsne_duplicated_embeddings"))
    }
    # Validating and clamping perplexity
    N <- nrow(embeddings_np)
    .check_num(N, exclusive_min = 3L)
    max_perp <- floor((N - 1L) / 3L)
    if(perplexity < 0) {
        warning(.conf("messages", "sits_tsne_neg_perp"))
        perplexity <- 30
    }
    if(perplexity > max_perp) {
        warning(.conf("messages", "sits_tsne_max_perp"))
        perplexity <- max(5, max_perp)
    }

    # Run t-SNE
    tsne_result <- Rtsne::Rtsne(embeddings_np, perplexity = perplexity, max_iter = as.integer(rounds))
    # Return results
    result <- list(
        tsne = tsne_result,
        labels = labels
    )
    class(result) <- "sits_tsne"
    return(result)
}

#' @title Run t-SNE on sits torch models
#' @name sits_tsne.torch_model_ltae
#' @description
#' Applies t-SNE for LTAE models using temporal encoder outputs as embeddings.
#' @inheritParams sits_tsne
#' @param remove_duplicates Logical. Should duplicated embeddings be removed before running t-SNE? Default is TRUE.
#' @param perplexity Numeric; Rtsne perplexity (default \code{30}). Will be clamped to
#'   \code{floor((N - 1) / 3)} based on the number of (possibly deduped) points.
#' @param rounds Integer; number of optimization iterations in t-SNE
#'   (maps to \code{Rtsne::Rtsne(max_iter=)}; default \code{1000}).
#' @importFrom Rtsne Rtsne
#' @export
sits_tsne.torch_model_ltae <- function(model, samples, remove_duplicates = TRUE, perplexity = 30, rounds = 1000, ...) {
    # Check required packages
    .check_require_packages(c("torch", "Rtsne"))
    # Retrieve internal torch model
    internal_model <- .ml_model(model)[["model"]]
    internal_model$eval()
    # Prepare and normalize input samples
    pred <- .predictors(samples)
    ml_stats <- .samples_stats(samples)
    pred <- .pred_normalize(pred, ml_stats)
    # Convert to tensor
    n_samples <- nrow(pred)
    n_times <- .samples_ntimes(samples)
    n_bands <- length(.samples_bands(samples))
    x <- array(as.matrix(.pred_features(pred)), dim = c(n_samples, n_times, n_bands))
    x <- torch::torch_tensor(x, dtype = torch::torch_float())
    # Get embeddings
    embeddings_np <- NULL
    internal_model$eval()
    torch::with_no_grad({
        spatial_out  <- internal_model$spatial_encoder(x)
        temporal_out <- internal_model$temporal_encoder(spatial_out)
        embeddings   <- internal_model$decoder(temporal_out)
        embeddings_np <- as.array(embeddings)
    })
    # Original labels
    labels <- samples[["label"]]
    # Handle duplicates
    if (remove_duplicates && any(duplicated(embeddings_np))) {
        dup_idx <- !duplicated(embeddings_np)
        embeddings_np <- embeddings_np[dup_idx, , drop = FALSE]
        labels <- labels[dup_idx]
        warning(.conf("messages", "sits_tsne_duplicated_embeddings"))
    }

    # Validating and clamping perplexity
    N <- nrow(embeddings_np)
    .check_num(N, exclusive_min = 3L)
    max_perp <- floor((N - 1L) / 3L)
    if(perplexity < 0) {
        warning(.conf("messages", "sits_tsne_neg_perp"))
        perplexity <- 30
    }
    if(perplexity > max_perp) {
        warning(.conf("messages", "sits_tsne_max_perp"))
        perplexity <- max(5, max_perp)
    }

    # Run t-SNE
    tsne_result <- Rtsne::Rtsne(embeddings_np, perplexity = perplexity, max_iter = as.integer(rounds))
    # Return both t-SNE result and aligned labels
    result <- list(
        tsne = tsne_result,
        labels = labels
    )
    class(result) <- "sits_tsne"
    return(result)
}

#' @title Run t-SNE on sits torch models
#' @name sits_tsne.torch_model_tempcnn
#' @description
#' Applies t-SNE for Temporal CNN models using temporal encoder outputs as embeddings.
#' @inheritParams sits_tsne
#' @param remove_duplicates Logical. Should duplicated embeddings be removed before running t-SNE? Default is TRUE.
#' @param perplexity Numeric; Rtsne perplexity (default \code{30}). Will be clamped to
#'   \code{floor((N - 1) / 3)} based on the number of (possibly deduped) points.
#' @param rounds Integer; number of optimization iterations in t-SNE
#'   (maps to \code{Rtsne::Rtsne(max_iter=)}; default \code{1000}).
#' @importFrom Rtsne Rtsne
#' @export
sits_tsne.torch_model_tempcnn <- function(model, samples, remove_duplicates = TRUE, perplexity = 30, rounds = 1000, ...) {
    # Check required packages
    .check_require_packages(c("torch", "Rtsne"))
    # Retrieve internal torch model
    internal_model <- .ml_model(model)[["model"]]
    internal_model$eval()
    # Prepare and normalize input samples
    pred <- .predictors(samples)
    ml_stats <- .samples_stats(samples)
    pred <- .pred_normalize(pred, ml_stats)
    # Convert to tensor
    n_samples <- nrow(pred)
    n_times <- .samples_ntimes(samples)
    n_bands <- length(.samples_bands(samples))
    x <- array(as.matrix(.pred_features(pred)), dim = c(n_samples, n_times, n_bands))
    x <- torch::torch_tensor(x, dtype = torch::torch_float())
    # Get embeddings
    embeddings_np <- NULL
    internal_model$eval()
    torch::with_no_grad({
        x <- torch::torch_transpose(x, 2L, 3L)  # Shape: (batch, channels, time)
        x <- internal_model$conv_bn_relu1(x)
        x <- internal_model$conv_bn_relu2(x)
        x <- internal_model$conv_bn_relu3(x)
        x <- internal_model$flatten(x)          # Shape: (batch, flattened_dim) → (batch, 1472) or similar
        embeddings <- internal_model$dense(x)   # Shape: (batch, 256) → perfect for t-SNE
        embeddings_np <- as.array(embeddings)
    })
    # Original labels
    labels <- samples[["label"]]
    # Handle duplicates
    if (remove_duplicates && any(duplicated(embeddings_np))) {
        dup_idx <- !duplicated(embeddings_np)
        embeddings_np <- embeddings_np[dup_idx, , drop = FALSE]
        labels <- labels[dup_idx]
        warning(.conf("messages", "sits_tsne_duplicated_embeddings"))
    }

    # Validating and clamping perplexity
    N <- nrow(embeddings_np)
    .check_num(N, exclusive_min = 3L)
    max_perp <- floor((N - 1L) / 3L)
    if(perplexity < 0) {
        warning(.conf("messages", "sits_tsne_neg_perp"))
        perplexity <- 30
    }
    if(perplexity > max_perp) {
        warning(.conf("messages", "sits_tsne_max_perp"))
        perplexity <- max(5, max_perp)
    }

    # Run t-SNE
    tsne_result <- Rtsne::Rtsne(embeddings_np, perplexity = perplexity, max_iter = as.integer(rounds))
    # Return results
    result <- list(
        tsne = tsne_result,
        labels = labels
    )
    class(result) <- "sits_tsne"
    return(result)
}

#' @title Run t-SNE on sits torch models
#' @name sits_tsne.torch_model_mlp
#' @description
#' Applies t-SNE for Multilayer Perceptron (MLP) models using temporal encoder outputs as embeddings.
#' @inheritParams sits_tsne
#' @param remove_duplicates Logical. Should duplicated embeddings be removed before running t-SNE? Default is TRUE.
#' @param perplexity Numeric; Rtsne perplexity (default \code{30}). Will be clamped to
#'   \code{floor((N - 1) / 3)} based on the number of (possibly deduped) points.
#' @param rounds Integer; number of optimization iterations in t-SNE
#'   (maps to \code{Rtsne::Rtsne(max_iter=)}; default \code{1000}).
#' @importFrom Rtsne Rtsne
#' @export
sits_tsne.torch_model_mlp <- function(model, samples, remove_duplicates = TRUE, perplexity = 30, rounds = 1000, ...) {
    # Check required packages
    .check_require_packages(c("torch", "Rtsne"))
    # Retrieve internal torch model
    internal_model <- .ml_model(model)[["model"]]
    internal_model$eval()
    # Prepare and normalize input samples
    pred <- .predictors(samples)
    ml_stats <- .samples_stats(samples)
    pred <- .pred_normalize(pred, ml_stats)
    # Convert to tensor
    n_samples <- nrow(pred)
    n_times <- .samples_ntimes(samples)
    n_bands <- length(.samples_bands(samples))
    x <- array(as.matrix(.pred_features(pred)), dim = c(n_samples, n_times, n_bands))
    x <- torch::torch_tensor(x, dtype = torch::torch_float())
    # Get embeddings
    embeddings_np <- NULL
    internal_model$eval()
    # Flatten temporal and band dimensions
    x_np <- as.array(x)
    n_samples <- dim(x_np)[1]
    n_times <- dim(x_np)[2]
    n_bands <- dim(x_np)[3]
    x_flat <- matrix(x_np, nrow = n_samples, ncol = n_times * n_bands)
    x_flat <- torch::torch_tensor(x_flat, dtype = torch::torch_float())
    embeddings_np <- torch::with_no_grad({
        x_input <- x_flat
        for (i in 1:(length(internal_model$model) - 1)) {
            x_input <- internal_model$model[[i]](x_input)
        }
        as.array(x_input)
    })
    # Original labels
    labels <- samples[["label"]]
    # Handle duplicates
    if (remove_duplicates && any(duplicated(embeddings_np))) {
        dup_idx <- !duplicated(embeddings_np)
        embeddings_np <- embeddings_np[dup_idx, , drop = FALSE]
        labels <- labels[dup_idx]
        warning(.conf("messages", "sits_tsne_duplicated_embeddings"))
    }
    # Validating and clamping perplexity
    N <- nrow(embeddings_np)
    .check_num(N, exclusive_min = 3L)
    max_perp <- floor((N - 1L) / 3L)
    if(perplexity < 0) {
        warning(.conf("messages", "sits_tsne_neg_perp"))
        perplexity <- 30
    }
    if(perplexity > max_perp) {
        warning(.conf("messages", "sits_tsne_max_perp"))
        perplexity <- max(5, max_perp)
    }

    # Run t-SNE
    tsne_result <- Rtsne::Rtsne(embeddings_np, perplexity = perplexity, max_iter = as.integer(rounds))
    # Return results
    result <- list(
        tsne = tsne_result,
        labels = labels
    )
    class(result) <- "sits_tsne"
    return(result)
}

#' @title Run t-SNE on sits torch models
#' @name sits_tsne.torch_model_lstm_fcn
#' @description
#' Applies t-SNE for lstm-fcn models using temporal encoder outputs as embeddings.
#' @inheritParams sits_tsne
#' @param remove_duplicates Logical. Should duplicated embeddings be removed before running t-SNE? Default is TRUE.
#' @param perplexity Numeric; Rtsne perplexity (default \code{30}). Will be clamped to
#'   \code{floor((N - 1) / 3)} based on the number of (possibly deduped) points.
#' @param rounds Integer; number of optimization iterations in t-SNE
#'   (maps to \code{Rtsne::Rtsne(max_iter=)}; default \code{1000}).
#' @importFrom Rtsne Rtsne
#' @export
sits_tsne.torch_model_lstm_fcn <- function(model, samples, remove_duplicates = TRUE, perplexity = 30, rounds = 1000, ...) {
    # Check required packages
    .check_require_packages(c("torch", "Rtsne"))
    # Retrieve internal torch model
    internal_model <- .ml_model(model)[["model"]]
    internal_model$eval()
    # Prepare and normalize input samples
    pred <- .predictors(samples)
    ml_stats <- .samples_stats(samples)
    pred <- .pred_normalize(pred, ml_stats)
    # Convert to tensor
    n_samples <- nrow(pred)
    n_times <- .samples_ntimes(samples)
    n_bands <- length(.samples_bands(samples))
    x <- array(as.matrix(.pred_features(pred)), dim = c(n_samples, n_times, n_bands))
    x <- torch::torch_tensor(x, dtype = torch::torch_float())
    # Get embeddings
    embeddings_np <- NULL
    internal_model$eval()
    torch::with_no_grad({
        # dimension shift and LSTM forward pass
        lstm_out <- x$permute(c(1, 3, 2)) |>
            internal_model$lstm()
        # FCN forward pass
        fcn_out <- x$permute(c(1, 3, 2)) |>
            internal_model$conv_bn_relu1() |>
            internal_model$conv_bn_relu2() |>
            internal_model$conv_bn_relu3() |>
            internal_model$pooling()
        # Concatenate upper and lower branches
        combined <- torch::torch_cat(list(lstm_out[[1]], fcn_out), dim = 2)
        embeddings <- internal_model$flatten(combined)
        embeddings_np <- as.array(embeddings)
    })
    # Original labels
    labels <- samples[["label"]]
    # Handle duplicates
    if (remove_duplicates && any(duplicated(embeddings_np))) {
        dup_idx <- !duplicated(embeddings_np)
        embeddings_np <- embeddings_np[dup_idx, , drop = FALSE]
        labels <- labels[dup_idx]
        warning(.conf("messages", "sits_tsne_duplicated_embeddings"))
    }

    # Validating and clamping perplexity
    N <- nrow(embeddings_np)
    .check_num(N, exclusive_min = 3L)
    max_perp <- floor((N - 1L) / 3L)
    if(perplexity < 0) {
        warning(.conf("messages", "sits_tsne_neg_perp"))
        perplexity <- 30
    }
    if(perplexity > max_perp) {
        warning(.conf("messages", "sits_tsne_max_perp"))
        perplexity <- max(5, max_perp)
    }

    # Run t-SNE
    tsne_result <- Rtsne::Rtsne(embeddings_np, perplexity = perplexity, max_iter = as.integer(rounds))
    # Return both t-SNE result and aligned labels
    result <- list(
        tsne = tsne_result,
        labels = labels
    )
    class(result) <- "sits_tsne"
    return(result)
}

#' @title Run t-SNE on sits torch models
#' @name sits_tsne.torch_model_mae_encoder
#' @description
#' Applies t-SNE for masked autoencoder models using the MAE's encoder outputs as embeddings.
#' @inheritParams sits_tsne
#' @param remove_duplicates Logical. Should duplicated embeddings be removed before running t-SNE? Default is TRUE.
#' @param perplexity Numeric; Rtsne perplexity (default \code{30}). Will be clamped to
#'   \code{floor((N - 1) / 3)} based on the number of (possibly deduped) points.
#' @param rounds Integer; number of optimization iterations in t-SNE
#'   (maps to \code{Rtsne::Rtsne(max_iter=)}; default \code{1000}).
#' @importFrom Rtsne Rtsne
#' @export
sits_tsne.torch_model_mae_encoder <- function(model, samples, remove_duplicates = TRUE, perplexity = 30, rounds = 1000, ...) {
    # Check required packages
    .check_require_packages(c("torch", "Rtsne"))
    # Retrieve internal torch model
    internal_model <- attr(model, "get_model")()
    internal_model$eval()
    # Prepare and normalize input samples
    pred <- .predictors(samples)
    ml_stats <- .samples_stats(samples)
    pred <- .pred_normalize(pred, ml_stats)
    # Convert to tensor
    n_samples <- nrow(pred)
    n_times <- .samples_ntimes(samples)
    n_bands <- length(.samples_bands(samples))
    x <- array(as.matrix(.pred_features(pred)), dim = c(n_samples, n_times, n_bands))
    x <- torch::torch_tensor(x, dtype = torch::torch_float())
    # Get embeddings
    embeddings_np <- NULL
    internal_model$eval()
    torch::with_no_grad({
        embeddings <- internal_model(x)
        embeddings_np <- as.array(embeddings)
    })
    # Original labels
    labels <- samples[["label"]]
    # Handle duplicates
    if (remove_duplicates && any(duplicated(embeddings_np))) {
        dup_idx <- !duplicated(embeddings_np)
        embeddings_np <- embeddings_np[dup_idx, , drop = FALSE]
        labels <- labels[dup_idx]
        warning(.conf("messages", "sits_tsne_duplicated_embeddings"))
    }

    # Validating and clamping perplexity
    N <- nrow(embeddings_np)
    .check_num(N, exclusive_min = 3L)
    max_perp <- floor((N - 1L) / 3L)
    if(perplexity < 0) {
        warning(.conf("messages", "sits_tsne_neg_perp"))
        perplexity <- 30
    }
    if(perplexity > max_perp) {
        warning(.conf("messages", "sits_tsne_max_perp"))
        perplexity <- max(5, max_perp)
    }

    # Run t-SNE
    tsne_result <- Rtsne::Rtsne(embeddings_np, perplexity = perplexity, max_iter = as.integer(rounds))
    # Return both t-SNE result and aligned labels
    result <- list(
        tsne = tsne_result,
        labels = labels
    )
    class(result) <- "sits_tsne"
    return(result)
}


#' @title Run t-SNE on sits torch models
#' @name sits_tsne.torch_model_contrastive_tcnnr
#' @description
#' Applies t-SNE for a contrastive-trained tempcnn model using its outputs as embeddings.
#' @inheritParams sits_tsne
#' @param remove_duplicates Logical. Should duplicated embeddings be removed before running t-SNE? Default is TRUE.
#' @param perplexity Numeric; Rtsne perplexity (default \code{30}). Will be clamped to
#'   \code{floor((N - 1) / 3)} based on the number of (possibly deduped) points.
#' @param rounds Integer; number of optimization iterations in t-SNE
#'   (maps to \code{Rtsne::Rtsne(max_iter=)}; default \code{1000}).
#' @importFrom Rtsne Rtsne
#' @export
sits_tsne.torch_model_contrastive_tcnn <- function(model, samples, remove_duplicates = TRUE, perplexity = 30, rounds = 1000, ...) {
    # Check required packages
    .check_require_packages(c("torch", "Rtsne"))
    # Retrieve internal torch model
    internal_model <- attr(model, "get_model")()
    internal_model$eval()
    # Prepare and normalize input samples
    pred <- .predictors(samples)
    ml_stats <- .samples_stats(samples)
    pred <- .pred_normalize(pred, ml_stats)
    # Convert to tensor
    n_samples <- nrow(pred)
    n_times <- .samples_ntimes(samples)
    n_bands <- length(.samples_bands(samples))
    x <- array(as.matrix(.pred_features(pred)), dim = c(n_samples, n_times, n_bands))
    x <- torch::torch_tensor(x, dtype = torch::torch_float())
    # Get embeddings
    embeddings_np <- NULL
    internal_model$eval()
    torch::with_no_grad({
        embeddings <- internal_model(x)
        embeddings_np <- as.array(embeddings)
    })
    # Original labels
    labels <- samples[["label"]]
    # Handle duplicates
    if (remove_duplicates && any(duplicated(embeddings_np))) {
        dup_idx <- !duplicated(embeddings_np)
        embeddings_np <- embeddings_np[dup_idx, , drop = FALSE]
        labels <- labels[dup_idx]
        warning(.conf("messages", "sits_tsne_duplicated_embeddings"))
    }

    # Validating and clamping perplexity
    N <- nrow(embeddings_np)
    .check_num(N, exclusive_min = 3L)
    max_perp <- floor((N - 1L) / 3L)
    if(perplexity < 0) {
        warning(.conf("messages", "sits_tsne_neg_perp"))
        perplexity <- 30
    }
    if(perplexity > max_perp) {
        warning(.conf("messages", "sits_tsne_max_perp"))
        perplexity <- max(5, max_perp)
    }

    # Run t-SNE
    tsne_result <- Rtsne::Rtsne(embeddings_np, perplexity = perplexity, max_iter = as.integer(rounds))
    # Return both t-SNE result and aligned labels
    result <- list(
        tsne = tsne_result,
        labels = labels
    )
    class(result) <- "sits_tsne"
    return(result)
}


#' @export
sits_tsne.default <- function(model, samples, ...) {
    stop(.conf("messages", "sits_tsne_unsupported_method"))
}
