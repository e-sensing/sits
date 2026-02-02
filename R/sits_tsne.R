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
sits_tsne <- function(model, samples, remove_duplicates = TRUE, perplexity = 30, rounds = 1000, ...){
    # Check required packages
    .check_require_packages(c("torch", "Rtsne"))

    # TODO implement .check_sits_tsne()

    # Retrieve internal torch model
    full_internal_model <- environment(model)[["model"]]

    # Pop linear layer to obtain encoder
    pop_last_top_level <- function(model) {
        # 1) derive top-level names from state_dict keys
        sd <- model$state_dict()
        if (length(sd) == 0) stop("Empty state_dict(); cannot infer modules.")
        keys <- names(sd)

        # first token before '.'; keep order of first appearance
        top <- character(0)
        seen <- new.env(parent = emptyenv())
        for (k in keys) {
            nm <- sub("^([^\\.]+).*", "\\1", k)
            if (!exists(nm, envir = seen, inherits = FALSE)) {
                assign(nm, TRUE, envir = seen)
                top <- c(top, nm)
            }
        }
        if (!length(top)) stop("Could not infer any top-level modules from state_dict().")

        # 2) find the last top-level that is actually an nn_module field and replace it
        for (nm in rev(top)) {
            mod <- tryCatch(model[[nm]], error = function(e) NULL)
            if (inherits(mod, "nn_module")) {
                model[[nm]] <- torch::nn_identity()
                return(model)
            }
        }
        stop("Found no accessible top-level nn_module to replace (from state_dict tokens).")
    }
    internal_model <- pop_last_top_level(full_internal_model)

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


