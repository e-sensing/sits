#' @title Run t-SNE on sits torch models
#' @name sits_tsne
#'
#' @description
#' \code{sits_tsne()} applies t-SNE dimensionality reduction to embeddings
#' produced by a \code{sits} Torch model for a given set of samples.
#' Embeddings are extracted from the model's encoder by replacing the last
#' top-level \code{nn_module} with an identity layer, so the forward pass
#' returns feature representations rather than final predictions.
#'
#' The function optionally removes duplicated embeddings before running
#' t-SNE to avoid numerical issues and to keep the output aligned with
#' unique feature vectors.
#'
#' @param model A \code{sits} Torch model, typically returned by
#'   \code{\link[sits]{sits_train}}.
#' @param samples A \code{sits} tibble containing time series samples used
#'   to compute embeddings. Labels are retrieved from the \code{label}
#'   column.
#' @param remove_duplicates Logical. If \code{TRUE} (default), remove
#'   duplicated embeddings before running t-SNE and drop the corresponding
#'   labels.
#' @param perplexity Numeric. t-SNE perplexity parameter. Values are
#'   validated against the number of samples and clamped to the maximum
#'   supported by the t-SNE implementation.
#' @param rounds Integer. Number of t-SNE iterations (\code{max_iter}).
#' @param ... Additional arguments passed to methods. Currently unused.
#'
#' @details
#' Input samples are converted to predictors and normalized using the
#' sample statistics associated with \code{samples}. The predictors are
#' reshaped into a 3D tensor of shape \code{[n_samples, n_times, n_bands]}
#' and passed through the internal Torch model in evaluation mode.
#'
#' When \code{remove_duplicates = TRUE}, duplicated rows in the embedding
#' matrix are removed before running t-SNE. A warning is emitted and both
#' embeddings and labels are subset to the unique rows to preserve
#' alignment.
#'
#' The \code{perplexity} parameter is validated to be non-negative and to
#' satisfy the constraint \code{perplexity <= floor((N - 1) / 3)}, where
#' \code{N} is the number of (possibly deduplicated) embedding vectors.
#'
#' @return A named list of class \code{"sits_tsne"} with:
#' \itemize{
#'   \item \code{tsne}: An \code{Rtsne} object containing the embedding
#'   coordinates in \code{tsne$Y}.
#'   \item \code{labels}: A character vector of labels aligned with rows in
#'   \code{tsne$Y}.
#' }
#'
#' @examples
#' if (sits_run_examples()) {
#'     samples <- samples_modis_ndvi
#'     model <- sits_train(
#'         samples = samples,
#'         ml_method = sits_tae(epochs = 2)
#'     )
#'     tsne <- sits_tsne(model, samples, perplexity = 30, rounds = 100)
#'     plot(tsne)
#' }
#'
sits_tsne <- function(embeddings,
                      remove_duplicates = TRUE,
                      perplexity = 30,
                      rounds = 1000,
                      ...) {
    # Check required packages
    .check_require_packages(c("Rtsne"))

    # TODO implement .check_sits_tsne()

    # Get embeddings' labels
    labels <- embeddings[["label"]]
    # Get embeddings' predictors
    pred <- embeddings |>
        .predictors() |>
        .pred_features()

    # Handle duplicates
    if (remove_duplicates && any(duplicated(pred))) {
        dup_idx <- !duplicated(pred)
        pred <- pred[dup_idx, , drop = FALSE]
        labels <- labels[dup_idx]
        warning(.conf("messages", "sits_tsne_duplicated_embeddings"))
    }

    # Validating and clamping perplexity
    N <- nrow(pred)
    .check_num(N, exclusive_min = 3L)
    max_perp <- floor((N - 1L) / 3L)
    if (perplexity < 0) {
        warning(.conf("messages", "sits_tsne_neg_perp"))
        perplexity <- 30
    }
    if (perplexity > max_perp) {
        warning(.conf("messages", "sits_tsne_max_perp"))
        perplexity <- max(5, max_perp)
    }

    # Run t-SNE
    tsne_result <- Rtsne::Rtsne(
        pred,
        perplexity = perplexity,
        max_iter = as.integer(rounds)
    )

    # Return both t-SNE result and aligned labels
    result <- list(
        tsne = tsne_result,
        labels = labels
    )

    class(result) <- "sits_tsne"
    return(result)
}
