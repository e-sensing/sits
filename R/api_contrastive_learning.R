#' @title Split samples into train/val pairs for label-guided pre-training
#' @name .contrastive_learning_data_split
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Alexandre Assuncao, \email{alexcarssuncao@@gmail.com}
#'
#' @description
#' Normalises \code{samples}, performs a train/validation split, and builds
#' view-pairs used as input to the contrastive loss. Unlike Barlow Twins, the
#' returned structure also includes integer label vectors because the
#' contrastive loss requires labels to build the positive mask.
#'
#' Two pairing strategies are supported:
#' \describe{
#'   \item{\code{"label"}}{For each anchor, the second view is drawn from
#'     the same class label, giving two semantically consistent views.
#'     Singletons fall back to self-pairing and trigger a warning.}
#'   \item{\code{"random"}}{Both views are drawn independently and uniformly
#'     at random from the split, regardless of label.}
#' }
#'
#' @param samples          \code{sits} tibble of labelled time-series samples.
#' @param validation_split Numeric in (0, 1). Fraction held out for validation.
#' @param num_pairs        Integer or \code{NULL}. Number of pairs per split.
#'   Defaults to the number of samples in each split when \code{NULL}.
#'
#' @return A named list with elements \code{train} and \code{val}.  Each
#'   element is itself a list with components:
#'   \describe{
#'     \item{\code{a}}{Numeric matrix of shape
#'       \code{[n_pairs, n_times * n_bands]} for view A.}
#'     \item{\code{b}}{Numeric matrix of same shape for view B.}
#'     \item{\code{labels}}{Integer vector of length \code{n_pairs} giving
#'       the class code (1-based) for each pair's anchor.}
#'   }
#'
.contrastive_learning_data_split <- function(samples,
                                             validation_split,
                                             num_pairs       = NULL) {
    # Compute normalisation statistics and build normalised feature matrix
    ml_stats <- .samples_stats(samples)
    preds    <- .predictors(samples)
    # [n, n_times*n_bands]
    feats    <- .pred_features_normalize(preds, stats = ml_stats)
    # length-n character vector
    labels_chr <- .pred_references(preds)           # character vector

    # Build integer label codes (1-based)
    unique_labels <- .samples_labels(samples)
    code_labels   <- seq_along(unique_labels)
    names(code_labels) <- unique_labels
    labels_int <- unname(code_labels[labels_chr])

    n_samples <- nrow(feats)

    # Train / validation split
    idx   <- sample.int(n_samples)
    n_val <- floor(length(idx) * validation_split)
    if (n_val > 0L) {
        val_idx   <- idx[seq_len(n_val)]
        train_idx <- idx[-seq_len(n_val)]
    } else {
        val_idx   <- integer(0L)
        train_idx <- idx
    }

    # Resolve number of pairs for each split
    np_train <- if (.has(num_pairs)) {
        min(num_pairs, length(train_idx))
    } else {
        length(train_idx)
    }
    np_val <- if (length(val_idx) > 0L) {
        if (.has(num_pairs)) {
            max(1L, floor(num_pairs * validation_split))
        } else {
            length(val_idx)
        }
    } else {
        0L
    }

    # ------------------------------------------------------------------
    # Inner helper: build one set of pairs from a given set of indices
    # ------------------------------------------------------------------
    make_pairs <- function(split_idx, n_pairs) {
        n <- length(split_idx)

        # Handle degenerate case
        if (n == 0L) {
            empty <- matrix(numeric(0L), nrow = 0L, ncol = ncol(feats))
            return(list(a = empty, b = empty, labels = integer(0L)))
        }

        # Sample anchors (view A)
        a_idx <- sample(split_idx, n_pairs, replace = TRUE)

        # Sample positives (view B)
        split_labels <- labels_chr[split_idx]
        has_singleton <- FALSE

        b_idx <- vapply(a_idx, function(ai) {
            same_class <- split_idx[split_labels == labels_chr[ai]]
            candidates <- same_class[same_class != ai]
            if (length(candidates) >= 1L) {
                sample(candidates, 1L)
            } else {
                has_singleton <<- TRUE
                ai
            }
        }, integer(1L))

        if (has_singleton && .message_warnings()) {
            warning(
                .conf("messages",
                      "sits_contrastive_learning_singleton_classes"),
                call. = FALSE
            )
        }

        list(
            a      = feats[a_idx, , drop = FALSE],
            b      = feats[b_idx, , drop = FALSE],
            labels = labels_int[a_idx]
        )
    }

    list(
        train = make_pairs(train_idx, np_train),
        val   = make_pairs(val_idx,   np_val)
    )
}

#' @title Torch Dataset for two-view pairs fo time series with labels
#' @name .contrastive_learning_dataset
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description
#' A \code{torch::dataset} that yields one item per view-pair. Each item is a
#' list with:
#' \describe{
#'   \item{\code{x}}{A \code{float} tensor of shape
#'     \code{[2, n_times, n_bands]}, where index 1 is view A and index 2
#'     is view B.}
#'   \item{\code{y}}{A scalar \code{long} tensor giving the integer class
#'     label of the anchor (used by the contrastive loss to
#'     build the positive mask within the batch).}
#' }
#'
#' @param pairs   List with elements \code{a}, \code{b} (each a numeric
#'   matrix of shape \code{[n_pairs, n_times * n_bands]}) and \code{labels}
#'   (integer vector of length \code{n_pairs}).
#' @param n_times Integer. Number of time steps in the time series.
#'
#' @return A \code{torch::dataset} object compatible with
#'   \code{torch::dataloader}.
#'
.contrastive_learning_dataset <- torch::dataset(
    name = "ConstLearningDataset",

    initialize = function(pairs, n_times) {
        self$a       <- pairs[["a"]]
        self$b       <- pairs[["b"]]
        self$labels  <- pairs[["labels"]]
        self$n_times <- as.integer(n_times)
        self$n_bands <- as.integer(ncol(pairs[["a"]]) / n_times)
    },

    .getitem = function(i) {
        nt <- self$n_times
        nb <- self$n_bands

        view_a <- torch::torch_tensor(
            array(self$a[i, ], dim = c(nt, nb)),
            dtype = torch::torch_float()
        )
        view_b <- torch::torch_tensor(
            array(self$b[i, ], dim = c(nt, nb)),
            dtype = torch::torch_float()
        )

        list(
            x = torch::torch_stack(list(view_a, view_b)),
            y = torch::torch_tensor(
                self$labels[i], dtype = torch::torch_long()
            )
        )
    },

    .length = function() {
        nrow(self$a)
    }
)
#' @title Supervised Contrastive Loss (SupCon)
#' @name .contrastive_learning_loss
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description
#' Implements the supervised contrastive loss (SupCon) from Khosla et al.
#' (2020), following the reference code by Tian (HobbitLong/SupContrast)
#' with the numerical stabilisation trick from the StableRep variant
#' (google-research/syn-rep-learn).
#'
#' SupCon uses \strong{all} views as both anchors and contrast.  Both views
#' are concatenated into a single pool of \code{2B} embeddings, and each
#' embedding is contrasted against every other embedding (excluding
#' itself).  This yields more informative gradients and is the
#' formulation recommended in the original paper (Eq. 2).
#'
#' @param input       Tensor of shape \code{[B, 2, proj_dim]} with two
#'   L2-normalised views per sample.
#' @param target      Long tensor of shape \code{[B]} with integer class
#'   labels.
#' @param scaling Numeric. Temperature scaling for the cosine
#'   similarities.
#'
#' @return A scalar tensor with the mean SupCon loss.
#'
#' @references
#' Khosla, P., Teterwak, P., Wang, C., et al. (2020).
#' \emph{Supervised Contrastive Learning}. arXiv:2004.11362.
#'
#' Tian, Y. (2020). Reference implementation.
#' \url{https://github.com/HobbitLong/SupContrast/blob/master/losses.py}
#'
.contrastive_learning_loss <- function(input, target, scaling) {
    # input: [B, 2, proj_dim]
    batch_size <- input$size(1)
    device <- input$device

    # 1. Unbind views and concatenate: [2B, D]
    z_a <- input[, 1, ]
    z_b <- input[, 2, ]
    contrast_feature <- torch::torch_cat(list(z_a, z_b), dim = 1L)
    # contrast_mode = "all": anchor = contrast
    anchor_feature <- contrast_feature
    n_views <- 2L

    # 2. Tile labels for the concatenated pool: [2B]
    labels_all <- torch::torch_cat(list(target, target), dim = 1L)
    labels_col <- labels_all$contiguous()$view(c(-1, 1))

    # 3. Positive mask: same-class pairs [2B, 2B]
    pos_mask <- torch::torch_eq(
        labels_col, labels_col$t()
    )$to(dtype = torch::torch_float(), device = device)

    # 4. Self-contrast mask: exclude the diagonal [2B, 2B]
    total <- batch_size * n_views
    self_mask <- (
        1.0 - torch::torch_eye(total, device = device)
    )$to(dtype = torch::torch_float())

    # Positives must not include self
    pos_mask <- pos_mask * self_mask

    # 5. Compute logits: cosine similarity / temperature
    logits <- torch::torch_matmul(
        anchor_feature, contrast_feature$t()
    ) / scaling

    # Numerical stability (StableRep trick): subtract row-wise max
    logits_max <- logits$max(dim = 2L, keepdim = TRUE)[[1]]$detach()
    logits <- logits - logits_max

    # 6. Log-probabilities (denominator excludes self)
    exp_logits <- torch::torch_exp(logits) * self_mask
    log_prob <- logits - torch::torch_log(
        exp_logits$sum(dim = 2L, keepdim = TRUE)
    )

    # 7. Mean log-prob over positives per anchor
    num_pos <- torch::torch_clamp(pos_mask$sum(dim = 2L), min = 1)
    mean_log_prob_pos <- (pos_mask * log_prob)$sum(dim = 2L) / num_pos

    # 8. Loss
    loss <- -mean_log_prob_pos
    loss$view(c(n_views, batch_size))$mean()
}
