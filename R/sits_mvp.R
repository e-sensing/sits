#' @title Create pseudo-labeled samples based on structural break magnitude
#'
#' @author Alexandre Assunção \email{alexcarssuncao@@gmail.com}
#'
#' @description
#' Function that assigns pseudo-labels to time series samples
#' based on the magnitude of their strongest structural break.
#'
#' For each sample, the function computes the best split that maximizes
#' the drop in signal (using \code{.mvp_best_split_drop()}) and extracts
#' a break score. The top and bottom \code{q} fraction of samples
#' (according to the break score) are selected and assigned pseudo-labels:
#' \code{"Event"} for large drops and \code{"NoEvent"} for small drops.
#'
#' The resulting dataset contains only the most unambiguous cases
#' (extreme tails of the score distribution), suitable for downstream
#' embedding learning, contrastive training, or event detection tasks.
#'
#' @param samples A \code{sits} tibble containing a \code{time_series}
#'   list-column. Each element must be a tibble with an \code{Index}
#'   column (dates) and one or more numeric band/index columns.
#'
#' @param q A numeric value in (0, 0.5], indicating the fraction of
#'   samples to select from each tail of the break score distribution.
#'   Default is \code{0.1}, meaning the top 10\% and bottom 10\%
#'   are retained.
#'
#' @param dir Optional character string indicating the direction of the
#'   break to prioritize (e.g., decreasing or increasing signal).
#'   Passed to \code{.mvp_best_split_drop()}. Default is \code{NULL}.
#'
#' @param combine Character string specifying how multiple bands/indices
#'   are combined when computing the break score. Passed to
#'   \code{.mvp_best_split_drop()}. Default is \code{"product"}.
#'
#' @return A \code{sits} tibble containing only the selected samples,
#'   with two additional columns:
#'   \describe{
#'     \item{break_score}{Numeric score measuring the magnitude of
#'       the strongest structural break.}
#'     \item{break_k}{Integer index of the optimal split position.}
#'   }
#'   The \code{label} column is overwritten with pseudo-labels
#'   \code{"Event"} and \code{"NoEvent"}.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Computes the best structural break for each sample.
#'   \item Ranks samples by break magnitude.
#'   \item Selects the top and bottom \code{q} proportion.
#'   \item Assigns pseudo-labels based on extreme break behavior.
#' }
#'
#' This approach assumes that strong signal drops correspond to
#' meaningful change events (e.g., deforestation), while weak or
#' negligible changes correspond to stable conditions.
sits_mvp <- function(samples,
                     q = 0.1,
                     dir = NULL,
                     combine = "product") {

    # Number of raw samples
    n <- nrow(samples)
    # Avoid add a global variable for 'Index'
    Index <- NULL

    # TODO Make sure dir param makes sense with samples' bands

    # Computing break score for base samples
    best_split_drops <- lapply(seq_len(n), function(idx) {
        ts <- samples[idx, ]$time_series[[1]] |>
            dplyr::select(-Index)
        .mvp_best_split_drop(ts, dir = dir, combine = combine)
    })

    # Assingning pseudo-labels to q% top and bottom samples
    scores <- vapply(best_split_drops, `[[`, numeric(1), "score")
    k_best <- vapply(best_split_drops, `[[`, integer(1), "k")

    # How many samples in each tail
    m <- max(1L, floor(q * n))

    # Sort scores in ascending order
    ord <- order(scores)
    idx_low  <- ord[1:m]
    idx_high <- ord[(n - m + 1):n]

    # Creating samples to generate embeddings
    scored_samples <- samples |>
        dplyr::mutate(break_score = scores, break_k = k_best)
    h_samples <- scored_samples[idx_high, ]
    h_samples$label <- rep("Event", m)
    l_samples <- scored_samples[idx_low, ]
    l_samples$label <- rep("NoEvent", m)

    # Reassign samples to contain only unambiguous cases
    samples <- dplyr::bind_rows(h_samples, l_samples)
    # Return the new samples
    samples
}
