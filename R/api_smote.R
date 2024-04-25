#' @title Oversample a dataset by SMOTE.
#' @name .smote_oversample
#' @keywords internal
#' @noRd
#' @description
#' Lifted from R package "scutr".
#'
#' @param data Dataset to be oversampled.
#' @param cls Class to be oversampled.
#' @param cls_col Column containing class information.
#' @param m Desired number of samples in the oversampled data.
#'
#' @return The oversampled dataset.
#'
.smote_oversample <- function(data, cls, cls_col, m) {
    col_ind <- which(names(data) == cls_col)
    orig_cols <- names(data)
    dup_size <- ceiling(m / sum(data[[cls_col]] == cls))
    # set the class to whether it is equal to the minority class
    data[[cls_col]] <- as.factor(data[[cls_col]] == cls)
    # SMOTE breaks for one-dim datasets. This adds a dummy column
    # so SMOTE can execute in that case. This does not affect how data is
    # synthesized
    if (ncol(data) == 2) {
        data[["dummy__col__"]] <- 0
    }
    # perform SMOTE
    smote_ret <- .smote_apply(
        data = data[, -col_ind],
        target = data[, col_ind],
        dup_size = dup_size
    )
    # rbind the original observations and sufficient samples of the synthetic
    # ones
    orig <- smote_ret[["orig_p"]]
    target_samp <- m - nrow(orig)
    synt <- smote_ret[["syn_data"]][
        sample.int(
            nrow(smote_ret[["syn_data"]]),
            size = target_samp,
            replace = target_samp > nrow(smote_ret[["syn_data"]])
        ),
    ]
    d_prime <- rbind(orig, synt)
    colnames(d_prime)[[ncol(d_prime)]] <- cls_col
    d_prime[[cls_col]] <- cls
    # remove the dummy column if necessary
    d_prime <- d_prime[, names(d_prime) != "dummy__col__"]
    # reorder the columns to be the same as the original data
    return(d_prime[, orig_cols])
}

#' @title Oversample a dataset by SMOTE.
#' @name .smote_apply
#' @keywords internal
#' @noRd
#' @description
#' Lifted from R package "smotefamily"
#' to reduce number of dependencies in "sits".
#' @author Wacharasak Siriseriwan <wacharasak.s@gmail.com>
#'
#'
#' @param data Dataset to be oversampled.
#' @param target Target data set
#' @param k The number of nearest neighbors during sampling process
#' @param dup_size The maximum times of synthetic minority instances
#'                  over original majority instances in the oversampling.
#'
#' @references
#'   Chawla, N., Bowyer, K., Hall, L. and Kegelmeyer, W. 2002.
#'   SMOTE: Synthetic minority oversampling technique.
#'   Journal of Artificial Intelligence Research. 16, 321-357.
#' @return A list with the following values.
#'
.smote_apply <- function(data, target, k = 5, dup_size = 0) {
    ncol_data <- ncol(data) # The number of attributes
    n_target <- table(target)
    # Extract a set of positive instances
    p_set <- subset(
        data,
        target == names(which.min(n_target))
    )[sample(min(n_target)), ]
    n_set <- subset(
        data,
        target != names(which.min(n_target))
    )
    p_class <- rep(names(which.min(n_target)), nrow(p_set))

    n_class <- target[target != names(which.min(n_target))]
    # The number of positive instances
    size_p <- nrow(p_set)
    # Get k nearest neighbors
    knear <- .smote_knearest(p_set, p_set, k)
    sum_dup <- dup_size
    syn_dat <- NULL
    for (i in 1:size_p) {
        if (is.matrix(knear)) {
            pair_idx <- knear[i, ceiling(stats::runif(sum_dup) * k)]
        } else {
            pair_idx <- rep(knear[i], sum_dup)
        }
        g <- stats::runif(sum_dup)
        p_i <- matrix(unlist(p_set[i, ]), sum_dup, ncol_data, byrow = TRUE)
        q_i <- as.matrix(p_set[pair_idx, ])
        syn_i <- p_i + g * (q_i - p_i)
        syn_dat <- rbind(syn_dat, syn_i)
    }

    p_set[, ncol_data + 1] <- p_class
    colnames(p_set) <- c(colnames(data), "class")
    n_set[, ncol_data + 1] <- n_class
    colnames(n_set) <- c(colnames(data), "class")

    rownames(syn_dat) <- NULL
    syn_dat <- data.frame(syn_dat)
    syn_dat[, ncol_data + 1] <- rep(names(which.min(n_target)), nrow(syn_dat))
    colnames(syn_dat) <- c(colnames(data), "class")
    new_data <- rbind(p_set, syn_dat, n_set)
    rownames(new_data) <- NULL
    d_result <- list(
        data = new_data,
        syn_data = syn_dat,
        orig_n = n_set,
        orig_p = p_set,
        k = k,
        k_all = NULL,
        dup_size = sum_dup,
        outcast = NULL,
        eps = NULL,
        method = "SMOTE"
    )
    class(d_result) <- "gen_data"

    return(d_result)
}
#' @title Find K nearest neighbors
#' @keywords internal
#' @noRd
#' @param q_data  Query data matrix
#' @param p_data  Input data matrix
#' @param n_clust maximum number of nearest neighbors to search
#' @return Index matrix of K nearest neighbor for each instance
.smote_knearest <- function(q_data, p_data, n_clust) {
    .check_require_packages("FNN")

    kn_dist <- FNN::knnx.index(q_data, p_data,
                           k = (n_clust + 1), algorithm = "kd_tree")
    kn_dist <- kn_dist * (kn_dist != row(kn_dist))
    que <- which(kn_dist[, 1] > 0)
    for (i in que) {
        kn_dist[i, which(kn_dist[i, ] == 0)] <- kn_dist[[i, 1]]
        kn_dist[[i, 1]] <- 0
    }
    return(kn_dist[, 2:(n_clust + 1)])
}
