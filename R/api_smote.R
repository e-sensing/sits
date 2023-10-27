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
        data$dummy__col__ <- 0
    }
    # perform SMOTE
    smoteret <- .smote_apply(data[, -col_ind],
        data[, col_ind],
        dup_size = dup_size
    )
    # rbind the original observations and sufficient samples of the synthetic
    # ones
    orig <- smoteret$orig_P
    target_samp <- m - nrow(orig)
    synt <- smoteret$syn_data[sample.int(
        nrow(smoteret$syn_data),
        size = target_samp,
        replace = target_samp > nrow(smoteret$syn_data)
    ), ]
    d_prime <- rbind(orig, synt)
    colnames(d_prime)[ncol(d_prime)] <- cls_col
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
#' @param K The number of nearest neighbors during sampling process
#' @param dup_size The maximum times of synthetic minority instances
#'                  over original majority instances in the oversampling.
#'
#' @references
#'   Chawla, N., Bowyer, K., Hall, L. and Kegelmeyer, W. 2002.
#'   SMOTE: Synthetic minority oversampling technique.
#'   Journal of Artificial Intelligence Research. 16, 321-357.
#' @return A list with the following values.
#'
.smote_apply <- function(data, target, K = 5, dup_size = 0) {
    ncD <- ncol(data) # The number of attributes
    n_target <- table(target)
    # Extract a set of positive instances
    P_set <- subset(
        data,
        target == names(which.min(n_target))
    )[sample(min(n_target)), ]
    N_set <- subset(
        data,
        target != names(which.min(n_target))
    )
    P_class <- rep(names(which.min(n_target)), nrow(P_set))

    N_class <- target[target != names(which.min(n_target))]
    # The number of positive instances
    sizeP <- nrow(P_set)
    # Get k nearest neighbors
    knear <- .smote_knearest(P_set, P_set, K)
    sum_dup <- dup_size
    syn_dat <- NULL
    for (i in 1:sizeP) {
        if (is.matrix(knear)) {
            pair_idx <- knear[i, ceiling(stats::runif(sum_dup) * K)]
        } else {
            pair_idx <- rep(knear[i], sum_dup)
        }
        g <- stats::runif(sum_dup)
        P_i <- matrix(unlist(P_set[i, ]), sum_dup, ncD, byrow = TRUE)
        Q_i <- as.matrix(P_set[pair_idx, ])
        syn_i <- P_i + g * (Q_i - P_i)
        syn_dat <- rbind(syn_dat, syn_i)
    }

    P_set[, ncD + 1] <- P_class
    colnames(P_set) <- c(colnames(data), "class")
    N_set[, ncD + 1] <- N_class
    colnames(N_set) <- c(colnames(data), "class")

    rownames(syn_dat) <- NULL
    syn_dat <- data.frame(syn_dat)
    syn_dat[, ncD + 1] <- rep(names(which.min(n_target)), nrow(syn_dat))
    colnames(syn_dat) <- c(colnames(data), "class")
    NewD <- rbind(P_set, syn_dat, N_set)
    rownames(NewD) <- NULL
    D_result <- list(
        data = NewD,
        syn_data = syn_dat,
        orig_N = N_set,
        orig_P = P_set,
        K = K,
        K_all = NULL,
        dup_size = sum_dup,
        outcast = NULL,
        eps = NULL,
        method = "SMOTE"
    )
    class(D_result) <- "gen_data"

    return(D_result)
}
#' @title Find K nearest neighbors
#' @keywords internal
#' @noRd
#' @param D  Query data matrix
#' @param P  Input data matrix
#' @param n_clust maximum number of nearest neighbors to search
#' @return Index matrix of K nearest neighbor for each instance
.smote_knearest <- function(D, P, n_clust) {
    .check_require_packages("FNN")

    knD <- FNN::knnx.index(D, P, k = (n_clust + 1), algorithm = "kd_tree")
    knD <- knD * (knD != row(knD))
    que <- which(knD[, 1] > 0)
    for (i in que) {
        knD[i, which(knD[i, ] == 0)] <- knD[i, 1]
        knD[i, 1] <- 0
    }
    return(knD[, 2:(n_clust + 1)])
}
