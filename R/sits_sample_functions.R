#' @title Sample a percentage of a time series
#' @name sits_sample
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Takes a sits tibble with different labels and
#'              returns a new tibble. For a given field as a group criterion,
#'              this new tibble contains a given number or percentage
#'              of the total number of samples per group.
#'              Parameter n: number of random samples.
#'              Parameter frac: a fraction of random samples.
#'              If n is greater than the number of samples for a given label,
#'              that label will be sampled with replacement. Also,
#'              if frac > 1 , all sampling will be done with replacement.
#'
#' @param  data       Input sits tibble.
#' @param  n          Number of samples to pick from each group of data.
#' @param  frac       Percentage of samples to pick from each group of data.
#' @param  oversample Oversample classes with small number of samples?
#' @return            A sits tibble with a fixed quantity of samples.
#' @examples
#' # Retrieve a set of time series with 2 classes
#' data(cerrado_2classes)
#' # Print the labels of the resulting tibble
#' sits_labels(cerrado_2classes)
#' # Samples the data set
#' data <- sits_sample(cerrado_2classes, n = 10)
#' # Print the labels of the resulting tibble
#' sits_labels(data)
#' @export
sits_sample <- function(data,
                        n = NULL,
                        frac = NULL,
                        oversample = TRUE) {

    # set caller to show in errors
    .check_set_caller("sits_sample")

    # verify if data is valid
    .check_samples(data)

    # verify if either n or frac is informed
    .check_that(
        x = !(purrr::is_null(n) & purrr::is_null(frac)),
        local_msg = "neither 'n' or 'frac' parameters were informed",
        msg = "invalid sample parameters"
    )

    groups <- by(data, data[["label"]], list)

    result_lst <- purrr::map(groups, function(class_samples) {
        if (!purrr::is_null(n)) {
            if (n > nrow(class_samples) && !oversample) {
                # should imbalanced class be oversampled?
                nrow <- nrow(class_samples)
            } else {
                nrow <- n
            }
            result <- dplyr::slice_sample(
                class_samples,
                n = nrow,
                replace = oversample
            )
        } else {
            result <- dplyr::slice_sample(
                class_samples,
                prop = frac,
                replace = oversample
            )
        }
        return(result)
    })

    result <- dplyr::bind_rows(result_lst)

    return(result)
}


#' @title Reduce imbalance in a set of samples
#' @name sits_reduce_imbalance
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description
#' Takes a sits tibble with different labels and
#' returns a new tibble. Deals with class imbalance
#' using the synthetic minority oversampling technique (SMOTE)
#' for oversampling. Undersampling is done using the SOM methods available in
#' the sits package.
#'
#'
#' @references
#' The reference paper on SMOTE is
#' N. V. Chawla, K. W. Bowyer, L. O.Hall, W. P. Kegelmeyer,
#' “SMOTE: synthetic minority over-sampling technique,”
#' Journal of artificial intelligence research, 321-357, 2002.
#'
#' Undersampling uses the SOM map developed by Lorena Santos and co-workers
#' and used in the sits_som_map() function.
#' The SOM map technique is described in the paper:
#' Lorena Santos, Karine Ferreira, Gilberto Camara, Michelle Picoli,
#' Rolf Simoes, “Quality control and class noise reduction of satellite
#' image time series”. ISPRS Journal of Photogrammetry and Remote Sensing,
#' vol. 177, pp 75-88, 2021. https://doi.org/10.1016/j.isprsjprs.2021.04.014.
#'
#'
#' @param  samples              Sample set to rebalance
#' @param  n_samples_over       Number of samples to oversample
#'                              for classes with samples less than this number.
#' @param  n_samples_under      Number of samples to undersample
#'                              for classes with samples more than this number.
#' @param  multicores           Number of cores to process the data (default 2).
#'
#' @return A sits tibble with reduced sample imbalance.
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#' @examples
#' if (sits_run_examples()) {
#'     # print the labels summary for a sample set
#'     sits_labels_summary(samples_modis_ndvi)
#'     # reduce the sample imbalance
#'     new_samples <- sits_reduce_imbalance(samples_modis_ndvi,
#'         n_samples_over = 200,
#'         n_samples_under = 200,
#'         multicores = 1
#'     )
#'     # print the labels summary for the rebalanced set
#'     sits_labels_summary(new_samples)
#' }
#' @export
sits_reduce_imbalance <- function(samples,
                                  n_samples_over = 200,
                                  n_samples_under = 400,
                                  multicores = 2) {

    # set caller to show in errors
    .check_set_caller("sits_reduce_imbalance")
    # pre-conditions
    .check_samples_train(samples)
    .check_int_parameter(n_samples_over)
    .check_int_parameter(n_samples_under)

    # check if number of required samples are correctly entered
    .check_that(
        n_samples_under >= n_samples_over,
        local_msg = paste0(
            "number of samples to undersample for large ",
            "classes should be higher or equal to number ",
            "of samples to oversample for small classes"
        ),
        msg = "invalid 'n_samples_over' and 'n_samples_under' parameters"
    )

    bands <- sits_bands(samples)
    labels <- sits_labels(samples)

    # params of output tibble
    lat <- 0.0
    long <- 0.0
    start_date <- samples$start_date[[1]]
    end_date <- samples$end_date[[1]]
    cube <- samples$cube[[1]]
    timeline <- sits_timeline(samples)

    # get classes to undersample
    classes_under <- samples %>%
        sits_labels_summary() %>%
        dplyr::filter(.data[["count"]] >= n_samples_under) %>%
        dplyr::pull("label")


    # get classes to oversample
    classes_over <- samples %>%
        sits_labels_summary() %>%
        dplyr::filter(.data[["count"]] <= n_samples_over) %>%
        dplyr::pull("label")


    new_samples <- .tibble()

    if (length(classes_under) > 0) {
        .sits_parallel_start(workers = multicores)
        on.exit(.sits_parallel_stop())

        samples_under_new <- .sits_parallel_map(classes_under, function(cls) {
            samples_cls <- dplyr::filter(samples, .data[["label"]] == cls)
            grid_dim <- ceiling(sqrt(n_samples_under / 4))

            som_map <- sits_som_map(
                samples_cls,
                grid_xdim = grid_dim,
                grid_ydim = grid_dim,
                rlen = 50
            )

            samples_under <- som_map$data %>%
                dplyr::group_by(.data[["id_neuron"]]) %>%
                dplyr::slice_sample(n = 4, replace = TRUE) %>%
                dplyr::ungroup()

            return(samples_under)
        })

        # bind under samples results
        samples_under_new <- dplyr::bind_rows(samples_under_new)
        new_samples <- dplyr::bind_rows(new_samples, samples_under_new)
    }

    if (length(classes_over) > 0) {
        .sits_parallel_start(workers = multicores)
        on.exit(.sits_parallel_stop())

        samples_over_new <- .sits_parallel_map(classes_over, function(cls) {
            samples_bands <- purrr::map(bands, function(band) {
                # selection of band
                dist_band <- samples %>%
                    sits_select(bands = band) %>%
                    dplyr::filter(.data[["label"]] == cls) %>%
                    .predictors() %>%
                    .[-1]
                # oversampling of band for the class
                dist_over <- .sits_oversample_smote(
                    data = dist_band,
                    cls = cls,
                    cls_col = "label",
                    m = n_samples_over
                )
                # put the oversampled data into a samples tibble
                samples_band <- slider::slide_dfr(dist_over, function(row) {
                    time_series <- tibble::tibble(
                        Index = as.Date(timeline),
                        values = unname(as.numeric(row[-1]))
                    )
                    colnames(time_series) <- c("Index", band)
                    tibble::tibble(
                        longitude = long,
                        latitude = lat,
                        start_date = as.Date(start_date),
                        end_date = as.Date(end_date),
                        label = row[["label"]],
                        cube = cube,
                        time_series = list(time_series)
                    )
                })
                class(samples_band) <- c("sits", class(samples_band))
                return(samples_band)
            })
            tb_class_new <- samples_bands[[1]]
            for (i in seq_along(samples_bands)[-1]) {
                tb_class_new <- sits_merge(tb_class_new, samples_bands[[i]])
            }
            return(tb_class_new)
        })

        # bind under samples results
        samples_over_new <- dplyr::bind_rows(samples_over_new)
        new_samples <- dplyr::bind_rows(new_samples, samples_over_new)
    }

    classes_ok <- labels[!(labels %in% classes_under |
        labels %in% classes_over)]
    if (length(classes_ok) > 0) {
        samples_classes_ok <- dplyr::filter(
            samples,
            .data[["label"]] %in% classes_ok
        )
        new_samples <- dplyr::bind_rows(new_samples, samples_classes_ok)
    }

    return(new_samples)
}
#' @title Oversample a dataset by SMOTE.
#' @name .sits_oversample_smote
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
.sits_oversample_smote <- function(data, cls, cls_col, m) {
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
    smoteret <- .sits_smote(data[, -col_ind],
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
#' @name .sits_smote
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
.sits_smote <- function(data, target, K = 5, dup_size = 0) {
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
    # The number of negative instances
    sizeN <- nrow(N_set)
    knear <- .sits_knearest(P_set, P_set, K)
    sum_dup <- .sits_n_dup_max(sizeP + sizeN, sizeP, sizeN, dup_size)
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

.sits_knearest <- function(D, P, n_clust) {
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
.sits_n_dup_max <- function(size_input, size_P, size_N, dup_size = 0) {
    # Size_P is the number of positive used
    # for generating actual size of P
    if (is.vector(dup_size) && length(dup_size) > 1) {
        if (length(which(dup_size == 0)) > 0) {
            sizeM <- floor((2 * size_N - size_input) / size_P)
        }
        if (length(which(dup_size == 0)) == 0) {
            sizeM <- max(dup_size)
        }
    }
    if (!is.vector(dup_size) || length(dup_size) == 1) {
        if (dup_size == 0) {
            sizeM <- floor((2 * size_N - size_input) / size_P)
        }
        if (dup_size != 0) {
            sizeM <- dup_size
        }
    }
    return(sizeM)
}
