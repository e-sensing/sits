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
#' @param  samples              Sample set to rebalance
#' @param  n_samples_over       Number of samples to oversample
#'                              for classes with samples less than this number.
#' @param  n_samples_under      Number of samples to undersample
#'                              for classes with samples more than this number.
#' @param  method               Method for oversampling (default = "smote")
#' @param  multicores           Number of cores to process the data (default 2).
#'
#' @return A sits tibble with reduced sample imbalance.
#'
#'
#' @note
#' Many training samples for Earth observation data analysis are imbalanced.
#' This situation arises when the distribution of samples associated
#' with each label is uneven.
#' Sample imbalance is an undesirable property of a training set.
#' Reducing sample imbalance improves classification accuracy.
#'
#' The function \code{sits_reduce_imbalance} increases the number of samples
#' of least frequent labels, and reduces the number of samples of most
#' frequent labels. To generate new samples, \code{sits}
#' uses the SMOTE method that estimates new samples by considering
#' the cluster formed by the nearest neighbors of each minority label.
#'
#' To perform undersampling, \code{sits_reduce_imbalance}) builds a SOM map
#' for each majority label based on the required number of samples.
#' Each dimension of the SOM is set to ceiling(sqrt(new_number_samples/4))
#' to allow a reasonable number of neurons to group similar samples.
#' After calculating the SOM map, the algorithm extracts four samples
#' per neuron to generate a reduced set of samples that approximates
#' the variation of the original one.
#' See also \code{\link[sits]{sits_som_map}}.
#'
#' @references
#' The reference paper on SMOTE is
#' N. V. Chawla, K. W. Bowyer, L. O.Hall, W. P. Kegelmeyer,
#' “SMOTE: synthetic minority over-sampling technique,”
#' Journal of artificial intelligence research, 321-357, 2002.
#'
#' The SOM map technique for time series is described in the paper:
#' Lorena Santos, Karine Ferreira, Gilberto Camara, Michelle Picoli,
#' Rolf Simoes, “Quality control and class noise reduction of satellite
#' image time series”. ISPRS Journal of Photogrammetry and Remote Sensing,
#' vol. 177, pp 75-88, 2021. https://doi.org/10.1016/j.isprsjprs.2021.04.014.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # print the labels summary for a sample set
#'     summary(samples_modis_ndvi)
#'     # reduce the sample imbalance
#'     new_samples <- sits_reduce_imbalance(samples_modis_ndvi,
#'         n_samples_over = 200,
#'         n_samples_under = 200,
#'         multicores = 1
#'     )
#'     # print the labels summary for the rebalanced set
#'     summary(new_samples)
#' }
#' @export
sits_reduce_imbalance <- function(samples,
                                  n_samples_over = 200,
                                  n_samples_under = 400,
                                  method = "smote",
                                  multicores = 2) {
    # set caller to show in errors
    .check_set_caller("sits_reduce_imbalance")
    # pre-conditions
    .check_samples_train(samples)
    .check_int_parameter(n_samples_over)
    .check_int_parameter(n_samples_under)

    # check if number of required samples are correctly entered
    .check_that(n_samples_under >= n_samples_over,
                msg = .conf("messages", "sits_reduce_imbalance_samples")
    )
    # get the bands and the labels
    bands <- .samples_bands(samples)
    labels <- .samples_labels(samples)
    # params of output tibble
    lat <- 0.0
    long <- 0.0
    start_date <- samples[["start_date"]][[1]]
    end_date <- samples[["end_date"]][[1]]
    cube <- samples[["cube"]][[1]]
    timeline <- .samples_timeline(samples)
    # get classes to undersample
    classes_under <- samples |>
        summary() |>
        dplyr::filter(.data[["count"]] >= n_samples_under) |>
        dplyr::pull("label")
    # get classes to oversample
    classes_over <- samples |>
        summary() |>
        dplyr::filter(.data[["count"]] <= n_samples_over) |>
        dplyr::pull("label")
    # create an output tibble
    new_samples <- .tibble()
    # under sampling
    if (length(classes_under) > 0) {
        # undersample classes with lots of data
        samples_under_new <- .som_undersample(
            samples = samples,
            classes_under = classes_under,
            n_samples_under = n_samples_under,
            multicores = multicores)

        # join get new samples
        new_samples <- dplyr::bind_rows(new_samples, samples_under_new)
    }
    # oversampling
    if (length(classes_over) > 0) {
        .parallel_start(workers = multicores)
        on.exit(.parallel_stop())
        # for each class, build synthetic samples using SMOTE
        samples_over_new <- .parallel_map(classes_over, function(cls) {
            # select the samples for the class
            samples_bands <- purrr::map(bands, function(band) {
                # selection of band
                dist_band <- samples |>
                    .samples_select_bands(band) |>
                    dplyr::filter(.data[["label"]] == cls) |>
                    .predictors()
                dist_band <- dist_band[-1]
                # oversampling of band for the class
                dist_over <- .smote_oversample(
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
        # bind oversampling results
        samples_over_new <- dplyr::bind_rows(samples_over_new)
        new_samples <- dplyr::bind_rows(new_samples, samples_over_new)
    }
    # keep classes (no undersampling nor oversampling)
    classes_ok <- labels[!(labels %in% classes_under |
                               labels %in% classes_over)]
    if (length(classes_ok) > 0) {
        samples_classes_ok <- dplyr::filter(
            samples,
            .data[["label"]] %in% classes_ok
        )
        new_samples <- dplyr::bind_rows(new_samples, samples_classes_ok)
    }
    # remove SOM additional columns
    colnames_sits <- setdiff(colnames(new_samples), c("id_neuron", "id_sample"))
    # return new sample set
    return(new_samples[, colnames_sits])
}
