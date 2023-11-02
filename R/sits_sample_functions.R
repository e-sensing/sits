#' @title Sample a percentage of a time series
#' @name sits_sample
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Takes a sits tibble with different labels and
#'              returns a new tibble. For a given field as a group criterion,
#'              this new tibble contains a percentage
#'              of the total number of samples per group.
#'              If frac > 1 , all sampling will be done with replacement.
#'
#' @param  data       Sits time series tibble (class = "sits")
#' @param  frac       Percentage of samples to extract
#'                    (range: 0.0 to 2.0, default = 0.2)
#' @param  oversample Logical: oversample classes with small number of samples?
#'                    (TRUE/FALSE)
#' @return            A sits tibble with a fixed quantity of samples.
#' @examples
#' # Retrieve a set of time series with 2 classes
#' data(cerrado_2classes)
#' # Print the labels of the resulting tibble
#' summary(cerrado_2classes)
#' # Sample by fraction
#' data_02 <- sits_sample(cerrado_2classes, frac = 0.2)
#' # Print the labels
#' summary(data_02)
#' @export
sits_sample <- function(data,
                        frac = 0.2,
                        oversample = TRUE) {
    # set caller to show in errors
    .check_set_caller("sits_sample")
    # verify if data and frac are valid
    .check_samples_ts(data)
    # check frac parameter
    .check_num_parameter(frac, min = 0.0, max = 2.0,
                         msg = "invalid frac parameter")
    # check oversample
    .check_lgl_parameter(oversample, msg = "invalid oversample parameter")
    # group the data by label
    groups <- by(data, data[["label"]], list)
    # for each group of samples, obtain the required subset
    result <- purrr::map_dfr(groups, function(class_samples) {
        result_class <- dplyr::slice_sample(
            class_samples,
            prop = frac,
            replace = oversample
        )
        return(result_class)
    })
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
#' @param  samples              Sample set to rebalance
#' @param  n_samples_over       Number of samples to oversample
#'                              for classes with samples less than this number.
#' @param  n_samples_under      Number of samples to undersample
#'                              for classes with samples more than this number.
#' @param  multicores           Number of cores to process the data (default 2).
#'
#' @return A sits tibble with reduced sample imbalance.
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
    # get the bands and the labels
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
        .parallel_start(workers = multicores)
        on.exit(.parallel_stop())
        # for each class, select some of the samples using SOM
        samples_under_new <- .parallel_map(classes_under, function(cls) {
            # select the samples for the class
            samples_cls <- dplyr::filter(samples, .data[["label"]] == cls)
            # set the dimension of the SOM grid
            grid_dim <- ceiling(sqrt(n_samples_under / 4))
            # build the SOM map
            som_map <- sits_som_map(
                samples_cls,
                grid_xdim = grid_dim,
                grid_ydim = grid_dim,
                rlen = 50
            )
            # select samples on the SOM grid using the neurons
            samples_under <- som_map$data |>
                dplyr::group_by(.data[["id_neuron"]]) |>
                dplyr::slice_sample(n = 4, replace = TRUE) |>
                dplyr::ungroup()
            return(samples_under)
        })
        # bind undersample results
        samples_under_new <- dplyr::bind_rows(samples_under_new)
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
                    sits_select(bands = band) |>
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
