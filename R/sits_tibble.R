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
    .sits_tibble_test(data)

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
                replace = FALSE
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
#' @description Takes a sits tibble with different labels and
#'              returns a new tibble. Deals with class imbalance
#'              using the synthetic minority oversampling technique (SMOTE)
#'              for oversampling.
#'              Undersampling is done using the SOM methods available in
#'              the sits package.
#'
#'              Please see the function scutr::SCUT() for more details.
#'
#' @references
#' Oversampling uses the "scutr::oversample_smote" function
#' implemented in the "scutr" package developed by Keenan Ganz and
#' avaliable in https://github.com/s-kganz/scutr
#' The reference paper on SMOTE is
#' N. V. Chawla, K. W. Bowyer, L. O.Hall, W. P. Kegelmeyer,
#' “SMOTE: synthetic minority over-sampling technique,”
#' Journal of artificial intelligence research, 321-357, 2002..
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
#'                              for classes with samples less than this number
#'                              (use n_samples_over = NULL to avoid
#'                              oversampling).
#' @param  n_samples_under      Number of samples to undersample
#'                              for classes with samples more than this number
#'                              (use n_samples_over = NULL to avoid
#'                              oversampling).
#'
#' @return A sits tibble with a fixed quantity of samples.
#' @examples
#' # Retrieve a set of time series with 2 classes
#' data(samples_modis_4bands)
#' # Print the labels of the resulting tibble
#' sits_labels_summary(samples_modis_4bands)
#' # Samples the data set
#' new_data <- sits_reduce_imbalance(samples_modis_4bands)
#' # Print the labels of the resulting tibble
#' sits_labels_summary(new_data)
#' @export
sits_reduce_imbalance <- function(
    samples,
    n_samples_over   = 200,
    n_samples_under  = 400) {

    # verifies if scutr package is installed
    if (!requireNamespace("scutr", quietly = TRUE)) {
        stop("Please install package scutr", call. = FALSE)
    }

    # set caller to show in errors
    .check_set_caller("sits_reduce_imbalance")
    # check if number of required samples are correctly entered
    if (!purrr::is_null(n_samples_over) || !purrr::is_null(n_samples_under)) {
        .check_that(
            x = n_samples_under >= n_samples_over,
            msg = paste0("number of samples to undersample for large classes",
                         " should be higher or equal to number of samples to ",
                         "oversample for small classes"
            )
        )
    }
    bands <- sits_bands(samples)
    labels <- sits_labels(samples)
    summary <- sits_labels_summary(samples)

    # params of output tibble
    lat <- 0.0
    long <- 0.0
    start_date <- samples$start_date[[1]]
    end_date   <- samples$end_date[[1]]
    cube <- samples$cube[[1]]
    timeline <- sits_timeline(samples)
    n_times <- length(timeline)

    if (!purrr::is_null(n_samples_under)) {
        classes_under <- samples %>%
            sits_labels_summary() %>%
            dplyr::filter(.data[["count"]] >= n_samples_under) %>%
            dplyr::pull(.data[["label"]])
    } else
        classes_under <- vector()

    if (!purrr::is_null(n_samples_over)) {
        classes_over <- samples %>%
            sits_labels_summary() %>%
            dplyr::filter(.data[["count"]] <= n_samples_over) %>%
            dplyr::pull(.data[["label"]])
    } else
        classes_over <- vector()

    classes_ok <- labels[!(labels %in% classes_under | labels %in% classes_over)]
    new_samples <- .sits_tibble()

    if (length(classes_under) > 0) {
        samples_under_new <- purrr::map_dfr(classes_under, function(cls){
            samples_cls <- dplyr::filter(samples, .data[["label"]] == cls)
            grid_dim <-  ceiling(sqrt(n_samples_under/4))

            som_map <- sits_som_map(samples_cls,
                                    grid_xdim = grid_dim,
                                    grid_ydim = grid_dim,
                                    rlen = 50
            )
            samples_under <- som_map$data %>%
                dplyr::group_by(id_neuron) %>%
                dplyr::slice_sample(n = 4, replace = TRUE) %>%
                dplyr::ungroup()
            return(samples_under)
        })
        new_samples <- dplyr::bind_rows(new_samples,
                                        samples_under_new
        )
    }

    if (length(classes_over) > 0) {
        samples_over_new <- purrr::map_dfr(classes_over, function(cls){
            samples_bands <- purrr::map(bands, function(band){
                # selection of band
                dist_band <- samples %>%
                    sits_select(band = band) %>%
                    dplyr::filter(.data[["label"]] == cls) %>%
                    .sits_distances() %>%
                    as.data.frame() %>%
                    .[-1]
                # oversampling of band for the class
                dist_over <- scutr::oversample_smote(
                    data = dist_band,
                    cls = cls,
                    cls_col = "reference",
                    m = n_samples_over
                )
                # put the oversampled data into a samples tibble
                samples_band <- slider::slide_dfr(dist_over, function(row){
                    time_series = tibble::tibble(
                        Index = as.Date(timeline),
                        values = unname(as.numeric(row[-1]))
                    )
                    colnames(time_series) <- c("Index", band)
                    tibble::tibble(
                        longitude = long,
                        latitude  = lat,
                        start_date = as.Date(start_date),
                        end_date = as.Date(end_date),
                        label = row[["reference"]],
                        cube = cube,
                        time_series = list(time_series)
                    )
                })
                class(samples_band) <- c("sits", class(samples_band))
                return(samples_band)
            })
            tb_class_new <- samples_bands[[1]]
            for (i in seq_along(samples_bands)[-1])
                tb_class_new <- sits_merge(tb_class_new, samples_bands[[i]])
            return(tb_class_new)
        })
        new_samples <- dplyr::bind_rows(new_samples,
                                        samples_over_new
        )
    }

    if (length(classes_ok) > 0) {
        samples_classes_ok <- dplyr::filter(samples,
                                            .data[["label"]] %in% classes_ok)
        new_samples <- dplyr::bind_rows(new_samples, samples_classes_ok)
    }
    return(new_samples)
}

#' @title Get the time series for a row of a sits tibble
#' @name sits_time_series
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Returns the time series associated to a row of the a sits tibble
#'
#' @param data     A sits tibble with one or more time series.
#' @return A tibble in sits format with the time series.
#' @examples
#' # Retrieve a set of time series with 2 classes
#' data(cerrado_2classes)
#' # Retrieve the first time series
#' sits_time_series(cerrado_2classes)
#' @export
sits_time_series <- function(data) {
    .sits_tibble_test(data)

    return(data$time_series[[1]])
}
#' @title Create a sits tibble to store the time series information
#' @name .sits_tibble
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function returns an empty sits tibble.
#' Sits tibbles are the main structures of sits package.
#' They contain both the satellite image time series and its metadata.
#' A sits tibble is a tibble with pre-defined columns that
#' has the metadata and data for each time series. The columns are
#' <longitude, latitude, start_date, end_date, label, cube, time_series>.
#' Most functions of sits package get a sits tibble as input
#' (with additional parameters)
#' and return another sits tibble as output.
#' This allows chaining functions over sits tibbles.
#'
#' @return A sits tibble.
#'
.sits_tibble <- function() {
    sits <- tibble::tibble(
        longitude = double(),
        latitude = double(),
        start_date = as.Date(character()),
        end_date = as.Date(character()),
        label = character(),
        cube = character(),
        time_series = list()
    )
    class(sits) <- c("sits", class(sits))
    return(sits)
}

#' @title Aligns dates of time series to a reference date
#' @name .sits_tibble_align_dates
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Converts the time indexes of a set of sits
#' tibble to a single reference year.
#' This function is useful to join many time series from
#' different years to a single year,
#' which is required by methods that combine many time series,
#' such as clustering methods.
#' The reference year is taken from the date of the start of the time series
#' available in the data cube.
#'
#' @param  data          Input sits tibble (useful for chaining functions).
#' @param  ref_dates     Dates to align the time series.
#' @return               The converted sits tibble
#'
.sits_tibble_align_dates <- function(data, ref_dates) {
    # verify that tibble is correct
    .sits_tibble_test(data)
    # function to shift a time series in time
    shift_ts <- function(d, k) {
        dplyr::bind_rows(
            utils::tail(d, k),
            utils::head(d, -k)
        )
    }
    # get the reference date
    start_date <- lubridate::as_date(ref_dates[1])
    # align the dates in the data
    data <- purrr::pmap_dfr(
        list(
            data$longitude,
            data$latitude,
            data$label,
            data$cube,
            data$time_series
        ),
        function(long, lat, lab, cb, ts) {
            # only rows that match  reference dates are kept
            if (length(ref_dates) == nrow(ts)) {
                # find the date of minimum distance to the reference date
                idx <- which.min(abs((lubridate::as_date(ts$Index)
                                      - lubridate::as_date(start_date)) / lubridate::ddays(1)))
                # shift the time series to match dates
                if (idx != 1) ts <- shift_ts(ts, -(idx - 1))
                # change the dates to the reference dates
                ts1 <- dplyr::mutate(ts, Index = ref_dates)
                # save the resulting row in the output tibble
                row <- tibble::tibble(
                    longitude = long,
                    latitude = lat,
                    start_date = lubridate::as_date(ref_dates[1]),
                    end_date = ref_dates[length(ref_dates)],
                    label = lab,
                    cube = cb,
                    time_series = list(ts1)
                )
            }
            return(row)
        }
    )
    return(data)
}
#'
#' @title Checks that the timeline of all time series of a data set are equal
#' @name .sits_tibble_prune
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function tests if all time series in a sits tibble
#' have the same number of samples, and returns a time series whose indices
#' match the majority of the samples.
#'
#' @param  data  Either a sits tibble or a raster metadata.
#' @return A pruned sits tibble.
#'
.sits_tibble_prune <- function(data) {
    # verify that tibble is correct
    .sits_tibble_test(data)

    n_samples <- data$time_series %>%
        purrr::map_int(function(t) {
            nrow(t)
        })

    # check if all time indices are equal to the median
    if (all(n_samples == stats::median(n_samples))) {
        message("Success!! All samples have the same number of time indices")
        return(data)
    } else {
        message("Some samples of time series do not have the same time indices
                as the majority of the data")
        # return the time series that have the same number of samples
        ind2 <- which(n_samples == stats::median(n_samples))
        return(data[ind2, ])
    }
}
#' @title Check that the requested bands exist in the samples
#' @name .sits_tibble_bands_check
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param samples       Time series with the samples
#' @param bands         Requested bands of the data sample
#' @return              Checked bands (cube bands if bands are NULL).
#'
.sits_tibble_bands_check <- function(samples, bands = NULL) {

    # set caller to show in errors
    .check_set_caller(".sits_tibble_bands_check")
    # check the bands are available
    sp_bands <- sits_bands(samples)
    if (purrr::is_null(bands)) {
        bands <- toupper(sp_bands)
    } else {
        bands <- toupper(bands)
        .check_chr_within(
            x = bands,
            within = sp_bands,
            msg = "required bands are not available in the samples"
        )
    }
    return(bands)
}

#' @title Tests if a sits tibble is valid
#' @name .sits_tibble_test
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Tests if a sits tibble exists or has data inside.
#'
#' @param data  A sits tibble.
#' @return Returns TRUE if data has data.
.sits_tibble_test <- function(data) {

    # set caller to show in errors
    .check_set_caller(".sits_tibble_test")
    .check_null(x = data, "invalid data parameter")
    .check_num(
        x = nrow(data),
        min = 1, msg = "invalid number of rows"
    )
    .check_chr_contains(
        x = colnames(data),
        contains = .config_get("sits_tibble_cols"),
        discriminator = "all_of",
        msg = "Data is not a valid sits tibble"
    )
    return(TRUE)
}
#' @title Apply a function to one band of a time series
#' @name .sits_fast_apply
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param  data      Tibble.
#' @param  col       Column where function should be applied
#' @param  fn        Function to be applied.
#' @return           Tibble where function has been applied.
#'
.sits_fast_apply <- function(data, col, fn, ...) {

    # pre-condition
    .check_chr_within(col,
                      within = names(data),
                      msg = "invalid column name"
    )
    # select data do unpack
    x <- data[col]
    # prepare to unpack
    x[["#.."]] <- seq_len(nrow(data))
    # unpack
    x <- tidyr::unnest(x, cols = dplyr::all_of(col))
    x <- dplyr::group_by(x, .data[["#.."]])
    # apply user function
    x <- fn(x, ...)
    # pack
    x <- dplyr::ungroup(x)
    x <- tidyr::nest(x, `..unnest_col` = -dplyr::any_of("#.."))
    # remove garbage
    x[["#.."]] <- NULL
    names(x) <- col
    # prepare result
    data[[col]] <- x[[col]]
    return(data)
}

#' @keywords internal
.sits_rename_bands <- function(x, bands) {
    UseMethod(".sits_rename_bands", x)
}

#' @export
.sits_rename_bands.sits <- function(x, bands) {
    data_bands <- sits_bands(x)

    # pre-condition
    .check_chr(bands,
               allow_empty = FALSE, len_min = length(data_bands),
               len_max = length(data_bands),
               msg = "invalid 'bands' value"
    )

    .sits_fast_apply(x, col = "time_series", fn = function(x) {

        # create a conversor
        new_bands <- colnames(x)
        names(new_bands) <- new_bands

        # rename
        new_bands[data_bands] <- toupper(bands)
        colnames(x) <- unname(new_bands)

        return(x)
    })
}

#' @export
.sits_rename_bands.raster_cube <- function(x, bands) {
    data_bands <- sits_bands(x)
    # pre-condition
    .check_chr(bands,
               allow_empty = FALSE,
               len_min = length(data_bands),
               len_max = length(data_bands),
               msg = "invalid 'bands' value"
    )
    .sits_fast_apply(x, col = "file_info", fn = function(x) {
        x <- tidyr::pivot_wider(x,
                                names_from = "band",
                                values_from = "path"
        )

        # create a conversor
        new_bands <- colnames(x)
        names(new_bands) <- new_bands

        # rename
        new_bands[data_bands] <- toupper(bands)
        colnames(x) <- unname(new_bands)

        x <- tidyr::pivot_longer(x,
                                 cols = toupper(bands),
                                 names_to = "band",
                                 values_to = "path"
        )

        return(x)
    })
}
