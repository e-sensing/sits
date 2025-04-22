#' @title Merge fraction bands (mixture models)
#' @noRd
#' @param samples Original samples
#' @param values  Values from time series of fraction bands
#' @return merge set of training samples
.samples_merge_fracs <- function(samples, values) {
    # Bind samples time series and fractions columns
    values <- dplyr::bind_cols(.ts(samples), values)
    # Transform time series into a list of time instances
    values <- tidyr::nest(values, time_series = c(-"sample_id", -"label"))
    # Assign the fractions and bands time series to samples
    samples[["time_series"]] <- values[["time_series"]]
    # Return a sits tibble
    samples
}
#' @title Split samples in groups
#' @noRd
#' @param samples Original samples
#' @param multicores  Number of cores
#' @return Split samples by ID
.samples_split_groups <- function(samples, multicores) {
    # Change multicores value in case multicores is greater than samples nrows
    multicores <- if (multicores > nrow(samples)) nrow(samples) else multicores
    # Create a new column to each group id
    samples[["group"]] <- rep(
        seq_len(multicores),
        each = ceiling(nrow(samples) / multicores)
    )[seq_len(nrow(samples))]
    # Split each group by an id
    dplyr::group_split(
        dplyr::group_by(samples, .data[["group"]]),
        .keep = FALSE
    )
}
#' @title Merge samples
#' @noRd
#' @param samples_lst List of samples
#' @return Training samples data.frame
.samples_merge_groups <- function(samples_lst) {
    # Binding the list items into a tibble
    samples <- dplyr::bind_rows(samples_lst)
    # add sits class to the tibble structure
    class(samples) <- c("sits", class(samples))
    # Return sits tibble
    samples
}
#' @title Create partitions of a data set
#' @name  .samples_create_folds
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Alexandre Ywata, \email{alexandre.ywata@@ipea.gov.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Split a sits tibble into k groups, based on the label.
#' @keywords internal
#' @noRd
#' @param data   A sits tibble to be partitioned.
#' @param folds  Number of folds
#'
#' @return A list of row position integers corresponding to the training data.
#'
.samples_create_folds <- function(data, folds = 5) {
    # verify if data exists
    # splits the data into k groups
    data[["folds"]] <- caret::createFolds(data[["label"]],
                                          k = folds,
                                          returnTrain = FALSE, list = FALSE)
    data
}
#' @title Extract time series from samples
#' @noRd
#' @param samples Data.frame with samples
#' @return Time series for the first sample
.samples_ts <- function(samples) {
    .check_set_caller(".samples_ts")
    # Check time_series column
    .check_that(.has_ts(samples))
    # Return time series of the first sample
    samples[["time_series"]][[1]]
}
#' @title Get number of temporal intervals in time series samples
#' @noRd
#' @param samples Data.frame with samples
#' @return Number of temporal intervals for the first sample
.samples_ntimes <- function(samples) {
    # Number of observations of the first sample governs whole samples data
    nrow(.samples_ts(samples))
}
#' @title Get bands of time series samples
#' @noRd
#' @param samples Data.frame with samples
#' @param dots    Other parameters to be included
#' @param include_base  Include base bands?
#' @return Bands for the first sample
.samples_bands <- function(samples, ...) {
    # Bands of the first sample governs whole samples data
    UseMethod(".samples_bands", samples)
}
#' @export
.samples_bands.sits <- function(samples, ...) {
    # Bands of the first sample governs whole samples data
    setdiff(names(.samples_ts(samples)), "Index")
}
#' @export
.samples_bands.sits_base <- function(samples, ..., include_base = TRUE) {
    # Bands of the first sample governs whole samples data
    bands <- .samples_bands.sits(samples)
    if (include_base) {
        bands <- c(
            bands, .samples_base_bands(samples)
        )
    }
    bands
}
#' @title Check if samples is base (has base property)
#' @noRd
#' @param samples Data.frame with samples
#' @return TRUE/FALSE
.samples_is_base <- function(samples) {
    inherits(samples, "sits_base")
}
#' @title Get samples base data (if available)
#' @noRd
#' @param samples Data.frame with samples
#' @return data.frame with base data.
.samples_base_data <- function(samples) {
    samples[["base_data"]]
}
#' @title Get bands of base data for samples
#' @noRd
#' @param samples Data.frame with samples
#' @return Bands for the first sample
.samples_base_bands <- function(samples) {
    # Bands of the first sample governs whole samples data
    setdiff(names(samples[["base_data"]][[1L]]), "Index")
}
#' @title Get timeline of time series samples
#' @noRd
#' @param samples Data.frame with samples
#' @return Timeline of the first sample
.samples_timeline <- function(samples) {
    as.Date(samples[["time_series"]][[1L]][["Index"]])
}
#' @title Select bands of time series samples
#' @noRd
#' @param samples Data.frame with samples
#' @param bands   Bands to be selected
#' @return Time series samples with the selected bands
.samples_select_bands <- function(samples, bands) {
    UseMethod(".samples_select_bands", samples)
}
#' @export
.samples_select_bands.sits <- function(samples, bands) {
    # Filter samples
    .ts(samples) <- .ts_select_bands(ts = .ts(samples),
                                     bands = bands)
    # Return samples
    samples
}
#' @export
.samples_select_bands.sits_base <- function(samples, bands) {
    ts_bands <- .samples_bands.sits(samples)
    ts_select_bands <- bands[bands %in% ts_bands]
    # Filter time series samples
    .ts(samples) <- .ts_select_bands(ts = .ts(samples),
                                     bands = ts_select_bands)
    # Return samples
    samples
}
#' @export
.samples_select_bands.patterns <- function(samples, bands) {
    # Filter samples
    .ts(samples) <- .ts_select_bands(ts = .ts(samples),
                                     bands = bands)
    # Return samples
    samples
}
#' @title Select time series samples based on a temporal interval
#' @noRd
#' @param samples      Data.frame with samples
#' @param start_date   First date of the interval
#' @param end_date     Last date of the interval
#' @return Time series samples filter by interval
.samples_filter_interval <- function(samples, start_date, end_date) {
    # Filter interval
    .ts(samples) <- .ts_filter_interval(
        ts = .ts(samples), start_date = start_date, end_date = end_date
    )
    # Update start_date and end_date columns with new values
    samples[["start_date"]] <- .ts_start_date(.ts(samples))
    samples[["end_date"]] <- .ts_end_date(.ts(samples))
    # Return samples
    samples
}
#' @title Get labels of time series samples
#' @noRd
#' @param samples Data.frame with samples
#' @return vector with labels
.samples_labels <- function(samples) {
    sort(unique(samples[["label"]]), na.last = TRUE)
}
#' @title Apply function to time series samples
#' @noRd
#' @param samples Data.frame with samples
#' @param fn      Function to be applied to sample
#' @param ...     Additional parameters for function
#' @return samples with applied function
.samples_foreach_ts <- function(samples, fn, ...) {
    # Apply function to each time_series
    samples[["time_series"]] <- lapply(samples[["time_series"]], fn, ...)
    # Return samples
    samples
}
#' @title Prune samples
#' @noRd
#' @param samples Data.frame with samples
#' @return Samples with the same number of temporal intervals as the first
.samples_prune <- function(samples) {
    .check_set_caller(".samples_prune")
    # Get the time series length for the first sample
    ntimes <- .samples_ntimes(samples)
    # Prune time series according to the first time series length and return
    .samples_foreach_ts(samples, function(ts) {
        .check_that(nrow(ts) >= ntimes)
        ts[seq_len(ntimes), ]
    })
}
#' @title Get sample statistics
#' @noRd
#' @param samples Data.frame with samples
#' @return List of Q02 and Q98 for normalization
.samples_stats <- function(samples) {
    # Get all time series
    preds <- .samples_ts(samples)
    # Select attributes
    preds <- preds[.samples_bands.sits(samples)]
    # Compute stats
    q02 <- apply(preds, 2L, stats::quantile, probs = 0.02, na.rm = TRUE)
    q98 <- apply(preds, 2L, stats::quantile, probs = 0.98, na.rm = TRUE)
    # Number of observations
    ntimes <- .samples_ntimes(samples)
    # Replicate stats
    q02 <- rep(unname(q02), each = ntimes)
    q98 <- rep(unname(q98), each = ntimes)
    # Return stats object
    list(q02 = q02, q98 = q98)
}
#' @title Split samples
#' @noRd
#' @param samples Data.frame with samples
#' @param split_intervals   Intervals for samples to be split
#' @return Samples split by desired intervals
.samples_split <- function(samples, split_intervals) {
    slider::slide_dfr(samples, function(sample) {
        ts <- sample[["time_series"]][[1L]]
        .map_dfr(split_intervals, function(index) {
            new_sample <- sample
            start <- index[[1L]]
            end <- index[[2L]]
            new_sample[["time_series"]][[1L]] <- ts[seq(start, end), ]
            new_sample[["start_date"]] <- ts[["Index"]][[start]]
            new_sample[["end_date"]] <- ts[["Index"]][[end]]
            new_sample
        })
    })
}
#' @title Allocate points for stratified sampling for accuracy estimation
#' @name .samples_alloc_strata
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param cube          Classified data cube (raster or vector)
#' @param samples_class Matrix with sampling design to be allocated
#' @param alloc         Allocation method chosen
#' @param dots          Other params for the function
#' @param multicores    Number of cores to work in parallel
#' @param progress      Show progress bar?
#' @return Points resulting from stratified sampling
#' @keywords internal
#' @noRd
.samples_alloc_strata <- function(cube,
                                  samples_class,
                                  alloc, ...,
                                  multicores = 2L,
                                  progress = TRUE) {
    UseMethod(".samples_alloc_strata", cube)
}
#' @export
.samples_alloc_strata.class_cube <- function(cube,
                                             samples_class,
                                             alloc, ...,
                                             multicores = 2L,
                                             progress = TRUE) {
    # check progress
    progress <- .message_progress(progress)
    # estimate size
    size <- samples_class[[alloc]]
    size <- ceiling(max(size) / nrow(cube))
    # get labels
    labels <- samples_class[["label"]]
    # Prepare parallel processing
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop(), add = TRUE)
    # Create assets as jobs
    cube_assets <- .cube_split_assets(cube)
    # Process each asset in parallel
    samples <- .jobs_map_parallel_dfr(cube_assets, function(tile) {
        rast <- .raster_open_rast(.tile_path(tile))
        cls <- samples_class |>
            dplyr::select("label_id", "label") |>
            dplyr::rename("id" = "label_id", "cover" = "label")
        levels(rast) <- cls
        # sampling!
        samples_sv <- .raster_sample(
            rast = rast,
            size = size,
            method = "stratified",
            as.points = TRUE
        )
        samples_sf <- sf::st_as_sf(samples_sv)
        # prepare results - factor just need to be renamed
        if (is.factor(samples_sf[["cover"]])) {
            samples_sf <- dplyr::rename(samples_sf, "label" = "cover")
        } else {
            # prepare results - non-factor must be transform to have label
            # get labels from `samples_class` by `label_id` to avoid errors
            samples_sf <- samples_sf |>
                dplyr::left_join(
                    samples_class, by = c("cover" = "label_id")
                ) |>
                dplyr::select("label", "geometry")
        }
        # prepare results - transform crs
        sf::st_transform(samples_sf, crs = "EPSG:4326")
    }, progress = progress)

    labels <- unique(labels)
    samples <- .map_dfr(labels, function(lab) {
        # get metadata for the current label
        samples_label <- samples_class |>
                            dplyr::filter(.data[["label"]] == lab)
        # extract alloc strategy
        samples_label <- unique(samples_label[[alloc]])
        # filter data
        samples |>
            dplyr::filter(.data[["label"]] == lab) |>
            dplyr::slice_sample(n = round(samples_label))
    })
    # transform to sf object
    sf::st_as_sf(samples)
}
#' @export
.samples_alloc_strata.class_vector_cube <- function(cube,
                                                    samples_class,
                                                    alloc, ...,
                                                    multicores = 2,
                                                    progress = TRUE) {
    # check progress
    progress <- .message_progress(progress)
    # Open segments and transform them to tibble
    segments_cube <- slider::slide_dfr(cube, function(tile) {
        .segments_read_vec(tile)
    })
    # Retrieve the required number of segments per class
    samples_lst <- segments_cube |>
        dplyr::group_by(.data[["class"]]) |>
        dplyr::group_map(function(cl, class) {
            # prepare class name
            class <- class[["class"]]
            # get metadata for the current label
            samples_label <- samples_class |>
                dplyr::filter(.data[["label"]] == class)
            # extract alloc strategy
            samples_label <- samples_label[[alloc]]
            # extract samples
            samples_label <- sf::st_sample(cl, samples_label)
            # prepare extracted samples
            sf_samples <- sf::st_sf(label = class, geometry = samples_label)
            # return!
            sf_samples
        })
    dplyr::bind_rows(samples_lst)
}
#' @title Converts samples to sits
#' @name .samples_convert_to_sits
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param samples       Data frame
#' @return data frame converted to sits
#' @keywords internal
#' @noRd
.samples_convert_to_sits <- function(samples) {
    samples <- tibble::as_tibble(samples)
    .set_class(samples, "sits", class(samples))
}
