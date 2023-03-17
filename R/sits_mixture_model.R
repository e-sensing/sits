#' @title Multiple endmember spectral mixture analysis
#'
#' @name sits_mixture_model
#'
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos,   \email{efelipecarlos@@gmail.com}
#' @author Rolf Simoes,     \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Alber Sanchez,   \email{alber.ipia@@inpe.br}
#'
#' @description Create a multiple endmember spectral mixture analyses fractions
#' images. We use the non-negative least squares (NNLS) solver to calculate the
#' fractions of each endmember. The NNLS was implemented by Jakob
#' Schwalb-Willmann in RStoolbox package (licensed as GPL>=3).
#'
#' @references \code{RStoolbox} package (https://github.com/bleutner/RStoolbox/)
#'
#' @param data        A sits data cube or a sits tibble.
#' @param endmembers  Reference spectral endmembers.
#'                    (see details below).
#' @param  ...        Parameters for specific functions.
#' @param rmse_band   A boolean indicating whether the error associated
#'                    with the linear model should be generated.
#'                    If true, a new band with errors for each pixel
#'                    is generated using the root mean square
#'                    measure (RMSE). Default is TRUE.
#' @param memsize     Memory available for the mixture model (in GB).
#' @param multicores  Number of cores to be used for generate the
#'                    mixture model.
#' @param output_dir  Directory for output images.
#' @param progress    Show progress bar? Default is TRUE.
#'
#' @return In case of a cube, a sits cube with the fractions of each endmember
#'         will be returned. The sum of all fractions is restricted
#'         to 1 (scaled from 0 to 10000), corresponding to the abundance of
#'         the endmembers in the pixels.
#'         In case of a tibble sits, the time series will be returned with the
#'         values corresponding to each fraction.
#'
#' @details
#'
#' The \code{endmembers} parameter should be a tibble, csv or
#' a shapefile. \code{endmembers} parameter must have the following columns:
#' \code{type}, which defines the endmembers that will be
#' created and the columns corresponding to the bands that will be used in the
#' mixture model. The band values must follow the product scale.
#' For example, in the case of sentinel-2 images the bands should be in the
#' range 0 to 1. See the \code{example} in this documentation for more details.
#'
#' @examples
#' if (sits_run_examples()) {
#'    # Create a sentinel-2 cube
#'    s2_cube <- sits_cube(
#'        source = "AWS",
#'        collection = "SENTINEL-S2-L2A-COGS",
#'        tiles = "20LKP",
#'        bands = c("B02", "B03", "B04", "B8A", "B11", "B12", "CLOUD"),
#'        start_date = "2019-06-13",
#'        end_date = "2019-06-30"
#'    )
#'
#'    # Cube regularization for 16 days and 160 meters
#'    reg_cube <- sits_regularize(
#'        cube = s2_cube,
#'        period = "P16D",
#'        res = 160,
#'        roi = c(lon_min = -65.54870165,
#'                lat_min = -10.63479162,
#'                lon_max = -65.07629670,
#'                lat_max = -10.36046639),
#'        multicores = 2,
#'        output_dir = tempdir()
#'    )
#'
#'    # Create the endmembers tibble
#'    em <- tibble::tribble(
#'           ~type, ~B02, ~B03,   ~B04,  ~B8A,  ~B11,   ~B12,
#'        "forest", 0.02, 0.0352, 0.0189, 0.28,  0.134, 0.0546,
#'          "land", 0.04, 0.065,  0.07,   0.36,  0.35,  0.18,
#'         "water", 0.07, 0.11,   0.14,   0.085, 0.004, 0.0026
#'    )
#'
#'    # Generate the mixture model
#'    mm <- sits_mixture_model(
#'        data = reg_cube,
#'        endmembers = em,
#'        memsize = 4,
#'        multicores = 2,
#'        output_dir = tempdir()
#'    )
#' }
#'
#' @export
sits_mixture_model <- function(data, endmembers, ...,
                               rmse_band = TRUE,
                               multicores = 2,
                               progress = TRUE) {
    # Pre-conditions
    .check_endmembers_parameter(endmembers)
    .check_lgl_parameter(rmse_band)
    .check_multicores(multicores)
    .check_progress(progress)

    data <- .conf_data_meta_type(data)
    UseMethod("sits_mixture_model", data)
}

#' @rdname sits_mixture_model
#' @export
sits_mixture_model.sits <- function(data, endmembers, ...,
                                    rmse_band = TRUE,
                                    multicores = 2,
                                    progress = TRUE) {
    # Pre-conditions
    .check_samples_train(data)
    # Transform endmembers to tibble
    em <- .endmembers_as_tbl(endmembers)
    # Check endmember format
    .check_endmembers_tbl(em)

    # Get endmembers bands
    bands <- setdiff(.sits_bands(data), .endmembers_fracs(em))
    # The samples is filtered here in case some fraction
    # is added as a band
    data <- .sits_select_bands(samples = data, bands = bands)
    # Pre-condition
    .check_endmembers_bands(em = em, bands = .sits_bands(data))

    # Fractions to be produced
    out_fracs <- .endmembers_fracs(em = em, include_rmse = rmse_band)

    # Prepare parallelization
    .sits_parallel_start(workers = multicores, log = FALSE)
    on.exit(.sits_parallel_stop(), add = TRUE)
    # Create mixture processing function
    mixture_fn <- .mixture_fn_nnls(em = em, rmse = rmse_band)
    # Create groups of samples as jobs
    samples_groups <- .samples_split_groups(
        samples = data, multicores = multicores
    )
    # Process each group of samples in parallel
    samples_fracs <- .sits_parallel_map(samples_groups, function(samples) {
        # Process the data
        output_samples <- .mixture_samples(
            samples = samples, em = em,
            mixture_fn = mixture_fn, out_fracs = out_fracs
        )
        return(output_samples)
    }, progress = progress)
    # Join groups samples as a sits tibble and return it
    .samples_merge_groups(samples_fracs)
}

#' @rdname sits_mixture_model
#' @export
sits_mixture_model.raster_cube <- function(data, endmembers, ...,
                                           rmse_band = TRUE,
                                           memsize = 4,
                                           multicores = 2,
                                           output_dir,
                                           progress = TRUE) {
    # Pre-conditions
    .check_is_raster_cube(data)
    .check_memsize(memsize)
    .check_output_dir(output_dir)
    .check_lgl_type(progress)

    # Transform endmembers to tibble
    em <- .endmembers_as_tbl(endmembers)
    # Check endmember format
    .check_endmembers_tbl(em)

    # Get endmembers bands
    bands <- setdiff(.cube_bands(data), .endmembers_fracs(em))
    # The cube is filtered here in case some fraction
    # is added as a band
    data <- .cube_filter_bands(cube = data, bands = bands)
    # Check if cube is regular
    .check_is_regular(data)
    # Pre-condition
    .check_endmembers_bands(
        em = em,
        bands = .cube_bands(data, add_cloud = FALSE)
    )
    # Fractions to be produced
    out_fracs <- .endmembers_fracs(em = em, include_rmse = rmse_band)

    # Check memory and multicores
    # Get block size
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(data)))
    # Check minimum memory needed to process one block
    job_memsize <- .jobs_memsize(
        job_size = .block_size(block = block, overlap = 0),
        npaths = length(bands) + length(out_fracs),
        nbytes = 8, proc_bloat = .conf("processing_bloat")
    )
    # Update multicores parameter
    multicores <- .jobs_max_multicores(
        job_memsize = job_memsize, memsize = memsize, multicores = multicores
    )
    # Update block parameter
    block <- .jobs_optimal_block(
        job_memsize = job_memsize, block = block,
        image_size = .tile_size(.tile(data)), memsize = memsize,
        multicores = multicores
    )
    # Prepare parallelization
    .sits_parallel_start(workers = multicores, log = TRUE,
                         output_dir = output_dir)
    on.exit(.sits_parallel_stop(), add = TRUE)
    # Create mixture processing function
    mixture_fn <- .mixture_fn_nnls(em = em, rmse = rmse_band)
    # Create features as jobs
    features_cube <- .cube_split_features(data)
    # Process each feature in parallel
    features_fracs <- .jobs_map_parallel_dfr(features_cube, function(feature) {
        # Process the data
        output_feature <- .mixture_feature(
            feature = feature,
            block = block,
            em = em,
            mixture_fn = mixture_fn,
            out_fracs = out_fracs,
            output_dir = output_dir
        )
        return(output_feature)
    }, progress = progress)
    # Join output features as a cube and return it
    .cube_merge_tiles(dplyr::bind_rows(list(features_cube, features_fracs)))
}

# ---- mixture functions ----

.mixture_samples <- function(samples, em, mixture_fn, out_fracs) {
    # Get the time series of samples time series
    values <- .ts(samples)
    # Endmembers bands
    em_bands <- .endmembers_bands(em)
    # Apply the non-negative least squares solver
    # the band parameter is used to ensure the order of bands
    values <- mixture_fn(values = .ts_values(ts = values, bands = em_bands))
    # Rename columns fractions
    colnames(values) <- out_fracs
    # Merge samples and fractions values
    .samples_merge_fracs(samples = samples, values = values)
}

.mixture_feature <- function(feature, block, em,
                             mixture_fn, out_fracs, output_dir) {
    # Output files
    out_files <- .file_eo_name(
        tile = feature, band = out_fracs,
        date = .tile_start_date(feature), output_dir = output_dir
    )
    # Resume feature
    if (.raster_is_valid(out_files, output_dir = output_dir)) {
        # # Callback final tile classification
        # .callback(process = "mixtureModel", event = "recovery",
        #           context = environment())
        message("Recovery: fractions ",
                paste0("'", out_fracs, "'", collapse = ", "),
                " already exists.")
        message("(If you want to produce a new image, please ",
                "change 'output_dir' parameters)")
        # Create tile based on template
        fracs_feature <- .tile_eo_from_files(
            files = out_files,
            fid = .fi_fid(.fi(feature)),
            bands = out_fracs,
            date = .tile_start_date(feature),
            base_tile = feature,
            update_bbox = FALSE
        )
        return(fracs_feature)
    }
    # Remove remaining incomplete fractions files
    unlink(out_files)
    # Create chunks as jobs
    chunks <- .tile_chunks_create(tile = feature, overlap = 0, block = block)
    # Process jobs sequentially
    block_files <- .jobs_map_sequential(chunks, function(chunk) {
        # Get job block
        block <- .block(chunk)
        .sits_debug_log(
            event = "block_size",
            key = "list",
            value = block
        )
        # Block file name for each fraction
        block_files <- .file_block_name(
            pattern = .file_pattern(out_files), block = block,
            output_dir = output_dir
        )
        # Resume processing in case of failure
        if (.raster_is_valid(block_files)) {
            return(block_files)
        }
        # Read bands data
        values <- .mixture_data_read(tile = feature, block = block, em = em)
        .sits_debug_log(
            event = "start_nnls_solver",
            key = "dim_values",
            value = dim(values)
        )
        # Apply the non-negative least squares solver
        values <- mixture_fn(values = as.matrix(values))
        .sits_debug_log(
            event = "end_nnls_solver",
            key = "dim_values",
            value = dim(values)
        )
        # Prepare fractions to be saved
        band_conf <- .tile_band_conf(tile = feature, band = out_fracs)
        offset <- .offset(band_conf)
        if (!is.null(offset) && offset != 0) {
            values <- values - offset
        }
        scale <- .scale(band_conf)
        if (!is.null(scale) && scale != 1) {
            values <- values / scale
        }
        # Prepare and save results as raster
        .sits_debug_log(
            event = "write_block",
            key = "files",
            value = block_files
        )
        .raster_write_block(
            files = block_files,
            block = block,
            bbox = .bbox(chunk),
            values = values,
            data_type = .data_type(band_conf),
            missing_value = .miss_value(band_conf),
            crop_block = NULL
        )
        # Free memory
        gc()
        # Returned block files for each fraction
        block_files
    })
    # Merge blocks into a new eo_cube tile feature
    fracs_feature <- .tile_eo_merge_blocks(
        files = out_files, bands = out_fracs, base_tile = feature,
        block_files = block_files, multicores = 1, update_bbox = FALSE
    )
    # Return a eo_cube tile feature
    fracs_feature
}

.mixture_data_read <- function(tile, block, em) {
    # for cubes that have a time limit to expire - mpc cubes only
    tile <- .cube_token_generator(tile)
    # Read and preprocess values from cloud
    # Get cloud values (NULL if not exists)
    cloud_mask <- .tile_cloud_read_block(tile = tile, block = block)
    # Get endmembers bands
    bands <- .endmembers_bands(em)
    # Read and preprocess values from each band
    values <- purrr::map_dfc(bands, function(band) {
        # Get band values (stops if band not found)
        values <- .tile_read_block(tile = tile, band = band, block = block)
        # Remove cloud masked pixels
        if (!is.null(cloud_mask)) {
            values[cloud_mask] <- NA
        }
        # Return values
        as.data.frame(values)
    })
    # Return values
    values
}

# ---- endmembers functions ----

.endmembers_type <- function(em) {
    if (is.data.frame(em)) {
        "tbl_df"
    } else if (is.character(em)) {
        ext <- tolower(.file_ext(em))
        if (ext %in% c("csv", "shp")) {
            ext
        } else {
            stop("not supported extension '", ext, "'")
        }
    } else {
        stop("invalid 'endmembers' parameter type")
    }
}

.endmembers_switch <- function(em, ...) {
    switch(.endmembers_type(em), ...)
}

.endmembers_as_tbl <- function(em) {
    em <- .endmembers_switch(
        em,
        "tbl_df" = em,
        "csv" = utils::read.csv(em),
        "shp" = sf::st_read(em)
    )
    # Ensure that all columns are in uppercase
    dplyr::rename_with(em, toupper)
}

.endmembers_bands <- function(em) {
    setdiff(colnames(em), "TYPE")
}

.endmembers_fracs <- function(em, include_rmse = FALSE) {
    if (!include_rmse) return(toupper(em[["TYPE"]]))
    toupper(c(em[["TYPE"]], "RMSE"))
}

.endmembers_as_matrix <- function(em) {
    bands <- .endmembers_bands(em)
    as.matrix(em[, bands])
}

# ---- samples functions ----

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

.samples_split_groups <- function(samples, multicores) {
    # Change multicores value in case multicores is greater than samples nrows
    multicores <- if (multicores > nrow(samples)) nrow(samples) else multicores
    # Create a new column to each group id
    samples[["group"]] <- rep(
        seq_len(multicores), each = ceiling(nrow(samples) / multicores)
    )[seq_len(nrow(samples))]
    # Split each group by an id
    dplyr::group_split(
        dplyr::group_by(samples, .data[["group"]]), .keep = FALSE)
}

.samples_merge_groups <- function(samples_lst) {
    # Binding the list items into a tibble
    samples <- dplyr::bind_rows(samples_lst)
    # add sits class to the tibble structure
    class(samples) <- c("sits", class(samples))
    # Return sits tibble
    samples
}

# ---- mixture model functions ----

.mixture_fn_nnls <- function(em, rmse) {
    em_mtx <- .endmembers_as_matrix(em)
    mixture_fn <- function(values) {
        # Check values length
        input_pixels <- nrow(values)
        # Process NNLS solver and return
        values <- C_nnls_solver_batch(
            x = as.matrix(values),
            em = em_mtx,
            rmse = rmse
        )
        # Are the results consistent with the data input?
        .check_processed_values(values, input_pixels)
        # Return values
        values
    }
    # Return a closure
    mixture_fn
}
