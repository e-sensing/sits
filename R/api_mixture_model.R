# ---- mixture functions ----
#' @title Apply a mixture model to a set of time series
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param  samples      Time series
#' @param  em           Endmembers bands
#' @param  mixture_fn   Function to be applied.
#' @param out_fracs     Names of output fraction images
#' @return              Set of time series with new fraction band values
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
#' @title Apply a mixture model to a raster feature
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param  feature      Raster feature where mixture is to be applied
#' @param  block        Image block
#' @param  em           Endmembers bands
#' @param  mixture_fn   Function to be applied.
#' @param  out_fracs    Names of output fraction images
#' @param  output_dir   Directory where fraction images will be saved
#' @return              Set of new fraction images
.mixture_feature <- function(feature, block, em,
                             mixture_fn, out_fracs, output_dir) {
    # Output files
    out_files <- .file_eo_name(
        tile = feature, band = out_fracs,
        date = .tile_start_date(feature), output_dir = output_dir
    )
    # Resume feature
    if (.raster_is_valid(out_files, output_dir = output_dir)) {
        .check_recovery(out_fracs)

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
        # Block file name for each fraction
        block_files <- .file_block_name(
            pattern = .file_pattern(out_files),
            block = block,
            output_dir = output_dir
        )
        # Resume processing in case of failure
        if (.raster_is_valid(block_files)) {
            return(block_files)
        }
        # Read bands data
        values <- .mixture_data_read(tile = feature, block = block, em = em)
        # Apply the non-negative least squares solver
        values <- mixture_fn(values = as.matrix(values))
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
        # Return block files for each fraction
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
#' @title Read data to compute a mixture model
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param  tile         Raster tile
#' @param  block        Image block
#' @param  em           Endmember values
#' @return              Set of values for new fraction images (by block)
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

# ---- mixture model functions ----
#' @title Solve linear mixture model using NNLS (Non-Negative Least Squares)
#' @keywords internal
#' @noRd
#' @param  em       Endmember values
#' @param  rmse     Root mean square error band
#' @return          Mixture model function to calculate fraction bands
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

# ---- endmembers functions ----
#' @title Check type of endmembers table
#' @keywords internal
#' @noRd
#' @param  em       Endmember values
#' @return          Type of endmember value specification (csv of tbl_df)
.endmembers_type <- function(em) {
    if (is.data.frame(em)) {
        "tbl_df"
    } else if (is.character(em)) {
        ext <- tolower(.file_ext(em))
        if (ext %in% c("csv")) {
            ext
        } else {
            stop("not supported extension '", ext, "'")
        }
    } else {
        stop("invalid 'endmembers' parameter type")
    }
}
#' @title Switch over type of endmembers table
#' @keywords internal
#' @noRd
#' @param  em       Endmember values
#' @return          Valid endmember specification (csv of tbl_df)
.endmembers_switch <- function(em, ...) {
    switch(.endmembers_type(em),
        ...
    )
}
#' @title Convert endmembers specification to data.frame
#' @keywords internal
#' @noRd
#' @param  em       Endmember values (csv of tbl_df)
#' @return          Endmembers as data.frame
.endmembers_as_tbl <- function(em) {
    em <- .endmembers_switch(
        em,
        "tbl_df" = em,
        "csv" = utils::read.csv(em)
    )
    # Ensure that all columns are in uppercase
    dplyr::rename_with(em, toupper)
}
#' @title Return bands in endmembers specification
#' @keywords internal
#' @noRd
#' @param  em       Endmember values
#' @return          Bands in endmember specification
.endmembers_bands <- function(em) {
    # endmembers tribble can be type or class
    type_class <- colnames(em)[[1]]
    setdiff(colnames(em), type_class)
}
#' @title Return fraction bands in endmembers specification
#' @keywords internal
#' @noRd
#' @param  em           Endmember values
#' @param  include_rmse Include root mean square error band?
#' @return          Bands in endmember specification
.endmembers_fracs <- function(em, include_rmse = FALSE) {
    # endmembers tribble can be type or class
    type_class <- toupper(colnames(em)[[1]])
    if (!include_rmse) {
        return(toupper(em[[type_class]]))
    }
    toupper(c(em[[type_class]], "RMSE"))
}
#' @title Transform endmembers specification to matrix
#' @keywords internal
#' @noRd
#' @param  em       Endmember values
#' @return          Endmember specification as a matrix
.endmembers_as_matrix <- function(em) {
    bands <- .endmembers_bands(em)
    as.matrix(em[, bands])
}
