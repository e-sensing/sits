#' @title Apply a glcm measure to a raster block
#' @name .glcm_feature
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  feature         Subset of a data cube containing the input bands
#'                         used in the expression
#' @param  block           Individual block that will be processed
#' @param  window_size     Size of the neighbourhood (if required)
#' @param  angles          The direction angles in radians related to the
#'                         central pixel and its neighbor.
#' @param  expr            Expression to be applied
#' @param  out_band        Output band
#' @param  in_bands        Input bands
#' @param  overlap         Overlap between tiles (if required)
#' @param  output_dir      Directory where image will be save
#'
#' @return                 A feature compose by a combination of tile and band.
.glcm_feature <- function(feature, block, window_size, angles, expr,
                          out_band, in_bands, overlap, output_dir) {
    # Output file
    out_file <- .file_eo_name(
        tile = feature, band = out_band,
        date = .tile_start_date(feature), output_dir = output_dir
    )
    # Resume feature
    if (.raster_is_valid(out_file, output_dir = output_dir)) {
        # recovery message
        .check_recovery(out_file)

        # Create tile based on template
        feature <- .tile_eo_from_files(
            files = out_file, fid = .fi_fid(.fi(feature)),
            bands = out_band, date = .tile_start_date(feature),
            base_tile = feature, update_bbox = FALSE
        )
        return(feature)
    }
    # Remove remaining incomplete fractions files
    unlink(out_file)
    # Create chunks as jobs
    chunks <- .tile_chunks_create(
        tile = feature, overlap = overlap, block = block
    )
    # Get band configuration
    band_conf <- .tile_band_conf(tile = feature, band = out_band)
    if (.has_not(band_conf)) {
        band_conf <- .conf("default_values", "INT2S")
    }
    # Get the default grey level
    n_grey <- .conf(c("glcm_options", "max_grey_level"))
    # Process jobs sequentially
    block_files <- .jobs_map_parallel(chunks, function(chunk) {
        # Get job block
        block <- .block(chunk)
        # Block file name for each fraction
        block_files <- .file_block_name(
            pattern = .file_pattern(out_file),
            block = block,
            output_dir = output_dir
        )
        # Resume processing in case of failure
        if (.raster_is_valid(block_files)) {
            return(block_files)
        }
        # Read bands data
        values <- .apply_data_read(
            tile = feature, block = block, in_bands = in_bands
        )
        # Fill with zeros remaining NA pixels
        values[[1]] <- C_fill_na(as.matrix(values[[1]]), 0)
        # Scale values
        values <- .glcm_scale(values, band_conf)
        # Evaluate expression here
        # Band and kernel evaluation
        values <- eval(
            expr = expr[[out_band]],
            envir = values,
            enclos = .glcm_functions(
                window_size = window_size,
                angles = angles,
                img_nrow = block[["nrows"]],
                img_ncol = block[["ncols"]],
                n_grey = n_grey
            )
        )
        # Prepare fractions to be saved
        offset <- .offset(band_conf)
        if (.has(offset) && offset != 0) {
            values <- values - offset
        }
        # Job crop block
        crop_block <- .block(.chunks_no_overlap(chunk))
        # Prepare and save results as raster
        .raster_write_block(
            files = block_files, block = block, bbox = .bbox(chunk),
            values = values, data_type = .data_type(band_conf),
            missing_value = .miss_value(band_conf),
            crop_block = crop_block
        )
        # Returned block files for each fraction
        block_files
    })
    # Merge blocks into a new eo_cube tile
    band_tile <- .tile_eo_merge_blocks(
        files = out_file,
        bands = out_band,
        band_conf = band_conf,
        base_tile = feature,
        block_files = block_files,
        multicores = 1,
        update_bbox = FALSE
    )
    return(band_tile)
}

#' @title Scale values based on a grey level range
#' @name .glcm_scale
#' @noRd
#' @param cube sits cube
#' @return a vector with the adjusted block size
.glcm_scale <- function(values, band_conf) {
    glcm_min <- .conf(c("glcm_options", "min_grey_level"))
    glcm_max <- .conf(c("glcm_options", "max_grey_level"))
    scale <- .scale(band_conf)
    values <- values / scale
    from <-  c(.min_value(band_conf), .max_value(band_conf))
    to <- c(glcm_min, glcm_max)

    values <- (values - from[1]) / diff(from) * diff(to) + to[1]
    return(values)
}

#' @title Get block size
#' @name .glcm_get_blocksize
#' @noRd
#' @param cube sits cube
#' @return a vector with the adjusted block size
.glcm_get_blocksize <- function(cube) {
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(cube)))
    glcm_block_size <- .conf(c("glcm_options", "block_size"))
    block[["nrows"]] <- min(block[["nrows"]], glcm_block_size)
    block[["ncols"]] <- min(block[["ncols"]], glcm_block_size)
    return(block)
}

#' @title Kernel function for window operations in spatial neighbourhoods
#' @name .glcm_functions
#' @noRd
#' @param window_size size of local window
#' @param img_nrow    image size in rows
#' @param img_ncol    image size in cols
#' @return glcm measures
.glcm_functions <- function(window_size, angles, img_nrow, img_ncol, n_grey) {
    result_env <- list2env(list(
        glcm_contrast = function(m) {
            C_glcm_contrast(
                x = .as_int(unlist(m)), angles = angles, nrows = img_nrow,
                ncols = img_ncol, window_size = window_size, n_grey = n_grey
            )
        },
        glcm_dissimilarity = function(m) {
            C_glcm_dissimilarity(
                x = .as_int(unlist(m)), angles = angles, nrows = img_nrow,
                ncols = img_ncol, window_size = window_size, n_grey = n_grey
            )
        },
        glcm_homogeneity = function(m) {
            C_glcm_homogeneity(
                x = .as_int(unlist(m)), angles = angles, nrows = img_nrow,
                ncols = img_ncol, window_size = window_size, n_grey = n_grey
            )
        },
        glcm_energy = function(m) {
            C_glcm_energy(
                x = .as_int(unlist(m)), angles = angles, nrows = img_nrow,
                ncols = img_ncol, window_size = window_size, n_grey = n_grey
            )
        },
        glcm_asm = function(m) {
            C_glcm_asm(
                x = .as_int(unlist(m)), angles = angles, nrows = img_nrow,
                ncols = img_ncol, window_size = window_size, n_grey = n_grey
            )
        },
        glcm_mean = function(m) {
            C_glcm_mean(
                x = .as_int(unlist(m)), angles = angles, nrows = img_nrow,
                ncols = img_ncol, window_size = window_size, n_grey = n_grey
            )
        },
        glcm_variance = function(m) {
            C_glcm_variance(
                x = .as_int(unlist(m)), angles = angles, nrows = img_nrow,
                ncols = img_ncol, window_size = window_size, n_grey = n_grey
            )
        },
        glcm_std = function(m) {
            C_glcm_std(
                x = .as_int(unlist(m)), angles = angles, nrows = img_nrow,
                ncols = img_ncol, window_size = window_size, n_grey = n_grey
            )
        },
        glcm_correlation = function(m) {
            C_glcm_correlation(
                x = .as_int(unlist(m)), angles = angles, nrows = img_nrow,
                ncols = img_ncol, window_size = window_size, n_grey = n_grey
            )
        }
    ), parent = parent.env(environment()), hash = TRUE)

    return(result_env)
}
