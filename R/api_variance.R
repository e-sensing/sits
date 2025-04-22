#' @title Calculate the variance of a tile
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @description Takes a probability cube and estimate the local variance
#'              of the logit of the probability,
#'              to support the choice of parameters for Bayesian smoothing.
#'
#' @param  tile              Tile of a data cube.
#' @param  band              Band to be processed.
#' @param  block             Block size
#' @param  overlap           Overlap between tiles
#' @param  smooth_fn         Function used for smoothing
#' @param  output_dir        Output directory for image files
#' @param  version           Version of resulting image
#'                           (in the case of multiple tests)
#' @return A variance tile.
.variance_tile <- function(tile,
                           band,
                           block,
                           overlap,
                           smooth_fn,
                           output_dir,
                           version) {
    # Output file
    out_file <- .file_derived_name(
        tile = tile, band = band, version = version,
        output_dir = output_dir
    )
    # Resume feature
    if (file.exists(out_file)) {
        .check_recovery()

        var_tile <- .tile_derived_from_file(
            file = out_file,
            band = band,
            base_tile = tile,
            labels = .tile_labels(tile),
            derived_class = "variance_cube",
            update_bbox = FALSE
        )
        return(var_tile)
    }
    # Create chunks as jobs
    chunks <- .tile_chunks_create(tile = tile, overlap = overlap, block = block)
    # Process jobs in parallel
    block_files <- .jobs_map_parallel_chr(chunks, function(chunk) {
        # Job block
        block <- .block(chunk)
        # Block file name
        block_file <- .file_block_name(
            pattern = .file_pattern(out_file),
            block = block,
            output_dir = output_dir
        )
        # Resume processing in case of failure
        if (.raster_is_valid(block_file)) {
            return(block_file)
        }
        # Read and preprocess values
        values <- .tile_read_block(
            tile = tile, band = .tile_bands(tile), block = block
        )
        # Apply the probability function to values
        values <- smooth_fn(values = values, block = block)
        # Prepare probability to be saved
        band_conf <- .conf_derived_band(
            derived_class = "variance_cube", band = band
        )
        offset <- .offset(band_conf)
        if (.has(offset) && offset != 0.0) {
            values <- values - offset
        }
        scale <- .scale(band_conf)
        if (.has(scale) && scale != 1.0) {
            values <- values / scale
        }
        # Job crop block
        crop_block <- .block(.chunks_no_overlap(chunk))
        # Prepare and save results as raster
        .raster_write_block(
            files = block_file, block = block, bbox = .bbox(chunk),
            values = values, data_type = .data_type(band_conf),
            missing_value = .miss_value(band_conf),
            crop_block = crop_block
        )
        # Free memory
        gc()
        # Return block file
        block_file
    })
    # Merge blocks into a new var_cube tile
    var_tile <- .tile_derived_merge_blocks(
        file = out_file,
        band = band,
        labels = .tile_labels(tile),
        base_tile = tile,
        block_files = block_files,
        derived_class = "variance_cube",
        multicores = .jobs_multicores(),
        update_bbox = FALSE
    )
    # Return var tile
    var_tile
}
#' @title Calculate the variance of a probability cube
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @description Takes a probability cube and estimate the local variance
#'              of the logit of the probability,
#'              to support the choice of parameters for Bayesian smoothing.
#'
#' @param  cube              Probability data cube.
#' @param  block             Block size
#' @param  window_size       Size of the neighborhood.
#' @param  neigh_fraction    Fraction of neighbors with highest probability
#'                           to be used in Bayesian inference.
#' @param  multicores        Number of cores to run the smoothing function
#' @param  memsize           Maximum overall memory (in GB) to run the
#'                           smoothing.
#' @param  output_dir        Output directory for image files
#' @param  version           Version of resulting image
#'                           (in the case of multiple tests)
#'
#' @return A variance data cube.
.variance <- function(cube,
                      block,
                      window_size,
                      neigh_fraction,
                      multicores,
                      memsize,
                      output_dir,
                      version) {
    # Smooth parameters checked in smooth function creation
    # Create smooth function
    smooth_fn <- .variance_fn(
        window_size = window_size,
        neigh_fraction = neigh_fraction
    )
    # Overlapping pixels
    overlap <- ceiling(window_size / 2L) - 1L
    # Smoothing
    # Process each tile sequentially
    .cube_foreach_tile(cube, function(tile) {
        # calculate variance
        .variance_tile(
            tile = tile,
            band = "variance",
            block = block,
            overlap = overlap,
            smooth_fn = smooth_fn,
            output_dir = output_dir,
            version = version
        )
    })
}
#' @title Calculate the variance smoothing function
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param  window_size       Size of the neighborhood.
#' @param  neigh_fraction    Fraction of neighbors with highest probability
#'                           to be used in Bayesian inference.
#'
#' @return A variance smoothing function.
.variance_fn <- function(window_size,
                         neigh_fraction) {
    # Check window size
    .check_int_parameter(window_size, min = 3L, is_odd = TRUE)
    # Create a window
    window <- matrix(1L, nrow = window_size, ncol = window_size)
    # Define smooth function
    smooth_fn <- function(values, block) {
        # Check values length
        input_pixels <- nrow(values)
        # Compute logit
        values <- log(values / (rowSums(values) - values))
        # Process variance
        values <- bayes_var(
            m = values,
            m_nrow = .nrows(block),
            m_ncol = .ncols(block),
            w = window,
            neigh_fraction = neigh_fraction
        )
        # Are the results consistent with the data input?
        .check_processed_values(values, input_pixels)
        # Return values
        values
    }
    # Return a closure
    smooth_fn
}
