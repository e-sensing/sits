#' @title Estimate ensemble prediction based on list of probs cubes
#'
#' @name  .comb
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param  probs_cubes   List of probability data cubes.
#' @param  uncert_cubes  List of uncertainty cubes to be used as local weights.
#' @param  comb_fn       Method to combine cubes
#' @param  band          Band to be processed
#' @param  memsize       Maximum overall memory (in GB) to run the
#'                       function.
#' @param  multicores    Number of cores to run the function.
#' @param  output_dir    Output directory for image files.
#' @param  version       Version of resulting image.
#' @param  progress      Show progress bar?
#' @return A combined probability cube
.comb <- function(probs_cubes,
                  uncert_cubes,
                  comb_fn,
                  band,
                  memsize,
                  multicores,
                  output_dir,
                  version,
                  progress, ...) {
    # Check memory and multicores
    # Get block size
    base_cube <- probs_cubes[[1]]
    block_size <- .raster_file_blocksize(
        .raster_open_rast(.tile_path(base_cube))
    )
    # Check minimum memory needed to process one block
    job_memsize <- .jobs_memsize(
        job_size = .block_size(block = block_size),
        npaths = length(probs_cubes) * nrow(base_cube) *
            length(.cube_labels(base_cube)),
        nbytes = 8,
        proc_bloat = .conf("processing_bloat_cpu")
    )
    # Update multicores parameter
    multicores <- .jobs_max_multicores(
        job_memsize = job_memsize,
        memsize = memsize,
        multicores = multicores
    )
    # Update block parameter
    block_size <- .jobs_optimal_block(
        job_memsize = job_memsize,
        block = block_size,
        image_size = .tile_size(.tile(base_cube)),
        memsize = memsize,
        multicores = multicores
    )
    # Prepare parallel processing
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop(), add = TRUE)
    # Call the combine method
    # Process each tile sequentially
    probs_cube <- .map_dfr(seq_len(nrow(base_cube)), function(i) {
        probs_tile <- .comb_tiles(
            probs_tiles = lapply(probs_cubes, .slice_dfr, i),
            uncert_tiles = lapply(uncert_cubes, .slice_dfr, i),
            band = band,
            comb_fn = comb_fn,
            block_size = block_size,
            output_dir = output_dir,
            version = version,
            progress = progress
        )
        probs_tile
    })
    probs_cube
}
#' @title Estimate ensemble prediction for tile-band-block combinations
#' @name  .comb_tiles
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param  probs_tiles   List of probability tiles.
#' @param  uncert_cubes  List of uncertainty tiles.
#' @param  band          Band to be processed
#' @param  comb_fn       Method to combine cubes
#' @param  block_size    Size of block to be processed
#' @param  output_dir    Output directory for image files.
#' @param  version       Version of resulting image.
#' @param  progress      Show progress bar?
#' @return A combined tile-band-block raster object
.comb_tiles <- function(probs_tiles,
                        uncert_tiles,
                        band,
                        comb_fn,
                        block_size,
                        output_dir,
                        version,
                        progress) {
    base_tile <- probs_tiles[[1]]
    # Output file
    out_file <- .file_derived_name(
        tile = base_tile,
        band = band,
        version = version,
        output_dir = output_dir
    )
    # Resume feature
    if (file.exists(out_file)) {
        .check_recovery(out_file)
        probs_tile <- .tile_derived_from_file(
            file = out_file,
            band = band,
            base_tile = base_tile,
            labels = .tile_labels(base_tile),
            derived_class = "probs_cube",
            update_bbox = FALSE
        )
        return(probs_tile)
    }
    # Create chunks as jobs
    chunks <- .tile_chunks_create(
        tile = base_tile,
        overlap = 0,
        block = block_size
    )
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
        values <- lapply(probs_tiles, function(tile) {
            .tile_read_block(
                tile = tile,
                band = .tile_bands(tile),
                block = block
            )
        })
        # If an uncertainty cube has been passed, read it
        uncert_values <- NULL
        if (.has(uncert_tiles)) {
            # Read and preprocess values
            uncert_values <- lapply(uncert_tiles, function(tile) {
                .tile_read_block(
                    tile = tile,
                    band = .tile_bands(tile),
                    block = block
                )
            })
        }
        # Apply the probability function to values
        values <- comb_fn(values, uncert_values = uncert_values)
        # Prepare probability to be saved
        band_conf <- .conf_derived_band(
            derived_class = "probs_cube", band = band
        )
        offset <- .offset(band_conf)
        if (offset != 0) {
            values <- values - offset
        }
        scale <- .scale(band_conf)
        if (scale != 1) {
            values <- values / scale
        }
        min <- .min_value(band_conf)
        max <- .max_value(band_conf)
        # check minimum and maximum values
        values[values < min] <- min
        values[values > max] <- max
        # Prepare and save results as raster
        .raster_write_block(
            files = block_file,
            block = block,
            bbox = .bbox(chunk),
            values = values,
            data_type = .data_type(band_conf),
            missing_value = .miss_value(band_conf),
            crop_block = NULL
        )
        # Free memory
        gc()
        # Return block file
        block_file
    }, progress = progress)
    # Merge blocks into a new probs_cube tile
    probs_tile <- .tile_derived_merge_blocks(
        file = out_file,
        band = band,
        labels = .tile_labels(base_tile),
        base_tile = base_tile,
        block_files = block_files,
        derived_class = "probs_cube",
        multicores = .jobs_multicores(),
        update_bbox = FALSE
    )
    # Return probs tile
    probs_tile
}
#---- combine functions ----
#' @title Combine probs tiles by average value
#' @name  .comb_fn_average
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param  cubes         List of probability cubes.
#' @param  weights       Weights for weigthed average
#' @return A combined tile-band-block raster object
.comb_fn_average <- function(cubes, weights) {
    # Average probability calculation
    comb_fn <- function(values, uncert_values = NULL) {
        # Check values length
        input_pixels <- nrow(values[[1]])
        # Combine by average
        values <- weighted_probs(values, weights)
        # get the number of labels
        n_labels <- length(.cube_labels(cubes[[1]]))
        # Are the results consistent with the data input?
        .check_processed_values(values, input_pixels)
        .check_processed_labels(values, n_labels)
        # Return values
        values
    }
    comb_fn
}
#' @title Combine probs tiles by uncertainty
#' @name  .comb_fn_uncertainty
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param  cubes         List of probability cubes.
#' @return A combined tile-band-block raster object
.comb_fn_uncertainty <- function(cubes) {
    # Average probability calculation
    comb_fn <- function(values, uncert_values) {
        # Check values length
        input_pixels <- nrow(values[[1]])
        # Combine by average
        values <- weighted_uncert_probs(values, uncert_values)
        # get the number of labels
        n_labels <- length(.cube_labels(cubes[[1]]))
        # Are the results consistent with the data input?
        .check_processed_values(values, input_pixels)
        .check_processed_labels(values, n_labels)
        # Return values
        values
    }
    comb_fn
}
