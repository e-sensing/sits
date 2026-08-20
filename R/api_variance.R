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
#' @param  progress        Check progress bar?
#' @return A variance tile.
.variance_tile <- function(tile,
                           band,
                           block,
                           overlap,
                           smooth_fn,
                           output_dir,
                           version,
                           progress) {
    # Output file
    out_file <- .file_derived_name(
        tile = tile, band = band, version = version,
        output_dir = output_dir
    )
    # Resume feature
    if (all(.raster_is_valid(out_file, output_dir = output_dir))) {
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
        if (all(.raster_is_valid(block_file))) {
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
    }, progress = progress)
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
#' @param  progress          Show progress bar?
#'
#' @return A variance data cube.
.variance <- function(cube,
                      block,
                      window_size,
                      neigh_fraction,
                      multicores,
                      memsize,
                      output_dir,
                      version,
                      progress) {
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
            version = version,
            progress = progress
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
#' @title Create a variance segment tile
#' @name .variance_segment_tile
#' @keywords internal
#' @noRd
#' @param tile tile of data cube
#' @param output_dir directory where files will be saved
#' @param version version name of resulting cube
#' @param progress progress bar
#' @param ...  additional parameter to extractextract function
#' @return variance tile
.variance_segment_tile <- function(tile,
                                   neigh_fraction,
                                   output_dir,
                                   version,
                                   progress, ...) {
    # Define output band
    band <- "variance"
    # Output file
    out_file <- .file_derived_name(
        tile = tile,
        band = band,
        version = version,
        output_dir = output_dir
    )
    # Resume feature
    if (all(.raster_is_valid(out_file, output_dir = output_dir))) {
        .check_recovery()
        var_tile <- .tile_derived_from_file(
            file = out_file,
            band = band,
            base_tile = tile,
            labels = .tile_labels(tile),
            derived_class = "variance_cube",
            update_bbox = FALSE
        )
        var_tile[["vector_info"]] <- tile[["vector_info"]]
        vector_classes <- c(
            .conf_vector_s3class("variance_vector_cube"),
            class(var_tile)
        )
        return(.cube_set_class(var_tile, vector_classes))
    }

    # Get tile band
    tile_band <- .tile_bands(tile)
    # Get band configuration
    band_conf <- .tile_band_conf(tile = tile, band = tile_band)
    # Get labels
    labels <- .tile_labels(tile)
    # Read the segments
    segments <- .segments_read_vec(tile)
    # Create ID for segments
    segments[["ID"]] <- seq_len(nrow(segments))
    # Open probability raster (all bands)
    probs_path <- .tile_path(tile)
    probs_rast <- .raster_open_rast(probs_path)

    # Extract values
    values <- .segments_extract_features(
        rast = probs_rast,
        segments = segments,
        fun = NULL,
        seg_id_col = "ID",
        ...
    )
    # Get ID from segments
    values_id <- values[["ID"]]
    # Probability columns (all bands in the probs raster)
    prob_cols <- setdiff(colnames(values), c("ID", "cell"))
    # Convert values to matrix
    values <- as.matrix(values[, prob_cols])
    # Apply scale and offset to extracted probability values
    values <- .tile_scale(tile = tile, band = tile_band, values = values)
    # Avoid zero or one values to prevent -Inf/Inf/NaN in logit
    values[values <= 0.00001] <- 0.00001
    values[values >= 0.99999] <- 0.99999

    # Calculate denom
    row_sums <- rowSums(values)
    denom <- row_sums - values
    denom[denom <= 0.00001] <- 0.00001
    # Calculate logits
    values <- log(values / denom)

    # Call C++ function to aggregate variance per segment
    values <- segment_variance(
        logits = values,
        ids = as.integer(values_id),
        n_segments = nrow(segments),
        neigh_fraction = neigh_fraction
    )

    segment_ids <- sort(unique(values_id))
    values <- values[segment_ids, , drop = FALSE]
    colnames(values) <- prob_cols

    # Unscale values
    values <- .tile_unscale(
        tile = tile,
        band = band,
        values = values,
        derived_class = "variance_cube"
    )

    # Rasterize: assign variance to all pixels within each segment
    seg_vect <- .raster_open_vect(segments[segment_ids, ])
    for (class_name in colnames(values)) {
        seg_vect[[class_name]] <- values[, class_name]
    }
    # Rasterize segments onto the template for each class individually
    var_rasts <- lapply(colnames(values), function(class_name) {
        template_rast <- .raster_rast(probs_rast, nlayers = 1L)
        .raster_rasterize(
            vect = seg_vect,
            rast = template_rast,
            field = class_name,
            fun = "max"
        )
    })
    # Combine the rasters into a multi-layer SpatRaster
    var_rast <- .raster_rast(var_rasts)
    # Set missing value
    var_rast <- .raster_set_na(var_rast, .miss_value(band_conf))
    # Write raster
    .raster_write_rast(
        rast = var_rast,
        file = out_file,
        data_type = .data_type(band_conf),
        overwrite = TRUE,
        missing_value = .miss_value(band_conf)
    )
    # Create output tile from file
    var_tile <- .tile_derived_from_file(
        file = out_file,
        band = band,
        base_tile = tile,
        derived_class = "variance_cube",
        labels = labels,
        update_bbox = FALSE
    )
    # Preserve vector_info from the input probs tile (segments are not modified)
    var_tile[["vector_info"]] <- tile[["vector_info"]]
    # Set variance_vector_cube + variance_cube chain
    vector_classes <- c(
        .conf_vector_s3class("variance_vector_cube"),
        class(var_tile)
    )
    .cube_set_class(var_tile, vector_classes)
}
