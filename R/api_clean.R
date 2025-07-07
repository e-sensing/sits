#' @title Cleans a subset of a image on block model
#' @name .clean_tile
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @noRd
#' @description
#' Applies a modal function to clean up possible noisy pixels keeping
#' the most frequently values within the neighborhood.
#' In a tie, the first value of the vector is considered.
#'
#' @param tile        Subset of a data cube
#' @param block       Image block to be cleaned
#' @param band        Band to be processed
#' @param window_size Size of local neighborhood
#' @param overlap     Overlap between blocks
#' @param output_dir  Directory where files will be saved.
#' @param version     Version of the output file.
#' @return            Cleaned tile-band-block asset
.clean_tile <- function(tile,
                        block,
                        band,
                        window_size,
                        overlap,
                        output_dir,
                        version,
                        progress) {
    # Output file
    out_file <- .file_derived_name(
        tile = tile, band = band, version = version, output_dir = output_dir
    )
    # Resume tile
    if (.raster_is_valid(out_file, output_dir = output_dir)) {
        # recovery message
        .check_recovery()
        # Create tile based on template
        tile <- .tile_derived_from_file(
            file = out_file, band = band,
            base_tile = tile, derived_class = .tile_derived_class(tile),
            labels = .tile_labels(tile),
            update_bbox = FALSE
        )
        return(tile)
    }
    # Create chunks as jobs
    chunks <- .tile_chunks_create(tile = tile, overlap = overlap, block = block)
    # Process jobs sequentially
    block_files <- .jobs_map_parallel_chr(chunks, function(chunk) {
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
        values <- .clean_data_read(
            tile = tile, block = block, band = band
        )
        # Apply kernel modal
        values <- C_kernel_modal(
            x = as.matrix(values),
            ncols = block[["ncols"]],
            nrows = block[["nrows"]],
            band = 0L,
            window_size = window_size
        )
        # Prepare fractions to be saved
        band_conf <- .tile_band_conf(tile = tile, band = band)
        # Job crop block
        crop_block <- .block(.chunks_no_overlap(chunk))
        # Prepare and save results as raster
        .raster_write_block(
            files = block_files, block = block, bbox = .bbox(chunk),
            values = values, data_type = .data_type(band_conf),
            missing_value = .miss_value(band_conf),
            crop_block = crop_block
        )
        # Free memory
        gc()
        # Returned block files for each fraction
        block_files
    }, progress = progress)
    # Merge blocks into a new class_cube tile
    .tile_derived_merge_blocks(
        file = out_file,
        band = band,
        labels = .tile_labels(tile),
        base_tile = tile,
        derived_class = .tile_derived_class(tile),
        block_files = block_files,
        multicores = 1L,
        update_bbox = FALSE
    )
}

#' @title Read data for cleaning operation
#' @name .clean_data_read
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @noRd
#' @param tile        Tile of a data cube
#' @param band        Band to be processed
#' @param block       Image block to be processed
#' @return            Values for tile-band-block combination
.clean_data_read <- function(tile, block, band) {
    # Get band values
    values <- as.data.frame(.tile_read_block(
        tile = tile, band = band, block = block
    ))
    # Set columns name
    colnames(values) <- band
    # Return values
    values
}
