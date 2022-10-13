#  Tile API
#
#  A cube consists of multiple tiles stacked together as rows of a
#  tibble. A tile is a only-one-row tibble that stores
#  metadata of a spatial partition of a cube.

#' @title Get first tile of a cube
#' @name .tile
#' @noRd
#' @description This function should be called
#' by all tile API function to ensure that only one tile will be processed.
#' @param cube A \code{cube} or a \code{tile}.
#' @return The first tile of a cube.
.tile <- function(cube) {
    UseMethod(".tile", cube)
}
#' @export
.tile.raster_cube <- function(cube) {
    cube[1, ]
}

#  Tile field accessors
#  These functions enable access components of tiles. A
#
.tile_source <- function(tile) {
    UseMethod(".tile_source", tile)
}
#' @export
.tile_source.raster_cube <- function(tile) {
    .as_chr(tile[["source"]][[1]])
}
#
.tile_collection <- function(tile) {
    UseMethod(".tile_collection", tile)
}

#' @export
.tile_collection.raster_cube <- function(tile) {
    .as_chr(tile[["collection"]][[1]])
}

#
.tile_name <- function(tile) {
    UseMethod(".tile_name", tile)
}
#
.tile_name.raster_cube <- function(tile) {
    .as_chr(tile[["tile"]][[1]])
}
#
.tile_ncols <- function(tile) {
    UseMethod(".tile_ncols", tile)
}
#' @export
.tile_ncols.raster_cube <- function(tile) {
    if ("ncols" %in% tile) {
        return(.ncols(tile)[[1]])
    }
    .ncols(.fi(tile))[[1]]
}
#
.tile_nrows <- function(tile) {
    UseMethod(".tile_nrows", tile)
}
#' @export
.tile_nrows.raster_cube <- function(tile) {
    if ("nrows" %in% tile) {
        return(.nrows(tile)[[1]])
    }
    .nrows(.fi(tile))[[1]]
}
#
.tile_size <- function(tile) {
    UseMethod(".tile_size", tile)
}
#' @export
.tile_size.raster_cube <- function(tile) {
    list(ncols = .tile_ncols(tile), nrows = .tile_nrows(tile))
}
#
.tile_labels <- function(tile) {
    UseMethod(".tile_labels", tile)
}
#' @export
.tile_labels.raster_cube <- function(tile) {
    .as_chr(tile[["labels"]][[1]])
}
#
`.tile_labels<-` <- function(tile, value) {
    UseMethod(".tile_labels<-", tile)
}
#' @export
`.tile_labels<-.raster_cube` <- function(tile, value) {
    tile <- .tile(tile)
    tile[["labels"]] <- list(.as_chr(value))
    tile
}
#
.tile_file_info <- function(tile) {
    UseMethod(".tile_file_info", tile)
}
#' @export
.tile_file_info.raster_cube <- function(tile) {
    .fi(tile) # Get the first file_info
}
#'
`.tile_file_info<-` <- function(tile, value) {
    UseMethod(".tile_file_info<-", tile)
}
#' @export
`.tile_file_info<-.raster_cube` <- function(tile, value) {
    tile <- .tile(tile)
    tile[["file_info"]] <- list(value)
    tile
}
#' @title Convert tile bbox to a sf polygon object.
#' @name .tile_as_sf
#' @param tile A tile.
#' @keywords internal
#' @noRd
#' @return sf object
.tile_as_sf <- function(tile) {
    UseMethod(".tile_as_sf", tile)
}
#' @export
.tile_as_sf.raster_cube <- function(tile) {
    .bbox_as_sf(.tile(tile))
}
#'
#' @title Get first date from tile
#' @name .tile_start_date
#' @keywords internal
#' @noRd
#' @param tile A tile.
#'
#' @return date
.tile_start_date <- function(tile) {
    UseMethod(".tile_start_date", tile)
}
#' @export
.tile_start_date.raster_cube <- function(tile) {
    .fi_min_date(.fi(tile))
}
#'
#' @title Get end date from file_info.
#' @name .tile_end_date
#' @keywords internal
#' @noRd
#' @param tile A tile.
#'
#' @return date
.tile_end_date <- function(tile) {
    UseMethod(".tile_end_date", tile)
}
#' @export
.tile_end_date.raster_cube <- function(tile) {
    .fi_max_date(.fi(tile))
}
#' @title Get unique timeline from file_info.
#' @name .tile_timeline
#' @keywords internal
#' @noRd
#' @param tile A tile.
#'
#' @return a timeline
.tile_timeline <- function(tile) {
    UseMethod(".tile_timeline", tile)
}
#' @export
.tile_timeline.raster_cube <- function(tile) {
    unique(.fi_timeline(.fi(tile)))
}
#' @title Get sorted unique bands from file_info.
#' @name .tile_path
#' @keywords internal
#' @noRd
#' @param tile A tile.
#' @param band A band in the tile
#' @param date A date in the tile
#'
#' @return First path of file info
.tile_path <- function(tile, band = NULL, date = NULL) {
    UseMethod(".tile_path", tile)
}
#' @export
.tile_path.raster_cube <- function(tile, band, date){
    if (!purrr::is_null(band) & !purrr::is_null(date)) {
        tile <- .tile_filter_bands(tile, band) %>%
            .tile_filter_date(date)
    }
    path <- .fi(.tile(tile)) %>% .fi_path()
    path
}
#' @title Get sorted unique bands from file_info.
#' @name .tile_bands
#' @keywords internal
#' @noRd
#' @param tile A tile.
#'
#' @return names of bands in the tile
.tile_bands <- function(tile) {
    UseMethod(".tile_bands", tile)
}

#' @export
.tile_bands.raster_cube <- function(tile) {
    unique(.fi_bands(.fi(tile)))
}
#'
#' @title Get a band definition from config.
#' @name .tile_band_conf
#' @keywords internal
#' @noRd
#' @param tile A tile.
#' @param band Band character vector.
#'
#' @return band_conf or band_cloud_conf
.tile_band_conf <- function(tile, band) {
    UseMethod(".tile_band_conf", tile)
}
#' @export
.tile_band_conf.eo_cube <- function(tile, band) {
    .conf_eo_band(
        source = .tile_source(tile), collection = .tile_collection(tile),
        band = band[[1]]
    )
}
#' @export
.tile_band_conf.derived_cube <- function(tile, band) {
    .conf_derived_band(derived_class = .tile_derived_class(tile), band = band)
}
#'
#' @title Filter file_info entries of a given \code{band}.
#' @name .tile_filter_bands
#' @keywords internal
#' @noRd
#' @param tile A tile.
#' @param bands Band names to be filtered.
#'
#' @return tile with selected files for the bands
.tile_filter_bands <- function(tile, bands) {
    UseMethod(".tile_filter_bands", tile)
}
#' @export
.tile_filter_bands.eo_cube <- function(tile, bands) {
    tile <- .tile(tile)
    .tile_file_info(tile) <-
        .fi_filter_bands(fi = .fi(tile), bands = .band_eo(bands))
    tile
}
#' @export
.tile_filter_bands.derived_cube <- function(tile, bands) {
    tile <- .tile(tile)
    .tile_file_info(tile) <-
        .fi_filter_bands(fi = .fi(tile), bands = .band_derived(bands))
    tile
}
#'
#' @title Does tile \code{bbox} intersect \code{roi} parameter?
#' @name .tile_intersects
#' @keywords internal
#' @noRd
#' @param tile A tile.
#' @param roi A region of interest (ROI).
#'
#' @return logical
.tile_intersects <- function(tile, roi) {
    UseMethod(".tile_intersects", tile)
}
#' @export
.tile_intersects.raster_cube <- function(tile, roi) {
    .intersects(x = .tile_as_sf(tile), y = .roi_as_sf(roi))
}
#' @title Filter file_info entries that intersect roi.
#' @name .tile_filter_spatial
#' @keywords internal
#' @noRd
#' @param tile A tile.
#' @param roi A region of interest (ROI).
#'
#' @return tile
.tile_filter_spatial <- function(tile, roi) {
    UseMethod(".tile_filter_spatial", tile)
}
#' @export
.tile_filter_spatial.raster_cube <- function(tile, roi) {
    tile <- .tile(tile)
    .tile_file_info(tile) <- .fi_filter_spatial(fi = .fi(tile), roi = roi)
    tile
}
#'
#' @title Is any date of tile's timeline between 'start_date'
#' and 'end_date'?
#' @name .tile_during
#' @keywords internal
#' @noRd
#' @param tile A tile.
#' @param start_date,end_date Date of start and end.
#'
#' @return logical
.tile_during <- function(tile, start_date, end_date) {
    UseMethod(".tile_during", tile)
}
#' @export
.tile_during.raster_cube <- function(tile, start_date, end_date) {
    any(.fi_during(
        fi = .fi(tile), start_date = start_date, end_date = end_date
    ))
}
#'
#' @title Filter file_info entries by 'start_date' and 'end_date.'
#' @name .tile_filter_interval
#' @keywords internal
#' @noRd
#' @param tile A tile.
#' @param start_date,end_date Date of start and end.
#'
#' @return file_info entries
.tile_filter_interval <- function(tile, start_date, end_date) {
    UseMethod(".tile_filter_interval", tile)
}
#' @export
.tile_filter_interval.raster_cube <- function(tile, start_date, end_date) {
    tile <- .tile(tile)
    .tile_file_info(tile) <- .fi_filter_interval(
        fi = .fi(tile), start_date = start_date, end_date = end_date
    )
    tile
}
#'
#' @title Filter file_info entries by date
#' @name .tile_filter_date
#' @keywords internal
#' @noRd
#' @param tile A tile.
#' @param date Desired date
#'
#' @return file_info entries
.tile_filter_date <- function(tile, date) {
    .tile_filter_interval(tile, start_date = date, end_date = date)
}
#'
#' @title Get derived class of a tile.
#' @name .tile_derived_class
#' @keywords internal
#' @noRd
#' @param tile A tile.
#'
#' @return derived class
.tile_derived_class <- function(tile) {
    UseMethod(".tile_derived_class", tile)
}
#' @export
.tile_derived_class.derived_cube <- function(tile) {
    class(tile)[[1]]
}
#'
#' @title Read and preprocess a block of band values from
#' file_info rasters.
#' @name .tile_read_block
#' @keywords internal
#' @noRd
#' @description
#' eo_cube tiles preprocess is slightly different from
#' derived_cube tiles. Values outside the range of minimum and maximum for
#' a band are replaced by NA in eo_cubes. In derived_cubes,
#' values outside allowed range are clamped and replaced by minimum or maximum
#' values.
#'
#' @param tile A tile.
#' @param band Band character vector.
#' @param block A block list with (col, row, ncols, nrows).
#'
#' @return set of values from a band of a tile inside a block
.tile_read_block <- function(tile, band, block) {
    UseMethod(".tile_read_block", tile)
}
#' @export
.tile_read_block.eo_cube <- function(tile, band, block) {
    fi <- .fi(tile)
    # Stops if band is not found
    values <- .fi_read_block(fi = fi, band = .band_eo(band), block = block)
    #
    # Log here
    #
    .sits_debug_log(
        event = "start_block_data_process",
        key = "band",
        value = band
    )
    # Correct missing, minimum, and maximum values and
    # apply scale and offset.
    band_conf <- .tile_band_conf(tile = tile, band = band)
    miss_value <- .miss_value(band_conf)
    if (.has(miss_value)) {
        values[values == miss_value] <- NA
    }
    min_value <- .min_value(band_conf)
    if (.has(min_value)) {
        values[values < min_value] <- NA
    }
    max_value <- .max_value(band_conf)
    if (.has(max_value)) {
        values[values > max_value] <- NA
    }
    scale <- .scale(band_conf)
    if (.has(scale) && scale != 1) {
        values <- values * scale
    }
    offset <- .offset(band_conf)
    if (.has(offset) && offset != 0) {
        values <- values + offset
    }
    #
    # Log here
    #
    .sits_debug_log(
        event = "end_block_data_process",
        key = "band",
        value = band
    )
    # Return values
    return(values)
}
#' @export
.tile_read_block.derived_cube <- function(tile, band, block) {
    fi <- .fi(tile)
    # Stops if band is not found
    values <- .fi_read_block(fi = fi, band = .band_derived(band), block = block)
    # Correct missing, minimum, and maximum values and
    # apply scale and offset.
    band_conf <- .tile_band_conf(tile = tile, band = band)
    miss_value <- .miss_value(band_conf)
    if (.has(miss_value)) {
        values[values == miss_value] <- NA
    }
    min_value <- .min_value(band_conf)
    if (.has(min_value)) {
        values[values < min_value] <- min_value
    }
    max_value <- .max_value(band_conf)
    if (.has(max_value)) {
        values[values > max_value] <- max_value
    }
    scale <- .scale(band_conf)
    if (.has(scale) && scale != 1) {
        values <- values * scale
    }
    offset <- .offset(band_conf)
    if (.has(offset) && offset != 0) {
        values <- values + offset
    }
    # Return values
    return(values)
}
#'
#' @title Read and preprocess a block of cloud values from
#' file_info rasters.
#' @name .tile_cloud_read_block
#' @keywords internal
#' @noRd
#' @param tile A tile.
#' @param block A block list with (col, row, ncols, nrows).
#'
#' @return set of values of a band of a tile in a block
.tile_cloud_read_block <- function(tile, block) {
    UseMethod(".tile_cloud_read_block", tile)
}
#' @export
.tile_cloud_read_block.eo_cube <- function(tile, block) {
    if (!.band_cloud() %in% .tile_bands(tile)) {
        return(NULL)
    }
    values <- .tile_read_block(
        tile = tile, band = .band_cloud(), block = block
    )
    #
    # Log here
    #
    .sits_debug_log(
        event = "start_block_data_process",
        key = "cloud_mask",
        value = "cloud_mask"
    )
    # Get cloud parameters
    cloud_conf <- .tile_band_conf(tile = tile, band = .band_cloud())
    interp_values <- .cloud_interp_values(cloud_conf)
    is_bit_mask <- .cloud_bit_mask(cloud_conf)
    # Prepare cloud_mask
    # Identify values to be removed
    if (!is_bit_mask) {
        values <- values %in% interp_values
    } else {
        values <- matrix(bitwAnd(values, sum(2^interp_values)) > 0,
                         nrow = length(values)
        )
    }
    #
    # Log here
    #
    .sits_debug_log(
        event = "end_block_data_process",
        key = "cloud_bit_mask",
        value = is_bit_mask
    )
    # Return values
    return(values)
}
#' @title Create chunks of a tile to be processed
#' @name .tile_chunks_create
#' @keywords internal
#' @noRd
#' @param tile tile to be processed
#' @param overlap overlap between tiles
#' @return set of chunks to be read from the file

.tile_chunks_create <- function(tile, overlap) {
    # Get block size
    block <-
        .raster_file_blocksize(.raster_open_rast(.tile_path(tile)))
    # Compute chunks
    .chunks_create(
        block = block,
        overlap = overlap,
        image_size = .tile_size(tile),
        image_bbox = .bbox(tile)
    )
}
#' @title read an EO tile from files
#' @name .tile_eo_from_files
#' @keywords internal
#' @noRd
#' @param files files to be read
#' @param fid   file ID
#' @param bands bands to be read in the files
#' @param date  date associated to the file
#' @param base_tile  reference tile used in the operation
#' @param update_bbox  should bbox be updated?
#' @return a base tile
.tile_eo_from_files <- function(files, fid, bands, date, base_tile,
                                update_bbox) {
    if (update_bbox) {
        # Open raster
        r_obj <- .raster_open_rast(files)
        # Update spatial bbox
        .xmin(base_tile) <- .raster_xmin(r_obj)
        .xmax(base_tile) <- .raster_xmax(r_obj)
        .ymin(base_tile) <- .raster_ymin(r_obj)
        .ymax(base_tile) <- .raster_ymax(r_obj)
        .crs(base_tile) <- .raster_crs(r_obj)
    }
    # Update file_info
    .tile_file_info(base_tile) <- .fi_eo_from_files(
        files = files, fid = fid, bands = bands, date = date
    )
    # Return eo tile
    base_tile
}
#' @title Merge block from an EO tile
#' @name .tile_eo_merge_blocks
#' @keywords internal
#' @noRd
#' @param files files to be merged
#' @param bands bands to be used in the files
#' @param base_tile  reference tile used in the operation
#' @param block_files files associated with the the blocks
#' @param multicores  multicores for processing
#' @param update_bbox  should bbox be updated?
#' @return an EO tile with merged blocks
.tile_eo_merge_blocks <- function(files, bands, base_tile, block_files,
                                  multicores, update_bbox) {
    # Get conf band
    band_conf <- .tile_band_conf(tile = base_tile, band = bands)
    # Create a template raster based on the first image of the tile
    .raster_merge_blocks(
        out_files = files, base_file = .tile_path(base_tile),
        block_files = block_files, data_type = .data_type(band_conf),
        missing_value = .miss_value(band_conf), multicores = multicores
    )
    # Create tile based on template
    tile <- .tile_eo_from_files(
        files = files, fid = .fi_fid(.fi(base_tile)), bands = bands,
        date = .fi_date(.fi(base_tile)), base_tile = base_tile,
        update_bbox = update_bbox
    )
    # If all goes well, delete block files
    unlink(unlist(block_files))
    # Return eo tile
    tile
}
#' @title Create a tile derived from a file
#' @name .tile_derived_from_file
#' @keywords internal
#' @noRd
#' @param files files to be merged
#' @param band  band to be used in the tile
#' @param base_tile  reference tile used in the operation
#' @param derived_class class of the derived tile
#' @param labels labels associated to the tile
#' @param update_bbox  should bbox be updated?
#' @return a new tile
.tile_derived_from_file <- function(file, band, base_tile, derived_class,
                                    labels = NULL, update_bbox) {
    if (update_bbox) {
        # Open raster
        r_obj <- .raster_open_rast(file)
        # Update spatial bbox
        .xmin(base_tile) <- .raster_xmin(r_obj)
        .xmax(base_tile) <- .raster_xmax(r_obj)
        .ymin(base_tile) <- .raster_ymin(r_obj)
        .ymax(base_tile) <- .raster_ymax(r_obj)
        .crs(base_tile) <- .raster_crs(r_obj)
    }
    # Update labels before file_info
    .tile_labels(base_tile) <- labels
    # Update file_info
    .tile_file_info(base_tile) <- .fi_derived_from_file(
        file = file, band = band, start_date = .tile_start_date(base_tile),
        end_date = .tile_end_date(base_tile)
    )
    # Set tile class and return tile
    .cube_set_class(base_tile, .conf_derived_s3class(derived_class))
}
#' @title Write values of a derived tile from a set of blocks
#' @name .tile_derived_merge_blocks
#' @keywords internal
#' @noRd
#' @param file file to be written
#' @param band  band to be used in the tile
#' @param labels labels associated to the tile
#' @param base_tile  reference tile used in the operation
#' @param derived_class class of the derived tile
#' @param block_files  files that host the blocks
#' @param multicores  number of parallel processes
#' @param update_bbox  should bbox be updated?
#' @return a new tile with files written
.tile_derived_merge_blocks <- function(file, band, labels, base_tile,
                                       derived_class, block_files, multicores,
                                       update_bbox) {
    # Get conf band
    band_conf <- .conf_derived_band(derived_class = derived_class, band = band)
    # Set base tile
    base_file <- if (update_bbox) NULL else .tile_path(base_tile)
    # Create a template raster based on the first image of the tile
    .raster_merge_blocks(
        out_files = file, base_file = base_file,
        block_files = block_files, data_type = .data_type(band_conf),
        missing_value = .miss_value(band_conf), multicores = multicores
    )
    # Create tile based on template
    tile <- .tile_derived_from_file(
        file = file, band = band, base_tile = base_tile,
        derived_class = derived_class, labels = labels,
        update_bbox = update_bbox
    )
    # If all goes well, delete block files
    unlink(block_files)
    # Return derived tile
    tile
}

#' @title Create a "probs" tile
#' @name .tile_probs_from_file
#' @keywords internal
#' @noRd
#' @param file file to be written
#' @param band  band to be used in the tile
#' @param base_tile  reference tile used in the operation
#' @param labels labels associated to the tile
#' @param update_bbox  should bbox be updated?
#' @return a new probs tile
.tile_probs_from_file <- function(file, band, base_tile, labels, update_bbox) {
    # Open block file to be merged
    r_obj <- .raster_open_rast(file)
    # Check number of labels is correct
    .check_that(
        x = .raster_nlayers(r_obj) == length(labels),
        local_msg = "number of image layers does not match labels",
        msg = "invalid 'file' parameter"
    )
    .tile_derived_from_file(
        file = file, band = band, base_tile = base_tile,
        derived_class = "probs_cube", labels = labels,
        update_bbox = update_bbox
    )
}
#' @title Write values of a probs tile from a set of blocks
#' @name .tile_probs_merge_blocks
#' @keywords internal
#' @noRd
#' @param file file to be written
#' @param band  band to be used in the tile
#' @param labels labels associated to the tile
#' @param base_tile  reference tile used in the operation
#' @param block_files  files that host the blocks
#' @param multicores  number of parallel processes
#' @param update_bbox  should bbox be updated?
#' @return a new probs tile with files written
.tile_probs_merge_blocks <- function(file, band, labels, base_tile,
                                     block_files, multicores, update_bbox) {
    # Open first block file to be merged
    r_obj <- .raster_open_rast(unlist(block_files)[[1]])
    # Check number of labels is correct
    .check_that(
        x = .raster_nlayers(r_obj) == length(labels),
        local_msg = "number of image layers does not match labels",
        msg = "invalid 'file' parameter"
    )
    # Create probs cube and return it
    .tile_derived_merge_blocks(
        file = file, band = band, labels = labels,
        base_tile = base_tile, derived_class = "probs_cube",
        block_files = block_files, multicores = multicores,
        update_bbox = update_bbox
    )
}

#' @title Create a "class" tile
#' @name .tile_class_from_file
#' @keywords internal
#' @noRd
#' @param file file to be written
#' @param band  band to be used in the tile
#' @param base_tile  reference tile used in the operation (probs)
#' @return a new probs tile
.tile_class_from_file <- function(file, band, base_tile) {
    .tile_derived_from_file(
        file = file, band = band, base_tile = base_tile,
        derived_class = "class_cube", labels = .tile_labels(base_tile),
        update_bbox = FALSE
    )
}
#' @title Write values of a class tile from a set of blocks
#' @name .tile_class_merge_blocks
#' @keywords internal
#' @noRd
#' @param file file to be written
#' @param band  band to be used in the tile
#' @param labels labels associated to the tile
#' @param base_tile  reference tile used in the operation
#' @param block_files  files that host the blocks
#' @param multicores  number of parallel processes
#' @return a new class tile with files written
.tile_class_merge_blocks <- function(file, band, labels, base_tile,
                                     block_files, multicores) {
    # Create class cube and return it
    .tile_derived_merge_blocks(
        file = file, band = band, labels = labels,
        base_tile = base_tile, derived_class = "class_cube",
        block_files = block_files, multicores = multicores,
        update_bbox = FALSE
    )
}

#' @title Create an "uncertainity" tile
#' @name .tile_uncertainty_from_file
#' @keywords internal
#' @noRd
#' @param file file to be written
#' @param band  band to be used in the tile
#' @param base_tile  reference tile used in the operation (probs)
#' @return a new uncertainty tile
.tile_uncertainty_from_file <- function(file, band, base_tile) {
    .tile_derived_from_file(
        file = file, band = band, base_tile = base_tile,
        derived_class = "uncertainty_cube", labels = .tile_labels(base_tile),
        update_bbox = FALSE
    )
}
#' @title Write values of a uncertainty tile from a set of blocks
#' @name .tile_uncertainty_merge_blocks
#' @keywords internal
#' @noRd
#' @param file file to be written
#' @param band  band to be used in the tile
#' @param labels labels associated to the tile
#' @param base_tile  reference tile used in the operation
#' @param block_files  files that host the blocks
#' @param multicores  number of parallel processes
#' @return a new uncertainty tile with files written
.tile_uncertainty_merge_blocks <- function(file, band, labels, base_tile,
                                           block_files, multicores) {
    # Create uncertainty cube and return it
    .tile_derived_merge_blocks(
        file = file, band = band, labels = labels,
        base_tile = base_tile, derived_class = "uncertainty_cube",
        block_files = block_files, multicores = multicores,
        update_bbox = FALSE
    )
}
#' @title Given a labelled cube, return the band information
#' @name .tile_area_freq
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param tile   Tile of a data cube
#'
#' @return Frequency of each label in the data cube
#'
.tile_area_freq <- function(tile) {

    UseMethod(".tile_area_freq", tile)
}
#' @export
.tile_area_freq.class_cube <- function(tile) {

    # open first raster
    r_obj <- .raster_open_rast(.tile_path(tile))

    # retrieve the frequency
    freq <- tibble::as_tibble(.raster_freq(r_obj))

    return(freq)
}
#' @title Given a tile and a band, return a set of values for chosen location
#' @name .tile_extract
#' @noRd
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Given a data cube, retrieve the time series of XY locations
#'
#' @param tile        Metadata about a data cube (one tile)
#' @param band        Name of the band to the retrieved
#' @param xy          Matrix with XY location
#'
#' @return Numeric matrix with raster values for each coordinate.
#'
.tile_extract <- function(tile, band, xy) {


    # set caller to show in errors
    .check_set_caller(".tile_extract")

    # pre-condition - one tile at a time
    .check_has_one_tile(tile)
    # does the cube contain the band?
    .check_band_in_cube(band, tile)

    # filter the files that contain the band
    band <- .tile_filter_bands(tile, band)

    # create a stack object
    r_obj <- .raster_open_rast(band$path)

    # extract the values
    values <- .raster_extract(r_obj, xy)

    # is the data valid?
    .check_that(
        x = nrow(values) == nrow(xy),
        msg = "error in retrieving data"
    )
    return(values)
}


#---- ml_model ----
#' @keywords internal
.ml_model <- function(ml_model) {
    if ("model" %in% ls(environment(ml_model))) {
        environment(ml_model)[["model"]]
    } else if ("torch_model" %in% ls(environment(ml_model))) {
        environment(ml_model)[["torch_model"]]
    } else {
        stop("cannot extract model object")
    }
}
#' @keywords internal
.ml_stats_0 <- function(ml_model) {
    # Old stats variable
    environment(ml_model)[["stats"]]
}
#' @keywords internal
.ml_stats <- function(ml_model) {
    # New stats variable
    environment(ml_model)[["ml_stats"]]
}
#' @keywords internal
.ml_samples <- function(ml_model) {
    environment(ml_model)[["samples"]]
}
#' @keywords internal
.ml_class <- function(ml_model) {
    class(ml_model)[[1]]
}
#' @keywords internal
.ml_features_name <- function(ml_model) {
    # Get feature names from variable used in training
    names(environment(ml_model)[["train_samples"]])[-2:0]
}
#' @keywords internal
.ml_bands <- function(ml_model) {
    .sits_bands(.ml_samples(ml_model))
}
#' @keywords internal
.ml_labels <- function(ml_model) {
    .sits_labels(.ml_samples(ml_model))
}
#' @keywords internal
.torch_serialize_model <- function(model) {
    # Open raw connection
    con <- rawConnection(raw(), open = "wr")
    # Close connection on exit
    on.exit(close(con), add = TRUE)
    # Serialize and save torch model on connection
    torch::torch_save(model, con)
    # Read serialized model and return
    rawConnectionValue(con)
}
#' @keywords internal
.torch_unserialize_model <- function(raw) {
    # Open raw connection to read model
    con <- rawConnection(raw)
    # Close connection on exit
    on.exit(close(con), add = TRUE)
    # Unserialize and load torch model from connection and return
    torch::torch_load(con)
}

#---- stats ----
#' @keywords internal
# Supports former version of stats
.stats_0_q02 <- function(stats, band) {
    quantile_02 <- 2
    stats[[band]][[quantile_02]]
}
#' @keywords internal
# Supports former version of stats
.stats_0_q98 <- function(stats, band) {
    quantile_98 <- 3
    stats[[band]][[quantile_98]]
}
#' @keywords internal
.stats_q02 <- function(stats) {
    stats[["q02"]]
}
#' @keywords internal
.stats_q98 <- function(stats) {
    stats[["q98"]]
}

#---- sits (samples) ----
#' @keywords internal
.sits_ts <- function(samples) {
    # Add sample_id column
    samples[["sample_id"]] <- seq_len(nrow(samples))
    # Extract time_series from column
    ts <- tidyr::unnest(
        data = samples[c("sample_id", "label", "time_series")],
        cols = "time_series"
    )
    # Select the same bands as in the first sample
    ts <- ts[c("sample_id", "label", "Index", .sits_bands(samples))]
    # Get the time series length for the first sample
    ntimes <- .sits_ntimes(samples)
    # Prune time series according to the first sample
    ts <- .by_dfr(data = ts, col = "sample_id", fn = function(x) {
        if (nrow(x) == ntimes) {
            x
        } else if (nrow(x) > ntimes) {
            x[seq_len(ntimes), ]
        } else {
            stop("time series length differs from first sample")
        }
    })
    # Return time series
    ts
}
#' @keywords internal
.sits_ntimes <- function(samples) {
    # Number of observations of the first sample governs whole samples data
    nrow(samples[["time_series"]][[1]])
}
#' @keywords internal
.sits_bands <- function(samples) {
    # Bands of the first sample governs whole samples data
    setdiff(names(samples[["time_series"]][[1]]), "Index")
}
#' @keywords internal
.sits_filter_bands <- function(samples, bands) {
    # Missing bands
    miss_bands <- bands[!bands %in% .sits_bands(samples)]
    if (.has(miss_bands)) {
        stop("band(s) ", paste0("'", miss_bands, "'", collapse = ", "),
             " not found")
    }
    .apply(samples, col = "time_series", function(x) {
        dplyr::select(x, dplyr::all_of(c("#..", "Index", bands)))
    })
}
#' @keywords internal
.sits_labels <- function(samples) {
    sort(unique(samples[["label"]]), na.last = TRUE)
}
#' @keywords internal
.sits_split <- function(samples, split_intervals) {
    slider::slide_dfr(samples, function(sample) {
        ts <- sample[["time_series"]][[1]]
        purrr::map_dfr(split_intervals, function(index) {
            new_sample <- sample
            start <- index[[1]]
            end <- index[[2]]
            new_sample[["time_series"]][[1]] <- ts[seq(start, end), ]
            new_sample[["start_date"]] <- ts[["Index"]][[start]]
            new_sample[["end_date"]] <- ts[["Index"]][[end]]
            new_sample
        })
    })
}
#' @keywords internal
.sits_predictors <- function(samples, ml_model = NULL) {
    # Get samples time series
    pred <- .sits_ts(samples)
    # By default get bands as the same of first sample
    bands <- .sits_bands(samples)
    # Preprocess time series
    if (.has(ml_model)) {
        # Update bands to the model bands
        bands <- .ml_bands(ml_model)
        # If a model is informed, get predictors from model bands
        pred <- pred[c(.pred_cols, bands)]
        # Normalize values for old version model classifiers that
        #   do not normalize values itself
        # Models trained after version 1.2 do this automatically before
        #   classification
        stats <- .ml_stats_0(ml_model) # works for old models only!!
        if (.has(stats)) {
            # Read and preprocess values of each band
            pred[bands] <- purrr::imap_dfc(pred[bands], function(values, band) {
                # Get old stats parameters
                q02 <- .stats_0_q02(stats, band)
                q98 <- .stats_0_q98(stats, band)
                if (!is.null(q02) && !is.null(q98)) {
                    # Use C_normalize_data_0 to process old version of
                    #   normalization
                    values <- C_normalize_data_0(
                        data = as.matrix(values), min = q02, max = q98
                    )
                    # Convert from matrix to vector and return
                    unlist(values)
                }
                # Return updated values
                values
            })
            # Return updated time series
            pred
        }
    }
    # Create predictors...
    pred <- pred[c(.pred_cols, bands)]
    # Add sequence 'index' column grouped by 'sample_id'
    pred <- .by_dfr(data = pred, col = "sample_id", fn = function(x) {
        x[["index"]] <- seq_len(nrow(x))
        x
    })
    # Rearrange data to create predictors
    pred <- tidyr::pivot_wider(
        data = pred, names_from = "index", values_from = bands,
        names_prefix = ifelse(length(bands) == 1, bands, ""),
        names_sep = ""
    )
    # Return predictors
    pred
}
#' @keywords internal
.sits_stats <- function(samples) {
    # Get all time series
    preds <- .sits_ts(samples)
    # Select attributes
    preds <- preds[.sits_bands(samples)]
    # Compute stats
    q02 <- apply(preds, 2, stats::quantile, probs = 0.02, na.rm = TRUE)
    q98 <- apply(preds, 2, stats::quantile, probs = 0.98, na.rm = TRUE)
    # Number of observations
    ntimes <- .sits_ntimes(samples)
    # Replicate stats
    q02 <- rep(unname(q02), each = ntimes)
    q98 <- rep(unname(q98), each = ntimes)
    # Return stats object
    list(q02 = q02, q98 = q98)
}

# ---- Predictors ----

.pred_cols <- c("sample_id", "label")
#' @keywords internal
.pred_features <- function(pred) {
    if (all(.pred_cols %in% names(pred))) {
        pred[, -2:0]
    } else {
        pred
    }
}
#' @keywords internal
`.pred_features<-` <- function(pred, value) {
    if (all(.pred_cols %in% names(pred))) {
        pred[, -2:0] <- value
    } else {
        pred[,] <- value
    }
    pred
}
#' @keywords internal
.pred_references <- function(pred) {
    if (all(.pred_cols %in% names(pred))) .as_chr(pred[["label"]]) else NULL
}
#' @keywords internal
.pred_normalize <- function(pred, stats) {
    values <- as.matrix(.pred_features(pred))
    values <- C_normalize_data(
        data = values, min = .stats_q02(stats), max = .stats_q98(stats)
    )
    .pred_features(pred) <- values
    # Return predictors
    pred
}
#' @keywords internal
.pred_create_partition <- function(pred, partitions) {
    pred[["part_id"]] <- .partitions(x = seq_len(nrow(pred)), n = partitions)
    tidyr::nest(pred, predictors = -"part_id")
}

# ---- Partitions ----
#' @keywords internal
.part_predictors <- function(part) {
    if (.has(part[["predictors"]])) part[["predictors"]][[1]] else NULL
}

# ---- expressions ----
#' @keywords internal
.expr_names <- function(expr) {
    if (is.call(expr)) {
        unique(unlist(lapply(as.list(expr)[-1], .expr_names)))
    } else if (is.name(expr)) {
        .as_chr(expr)
    } else {
        character()
    }
}
#' @keywords internal
.expr_calls <- function(expr) {
    if (is.call(expr)) {
        unique(c(
            paste0(expr[[1]]), unlist(lapply(as.list(expr)[-1], .expr_calls))
        ))
    } else {
        character()
    }
}

# ---- gdal API ----
.gdal_data_type <- c(
    "INT1U" = "Byte", "INT2U" = "UInt16", "INT2S" = "Int16",
    "INT4U" = "UInt32", "INT4S" = "Int32", "FLT4S" = "Float32",
    "FLT8S" = "Float64"
)
#' @keywords internal
.gdal_params <- function(params) {
    # Check if parameters are named
    if (!all(.has_name(params))) {
        stop("parameters should be named")
    }
    unlist(mapply(function(par, val) {
        if (is.logical(val)) {
            if (val) par else NULL
        } else if (is.list(val)) {
            c(par, unlist(val))
        } else {
            .as_chr(rbind(par, val))
        }
    }, names(params), unname(params), USE.NAMES = FALSE))
}
#' @keywords internal
.gdal_translate <- function(file, base_file, params, quiet) {
    sf::gdal_utils(
        util = "translate", source = base_file[[1]], destination = file[[1]],
        options = .gdal_params(params), quiet = quiet
    )
}
#' @keywords internal
.gdal_warp <- function(file, base_files, params, quiet) {
    sf::gdal_utils(
        util = "warp", source = base_files, destination = file[[1]],
        options = .gdal_params(params), quiet = quiet
    )
}
#' @keywords internal
.gdal_template_from_file <- function(base_file, file, nlayers, miss_value,
                                     data_type) {
    # Convert to gdal data type
    data_type <- .gdal_data_type[[data_type]]
    # Output file
    file <- .try({
        .gdal_translate(
            file = file,
            base_file = base_file,
            params = list(
                "-ot" = data_type,
                "-of" = .conf("gdal_presets", "image", "of"),
                "-b" = rep(1, nlayers),
                "-scale" = list(0, 1, miss_value, miss_value),
                "-a_nodata" = miss_value,
                "-co" = .conf("gdal_presets", "image", "co")
            ),
            quiet = TRUE
        )
    },
    .rollback = {
        unlink(file)
    },
    .finally = {
        # Delete auxiliary files
        unlink(paste0(file, ".aux.xml"))
    })
    # Return file
    file
}
#' @keywords internal
.gdal_template_block <- function(block, bbox, file, nlayers, miss_value,
                                 data_type) {
    # Get first file
    file <- file[[1]]
    # Convert to gdal data type
    data_type <- .gdal_data_type[[data_type]]
    # Output file
    file <- .try({
        .gdal_translate(
            file = file,
            # GDAL does not allow raster creation, to bypass this limitation
            # Let's base our raster creation by using a tiny template
            # (647 Bytes)
            base_file = system.file(
                "extdata/raster/gdal/template.tif", package = "sits"
            ),
            params = list(
                "-ot" = data_type,
                "-of" = .conf("gdal_presets", "block", "of"),
                "-b" = rep(1, nlayers),
                "-outsize" = list(.ncols(block), .nrows(block)),
                "-scale" = list(0, 1, miss_value, miss_value),
                "-a_srs" = .crs(bbox),
                "-a_ullr" = list(
                    .xmin(bbox), .ymax(bbox), .xmax(bbox), .ymin(bbox)
                ),
                "-a_nodata" = miss_value,
                "-co" = .conf("gdal_presets", "block", "co")
            ),
            quiet = TRUE
        )
    },
    .rollback = {
        unlink(file)
    },
    .finally = {
        # Delete auxiliary files
        unlink(paste0(file, ".aux.xml"))
    })
    # Return file
    file
}
#' @keywords internal
.gdal_merge_into <- function(file, base_files, multicores) {
    # Merge src_files
    file <- .try({
        .gdal_warp(
            file = file,
            base_files = base_files,
            params = list(
                "-wo" = paste0("NUM_THREADS=", multicores),
                "-multi" = TRUE,
                "-q" = TRUE,
                "-overwrite" = FALSE
            ),
            quiet = TRUE
        )
    },
    .rollback = {
        unlink(file)
    },
    .finally = {
        # Delete auxiliary files
        unlink(paste0(file, ".aux.xml"))
    })
    # Return file
    file
}
