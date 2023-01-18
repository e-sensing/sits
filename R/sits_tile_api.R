#' @title Tile API
#' @noRd
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description
#' A cube consists of multiple tiles stacked together as rows of a
#' tibble. A tile is a only-one-row tibble that stores
#' metadata of a spatial partition of a cube.
#'
NULL

#' @title Get first tile of a cube
#' @noRd
#' @description
#' This function should be called by all tile API function to ensure that
#' only one tile will be processed.
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
`.tile_name<-` <- function(tile, value) {
    UseMethod(".tile_name<-", tile)
}
#' @export
`.tile_name<-.raster_cube` <- function(tile, value) {
    tile <- .tile(tile)
    tile[["tile"]] <- .as_chr(value)
    tile
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
.tile_xres <- function(tile) {
    UseMethod(".tile_xres", tile)
}
#' @export
.tile_xres.raster_cube <- function(tile) {
    .xres(.fi(tile))
}
#
.tile_yres <- function(tile) {
    UseMethod(".tile_yres", tile)
}
#' @export
.tile_yres.raster_cube <- function(tile) {
    .yres(.fi(tile))
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

#' @title Convert tile \code{bbox} to a sf polygon object.
#' @noRd
#' @param tile A tile.
#' @return sf object
.tile_as_sf <- function(tile) {
    UseMethod(".tile_as_sf", tile)
}
#' @export
.tile_as_sf.raster_cube <- function(tile) {
    .bbox_as_sf(.bbox(.tile(tile)))
}

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
#' @return Path of first asset in `file_info`
.tile_path <- function(tile, band = NULL, date = NULL) {
    UseMethod(".tile_path", tile)
}

#' @export
.tile_path.raster_cube <- function(tile, band = NULL, date = NULL) {
    if (.has(band)) {
        tile <- .tile_filter_bands(tile = tile, bands = band[[1]])
    }
    if (.has(date)) {
        tile <- .tile_filter_dates(tile = tile, dates = date[[1]])
    }
    # Get path of first asset
    path <- .fi_path(.fi(tile))
    # Return path
    path
}

#' @title Get sorted unique bands from file_info.
#' @name .tile_path
#' @keywords internal
#' @noRd
#' @param tile A tile.
#' @param band Band name.
#'
#' @returns Paths to `file_info` assets
.tile_paths <- function(tile, band = NULL) {
    UseMethod(".tile_paths", tile)
}

#' @export
.tile_paths.raster_cube <- function(tile, band = NULL) {
    if (.has(band)) {
        tile <- .tile_filter_bands(tile = tile, bands = band)
    }
    # Get assets path
    paths <- .fi_paths(.fi(tile))
    # Return paths
    paths
}

#' @title Get unique satellite name from tile.
#' @name .tile_satellite
#' @keywords internal
#' @noRd
#' @param tile A tile.
#'
#' @return satellite name in the tile
.tile_satellite <- function(tile) {
    UseMethod(".tile_satellite", tile)
}

#' @export
.tile_satellite.raster_cube <- function(tile) {
    tile <- .tile(tile)
    .as_chr(tile[["satellite"]])
}

#' @title Get unique sensor name from tile.
#' @name .tile_sensor
#' @keywords internal
#' @noRd
#' @param tile A tile.
#'
#' @return sensor name in the tile
.tile_sensor <- function(tile) {
    UseMethod(".tile_sensor", tile)
}

#' @export
.tile_sensor.raster_cube <- function(tile) {
    tile <- .tile(tile)
    .as_chr(tile[["sensor"]])
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
    .conf_derived_band(
        derived_class = .tile_derived_class(tile), band = band[[1]]
    )
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
    .fi(tile) <- .fi_filter_bands(fi = .fi(tile), bands = .band_eo(bands))
    tile
}
#' @export
.tile_filter_bands.derived_cube <- function(tile, bands) {
    tile <- .tile(tile)
    .fi(tile) <- .fi_filter_bands(fi = .fi(tile), bands = .band_derived(bands))
    tile
}
#'
#' @title Get crs from tile
#' @name .tile_crs
#' @keywords internal
#' @noRd
#' @param tile A tile.
#'
#' @return character
.tile_crs <- function(tile) {
    UseMethod(".tile_crs", tile)
}
#' @export
.tile_crs.raster_cube <- function(tile) {
    tile <- .tile(tile)
    .crs(tile)
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
    .fi(tile) <- .fi_filter_spatial(fi = .fi(tile), roi = roi)
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
    .fi(tile) <- .fi_filter_interval(
        fi = .fi(tile), start_date = start_date, end_date = end_date
    )
    tile
}
#'
#' @title Filter file_info entries by date
#' @name .tile_filter_dates
#' @keywords internal
#' @noRd
#' @param tile A tile.
#' @param dates Desired date
#'
#' @return file_info entries
.tile_filter_dates <- function(tile, dates) {
    tile <- .tile(tile)
    .fi(tile) <- .fi_filter_dates(fi = .fi(tile), dates = dates)
    tile
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

.tile_chunks_create <- function(tile, overlap, block = NULL) {
    # Get block size
    block <- .default(
        x = block,
        default = .raster_file_blocksize(.raster_open_rast(.tile_path(tile)))
    )
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
    .fi(base_tile) <- .fi_eo_from_files(
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
        date = .fi_min_date(.fi(base_tile)), base_tile = base_tile,
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
    .fi(base_tile) <- .fi_derived_from_file(
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
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param tile   Tile of a data cube
#'
#' @return Frequency of each label in the data cube
#' @name .tile_area_freq
#' @keywords internal
#' @noRd
.tile_area_freq <- function(tile) {

    UseMethod(".tile_area_freq", tile)
}
#' @export
.tile_area_freq.class_cube <- function(tile) {
    # Open first raster
    r_obj <- .raster_open_rast(.tile_path(tile))
    # Retrieve the frequency
    freq <- tibble::as_tibble(.raster_freq(r_obj))
    # Return frequencies
    freq
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

    # Create a stack object
    r_obj <- .raster_open_rast(.tile_paths(tile = tile, band = band))
    # Extract the values
    values <- .raster_extract(r_obj, xy)
    # Is the data valid?
    if (nrow(values) != nrow(xy)) {
        stop("number of extracted points differ from requested points")
    }
    # Return values
    values
}

.tile_contains_cloud <- function(tile) {
    .source_cloud() %in% .tile_bands(tile)
}

.tile_contains_roi <- function(tile, roi) {
    # Transform roi and bbox to sf
    roi_bbox  <- .roi_as_sf(roi = roi, as_crs = .tile_crs(tile))
    tile_bbox <- .bbox_as_sf(bbox = .bbox(tile), as_crs = .tile_crs(tile))
    # Verify if the roi contains tile bbox
    sf::st_contains(
        x = roi_bbox,
        y = tile_bbox,
        sparse = FALSE
    )
}

#---- ml_model ----

.ml_model <- function(ml_model) {
    if ("model" %in% ls(environment(ml_model))) {
        environment(ml_model)[["model"]]
    } else if ("torch_model" %in% ls(environment(ml_model))) {
        environment(ml_model)[["torch_model"]]
    } else {
        stop("cannot extract model object")
    }
}

.ml_stats_0 <- function(ml_model) {
    # Old stats variable
    environment(ml_model)[["stats"]]
}

.ml_stats <- function(ml_model) {
    # New stats variable
    environment(ml_model)[["ml_stats"]]
}

.ml_samples <- function(ml_model) {
    environment(ml_model)[["samples"]]
}

.ml_class <- function(ml_model) {
    class(ml_model)[[1]]
}

.ml_features_name <- function(ml_model) {
    # Get feature names from variable used in training
    names(environment(ml_model)[["train_samples"]])[-2:0]
}

.ml_bands <- function(ml_model) {
    .sits_bands(.ml_samples(ml_model))
}

.ml_labels <- function(ml_model) {
    .sits_labels(.ml_samples(ml_model))
}

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

.torch_unserialize_model <- function(raw) {
    # Open raw connection to read model
    con <- rawConnection(raw)
    # Close connection on exit
    on.exit(close(con), add = TRUE)
    # Unserialize and load torch model from connection and return
    torch::torch_load(con)
}

#---- stats ----

#' @title Supports former version of stats
#' @noRd
.stats_0_q02 <- function(stats, band) {
    quantile_02 <- 2
    stats[[band]][[quantile_02]]
}

#' @title Supports former version of stats
#' @noRd
.stats_0_q98 <- function(stats, band) {
    quantile_98 <- 3
    stats[[band]][[quantile_98]]
}

.stats_q02 <- function(stats) {
    stats[["q02"]]
}

.stats_q98 <- function(stats) {
    stats[["q98"]]
}

#---- time_series ----

.ts_cols <- c("sample_id", "label")

.is_ts <- function(x) {
    "Index" %in% names(x) && is.data.frame(x)
}

.has_ts <- function(x) {
    "time_series" %in% names(x) && .is_ts(x[["time_series"]][[1]])
}

.ts <- function(x) {
    # Check time_series column
    if (!.has_ts(x)) {
        stop("time_series not found")
    }
    # Add sample_id column
    x[["sample_id"]] <- seq_along(x[["time_series"]])
    # Extract time_series from column
    ts <- tidyr::unnest(
        data = x[c(.ts_cols, "time_series")],
        cols = "time_series"
    )
    # Return time series
    ts
}

`.ts<-` <- function(x, value) {
    if (!.is_ts(value)) {
        stop("invalid time series value")
    }
    # Pack time series
    value <- tidyr::nest(value, time_series = -dplyr::all_of(.ts_cols))
    x[["time_series"]] <- value[["time_series"]]
    # Return samples
    x
}

.ts_index <- function(ts) {
    .as_date(ts[["Index"]])
}

.ts_bands <- function(ts) {
    setdiff(colnames(ts), c(.ts_cols, "Index"))
}

.ts_select_bands <- function(ts, bands) {
    # Check missing bands
    miss_bands <- bands[!bands %in% .ts_bands(ts)]
    if (.has(miss_bands)) {
        stop("band(s) ", .collapse("'", miss_bands, "'"), " not found")
    }
    # Select the same bands as in the first sample
    ts <- ts[unique(c(.ts_cols, "Index", bands))]
    # Return time series
    ts
}

.ts_values <- function(ts, bands = NULL) {
    # Get the time series of samples
    bands <- .default(bands, .ts_bands(ts))
    # Check missing bands
    miss_bands <- bands[!bands %in% .ts_bands(ts)]
    if (.has(miss_bands)) {
        stop("band(s) ", .collapse("'", miss_bands, "'"), " not found")
    }
    ts[bands]
}

#---- sits (samples) ----

.sits_ts <- function(samples) {
    # Check time_series column
    if (!.has_ts(samples)) {
        stop("time_series column not found")
    }
    # Return time series of the first sample
    samples[["time_series"]][[1]]
}

.sits_ntimes <- function(samples) {
    # Number of observations of the first sample governs whole samples data
    nrow(.sits_ts(samples))
}

.sits_bands <- function(samples) {
    # Bands of the first sample governs whole samples data
    setdiff(names(.sits_ts(samples)), "Index")
}
.sits_select_bands <- function(samples, bands) {
    # Filter samples
    .ts(samples) <- .ts_select_bands(ts = .ts(samples), bands = bands)
    # Return samples
    samples
}

.sits_labels <- function(samples) {
    sort(unique(samples[["label"]]), na.last = TRUE)
}

.sits_filter_labels <- function(samples, labels) {
    # Check missing labels
    miss_labels <- labels[!labels %in% .sits_labels(samples)]
    if (.has(miss_labels)) {
        stop("label(s) ", .collapse("'", miss_labels, "'"), " not found")
    }
    # Filter labels
    samples <- samples[samples[["label"]] %in% labels, ]
    # Return samples
    samples
}

.sits_foreach_ts <- function(samples, fn, ...) {
    # Apply function to each time_series
    samples[["time_series"]] <- lapply(samples[["time_series"]], fn, ...)
    # Return samples
    samples
}

.sits_prune <- function(samples) {
    # Get the time series length for the first sample
    ntimes <- .sits_ntimes(samples)
    # Prune time series according to the first time series length and return
    .sits_foreach_ts(samples, function(ts) {
        if (nrow(ts) >= ntimes) {
            ts[seq_len(ntimes), ]
        } else {
            stop("time series length is smaller than the first sample")
        }
    })
}

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

# ---- Predictors ----

.pred_cols <- c("sample_id", "label")

.predictors <- function(samples, ml_model = NULL) {
    # Prune samples time series
    samples <- .sits_prune(samples)
    # Get samples time series
    pred <- .ts(samples)
    # By default get bands as the same of first sample
    bands <- .sits_bands(samples)
    # Preprocess time series
    if (.has(ml_model)) {
        # If a model is informed, get predictors from model bands
        bands <- .ml_bands(ml_model)
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
                if (.has(q02) && .has(q98)) {
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
        data = pred, names_from = "index", values_from = dplyr::all_of(bands),
        names_prefix = if (length(bands) == 1) bands else "",
        names_sep = ""
    )
    # Return predictors
    pred
}

.pred_features <- function(pred) {
    if (all(.pred_cols %in% names(pred))) {
        pred[, -2:0]
    } else {
        pred
    }
}

`.pred_features<-` <- function(pred, value) {
    if (all(.pred_cols %in% names(pred))) {
        pred[, seq_len(ncol(pred) - 2) + 2] <- value
    } else {
        pred[,] <- value
    }
    pred
}

.pred_references <- function(pred) {
    if (all(.pred_cols %in% names(pred))) .as_chr(pred[["label"]]) else NULL
}

.pred_normalize <- function(pred, stats) {
    values <- as.matrix(.pred_features(pred))
    values <- C_normalize_data(
        data = values, min = .stats_q02(stats), max = .stats_q98(stats)
    )
    .pred_features(pred) <- values
    # Return predictors
    pred
}

.pred_create_partition <- function(pred, partitions) {
    pred[["part_id"]] <- .partitions(x = seq_len(nrow(pred)), n = partitions)
    tidyr::nest(pred, predictors = -"part_id")
}

# ---- Partitions ----

.part_predictors <- function(part) {
    .default(part[["predictors"]][[1]])
}

# ---- expressions ----

.expr_names <- function(expr) {
    if (is.call(expr)) {
        unique(unlist(lapply(as.list(expr)[-1], .expr_names)))
    } else if (is.name(expr)) {
        .as_chr(expr)
    } else {
        character()
    }
}

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

.gdal_params <- function(params) {
    # Check if parameters are named
    if (!all(.has_name(params))) {
        stop("parameters should be named")
    }
    unlist(mapply(function(par, val) {
        if (is.null(val)) {
            NULL
        } else if (is.logical(val)) {
            if (val) par else NULL
        } else if (is.list(val)) {
            c(par, unlist(val))
        } else {
            .as_chr(rbind(par, val))
        }
    }, names(params), unname(params), USE.NAMES = FALSE))
}

.gdal_translate <- function(file, base_file, params, quiet) {
    sf::gdal_utils(
        util = "translate", source = base_file[[1]], destination = file[[1]],
        options = .gdal_params(params), quiet = quiet
    )
}

.gdal_warp <- function(file, base_files, params, quiet) {
    sf::gdal_utils(
        util = "warp", source = base_files, destination = file[[1]],
        options = .gdal_params(params), quiet = quiet
    )
}

.gdal_buildvrt <- function(file, base_files, quiet) {
    sf::gdal_utils(
        util = "buildvrt", source = base_files,
        destination = file, quiet = quiet
    )
}

.gdal_addo <- function(base_file) {
    conf_cog <- .conf("gdal_presets", "cog")
    suppressMessages(
        sf::gdal_addo(
            file = base_file,
            method = conf_cog[["method"]],
            overviews = conf_cog[["overviews"]]
        )
    )
}

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
