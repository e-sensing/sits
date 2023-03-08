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
    cube <- .cube(cube)
    if (nrow(cube) >= 1) return(cube[1, ])
    cube
}

#  Tile field accessors
#  These functions enable access components of tiles. A
#
.tile_source <- function(tile) {
    UseMethod(".tile_source", tile)
}
#' @export
.tile_source.raster_cube <- function(tile) {
    tile <- .tile(tile)
    .as_chr(tile[["source"]])
}
#
.tile_collection <- function(tile) {
    UseMethod(".tile_collection", tile)
}

#' @export
.tile_collection.raster_cube <- function(tile) {
    tile <- .tile(tile)
    .as_chr(tile[["collection"]])
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
    tile <- .tile(tile)
    .as_chr(tile[["tile"]])
}
#
.tile_ncols <- function(tile) {
    UseMethod(".tile_ncols", tile)
}
#' @export
.tile_ncols.raster_cube <- function(tile) {
    tile <- .tile(tile)
    if ("ncols" %in% tile) {
        return(.ncols(tile))
    }
    .ncols(.fi(tile))
}
#
.tile_nrows <- function(tile) {
    UseMethod(".tile_nrows", tile)
}
#' @export
.tile_nrows.raster_cube <- function(tile) {
    tile <- .tile(tile)
    if ("nrows" %in% tile) {
        return(.nrows(tile))
    }
    .nrows(.fi(tile))
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
    tile <- .tile(tile)
    .xres(.fi(tile))
}
#
.tile_yres <- function(tile) {
    UseMethod(".tile_yres", tile)
}
#' @export
.tile_yres.raster_cube <- function(tile) {
    tile <- .tile(tile)
    .yres(.fi(tile))
}
#
.tile_labels <- function(tile) {
    UseMethod(".tile_labels", tile)
}
#' @export
.tile_labels.raster_cube <- function(tile) {
    tile <- .tile(tile)
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
    tile <- .tile(tile)
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
    tile <- .tile(tile)
    .fi_max_date(.fi(tile))
}

#' @title Get fid from tile
#' @name .tile_fid
#' @keywords internal
#' @noRd
#' @param tile A tile.
#'
#' @return date
.tile_fid <- function(tile) {
    UseMethod(".tile_fid", tile)
}
#' @export
.tile_fid.raster_cube <- function(tile) {
    tile <- .tile(tile)
    .fi_fid(.fi(tile))
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
    tile <- .tile(tile)
    sort(unique(.fi_timeline(.fi(tile))))
}
.tile_is_complete <- function(tile) {
    UseMethod(".tile_is_complete", tile)
}
#' @export
.tile_is_complete.raster_cube <- function(tile) {
    tile <- .tile(tile)
    .fi_is_complete(.fi(tile))
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
    tile <- .tile(tile)
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
.tile_paths <- function(tile, bands = NULL) {
    UseMethod(".tile_paths", tile)
}
#' @export
.tile_paths.raster_cube <- function(tile, bands = NULL) {
    tile <- .tile(tile)
    if (.has(bands)) {
        tile <- .tile_filter_bands(tile = tile, bands = bands)
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
.tile_bands <- function(tile, add_cloud = TRUE) {
    UseMethod(".tile_bands", tile)
}
#' @export
.tile_bands.raster_cube <- function(tile, add_cloud = TRUE) {
    tile <- .tile(tile)
    bands <- unique(.fi_bands(.fi(tile)))
    if (add_cloud) return(bands)
    setdiff(bands, .band_cloud())
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
.tile_bbox <- function(tile, as_crs = NULL) {
    UseMethod(".tile_bbox", tile)
}
#' @export
.tile_bbox.raster_cube <- function(tile, as_crs = NULL) {
    tile <- .tile(tile)
    .bbox(tile, as_crs = as_crs)
}
#' @title Convert tile \code{bbox} to a sf polygon object.
#' @noRd
#' @param tile A tile.
#' @return sf object
.tile_as_sf <- function(tile, as_crs = NULL) {
    UseMethod(".tile_as_sf", tile)
}
#' @export
.tile_as_sf.raster_cube <- function(tile, as_crs = NULL) {
    .bbox_as_sf(.tile_bbox(tile), as_crs = as_crs)
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
    .intersects(.tile_as_sf(tile), .roi_as_sf(roi))
}
.tile_within <- function(tile, roi) {
    UseMethod(".tile_within", tile)
}
.tile_within.raster_cube <- function(tile, roi) {
    .within(.tile_as_sf(tile), .roi_as_sf(roi))
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
    tile <- .tile(tile)
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
    tile <- .tile(tile)
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
    tile <- .tile(tile)
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
        image_bbox = .tile_bbox(tile)
    )
}

.tile_from_file <- function(file, base_tile, band, update_bbox, labels = NULL) {
    UseMethod(".tile_from_file", base_tile)
}

#' @export
.tile_from_file.eo_cube <- function(file, base_tile, band, update_bbox,
                                    labels = NULL) {
    .tile_eo_from_files(
        files = file,
        fid = .tile_fid(base_tile),
        bands = band,
        date = .tile_start_date(base_tile),
        base_tile = base_tile,
        update_bbox = update_bbox
    )
}

#' @export
.tile_from_file.derived_cube <- function(file, base_tile, band, update_bbox,
                                         labels = NULL) {
    .tile_derived_from_file(
        file = file,
        band = band,
        base_tile = base_tile,
        derived_class = .tile_derived_class(base_tile),
        labels = labels,
        update_bbox = update_bbox
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
    base_tile <- .tile(base_tile)
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
    base_tile <- .tile(base_tile)
    # Get conf band
    band_conf <- .tile_band_conf(tile = base_tile, band = bands)
    # Create a template raster based on the first image of the tile
    .raster_merge_blocks(
        out_files = files,
        base_file = .tile_path(base_tile),
        block_files = block_files,
        data_type = .data_type(band_conf),
        missing_value = .miss_value(band_conf),
        multicores = multicores
    )
    # Create tile based on template
    tile <- .tile_eo_from_files(
        files = files,
        fid = .fi_fid(.fi(base_tile)),
        bands = bands,
        date = .fi_min_date(.fi(base_tile)),
        base_tile = base_tile,
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
    base_tile <- .tile(base_tile)
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
        file = file,
        band = band,
        start_date = .tile_start_date(base_tile),
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
    base_tile <- .tile(base_tile)
    # Get conf band
    band_conf <- .conf_derived_band(
        derived_class = derived_class,
        band = band
    )
    # Set base tile
    base_file <- if (update_bbox) NULL else .tile_path(base_tile)
    # Create a template raster based on the first image of the tile
    .raster_merge_blocks(
        out_files = file,
        base_file = base_file,
        block_files = block_files,
        data_type = .data_type(band_conf),
        missing_value = .miss_value(band_conf),
        multicores = multicores
    )
    # Create tile based on template
    tile <- .tile_derived_from_file(
        file = file,
        band = band,
        base_tile = base_tile,
        derived_class = derived_class,
        labels = labels,
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
        file = file,
        band = band,
        base_tile = base_tile,
        derived_class = "probs_cube",
        labels = labels,
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
        file = file,
        band = band,
        labels = labels,
        base_tile = base_tile,
        derived_class = "probs_cube",
        block_files = block_files,
        multicores = multicores,
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
#' @param update_bbox  should bbox be updated?
#' @return a new probs tile
.tile_class_from_file <- function(file, band, base_tile, update_bbox = FALSE) {
    .tile_derived_from_file(
        file = file,
        band = band,
        base_tile = base_tile,
        derived_class = "class_cube",
        labels = .tile_labels(base_tile),
        update_bbox = update_bbox
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
        file = file,
        band = band,
        labels = labels,
        base_tile = base_tile,
        derived_class = "class_cube",
        block_files = block_files,
        multicores = multicores,
        update_bbox = FALSE
    )
}

#' @title Create an "uncertainity" tile
#' @name .tile_uncert_from_file
#' @keywords internal
#' @noRd
#' @param file file to be written
#' @param band  band to be used in the tile
#' @param base_tile  reference tile used in the operation (probs)
#' @param update_bbox  should bbox be updated?
#' @return a new uncertainty tile
.tile_uncert_from_file <- function(file, band, base_tile, update_bbox = FALSE) {
    .tile_derived_from_file(
        file = file,
        band = band,
        base_tile = base_tile,
        derived_class = "uncertainty_cube",
        labels = .tile_labels(base_tile),
        update_bbox = update_bbox
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
        file = file,
        band = band,
        labels = labels,
        base_tile = base_tile,
        derived_class = "uncertainty_cube",
        block_files = block_files,
        multicores = multicores,
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
    r_obj <- .raster_open_rast(.tile_paths(tile = tile, bands = band))
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
    tile <- .tile(tile)
    .fi_contains_cloud(.fi(tile))
}
