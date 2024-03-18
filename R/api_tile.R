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
    cube[1, ]
}
#' @export
.tile.default <- function(cube) {
    cube <- tibble::as_tibble(cube)
    cube <- .cube_find_class(cube)
    tile <- .tile(cube)
    return(tile)
}

#' @title Get source cloud provider for a tile
#' @noRd
#' @param tile A tile.
#' @return Source cloud provider
.tile_source <- function(tile) {
    UseMethod(".tile_source", tile)
}
#' @export
.tile_source.raster_cube <- function(tile) {
    tile <- .tile(tile)
    .as_chr(tile[["source"]])
}
#' @export
.tile_source.default <- function(tile) {
    tile <- tibble::as_tibble(tile)
    tile <- .cube_find_class(tile)
    source <- .tile_source(tile)
    return(source)
}
#' @title Get image collection for a tile
#' @noRd
#' @param tile A tile.
#' @return Image collection
.tile_collection <- function(tile) {
    UseMethod(".tile_collection", tile)
}
#' @export
.tile_collection.raster_cube <- function(tile) {
    tile <- .tile(tile)
    .as_chr(tile[["collection"]])
}
#' @export
.tile_collection.default <- function(tile) {
    tile <- tibble::as_tibble(tile)
    tile <- .cube_find_class(tile)
    collection <- .tile_collection(tile)
    return(collection)
}
#' @title Get/Set tile name
#' @noRd
#' @param tile A tile.
#' @return Name of the tile
.tile_name <- function(tile) {
    UseMethod(".tile_name", tile)
}
#' @export
.tile_name.raster_cube <- function(tile) {
    tile <- .tile(tile)
    .as_chr(tile[["tile"]])
}
#' @export
.tile_name.default <- function(tile) {
    tile <- tibble::as_tibble(tile)
    tile <- .cube_find_class(tile)
    name <- .tile_name(tile)
    return(name)
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
#' @title Get tile number of columns
#' @noRd
#' @param tile A tile.
#' @return Number of columns
.tile_ncols <- function(tile) {
    UseMethod(".tile_ncols", tile)
}
#' @export
.tile_ncols.raster_cube <- function(tile) {
    tile <- .tile(tile)
    .ncols(.fi(tile))
}
#' @export
.tile_ncols.default <- function(tile) {
    tile <- tibble::as_tibble(tile)
    tile <- .cube_find_class(tile)
    ncols <- .tile_ncols(tile)
    return(ncols)
}
#' @title Get tile number of rows
#' @noRd
#' @param tile A tile.
#' @return Number of rows
.tile_nrows <- function(tile) {
    UseMethod(".tile_nrows", tile)
}
#' @export
.tile_nrows.raster_cube <- function(tile) {
    tile <- .tile(tile)
    .nrows(.fi(tile))
}
#' @export
.tile_nrows.default <- function(tile) {
    tile <- tibble::as_tibble(tile)
    tile <- .cube_find_class(tile)
    nrows <- .tile_nrows(tile)
    return(nrows)
}
#' @title Get tile size
#' @noRd
#' @param tile A tile.
#' @return Size (list of nrows x ncols)
.tile_size <- function(tile) {
    UseMethod(".tile_size", tile)
}
#' @export
.tile_size.raster_cube <- function(tile) {
    list(ncols = .tile_ncols(tile), nrows = .tile_nrows(tile))
}
#' @export
.tile_size.default <- function(tile) {
    tile <- tibble::as_tibble(tile)
    tile <- .cube_find_class(tile)
    size <- .tile_size(tile)
    return(size)
}
#' @title Get X resolution
#' @noRd
#' @param tile A tile.
#' @return x resolution
.tile_xres <- function(tile) {
    UseMethod(".tile_xres", tile)
}
#' @export
.tile_xres.raster_cube <- function(tile) {
    tile <- .tile(tile)
    .xres(.fi(tile))
}
#' @export
.tile_xres.default <- function(tile) {
    tile <- tibble::as_tibble(tile)
    tile <- .cube_find_class(tile)
    xres <- .tile_xres(tile)
    return(xres)
}
#' @title Get Y resolution
#' @noRd
#' @param tile A tile.
#' @return y resolution
.tile_yres <- function(tile) {
    UseMethod(".tile_yres", tile)
}
#' @export
.tile_yres.raster_cube <- function(tile) {
    tile <- .tile(tile)
    .yres(.fi(tile))
}
#' @export
.tile_yres.default <- function(tile) {
    tile <- tibble::as_tibble(tile)
    tile <- .cube_find_class(tile)
    yres <- .tile_yres(tile)
    return(yres)
}

#' @title Update tile labels
#' @noRd
#' @param tile   A tile.
#' @param labels A character vector with new labels
#' @return vector of labels
.tile_update_label <- function(tile, labels) {
    UseMethod(".tile_update_label", tile)
}

#' @export
.tile_update_label.class_cube <- function(tile, labels) {
    # Open classified raster
    tile_rast <- .raster_open_rast(.tile_path(tile))
    # Get frequency values
    freq_tbl <- .raster_freq(tile_rast)
    # Get tile labels
    tile_labels <- .tile_labels(tile)
    if (is.null(names(tile_labels))) {
        names(tile_labels) <- seq_along(tile_labels)
    }
    # Get new labels values
    tile_labels <- tile_labels[.as_chr(freq_tbl[["value"]])]
    # Set new labels
    .tile_labels(tile) <- tile_labels
    # Return tile with updated labels
    return(tile)
}

#' @export
.tile_update_label.default <- function(tile, labels) {
    stop("tile is not a classified cube")
}

#' @title Get/Set labels
#' @noRd
#' @param tile A tile.
#' @return vector of labels
.tile_labels <- function(tile) {
    UseMethod(".tile_labels", tile)
}
#' @export
.tile_labels.raster_cube <- function(tile) {
    tile <- .tile(tile)
    tile[["labels"]][[1]]
}
#' @export
.tile_labels.default <- function(tile) {
    tile <- tibble::as_tibble(tile)
    tile <- .cube_find_class(tile)
    labels <- .tile_labels(tile)
    return(labels)
}
#
`.tile_labels<-` <- function(tile, value) {
    UseMethod(".tile_labels<-", tile)
}
#' @export
`.tile_labels<-.raster_cube` <- function(tile, value) {
    tile <- .tile(tile)
    tile[["labels"]] <- list(value)
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
#' @export
.tile_start_date.default <- function(tile) {
    tile <- tibble::as_tibble(tile)
    tile <- .cube_find_class(tile)
    start_date <- .tile_start_date(tile)
    return(start_date)
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
#' @export
.tile_end_date.default <- function(tile) {
    tile <- tibble::as_tibble(tile)
    tile <- .cube_find_class(tile)
    end_date <- .tile_end_date(tile)
    return(end_date)
}
#' @title Get fid from tile
#' @name .tile_fid
#' @keywords internal
#' @noRd
#' @param tile A tile.
#' @return file ID
.tile_fid <- function(tile) {
    UseMethod(".tile_fid", tile)
}
#' @export
.tile_fid.raster_cube <- function(tile) {
    tile <- .tile(tile)
    .fi_fid(.fi(tile))
}
#' @export
.tile_fid.default <- function(tile) {
    tile <- tibble::as_tibble(tile)
    tile <- .cube_find_class(tile)
    fid <- .tile_fid(tile)
    return(fid)
}
#' @title Get unique timeline from file_info.
#' @name .tile_timeline
#' @keywords internal
#' @noRd
#' @param tile A tile.
#' @return a timeline
.tile_timeline <- function(tile) {
    UseMethod(".tile_timeline", tile)
}
#' @export
.tile_timeline.raster_cube <- function(tile) {
    tile <- .tile(tile)
    sort(unique(.fi_timeline(.fi(tile))))
}
#' @export
.tile_timeline.default <- function(tile) {
    tile <- tibble::as_tibble(tile)
    tile <- .cube_find_class(tile)
    timeline <- .tile_timeline(tile)
    return(timeline)
}
#' @title Check if tile is complete
#' @name .tile_is_complete
#' @keywords internal
#' @noRd
#' @param tile A tile.
#' @return TRUE/FALSE
.tile_is_complete <- function(tile) {
    UseMethod(".tile_is_complete", tile)
}
#' @export
.tile_is_complete.raster_cube <- function(tile) {
    tile <- .tile(tile)
    .fi_is_complete(.fi(tile))
}
#' @export
.tile_is_complete.default <- function(tile) {
    tile <- tibble::as_tibble(tile)
    tile <- .cube_find_class(tile)
    is_complete <- .tile_is_complete(tile)
    return(is_complete)
}
#' @title Get path of first asset from file_info.
#' @name .tile_path
#' @keywords internal
#' @noRd
#' @param tile A tile.
#' @param band A band in the tile
#' @param date A date in the tile
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
#' @export
.tile_path.default <- function(tile, band = NULL, date = NULL) {
    tile <- tibble::as_tibble(tile)
    tile <- .cube_find_class(tile)
    path <- .tile_path(tile, band, date)
    return(path)
}
#' @title Get all file paths from file_info.
#' @name .tile_paths
#' @keywords internal
#' @noRd
#' @param tile A tile.
#' @param bands Required bands
#' @return Paths of assets in `file_info` filtered by bands
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
    return(paths)
}
#' @export
.tile_paths.default <- function(tile, bands = NULL) {
    tile <- tibble::as_tibble(tile)
    tile <- .cube_find_class(tile)
    paths <- .tile_paths(tile, bands)
    return(paths)
}
#' @title Get unique satellite name from tile.
#' @name .tile_satellite
#' @keywords internal
#' @noRd
#' @param tile A tile.
#' @return satellite name in the tile
.tile_satellite <- function(tile) {
    UseMethod(".tile_satellite", tile)
}

#' @export
.tile_satellite.raster_cube <- function(tile) {
    tile <- .tile(tile)
    .as_chr(tile[["satellite"]])
}
#' @export
.tile_satellite.default <- function(tile) {
    tile <- tibble::as_tibble(tile)
    tile <- .cube_find_class(tile)
    satellite <- .tile_satellite(tile)
    return(satellite)
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
#' @export
.tile_sensor.default <- function(tile) {
    tile <- tibble::as_tibble(tile)
    tile <- .cube_find_class(tile)
    sensor <- .tile_sensor(tile)
    return(sensor)
}
#' @title Get sorted unique bands from file_info.
#' @name .tile_bands
#' @keywords internal
#' @noRd
#' @param tile A tile.
#' @return names of bands in the tile
.tile_bands <- function(tile, add_cloud = TRUE) {
    UseMethod(".tile_bands", tile)
}
#' @export
.tile_bands.raster_cube <- function(tile, add_cloud = TRUE) {
    tile <- .tile(tile)
    bands <- unique(.fi_bands(.fi(tile)))
    if (add_cloud) {
        return(bands)
    }
    setdiff(bands, .band_cloud())
}
#' @export
.tile_bands.default <- function(tile, add_cloud = TRUE) {
    tile <- tibble::as_tibble(tile)
    tile <- .cube_find_class(tile)
    bands <- .tile_bands(tile, add_cloud)
    return(bands)
}
#' @title Set bands in tile file_info.
#' @rdname .tile_bands
#' @keywords internal
#' @noRd
#' @param tile A tile.
#'
#' @return tile with renamed bands
`.tile_bands<-` <- function(tile, value) {
    UseMethod(".tile_bands<-", tile)
}
#' @export
`.tile_bands<-.raster_cube` <- function(tile, value) {
    tile <- .tile(tile)
    bands <- .tile_bands(tile)
    .check_that(
        length(bands) == length(value),
        local_msg = paste0("bands must have length ", length(bands)),
        msg = "invalid band list"
    )
    rename <- value
    names(rename) <- bands
    .fi(tile) <- .fi_rename_bands(.fi(tile), rename = rename)
    tile
}
#'
#' @title Get a band definition from config.
#' @name .tile_band_conf
#' @keywords internal
#' @noRd
#' @param tile   A tile.
#' @param band   Band character vector.
#'
#' @return band_conf or band_cloud_conf
.tile_band_conf <- function(tile, band) {
    UseMethod(".tile_band_conf", tile)
}
#' @export
.tile_band_conf.eo_cube <- function(tile, band) {
    band_conf <- .conf_eo_band(
        source = .tile_source(tile), collection = .tile_collection(tile),
        band = band[[1]]
    )
    if (.has(band_conf))
        return(band_conf)

    if (band %in% .tile_bands(tile)) {
        band_path <- .tile_path(tile, band)
        rast <- terra::rast(band_path)
        data_type <- terra::datatype(rast)
        band_conf <- .conf("default_values", data_type)
        return(band_conf)
    }
    return(NULL)
}
#' @export
.tile_band_conf.derived_cube <- function(tile, band) {
    .conf_derived_band(
        derived_class = .tile_derived_class(tile), band = band[[1]]
    )
}
#' @export
.tile_band_conf.default <- function(tile, band) {
    tile <- tibble::as_tibble(tile)
    tile <- .cube_find_class(tile)
    band_conf <- .tile_band_conf(tile, band)
    return(band_conf)
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
#' @export
.tile_filter_bands.class_cube <- function(tile, bands) {
    tile <- .tile(tile)
    .fi(tile) <- .fi_filter_bands(fi = .fi(tile), bands = "class")
    tile
}
#' @export
.tile_filter_bands.default <- function(tile, bands) {
    tile <- tibble::as_tibble(tile)
    tile <- .cube_find_class(tile)
    tile <- .tile_filter_bands(tile, bands)
    return(tile)
}
#'
#' @title Get crs from tile
#' @name .tile_crs
#' @keywords internal
#' @noRd
#' @param tile A tile.
#'
#' @return CRS
.tile_crs <- function(tile) {
    UseMethod(".tile_crs", tile)
}
#' @export
.tile_crs.raster_cube <- function(tile) {
    tile <- .tile(tile)
    .crs(tile)
}
#' @export
.tile_crs.default <- function(tile) {
    tile <- tibble::as_tibble(tile)
    tile <- .cube_find_class(tile)
    crs <- .tile_crs(tile)
    return(crs)
}
#' @title Get bbox from tile
#' @name .tile_bbox
#' @keywords internal
#' @noRd
#' @param tile A tile.
#'
#' @return bbox
.tile_bbox <- function(tile, as_crs = NULL) {
    UseMethod(".tile_bbox", tile)
}
#' @export
.tile_bbox.raster_cube <- function(tile, as_crs = NULL) {
    tile <- .tile(tile)
    .bbox(tile, as_crs = as_crs)
}
#' @export
.tile_bbox.default <- function(tile, as_crs = NULL) {
    tile <- tibble::as_tibble(tile)
    tile <- .cube_find_class(tile)
    bbox <- .tile_bbox(tile, as_crs = as_crs)
    return(bbox)
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
#' @export
.tile_as_sf.default <- function(tile, as_crs = NULL) {
    tile <- tibble::as_tibble(tile)
    tile <- .cube_find_class(tile)
    sf_obj <- .tile_as_sf(tile, as_crs = as_crs)
    return(sf_obj)
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
#' @export
.tile_intersects.default <- function(tile, roi) {
    tile <- tibble::as_tibble(tile)
    tile <- .cube_find_class(tile)
    intersects <- .tile_intersects(tile, roi)
    return(intersects)
}
#' @title Is tile inside roi?
#' @name .tile_within
#' @keywords internal
#' @noRd
#' @param tile A tile.
#' @param roi A region of interest (ROI).
#'
#' @return logical
.tile_within <- function(tile, roi) {
    UseMethod(".tile_within", tile)
}
#' @export
.tile_within.raster_cube <- function(tile, roi) {
    .within(.tile_as_sf(tile), .roi_as_sf(roi))
}
#' @export
.tile_within.default <- function(tile, roi) {
    tile <- tibble::as_tibble(tile)
    tile <- .cube_find_class(tile)
    within <- .tile_within(tile, roi)
    return(within)
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
#' @export
.tile_during.default <- function(tile, start_date, end_date) {
    tile <- tibble::as_tibble(tile)
    tile <- .cube_find_class(tile)
    result <- .tile_during(tile, start_date, end_date)
    return(result)
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
#' @export
.tile_filter_interval.default <- function(tile, start_date, end_date) {
    tile <- tibble::as_tibble(tile)
    tile <- .cube_find_class(tile)
    tile <- .tile_filter_interval(tile, start_date, end_date)
    return(tile)
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
    .debug_log(
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
    .debug_log(
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
#' @export
.tile_read_block.default <- function(tile, band, block) {
    tile <- tibble::as_tibble(tile)
    tile <- .cube_find_class(tile)
    tile <- .tile_read_block(tile, band, block)
    return(tile)
}
#'
#' @title Read and preprocess a block of cloud values from
#'        file_info rasters.
#' @name .tile_cloud_read_block
#' @keywords internal
#' @noRd
#' @param tile A tile.
#' @param block A block list with (col, row, ncols, nrows).
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
    .debug_log(
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
    .debug_log(
        event = "end_block_data_process",
        key = "cloud_bit_mask",
        value = is_bit_mask
    )
    # Return values
    return(values)
}
#' @export
.tile_cloud_read_block.default <- function(tile, block) {
    tile <- tibble::as_tibble(tile)
    tile <- .cube_find_class(tile)
    tile <- .tile_cloud_read_block(tile, block)
    return(tile)
}
#' @title Create chunks of a tile to be processed
#' @name .tile_chunks_create
#' @keywords internal
#' @noRd
#' @param tile tile to be processed
#' @param overlap overlap between tiles
#' @param block   Current block
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
#' @title Get tile from file
#' @keywords internal
#' @noRd
#' @param file  Raster file
#' @param base_tile  reference tile used in the operation
#' @param band Spectral band
#' @param update_bbox  should bbox be updated?
#' @param labels Labels for classified cube
#' @return set of values of a band of a tile in a block
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
#' @export
.tile_from_file.default <- function(file, base_tile, band, update_bbox,
                                    labels = NULL) {
    base_tile <- tibble::as_tibble(base_tile)
    base_tile <- .cube_find_class(base_tile)
    base_tile <- .tile_from_file(file, base_tile, band, update_bbox,
                            labels = NULL)
    return(base_tile)
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
#' @param files         Files to be merged
#' @param bands         Bands to be merged
#' @param band_conf     Band confuguration
#' @param base_tile     Reference tile used in the operation
#' @param block_files   Files associated with the the blocks
#' @param multicores    Multicores for processing
#' @param update_bbox   Should bbox be updated?
#' @return an EO tile with merged blocks
.tile_eo_merge_blocks <- function(files, bands, band_conf,
                                  base_tile, block_files,
                                  multicores, update_bbox) {
    base_tile <- .tile(base_tile)
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
                                    labels = NULL, update_bbox = FALSE) {
    if (derived_class %in% c("probs_cube", "variance_cube")) {
        # Open first block file to be merged
        r_obj <- .raster_open_rast(file)
        # Check number of labels is correct
        .check_that(
            x = .raster_nlayers(r_obj) == length(labels),
            local_msg = "number of image layers does not match labels",
            msg = "invalid 'file' parameter"
        )
    }

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

#' @title Create a tile derived from a segment file
#' @name .tile_segments_from_file
#' @keywords internal
#' @noRd
#' @param file  file to be merged
#' @param band  band to be used in the tile
#' @param base_tile  reference tile used in the operation
#' @param vector_class class of the vector tile
#' @param update_bbox  should bbox be updated?
#' @return a new tile
.tile_segments_from_file <- function(file, band, base_tile, vector_class,
                                     labels = NULL, update_bbox = FALSE) {
    v_obj <- .vector_read_vec(file_path = file)
    base_tile <- .tile(base_tile)
    bbox <- .vector_bbox(v_obj)
    if (update_bbox) {
        # Update spatial bbox
        .xmin(base_tile) <- bbox[["xmin"]]
        .xmax(base_tile) <- bbox[["xmax"]]
        .ymin(base_tile) <- bbox[["ymin"]]
        .ymax(base_tile) <- bbox[["ymax"]]
        .crs(base_tile) <- .vector_crs(v_obj, wkt = TRUE)
    }
    # Update labels before file_info
    .tile_labels(base_tile) <- labels
    # Update file_info
    .vi(base_tile) <- .vi_segment_from_file(
        file = file,
        base_tile = base_tile,
        band = band,
        start_date = .tile_start_date(base_tile),
        end_date = .tile_end_date(base_tile)
    )
    # Set tile class and return tile
    seg_classes <- c(.conf_vector_s3class(vector_class), class(base_tile))
    .cube_set_class(base_tile, seg_classes)
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
#' @param update_bbox should bbox be updated?
#' @return a new tile with files written
.tile_derived_merge_blocks <- function(file, band, labels, base_tile,
                                       derived_class, block_files, multicores,
                                       update_bbox = FALSE) {
    if (derived_class %in% c("probs_cube", "variance_cube")) {
        # Open first block file to be merged
        r_obj <- .raster_open_rast(unlist(block_files)[[1]])
        # Check number of labels is correct
        .check_that(
            x = .raster_nlayers(r_obj) == length(labels),
            local_msg = "number of image layers does not match labels",
            msg = "invalid 'file' parameter"
        )
    }
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

#' @title Write values of a derived tile from a set of blocks segments
#' @name .tile_segment_merge_blocks
#' @keywords internal
#' @noRd
#' @param block_files blocks to be merged
#' @param base_tile  reference tile used in the operation
#' @param band  band to be used in the tile
#' @param derived_class class of the derived tile
#' @param out_file output file name
#' @param update_bbox  should bbox be updated?
#' @return a new tile with files written
.tile_segment_merge_blocks <- function(block_files, base_tile, band, vector_class,
                                       out_file, update_bbox = FALSE) {
    base_tile <- .tile(base_tile)
    # Read all blocks file
    vec_segments <- purrr::map_dfr(block_files, .vector_read_vec)
    # Define an unique ID
    vec_segments[["pol_id"]] <- seq_len(nrow(vec_segments))
    # Write all segments
    .vector_write_vec(v_obj = vec_segments, file_path = out_file)
    # Create tile based on template
    tile <- .tile_segments_from_file(
        file = out_file,
        band = band,
        base_tile = base_tile,
        vector_class = vector_class,
        update_bbox = update_bbox
    )
    # If all goes well, delete block files
    unlink(block_files)
    # Return derived tile
    tile
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
#' @export
.tile_area_freq.raster_cube <- function(tile) {
    stop("Cube is not a classified cube")
}
#' @export
.tile_area_freq.default <- function(tile) {
    tile <- tibble::as_tibble(tile)
    tile <- .cube_find_class(tile)
    tile <- .tile_area_freq(tile)
    return(tile)
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
#' @title Given a tile and a band, return a set of values for segments
#' @name .tile_extract_segments
#' @noRd
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Given a data cube, retrieve the time series of XY locations
#'
#' @param tile ... TODO: document
#' @param band ...
#' @param chunk ...
#'
#' @return Data.frame with values per polygon.
.tile_extract_segments <- function(tile, band, chunk) {
    tile <- .tile(tile)
    fi <- .fi(tile)
    fi <- .fi_filter_bands(fi = fi, bands = band)
    files <- .fi_paths(fi)
    # Create a SpatRaster object
    r_obj <- .raster_open_rast(files)
    names(r_obj) <- paste0(band, "-", seq_len(terra::nlyr(r_obj)))
    # Read the segments
    segments <- .vector_read_vec(chunk[["segments"]][[1]])
    # Extract the values
    values <- exactextractr::exact_extract(
        x = r_obj,
        y = segments,
        fun = NULL,
        include_cols = "pol_id"
    )
    values <- dplyr::bind_rows(values)
    values <- dplyr::select(values, -"coverage_fraction")
    # Return values
    return(as.matrix(values))
}
#' @title Check if tile contains cloud band
#' @keywords internal
#' @noRd
#' @param tile input tile
#' @return TRUE/FALSE
.tile_contains_cloud <- function(tile) {
    tile <- .tile(tile)
    .fi_contains_cloud(.fi(tile))
}
#' @title Measure classification time start
#' @name .tile_classif_start
#' @keywords internal
#' @noRd
#' @param tile input tile
#' @param verbose     TRUE/FALSE
#' @return start time for classification
#'
.tile_classif_start <- function(tile, verbose) {
    start_time <- Sys.time()
    if (verbose) {
        message(
            "Starting classification of tile '",
            tile[["tile"]], "' at ", start_time
        )
    }
    return(start_time)
}
#' @title Measure classification time
#' @name .tile_classif_end
#' @keywords internal
#' @noRd
#' @param tile input tile
#' @param start_time  starting time for classification
#' @param verbose     TRUE/FALSE
#'
#' @return end time for classification
#'
.tile_classif_end <- function(tile, start_time, verbose) {
    end_time <- Sys.time()
    if (verbose) {
        message("Tile '", tile[["tile"]], "' finished at ", end_time)
        message(
            "Elapsed time of ",
            format(round(end_time - start_time, digits = 2))
        )
        message("")
    }
    return(invisible(end_time))
}

