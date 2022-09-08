#---- bbox api ----
.bbox_fields <- c("xmin", "xmax", "ymin", "ymax", "crs")

.bbox_fields_nocrs <- c("xmin", "xmax", "ymin", "ymax")

.bbox_type <- function(bbox) {
    if (all(.bbox_fields %in% names(bbox)))
        "with_crs"
    else if (all(.bbox_fields_nocrs %in% names(bbox)))
        "without_crs"
    else
        stop("object does not have a valid bbox")
}

.bbox_switch <- function(bbox, ...) {
    switch(.bbox_type(bbox), ...)
}

.bbox <- function(bbox, crs = NULL) {
    .bbox_switch(
        bbox,
        with_crs = {
            if (!is.null(crs)) {
                warning("object has crs, ignoring provided 'crs' parameter")
            }
            c(bbox[.bbox_fields])
        },
        without_crs = {
            if (is.null(crs)) {
                warning("object has no crs, assuming 'EPSG:4326'")
                crs <- 4326
            }
            c(bbox[.bbox_fields_nocrs], crs = crs)
        }
    )
}

.bbox_xmin <- function(bbox) {
    bbox[["xmin"]]
}

.bbox_xmax <- function(bbox) {
    bbox[["xmax"]]
}

.bbox_ymin <- function(bbox) {
    bbox[["ymin"]]
}

.bbox_ymax <- function(bbox) {
    bbox[["ymax"]]
}

.bbox_crs <- function(bbox) {
    bbox[["crs"]]
}

.bbox_as_sf <- function(bbox, as_crs = NULL) {
    if (length(unique(bbox[["crs"]])) > 1 && is.null(as_crs)) {
        warning("object has multiples crs values, transforming to 'EPSG:4326'")
        as_crs <- 4326
    }
    purrr::pmap_dfr(as.list(bbox), function(xmin, xmax, ymin, ymax, crs, ...) {
        geom <- sf::st_sf(
            geometry = sf::st_sfc(
                sf::st_polygon(list(
                    rbind(c(xmin, ymax), c(xmax, ymax), c(xmax, ymin),
                          c(xmin, ymin), c(xmin, ymax))
                ))
            ),
            crs = crs
        )
        if (!is.null(as_crs)) {
            geom <- sf::st_transform(geom, crs = as_crs)
        }
        geom
    })
}


# s2_cube <- sits_cube(
#     source = "AWS",
#     collection = "SENTINEL-S2-L2A-COGS",
#     tiles = c("20LKP", "20LLP", "20LNQ", "21LTH"),
#     bands = c("B08", "B11"),
#     start_date = "2018-07-12",
#     end_date = "2019-07-28"
# )
# .bbox(s2_cube)
# .bbox_as_sf(s2_cube[1:3,])
# .bbox_as_sf(s2_cube)
# .bbox_as_sf(s2_cube, as_crs = 4326)

#---- roi api ----
.roi_type <- function(roi) {
    if (all(c("lon_min", "lon_max", "lat_min", "lat_max") %in% names(roi)))
        "ll"
    else if (inherits(roi, c("sf", "sfc")))
        "sf"
    else if (all(c("xmin", "xmax", "ymin", "ymax") %in% names(roi)))
        "xy"
    else
        stop("invalid 'roi' parameter")
}

.roi_switch <- function(roi, ...) {
    switch(.roi_type(roi), ...)
}

.roi_xy_as_sf <- function(roi) {
    .bbox_as_sf(roi)
}

.roi_ll_as_sf <- function(roi) {
    .bbox_as_sf(c(
        xmin = roi[["lon_min"]],
        xmax = roi[["lon_max"]],
        ymin = roi[["lat_min"]],
        ymax = roi[["lat_max"]],
        crs = 4326)
    )
}

.roi_as_sf <- function(roi) {
    .roi_switch(
        roi,
        "sf" = roi,
        "xy" = .roi_xy_as_sf(roi),
        "ll" = .roi_ll_as_sf(roi)
    )
}

# .roi_as_sf(c(lon_min = 1, lon_max = 2, lat_min = 3, lat_max = 4))
# .roi_as_sf(c(xmin = 1, xmax = 2, ymin = 3, ymax = 4))
# .roi_as_sf(c(xmin = 1, xmax = 2, ymin = 3, ymax = 4, crs = 4674))

#---- utils api ----

.intersects <- function(x, y) {
    as_crs <- sf::st_crs(x)
    y <- sf::st_transform(y, crs = as_crs)
    apply(sf::st_intersects(x, y, sparse = FALSE), 1, any)
}

.try <- function(expr, ..., rollback = NULL, default = NULL, msg = NULL) {
    has_default <- !missing(default)
    tryCatch(expr, ..., error = function(e) {
        if (!is.null(rollback)) rollback
        if (has_default) return(default)
        stop(if (!is.null(msg)) msg else e$message)
    })
}

.date <- function(x) {
    lubridate::as_date(unlist(x))
}

.between <- function(x, min, max) {
    min <= x & x <= max
}

.period_check <- function(period) {
    if (!grepl("^P[0-9]+(D|M|Y)$", period))
        stop("invalid period format")
}
.period_val <- function(period) {
    .period_check(period)
    as.numeric(gsub("^P([0-9]+)(D|M|Y)", "\\1", period))
}

.period_unit <- function(period) {
    .period_check(period)
    unit <- c(D = "day", M = "month", Y = "year")
    unit[[gsub("^P[0-9]+(D|M|Y)$", "\\1", period)]]
}

.bands <- function(..., cloud = FALSE) {
    if (cloud) return(c(...))
    setdiff(c(...), .band_cloud())
}

.set_class <- function(x, ...) {
    class(x) <- unique(c(...))
    x
}

.set_cube_class <- function(x, ...) {
    .set_class(x, ..., c("sits_cube", "tbl_df", "tbl", "data.frame"))
}

#---- config ----

.conf_exists <- function(...) {
    key <- c(...)
    !is.null(.try(sits_env[["config"]][[key]], default = NULL))
}

.conf <- function(...) {
    key <- c(...)
    if (!.conf_exists(key))
        stop("key '", paste(key, collapse = "->"), "' not found in config")
    sits_env[["config"]][[c(key)]]
}

# eo_cube

.conf_eo_band_exists <- function(source, collection, band) {
    .conf_exists("sources", source, "collections", collection, "bands", band)
}

.conf_eo_band <- function(source, collection, band) {
    if (!.conf_eo_band_exists(source, collection, band))
        return(.conf("default_values", "eo_cube"))
    .conf("sources", source, "collections", collection, "bands", band)
}

# derived_cube

.conf_derived_band <- function(derived_class, band) {
    .conf("derived_cube", derived_class, "bands", band)
}

.band_data_type <- function(conf) {
    conf[["data_type"]][[1]]
}

.band_miss_value <- function(conf) {
    conf[["missing_value"]][[1]]
}

.band_min_value <- function(conf) {
    conf[["minimum_value"]][[1]]
}

.band_max_value <- function(conf) {
    conf[["maximum_value"]][[1]]
}

.band_scale <- function(conf) {
    conf[["scale_factor"]][[1]]
}

.band_offset <- function(conf) {
    conf[["offset_value"]][[1]]
}

.band_cloud_interp_values <- function(conf) {
    conf[["interp_values"]]
}

.band_cloud_bit_mask <- function(conf) {
    conf[["bit_mask"]][[1]]
}

.band_cloud <- function() {
    "CLOUD"
}

# probs_cube

.conf_probs_class <- function() {
    .conf("derived_cube", "probs_cube", "s3_class")
}

.conf_probs_band_data_type <- function() {
    .conf("derived_cube", "probs_cube", "bands", "probs", "data_type")
}

.conf_probs_band_scale <- function() {
    .conf("derived_cube", "probs_cube", "bands", "probs", "scale_factor")
}

# .conf_exists("sources", "BDC", "collections", "MOD13Q1-6")
# .conf_exists("sources", "BDC", "collections", "MOD13Q1-7")
# .conf_eo_band("BDC", "MOD13Q1-6", "NIR", "missing_value")
# .conf_eo_band("BDC", "MOD13Q1-6", "NBR", "missing_value")
# .conf_derived_cube("probs_cube", "bands")
# .conf_derived_cube("probs_cube", "NBR")
# .conf_derived_cube_band("probs_cube", "probs", "missing_value")
# .conf_derived_cube_band("probs_cube", "bayes", "missing_value")


#---- fi api ----

.fi_type <- function(fi) {
    if ("date" %in% names(fi))
        "eo"
    else if (all(c("start_date", "end_date") %in% names(fi)))
        "derived"
    else
        stop("invalid file info")
}

.fi_switch <- function(fi, ...) {
    switch(.fi_type(fi), ...)
}

.fi <- function(x) {
    x[["file_info"]][[1]]
}

.fi_eo <- function(fid, band, date, ncols, nrows, xres, yres, xmin,
                   xmax, ymin, ymax, path, miss_value, min_value,
                   max_value, scale, offset, source, collection) {
    # Get band defaults for eo cube from config
    conf_band <- .conf_eo_band(source = source, collection = collection,
                               band = band)
    # Create a new earth observation file_info
    tibble::tibble(
        fid = fid, band = band, date = date,
        ncols = ncols, nrows = nrows, xres = xres,
        yres = yres, xmin = xmin, xmax = xmax,
        ymin = ymin, ymax = ymax, path = path,
        miss_value = .band_miss_value(conf_band),
        min_value  = .band_min_value(conf_band),
        max_value = .band_max_value(conf_band),
        scale = .band_scale(conf_band),
        offset = .band_offset(conf_band)
    )

}

.fi_derived <- function(band, start_date, end_date, ncols, nrows,
                        xres, yres, xmin, xmax, ymin, ymax, crs, path) {
    # Create a new derived file_info
    tibble::tibble(
        band = band, start_date = start_date,
        end_date = end_date, ncols = ncols,
        nrows = nrows, xres = xres, yres = yres,
        xmin = xmin, xmax = xmax, ymin = ymin,
        ymax = ymax, crs = crs, path = path
    )
}

.fi_derived_from_file <- function(file, band, derived_class, start_date,
                                  end_date) {
    file <- path.expand(file)
    r_obj <- .raster_open_rast(file)
    .fi_derived(
        band = band, start_date = start_date,
        end_date = end_date, ncols = .raster_ncols(r_obj),
        nrows = .raster_nrows(r_obj), xres = .raster_xres(r_obj),
        yres = .raster_yres(r_obj), xmin = .raster_xmin(r_obj),
        xmax = .raster_xmax(r_obj), ymin = .raster_ymin(r_obj),
        ymax = .raster_ymax(r_obj), crs = .raster_crs(r_obj),
        path = file
    )
}

.fi_fid <- function(fi) {
    fi[["fid"]]
}

.fi_bands <- function(fi) {
    sort(unique(fi[["band"]]))
}

.fi_min_date <- function(fi) {
    .fi_switch(
        fi = fi,
        eo = min(.date(fi[["date"]])),
        derived = min(.date(fi[["start_date"]]))
    )
}

.fi_max_date <- function(fi) {
    .fi_switch(
        fi = fi,
        eo = max(.date(fi[["date"]])),
        derived = max(.date(fi[["end_date"]]))
    )
}

.fi_timeline <- function(fi) {
    .fi_switch(
        fi = fi,
        eo = unique(.date(fi[["date"]])),
        derived = unique(.date(fi[["start_date"]]))
    )
}

.fi_xres <- function(fi) {
    fi[["xres"]]
}

.fi_yres <- function(fi) {
    fi[["yres"]]
}

.fi_nrows <- function(fi) {
    fi[["nrows"]]
}

.fi_ncols <- function(fi) {
    fi[["ncols"]]
}

.fi_paths <- function(fi) {
    fi[["path"]]
}

.fi_path <- function(fi) {
    fi[["path"]][[1]]
}

.fi_during <- function(fi, start_date, end_date) {
    fi_start <- .fi_min_date(fi)
    fi_end <- .fi_max_date(fi)
    any(.between(fi_start, start_date, end_date),
        .between(fi_end, start_date, end_date),
        .between(start_date, fi_start, fi_end),
        .between(end_date, fi_start, fi_end))
}

.fi_temporal_filter <- function(fi, start_date, end_date) {
    if (is.null(start_date)) {
        start_date <- .fi_min_date(fi)
    }
    if (is.null(end_date)) {
        end_date <- .fi_max_date(fi)
    }
    dplyr::filter(fi, !!start_date <= .data[["date"]],
                  .data[["date"]] <= !!end_date)
}

.fi_band_filter <- function(fi, band) {
    fi <- dplyr::filter(fi, band %in% !!band)
}

.fi_read_block <- function(fi, band, block) {
    fi <- .fi_band_filter(fi = fi, band = band)
    files <- .fi_paths(fi)
    if (length(files) == 0) return(NULL)
    # Read values from files (pixels in rows, bands in cols)
    .raster_read_stack(
        files = files,
        block = block
    )
}

#---- tile api (generic) ----

.tile_source <- function(tile) {
    UseMethod(".tile_source", tile)
}

.tile_collection <- function(tile) {
    UseMethod(".tile_collection", tile)
}

.tile_name <- function(tile) {
    UseMethod(".tile_name", tile)
}

.tile_xmin <- function(tile) {
    UseMethod(".tile_xmin", tile)
}

.tile_xmax <- function(tile) {
    UseMethod(".tile_xmax", tile)
}

.tile_ymin <- function(tile) {
    UseMethod(".tile_ymin", tile)
}

.tile_ymax <- function(tile) {
    UseMethod(".tile_ymax", tile)
}

.tile_crs <- function(tile) {
    UseMethod(".tile_crs", tile)
}

.tile_ncols <- function(tile) {
    UseMethod(".tile_ncols", tile)
}

.tile_nrows <- function(tile) {
    UseMethod(".tile_nrows", tile)
}

.tile_labels <- function(tile) {
    UseMethod(".tile_labels", tile)
}

.tile_bands <- function(tile) {
    UseMethod(".tile_bands", tile)
}

.tile_cloud_interp_values <- function(tile) {
    UseMethod(".tile_cloud_interp_values", tile)
}

.tile_cloud_bit_mask <- function(tile) {
    UseMethod(".tile_cloud_bit_mask", tile)
}

.tile_derived_class <- function(tile) {
    UseMethod(".tile_derived_class", tile)
}

.tile_as_sf <- function(tile) {
    UseMethod(".tile_as_sf", tile)
}

.tile_intersects <- function(tile, roi) {
    UseMethod(".tile_intersects", tile)
}

.tile_during <- function(tile, start_date, end_date) {
    UseMethod(".tile_during", tile)
}

.tile_start_date <- function(tile) {
    UseMethod(".tile_start_date", tile)
}

.tile_end_date <- function(tile) {
    UseMethod(".tile_end_date", tile)
}

.tile_timeline <- function(tile) {
    UseMethod(".tile_timeline", tile)
}

.tile_file_info <- function(tile) {
    UseMethod(".tile_file_info", tile)
}

`.tile_file_info<-` <- function(tile, value) {
    UseMethod(".tile_file_info<-", tile)
}

.tile_read_block <- function(tile, bands, block, impute_fn, filter_fn,
                             ml_model) {
    UseMethod(".tile_read_block", tile)
}

#---- tile api (raster_cube) ----

#' @export
.tile_source.raster_cube <- function(tile) {
    tile[["source"]][[1]]
}

#' @export
.tile_collection.raster_cube <- function(tile) {
    tile[["collection"]][[1]]
}

#' @export
.tile_name.raster_cube <- function(tile) {
    tile[["tile"]][[1]]
}

#' @export
.tile_xmin.raster_cube <- function(tile) {
    .bbox_xmin(tile)[[1]]
}

#' @export
.tile_xmax.raster_cube <- function(tile) {
    .bbox_xmax(tile)[[1]]
}

#' @export
.tile_ymin.raster_cube <- function(tile) {
    .bbox_ymin(tile)[[1]]
}

#' @export
.tile_ymax.raster_cube <- function(tile) {
    .bbox_ymax(tile)[[1]]
}

#' @export
.tile_crs.raster_cube <- function(tile) {
    .bbox_crs(tile)[[1]]
}

#' @export
.tile_ncols.raster_cube <- function(tile) {
    if ("ncols" %in% tile) return(tile[["ncols"]][[1]])
    .fi_ncols(.fi(tile))[[1]]
}

#' @export
.tile_nrows.raster_cube <- function(tile) {
    if ("nrows" %in% tile) return(tile[["nrows"]][[1]])
    .fi_nrows(.fi(tile))[[1]]
}

#' @export
.tile_labels.raster_cube <- function(tile) {
    tile[["labels"]][[1]]
}

#' @export
.tile_bands.raster_cube <- function(tile) {
    .fi_bands(.fi(tile))
}

#' @export
.tile_as_sf.raster_cube <- function(tile) {
    .bbox_as_sf(tile)
}

#' @export
.tile_intersects.raster_cube <- function(tile, roi) {
    .intersects(.tile_as_sf(tile), .roi_as_sf(roi))
}

#' @export
.tile_during.raster_cube <- function(tile, start_date, end_date) {
    .fi_during(
        fi = .fi(tile),
        start_date = start_date,
        end_date = end_date
    )
}

#' @export
.tile_start_date.raster_cube <- function(tile) {
    fi <- .fi(tile)
    .fi_min_date(fi)
}

#' @export
.tile_end_date.raster_cube <- function(tile) {
    fi <- .fi(tile)
    .fi_max_date(fi)
}

#' @export
.tile_timeline.raster_cube <- function(tile) {
    fi <- .fi(tile)
    .fi_timeline(fi)
}

#' @export
.tile_file_info.raster_cube <- function(tile) {
    .fi(tile)
}

#' @export
`.tile_file_info<-.raster_cube` <- function(tile, value) {
    tile[["file_info"]][[1]] <- value
    tile
}

#---- tile api (eo_cube) ----

#' @export
.tile_cloud_interp_values.eo_cube <- function(tile) {
    conf_band <- .conf_eo_band(
        source = .tile_source(tile),
        collection = .tile_collection(tile),
        band = .band_cloud())
    .band_cloud_interp_values(conf_band)
}

#' @export
.tile_cloud_bit_mask.eo_cube <- function(tile) {
    conf_band <- .conf_eo_band(
        source = .tile_source(tile),
        collection = .tile_collection(tile),
        band = .band_cloud())
    .band_cloud_bit_mask(conf_band)
}

# .tile_cloud_interp_values(s2_cube[1,])
# .tile_cloud_bit_mask(s2_cube[2,])
# .tile_band_miss_value(s2_cube[3,], "B08")
# .tile_band_min_value(s2_cube[4,], "B8A")
# .tile_band_max_value(s2_cube[1,], "B04")
# .tile_band_scale(s2_cube[2,], "B11")
# .tile_band_offset(s2_cube[3,], "B03")
# .tile_band_miss_value(s2_cube[3,], "NBR")
# .tile_band_min_value(s2_cube[4,], "NBR")
# .tile_band_max_value(s2_cube[1,], "NBR")
# .tile_band_scale(s2_cube[2,], "NBR")
# .tile_band_offset(s2_cube[3,], "NBR")



#---- tile api (derived_cube) ----

#' @export
.tile_derived_class.derived_cube <- function(tile) {
    class(tile)[[1]]
}

#---- tile creators ----

.tile_derived_from_file <- function(file, band, base_tile, derived_class,
                                    labels = NULL) {
    # Open raster
    r_obj <- .raster_open_rast(file)
    # Check number of labels
    if (!is.null(labels)) {
        .check_that(
            x = .raster_nlayers(r_obj) == length(labels),
            local_msg = "number of file layers does not match labels",
            msg = "invalid 'file' and 'labels' parameter"
        )
    }
    # Create file_info based on file
    .tile_file_info(base_tile) <- .fi_derived_from_file(
        file = file, band = band, derived_class = derived_class,
        start_date = .tile_start_date(base_tile),
        end_date = .tile_end_date(base_tile)
    )
    # If there are labels, include them before file_info
    base_tile[["labels"]] <- NULL
    if (!is.null(labels)) {
        base_tile <- tibble::add_column(base_tile, labels = list(labels),
                                        .before = "file_info")
    }
    # Set tile class
    .set_cube_class(base_tile, derived_class)
}

# ---- tile probs api ----

.tile_probs_from_file <- function(file, band, base_tile, labels) {
    .tile_derived_from_file(
        file = file,
        band = band,
        base_tile = base_tile,
        derived_class = .conf_probs_class(),
        labels = labels
    )
}

.tile_probs_merge_blocks <- function(file, band, labels, base_tile,
                                     block_files, multicores) {
    # Get data type
    data_type <- .conf_probs_band_data_type()
    # Create a template raster based on the first image of the tile
    .raster_template(
        base_file = .fi_path(.fi(base_tile)),
        out_file = file, data_type = data_type,
        nlayers = max(1, length(labels))
    )
    # Create tile based on template
    tile <- .tile_probs_from_file(
        file = file, band = band, base_tile = base_tile, labels = labels
    )
    # Merge blocks into template file
    .try(
        .raster_merge(
            files = block_files, out_file = file,
            data_type = data_type, multicores = multicores,
            overwrite = FALSE),
        rollback = unlink(file)
    )
    # If all goes well, delete block files
    unlink(block_files)
    tile
}

#---- cube api (generics) ----

.cube_foreach_tile <- function(cube, fn, ...) {
    UseMethod(".cube_foreach_tile", cube)
}

.cube_intersects <- function(cube, roi) {
    UseMethod(".cube_intersects", cube)
}

.cube_spatial_filter <- function(cube, roi) {
    UseMethod(".cube_spatial_filter", cube)
}

.cube_during <- function(cube, start_date, end_date) {
    UseMethod(".cube_during", cube)
}

.cube_start_date <- function(cube, innermost) {
    UseMethod(".cube_start_date", cube)
}

.cube_end_date <- function(cube, innermost) {
    UseMethod(".cube_end_date", cube)
}

.cube_timeline <- function(cube, period, origin) {
    UseMethod(".cube_timeline", cube)
}

.cube_temporal_filter <- function(cube, start_date, end_date) {
    UseMethod(".cube_temporal_filter", cube)
}

#---- cube api (raster_cube) ----

#' @export
.cube_foreach_tile.raster_cube <- function(cube, fn, ...) {
    slider::slide_dfr(cube, fn, ...)
}

#' @export
.cube_intersects.raster_cube <- function(cube, roi) {
    slider::slide_lgl(cube, .tile_intersects, roi = .roi_as_sf(roi))
}

#' @export
.cube_spatial_filter.raster_cube <- function(cube, roi) {
    cube[.cube_intersects(cube, roi), ]
}

#' @export
.cube_during.raster_cube <- function(cube, start_date, end_date) {
    slider::slide_lgl(cube, .tile_during, start_date = start_date,
                      end_date = end_date)
}

#' @export
.cube_start_date.raster_cube <- function(cube, innermost = TRUE) {
    dates <- .date(slider::slide(cube, .tile_start_date))
    if (innermost) return(max(dates))
    min(dates)
}

#' @export
.cube_end_date.raster_cube <- function(cube, innermost = TRUE) {
    dates <- .date(slider::slide(cube, .tile_end_date))
    if (innermost) return(min(dates))
    max(dates)
}

#' @export
.cube_temporal_filter.raster_cube <- function(cube, start_date, end_date) {
    cube[.cube_during(cube, start_date, end_date), ]
}

#' @export
.cube_timeline.raster_cube <- function(cube, period = "P1D",
                                       origin = .cube_start_date(cube, innermost = FALSE)) {
    values <- slider::slide_dfr(cube, function(tile) {
        tibble::tibble(tile = tile[["tile"]],
                       timeline = .tile_timeline(!!tile))
    })
    values <- dplyr::filter(values, !!origin <= .data[["timeline"]])
    values <- dplyr::arrange(values, .data[["timeline"]])
    values <- slider::slide_period_dfr(
        values,
        values[["timeline"]],
        .period_unit(period),
        function(x) {
            x[["start_date"]] <- min(x[["timeline"]])
            x[["end_date"]] <- max(x[["timeline"]])
            dplyr::count(x, .data[["start_date"]], .data[["end_date"]],
                         .data[["tile"]])
        },
        .every = .period_val(period),
        .origin = origin,
        .complete = TRUE
    )
    tidyr::pivot_wider(values, id_cols = c("start_date", "end_date"),
                       names_from = "tile", values_from = "n")
}

# s2_cube <- sits_cube(
#     source = "AWS",
#     collection = "SENTINEL-S2-L2A-COGS",
#     tiles = c("20LKP", "20LLP", "20LNQ", "21LTH"),
#     bands = c("B08", "B11"),
#     start_date = "2018-07-12",
#     end_date = "2019-07-28"
# )
# .cube_intersects(s2_cube, s2_cube[2:3,])
# .cube_intersects(s2_cube, s2_cube[4,])
# .cube_spatial_filter(s2_cube, .bbox_as_sf(s2_cube[3:4,]))
# .cube_spatial_filter(s2_cube, .bbox_as_sf(s2_cube[3:4,], as_crs = 4326))
# .cube_start_date(s2_cube)
# .cube_end_date(s2_cube)
# .cube_temporal_filter(s2_cube, start_date = "2017-01-01", end_date = "2018-07-12")
# .cube_temporal_filter(s2_cube, start_date = "2017-01-01", end_date = "2018-07-14")
# .cube_temporal_filter(s2_cube, start_date = "2019-07-25", end_date = "2020-07-28")
# .cube_temporal_filter(s2_cube, start_date = "2019-07-28", end_date = "2020-07-28")


#---- ml_model ----

.ml_model_stats <- function(ml_model) {
    environment(ml_model)[["stats"]]
}

.ml_model_stats_quant2_band <- function(ml_model, band) {
    stats <- .ml_model_stats(ml_model)
    if (is.null(stats)) return(NULL)
    stats[["quant_2"]][[band]]
}

.ml_model_stats_quant98_band <- function(ml_model, band) {
    stats <- .ml_model_stats(ml_model)
    if (is.null(stats)) return(NULL)
    stats[["quant_98"]][[band]]
}

.ml_model_samples <- function(ml_model) {
    environment(ml_model)[["samples"]]
}

.ml_model_attr_names <- function(ml_model) {
    sample <- .ml_model_samples(ml_model)[1, ]
    names(.sits_distances(sample))[-2:0]
}

.ml_model_bands <- function(ml_model) {
    .samples_bands(.ml_model_samples(ml_model))
}

.ml_model_labels <- function(ml_model) {
    .samples_labels(.ml_model_samples(ml_model))
}

.ml_norm_param <- function(data) {
    ts <- dplyr::select(dplyr::bind_rows(data$time_series), -.data[["Index"]])
    med <- dplyr::summarise(ts, dplyr::across(
        .fns = stats::median, na.rm = TRUE
    ))
    quant_2 <- dplyr::summarise(ts, dplyr::across(
        .fns = stats::quantile, probs = 0.02, na.rm = TRUE
    ))
    quant_98 <- dplyr::summarise(ts, dplyr::across(
        .fns = stats::quantile, probs = 0.98, na.rm = TRUE
    ))
    stats <- list(med = med, quant_2 = quant_2, quant_98 = quant_98)

    return(stats)
}

#---- samples ----
.samples_bands <- function(samples) {
    setdiff(names(samples[["time_series"]][[1]]), "Index")
}

.samples_labels <- function(samples) {
    sort(unique(samples[["label"]]))
}

.samples_data <- function(samples, band) {
    samples <- dplyr::transmute(
        samples,
        id = seq_len(dplyr::n()),
        time_series = .data[["time_series"]])
    samples <- tidyr::unnest(
        samples, "time_series")
    samples <- dplyr::group_by(
        samples, .data[["id"]])
    samples <- dplyr::mutate(
        samples, Index = seq_len(dplyr::n()))
    samples <- dplyr::ungroup(samples)
    samples <- dplyr::select(
        samples, .data[["id"]], .data[["Index"]], !!band)
    samples <- tidyr::pivot_wider(
        samples,
        id_cols = "id",
        names_from = "Index",
        values_from = !!band,
        names_prefix = paste0(band, "."),
        names_sep = "_"
    )
    dplyr::select(samples, -.data[["id"]])
}

.samples_normalize <- function(samples, stats) {
    bands <- .samples_bands(samples)

    data_norm <- .apply_across(samples, fn = function(b, stats) {
        band <- dplyr::cur_column()
        quant_2 <- stats[["quant_2"]][[band]]
        quant_98 <- stats[["quant_98"]][[band]]
        c(normalize_data(as.matrix(b), quant_2, quant_98))
    }, stats = stats)
}
