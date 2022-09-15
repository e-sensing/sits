
#---- sits api (generic) ----

.as_date <- function(x) {
    lubridate::as_date(unlist(x))
}

.try <- function(expr, ..., .rollback = NULL, .default = NULL,
                 .msg_error = NULL) {
    has_default <- !missing(.default)
    tryCatch(expr, ..., error = function(e) {
        if (!is.null(.rollback)) .rollback
        if (has_default) return(.default)
        stop(if (!is.null(.msg_error)) .msg_error else e$message)
    })
}

.intersects <- function(x, y) {
    as_crs <- sf::st_crs(x)
    y <- sf::st_transform(y, crs = as_crs)
    apply(sf::st_intersects(x, y, sparse = FALSE), 1, any)
}

.between <- function(x, min, max) {
    min <= x & x <= max
}

.set_class <- function(x, ...) {
    class(x) <- unique(c(...))
    x
}

.compact <- function(x) {
    value <- unique(x)
    if (length(value) != 1) return(x)
    value
}

.xmin <- function(x) {
    as.numeric(.compact(x[["xmin"]]))
}

`.xmin<-` <- function(x, value) {
    x[["xmin"]] <- value
    x
}

.xmax <- function(x) {
    as.numeric(.compact(x[["xmax"]]))
}

`.xmax<-` <- function(x, value) {
    x[["xmax"]] <- value
    x
}

.ymin <- function(x) {
    as.numeric(.compact(x[["ymin"]]))
}

`.ymin<-` <- function(x, value) {
    x[["ymin"]] <- value
    x
}

.ymax <- function(x) {
    as.numeric(.compact(x[["ymax"]]))
}

`.ymax<-` <- function(x, value) {
    x[["ymax"]] <- value
    x
}

.crs <- function(x) {
    crs <- .compact(x[["crs"]])
    if (!is.numeric(crs)) return(as.character(crs))
    paste0("EPSG:", crs)
}

`.crs<-` <- function(x, value) {
    x[["crs"]] <- value
    x
}

.col <- function(x) {
    as.integer(.compact(x[["col"]]))
}

.row <- function(x) {
    as.integer(.compact(x[["row"]]))
}

.ncols <- function(x) {
    as.integer(.compact(x[["ncols"]]))
}

.nrows <- function(x) {
    as.integer(.compact(x[["nrows"]]))
}

.xres <- function(x) {
    (.xmax(x) - .xmin(x)) / .ncols(x)
}

.yres <- function(x) {
    (.ymax(x) - .ymin(x)) / .nrows(x)
}

.block_cols <- c("col", "row", "ncols", "nrows")

.block <- function(x) {
    if (!all(.block_cols %in% names(x)))
        return(NULL)
    as.list(x[.block_cols])
}

#---- bbox api ----

.bbox_cols <- c("xmin", "xmax", "ymin", "ymax")

.bbox <- function(x, ..., default_crs = NULL) {
    if (!all(.bbox_cols %in% names(x)))
        return(NULL)
    if ("crs" %in% names(x))
        return(as.list(x[c(.bbox_cols, "crs")]))
    if (is.null(default_crs)) {
        warning("object has no crs, assuming 'EPSG:4326'")
        default_crs <- "EPSG:4326"
    }
    as.list(c(x[.bbox_cols], list(crs = default_crs)))
}

.bbox_as_sf <- function(bbox, ..., default_crs = NULL, as_crs = NULL) {
    bbox <- .bbox(bbox, default_crs = default_crs)
    if (!all(c(.bbox_cols, "crs") %in% names(bbox)))
        stop("object does not have a valid bbox")
    if (length(.crs(bbox)) > 1 && is.null(as_crs)) {
        warning("object has multiples crs values, reprojecting to EPSG:4326\n",
                "(use 'as_crs' to reproject to a different crs value)")
        as_crs <- "EPSG:4326"
    }
    # Convert to sf object
    purrr::pmap_dfr(as.list(bbox), function(xmin, xmax, ymin, ymax, crs, ...) {
        geom <- sf::st_sf(
            geometry = sf::st_sfc(sf::st_polygon(list(
                rbind(c(xmin, ymax), c(xmax, ymax), c(xmax, ymin),
                      c(xmin, ymin), c(xmin, ymax))
            ))),
            crs = crs
        )
        if (!is.null(as_crs))
            geom <- sf::st_transform(geom, crs = as_crs)
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
# .bbox_as_sf(s2_cube[1:2,])
# .bbox_as_sf(s2_cube)
# .bbox_as_sf(s2_cube, as_crs = 4326)
# .bbox(c(xmin = 1, xmax = 2, ymin = 3, ymax = 4))
# .bbox(c(xmin = 1, xmax = 2, ymin = 3, ymax = 4, crs = 4326))

#---- roi api ----

.roi_lonlat_cols <- c("lon_min", "lon_max", "lat_min", "lat_max")

.roi_type <- function(roi) {
    if (inherits(roi, c("sf", "sfc")))
        "sf"
    else if (all(.bbox_cols %in% names(roi)))
        "bbox"
    else if (all(.roi_lonlat_cols %in% names(roi)))
        "lonlat"
    else
        stop("invalid 'roi' parameter")
}

.roi_switch <- function(roi, ...) {
    switch(.roi_type(roi), ...)
}

.roi_as_sf <- function(roi, default_crs = NULL) {
    .roi_switch(
        roi,
        sf = roi,
        bbox = .bbox_as_sf(roi, default_crs = default_crs),
        lonlat = .bbox_as_sf(list(
            xmin = roi[["lon_min"]],
            xmax = roi[["lon_max"]],
            ymin = roi[["lat_min"]],
            ymax = roi[["lat_max"]],
            crs = "EPSG:4326"
        ))
    )
}

# .roi_as_sf(c(lon_min = 1, lon_max = 2, lat_min = 3, lat_max = 4))
# .roi_as_sf(c(xmin = 1, xmax = 2, ymin = 3, ymax = 4))
# .roi_as_sf(c(xmin = 1, xmax = 2, ymin = 3, ymax = 4), default_crs = 4674)
# .roi_as_sf(c(xmin = 1, xmax = 2, ymin = 3, ymax = 4, crs = 4674))


#---- period api ----

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

#---- config ----

.conf_exists <- function(...) {
    key <- c(...)
    !is.null(.try(sits_env[["config"]][[key]], .default = NULL))
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

.conf_derived_s3class <- function(derived_class) {
    .conf("derived_cube", derived_class, "s3_class")
}

.conf_derived_band <- function(derived_class, band) {
    .conf("derived_cube", derived_class, "bands", band)
}

.band_data_type <- function(conf) {
    as.character(conf[["data_type"]][[1]])
}

.band_miss_value <- function(conf) {
    as.numeric(conf[["missing_value"]][[1]])
}

.band_min_value <- function(conf) {
    as.numeric(conf[["minimum_value"]][[1]])
}

.band_max_value <- function(conf) {
    as.numeric(conf[["maximum_value"]][[1]])
}

.band_scale <- function(conf) {
    as.numeric(conf[["scale_factor"]][[1]])
}

.band_offset <- function(conf) {
    as.numeric(conf[["offset_value"]][[1]])
}

.band_cloud_interp_values <- function(conf) {
    as.integer(conf[["interp_values"]])
}

.band_cloud_bit_mask <- function(conf) {
    as.integer(conf[["bit_mask"]][[1]])
}

.band_cloud <- function() {
    "CLOUD"
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
        "eo_cube"
    else if (all(c("start_date", "end_date") %in% names(fi)))
        "derived_cube"
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
                   xmax, ymin, ymax, path) {
    # Create a new earth observation file_info
    tibble::tibble(
        fid = fid, band = band, date = date,
        ncols = ncols, nrows = nrows, xres = xres,
        yres = yres, xmin = xmin, xmax = xmax,
        ymin = ymin, ymax = ymax, path = path
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

.fi_derived_from_file <- function(file, band, start_date, end_date) {
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
    as.character(fi[["fid"]])
}

.fi_bands <- function(fi) {
    sort(unique(fi[["band"]]))
}

.fi_min_date <- function(fi) {
    .fi_switch(
        fi = fi,
        eo_cube = min(.as_date(fi[["date"]])),
        derived_cube = min(.as_date(fi[["start_date"]]))
    )
}

.fi_max_date <- function(fi) {
    .fi_switch(
        fi = fi,
        eo_cube = max(.as_date(fi[["date"]])),
        derived_cube = max(.as_date(fi[["end_date"]]))
    )
}

.fi_timeline <- function(fi) {
    .fi_switch(
        fi = fi,
        eo_cube = .as_date(fi[["date"]]),
        derived_cube = .as_date(c(fi[["start_date"]], fi[["end_date"]]))
    )
}

.fi_paths <- function(fi) {
    as.character(fi[["path"]])
}

.fi_path <- function(fi) {
    as.character(fi[["path"]][[1]])
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
    if (is.null(start_date)) start_date <- .fi_min_date(fi)
    if (is.null(end_date)) end_date <- .fi_max_date(fi)
    dplyr::filter(fi, !!start_date <= .data[["date"]],
                  .data[["date"]] <= !!end_date)
}

.fi_band_filter <- function(fi, band) {
    dplyr::filter(fi, band %in% !!band)
}

.fi_read_block <- function(fi, band, block) {
    fi <- .fi_band_filter(fi = fi, band = band)
    files <- .fi_paths(fi)
    if (length(files) == 0) return(NULL)
    # Read values from files (pixels in rows, bands in cols)
    .raster_read_rast(files = files, block = block)
}

.fi_foreach_image <- function(fi, fn, ...) {
    #
}

#---- tile api (generic) ----

.tile <- function(cube) {
    UseMethod(".tile", cube)
}

.tile_source <- function(tile) {
    UseMethod(".tile_source", tile)
}

.tile_collection <- function(tile) {
    UseMethod(".tile_collection", tile)
}

.tile_name <- function(tile) {
    UseMethod(".tile_name", tile)
}

.tile_labels <- function(tile) {
    UseMethod(".tile_labels", tile)
}

`.tile_labels<-` <- function(tile, value) {
    UseMethod(".tile_labels<-", tile)
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

.tile_band_conf <- function(tile, band) {
    UseMethod(".tile_band_conf", tile)
}

.tile_ncols <- function(tile) {
    UseMethod(".tile_ncols", tile)
}

.tile_nrows <- function(tile) {
    UseMethod(".tile_nrows", tile)
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
.tile.raster_cube <- function(cube) {
    cube[1,]
}

#' @export
.tile_source.raster_cube <- function(tile) {
    as.character(tile[["source"]][[1]])
}

#' @export
.tile_collection.raster_cube <- function(tile) {
    as.character(tile[["collection"]][[1]])
}

#' @export
.tile_name.raster_cube <- function(tile) {
    as.character(tile[["tile"]][[1]])
}

#' @export
.tile_labels.raster_cube <- function(tile) {
    as.character(tile[["labels"]][[1]])
}

#' @export
`.tile_labels<-.raster_cube` <- function(tile, value) {
    tile[["labels"]] <- NULL
    tile[["labels"]] <- list(value)
    tile
}

#' @export
.tile_bands.raster_cube <- function(tile) {
    .fi_bands(.fi(tile))
}

#' @export
.tile_ncols.raster_cube <- function(tile) {
    if ("ncols" %in% tile) return(.ncols(tile)[[1]])
    unique(.ncols(.fi(tile)))
}

#' @export
.tile_nrows.raster_cube <- function(tile) {
    if ("nrows" %in% tile) return(.nrows(tile)[[1]])
    unique(.nrows(.fi(tile)))
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
    .fi_min_date(.fi(tile))
}

#' @export
.tile_end_date.raster_cube <- function(tile) {
    .fi_max_date(.fi(tile))
}

#' @export
.tile_timeline.raster_cube <- function(tile) {
    .fi_timeline(.fi(tile))
}

#' @export
.tile_file_info.raster_cube <- function(tile) {
    .fi(tile)
}

#' @export
`.tile_file_info<-.raster_cube` <- function(tile, value) {
    tile[["file_info"]] <- NULL
    tile[["file_info"]] <- list(value)
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

#' @export
.tile_band_conf.eo_cube <- function(tile, band) {
    .conf_eo_band(
        source = .tile_source(tile),
        collection = .tile_collection(tile),
        band = band
    )
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
.tile_band_conf.derived_cube <- function(tile, band) {
    .conf_derived_band(
        derived_class = .tile_derived_class(tile),
        band = band
    )
}

#' @export
.tile_derived_class.derived_cube <- function(tile) {
    class(tile)[[1]]
}

#---- tile constructors ----

.tile_derived_from_file <- function(file, band, base_tile, derived_class,
                                    labels = NULL) {
    # Open raster
    r_obj <- .raster_open_rast(file)
    # Update spatial bbox
    .xmin(base_tile) <- .raster_xmin(r_obj)
    .xmax(base_tile) <- .raster_xmax(r_obj)
    .ymin(base_tile) <- .raster_ymin(r_obj)
    .ymax(base_tile) <- .raster_ymax(r_obj)
    .crs(base_tile) <- .raster_crs(r_obj)
    # Update labels before file_info
    .tile_labels(base_tile) <- labels
    # Update file_info
    .tile_file_info(base_tile) <- .fi_derived_from_file(
        file = file, band = band, start_date = .tile_start_date(base_tile),
        end_date = .tile_end_date(base_tile)
    )
    # Set tile class
    .cube_set_class(base_tile, .conf_derived_s3class(derived_class))
}

# ---- tile probs api ----

.tile_probs_from_file <- function(file, band, base_tile, labels) {
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
        labels = labels
    )
}

.tile_probs_merge_blocks <- function(file, band, labels, base_tile,
                                     block_files, multicores) {
    # Get conf band
    conf_band <- .conf_derived_band(derived_class = "probs_cube", band = band)
    # Get data type
    data_type <- .band_data_type(conf_band)
    # Create a template raster based on the first image of the tile
    .raster_merge_blocks(
        base_file = .fi_path(.fi(base_tile)),
        block_files = block_files,
        out_file = file,
        data_type = data_type,
        missing_value = .band_miss_value(conf_band),
        multicores = multicores
    )
    # Create tile based on template
    tile <- .tile_probs_from_file(
        file = file, band = band,
        base_tile = base_tile, labels = labels
    )
    # If all goes well, delete block files
    unlink(block_files)
    tile
}

# ---- tile class api ----

.tile_class_from_file <- function(file, band, derived_class, base_tile) {
    .tile_derived_from_file(
        file = file,
        band = band,
        base_tile = base_tile,
        derived_class = derived_class,
        labels = .tile_labels(base_tile)
    )
}

.tile_class_merge_blocks <- function(file, band, derived_class, labels,
                                     base_tile, block_files, multicores) {
    # Get band conf
    conf_band <- .conf_derived_band(derived_class = derived_class, band = band)
    # Get data type
    data_type <- .band_data_type(conf_band)
    # Create a template raster based on the first image of the tile
    .raster_merge_blocks(
        base_file = .fi_path(.fi(base_tile)),
        block_files = block_files,
        out_file = file,
        data_type = data_type,
        missing_value = .band_miss_value(conf_band),
        multicores = multicores
    )
    # Create tile based on template
    tile <- .tile_class_from_file(
        file = file, band = band, derived_class = derived_class,
        base_tile = base_tile
    )
    # If all goes well, delete block files
    unlink(block_files)
    tile
}


# ---- tile smooth api ----

#---- cube api (utils) ----

.cube_set_class <- function(x, ...) {
    .set_class(x, ..., c("sits_cube", "tbl_df", "tbl", "data.frame"))
}

#---- cube api (generics) ----
.cube_select <- function(cube, bands) {
    UseMethod(".cube_select", cube)
}

.cube_select.raster_cube <- function(cube, bands) {
    .cube_foreach_tile(cube, function(tile) {
        # default bands
        if (is.null(bands)) {
            bands <- .cube_bands(tile)
        }

        # pre-condition - check bands
        .check_cube_bands(tile, bands = bands)

        db_info <- .fi_band_filter(.fi(tile), band = bands)
        tile$file_info[[1]] <- db_info
        return(tile)
    })
}

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
    dates <- .as_date(slider::slide(cube, .tile_start_date))
    if (innermost) return(max(dates))
    min(dates)
}

#' @export
.cube_end_date.raster_cube <- function(cube, innermost = TRUE) {
    dates <- .as_date(slider::slide(cube, .tile_end_date))
    if (innermost) return(min(dates))
    max(dates)
}

#' @export
.cube_temporal_filter.raster_cube <- function(cube, start_date, end_date) {
    cube[.cube_during(cube, start_date, end_date), ]
}

#' @export
.cube_timeline.raster_cube <- function(cube, period = "P1D", origin = NULL) {
    if (is.null(origin)) {
        origin <- .cube_start_date(cube, innermost = FALSE)
    }
    values <- slider::slide_dfr(cube, function(tile) {
        tibble::tibble(tile = tile[["tile"]], dates = .tile_timeline(!!tile))
    })
    values <- dplyr::filter(values, !!origin <= .data[["dates"]])
    values <- dplyr::arrange(values, .data[["dates"]])
    values <- slider::slide_period_dfr(
        values, values[["dates"]], .period_unit(period),
        function(x) {
            x[["from_date"]] <- min(x[["dates"]])
            x[["to_date"]] <- max(x[["dates"]])
            dplyr::count(
                x, .data[["from_date"]], .data[["to_date"]], .data[["tile"]]
            )
        },
        .every = .period_val(period), .origin = origin, .complete = TRUE
    )
    id_cols <- c("from_date", "to_date")
    if (all(values[["from_date"]] == values[["to_date"]])) {
        values[["date"]] <- values[["from_date"]]
        id_cols <- "date"
    }
    tidyr::pivot_wider(
        values, id_cols = id_cols, names_from = "tile", values_from = "n"
    )
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


.ml_stats_create <- function(samples) {
    ts <- dplyr::bind_rows(samples[["time_series"]])
    # Remove Index column
    ts <- dplyr::select(ts, -.data[["Index"]])
    # Compute median
    med <- dplyr::summarise(ts, dplyr::across(
        dplyr::everything(), stats::median, na.rm = TRUE
    ))
    # Compute quantile 0.02
    q02 <- dplyr::summarise(ts, dplyr::across(
        dplyr::everything(), stats::quantile, probs = 0.02, na.rm = TRUE
    ))
    # Compute quantile 0.98
    q98 <- dplyr::summarise(ts, dplyr::across(
        dplyr::everything(), stats::quantile, probs = 0.98, na.rm = TRUE
    ))
    # Return stats object
    dplyr::bind_rows(med, q02, q98)
}

.ml_stats <- function(ml_model) {
    environment(ml_model)[["stats"]]
}

.ml_samples <- function(ml_model) {
    environment(ml_model)[["samples"]]
}

.ml_class <- function(ml_model) {
    class(ml_model)[[1]]
}

.ml_attr_names <- function(ml_model) {
    # Get attr names from variable used in training
    names(environment(ml_model)[["train_samples"]])[-2:0]
}

.ml_bands <- function(ml_model) {
    .sits_bands(.ml_samples(ml_model))
}

.ml_labels <- function(ml_model) {
    .sits_labels(.ml_samples(ml_model))
}

.ml_foreach_band <- function(ml_model, fn, ...) {
    purrr::map(.ml_bands(ml_model), fn, ...)
}

# ---- stats ----
.stats_q02_band <- function(stats, band) {
    quantile_02 <- 2
    stats[[band]][[quantile_02]]
}

.stats_q98_band <- function(stats, band) {
    quantile_98 <- 3
    stats[[band]][[quantile_98]]
}

#---- sits (samples) ----

.sits_bands <- function(samples) {
    setdiff(names(samples[["time_series"]][[1]]), "Index")
}

.sits_select <- function(samples, bands) {
    .sits_fast_apply(samples, col = "time_series", function(x) {
        dplyr::select(x, dplyr::all_of(c("#..", "Index", bands)))
    })
}

.sits_labels <- function(samples) {
    sort(unique(samples[["label"]]))
}

.distances <- function(samples, bands, label_col = TRUE) {
    label <- NULL
    if (label_col) {
        samples <- dplyr::mutate(samples, reference = .data[["label"]])
        label <- "reference"
    }
    samples <- samples[c(label, "time_series")]
    samples[["original_row"]] <- seq_len(nrow(samples))
    samples <- tidyr::unnest(samples, "time_series")
    samples <- samples[c("original_row", label, bands)]
    samples <- dplyr::group_by(samples, .data[["original_row"]])
    samples <- dplyr::mutate(samples, index = seq_len(dplyr::n()))
    samples <- dplyr::ungroup(samples)
    # Arrange data: samples x bands/index
    samples <- tidyr::pivot_wider(
        samples, names_from = "index", values_from = bands,
        names_prefix = ifelse(length(bands) == 1, bands, ""),
        names_sep = ""
    )
    # Remove 'id' column
    return(samples)
}

.sits_normalize <- function(samples, stats) {
    .apply_across(samples, function(b) {
        band <- dplyr::cur_column()
        quant_02 <- .stats_q02_band(stats, band)
        quant_98 <- .stats_q98_band(stats, band)
        c(normalize_data(as.matrix(b), quant_02, quant_98))
    })
}

.sits_get_chunk_ts <- function(samples, nchunks) {
    ngroup <- ceiling(nrow(samples) / nchunks)
    group_id <- rep(seq_len(nchunks), each = ngroup)[seq_len(nrow(samples))]

    samples[["group_id"]] <- group_id
    dplyr::group_split(
        dplyr::group_by(samples, .data[["group_id"]]),
        .keep = FALSE
    )
}
