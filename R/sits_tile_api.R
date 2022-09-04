.roi_as_sf <- function(roi) {
    xy_names <- c("xmin", "xmax", "ymin", "ymax")
    ll_names <- c("lon_min", "lon_max", "lat_min", "lat_max")

    if (inherits(roi, "sf")) {
        roi
    } else if (all(xy_names %in% names(roi))) {
        .bbox_as_sf(
            xmin = roi[["xmin"]],
            xmax = roi[["xmax"]],
            ymin = roi[["ymin"]],
            ymax = roi[["ymax"]],
            crs = if (is.null(roi[["crs"]])) 4326 else roi[["crs"]]
        )
    } else if (all(ll_names %in% names(roi))) {
        .bbox_as_sf(
            xmin = roi[["lon_min"]],
            xmax = roi[["lon_max"]],
            ymin = roi[["lat_min"]],
            ymax = roi[["lat_max"]],
            crs = 4326
        )
    } else {
        .check_that(
            is.null(roi),
            local_msg = "invalid ROI definition",
            msg = "invalid 'roi' parameter"
        )
    }
}

.bbox_as_sf <- function(xmin, xmax, ymin, ymax, crs) {
    geom <- sf::st_sfc(sf::st_polygon(list(
        rbind(c(xmin, ymax), c(xmax, ymax),
              c(xmax, ymin), c(xmin, ymin),
              c(xmin, ymax)))
    ))
    sf_obj <- sf::st_sf(
        geometry = geom,
        crs = crs
    )
    return(sf_obj)
}


#
# cube -> cube
#
.cube_spatial_filter <- function(cube, roi) {
    slider::slide_dfr(
        cube,
        .tile_intersects,
        roi = roi
    )
}

.cube_temporal_filter <- function(cube, start_date, end_date) {
    slider::slide_dfr(
        cube,
        .tile_temporal_filter,
        start_date = start_date,
        end_date = end_date
    )
}

#
# tile -> tile
#
.tile_intersects <- function(tile, roi) {
    # Reproject roi to tile CRS
    roi <- sf::st_transform(
        x = roi,
        crs = tile[["crs"]]
    )
    # Compute intersecting jobs
    any(c(sf::st_intersects(
        x = .tile_as_sf(tile),
        y = roi,
        sparse = FALSE)
    ))
}

.tile_as_sf <- function(tile) {
    .bbox_as_sf(
        xmin = tile[["xmin"]],
        xmax = tile[["xmax"]],
        ymin = tile[["ymin"]],
        ymax = tile[["ymax"]],
        crs =  tile[["crs"]]
    )
}

.tile_temporal_filter <- function(tile, start_date, end_date) {
    .fi(tile) <- .fi_temporal_filter(
        .fi(tile),
        start_date = start_date,
        end_date = end_date
    )
}

.tile_start_date <- function(tile) {
    fi <- .fi(tile)
    min(.fi_timeline(fi))
}

.tile_end_date <- function(tile) {
    fi <- .fi(tile)
    max(.fi_timeline(fi))
}

.tile_create_probs <- function(tile, labels, out_file, version) {
    band_name <- .config_get("probs_cube_band")
    start_date <- .tile_start_date(tile)
    end_date  <- .tile_end_date(tile)

    .tile_create(

    )

    # Create a template raster based on the first image of the tile
    r_obj <- .raster_template(
        file = .file_info_path(tile),
        out_file = out_file,
        data_type = .config_get("probs_cube_data_type"),
        nlayers = length(labels),
        missing_value = .config_get("probs_cube_missing_value")
    )

    # Set the file information
    file_info <- tibble::tibble(
        band = band_name,
        start_date = start_date,
        end_date = end_date,
        xmin = .raster_xmin(r_obj),
        xmax = .raster_xmax(r_obj),
        ymin = .raster_ymin(r_obj),
        ymax = .raster_ymax(r_obj),
        xres = .raster_xres(r_obj),
        yres = .raster_yres(r_obj),
        nrows = .raster_nrows(r_obj),
        ncols = .raster_ncols(r_obj),
        path = out_file
    )

    # Set the metadata for the probability cube
    tile <- .tile_create(
        source = tile[["source"]],
        collection = tile[["collection"]],
        satellite = tile[["satellite"]],
        sensor = tile[["sensor"]],
        tile = tile[["tile"]],
        xmin = .raster_xmin(r_obj),
        xmax = .raster_xmax(r_obj),
        ymin = .raster_ymin(r_obj),
        ymax = .raster_ymax(r_obj),
        crs = .raster_crs(r_obj),
        labels = labels,
        file_info = file_info
    )

    class_tile <- .config_get("probs_cube_class")
    class(tile) <- unique(c(class_tile, class(tile)))

    return(tile)
}

.tile_create <- function(tile,
                         tile_class,
                         labels = NULL,
                         band_name,
                         data_type,
                         missing_value,
                         out_file) {
    start_date <- .tile_start_date(tile)
    end_date  <- .tile_end_date(tile)

    tile[["file_info"]][[1]] <- .fi_create(
        tile = tile,
        band_name = band_name,
        start_date = start_date,
        end_date = end_date,
        nlayers = if (is.null(labels)) 1 else length(labels),
        data_type = data_type,
        missing_value = missing_value,
        out_file = out_file
    )
    # if there are labels, include them
    if (!is.null(labels)) {
        tile <- tibble::add_column(
            tile,
            labels = list(labels),
            .before = "file_info"
        )
    }

    class(tile) <- tile_class

    return(tile)
}

.fi_create <- function(tile,
                       band_name,
                       start_date,
                       end_date,
                       nlayers,
                       data_type,
                       missing_value,
                       out_file) {
    # Create a template raster based on the first image of the tile
    r_obj <- .raster_template(
        file = .file_info_path(tile),
        out_file = out_file,
        data_type = data_type,
        nlayers = nlayers,
        missing_value = missing_value
    )
    # Set the file information
    tibble::tibble(
        band = band_name,
        start_date = start_date,
        end_date = end_date,
        xmin = .raster_xmin(r_obj),
        xmax = .raster_xmax(r_obj),
        ymin = .raster_ymin(r_obj),
        ymax = .raster_ymax(r_obj),
        xres = .raster_xres(r_obj),
        yres = .raster_yres(r_obj),
        nrows = .raster_nrows(r_obj),
        ncols = .raster_ncols(r_obj),
        path = out_file
    )
}

#
# fi -> fi
#

.fi <- function(tile) {
    tile[["file_info"]][[1]]
}

`.fi<-` <- function(tile, fi) {
    tile[["file_info"]][[1]] <- fi
    tile
}

.fi_timeline <- function(fi) {
    unique(as.Date(fi[["date"]]))
}

.fi_temporal_filter <- function(fi, start_date, end_date) {
    if (fi[["band"]] %in% .config_get("sits_results_bands")) {
        class(fi) <- unique(c("class_cube", class(fi)))
    } else {
        class(fi) <- unique(c("eo_cube", class(fi)))
    }
    UseMethod(".fi_temporal_filter", fi)
}

.fi_temporal_filter.eo_cube <- function(fi, start_date, end_date) {
    if (is.null(start_date)) {
        start_date <- min(fi[["date"]])
    }
    if (is.null(end_date)) {
        end_date <- max(fi[["date"]])
    }

    dplyr::filter(
        fi,
        !!start_date <= .data[["date"]],
        .data[["date"]] <= !!end_date
    )
}

.fi_temporal_filter.class_cube <- function(fi, start_date, end_date) {
    if (is.null(start_date)) {
        start_date <- min(fi[["start_date"]])
    }
    if (is.null(end_date)) {
        end_date <- max(fi[["end_date"]])
    }

    dplyr::filter(
        fi,
        !!start_date <= .data[["start_date"]],
        .data[["start_date"]] <= !!end_date,
        !!start_date <= .data[["end_date"]],
        .data[["end_date"]] <= !!end_date,
    )
}
