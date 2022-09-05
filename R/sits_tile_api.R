#---- roi api ----

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

#---- bbox api ----
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

#---- cube api ----

.cube_spatial_filter <- function(cube, roi) {
    intersects <- slider::slide_lgl(
        cube,
        .tile_intersects,
        roi = roi
    )
    cube[intersects,]
}

.cube_temporal_filter <- function(cube, start_date, end_date) {
    intersects <- slider::slide_lgl(
        cube,
        .tile_during,
        start_date = start_date,
        end_date = end_date
    )
    cube[intersects,]
}


#---- tile api ----

.tile_cloud_interp_values <- function(tile) {
    # Get value from config
    value <- .config_get(key = c(
        "sources", tile[["source"]],
        "collections", tile[["collection"]],
        "bands", .source_cloud(),
        "interp_values"
    ))
    # Post-condition
    .check_num(
        x = value,
        msg = "invalid 'interp_values' in config file"
    )
    return(value)
}

.tile_cloud_bit_mask <- function(tile) {
    # Get value from config
    value <- .config_get(key = c(
        "sources", tile[["source"]],
        "collections", tile[["collection"]],
        "bands", .source_cloud(),
        "bit_mask"
    ))
    # Post-condition
    .check_num(
        x = value,
        msg = "invalid 'bit_mask' in config file"
    )
    return(value)
}

.tile_band_missing_value <- function(tile, band) {
    # Get value from config
    value <- .config_get(
        key = c(
            "sources", tile[["source"]],
            "collections", tile[["collection"]],
            "bands", band[[1]], "missing_value"
        ),
        default = .config_get("raster_cube_missing_value")
    )
    # Post-condition
    .check_num(
        x = value,
        len_min = 1, len_max = 1,
        msg = "invalid 'missing_value' in config file"
    )
    return(value)
}

.tile_band_minimum_value <- function(tile, band) {
    # Get value from config
    value <- .config_get(
        key = c(
            "sources", tile[["source"]],
            "collections", tile[["collection"]],
            "bands", band[[1]], "minimum_value"
        ),
        default = .config_get("raster_cube_minimum_value")
    )
    # Post-condition
    .check_num(
        x = value,
        len_min = 1, len_max = 1,
        msg = "invalid 'minimum_value' in config file"
    )
    return(value)
}

.tile_band_maximum_value <- function(tile, band) {
    # Get value from config
    value <- .config_get(
        key = c(
            "sources", tile[["source"]],
            "collections", tile[["collection"]],
            "bands", band[[1]], "maximum_value"
        ),
        default = .config_get("raster_cube_maximum_value")
    )
    # Post-condition
    .check_num(
        x = value,
        len_min = 1, len_max = 1,
        msg = "invalid 'maximum_value' in config file"
    )
    return(value)
}


.tile_band_scale_factor <- function(tile, band) {
    # Get value from config
    value <- .config_get(
        key = c(
            "sources", tile[["source"]],
            "collections", tile[["collection"]],
            "bands", band[[1]], "scale_factor"
        ),
        default = .config_get("raster_cube_scale_factor")
    )
    # Post-condition
    .check_num(
        x = value,
        exclusive_min = 0,
        len_min = 1, len_max = 1,
        msg = "invalid 'scale_factor' in config file"
    )
    return(value)
}


.tile_band_offset_value <- function(tile, band) {
    # Get value from config
    value <- .config_get(
        key = c(
            "sources", tile[["source"]],
            "collections", tile[["collection"]],
            "bands", band[[1]], "offset_value"
        ),
        default = .config_get("raster_cube_offset_value")
    )
    # Post-condition
    .check_num(
        x = value,
        len_min = 1, len_max = 1,
        msg = "invalid 'offset_value' in config file"
    )
    return(value)
}

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

.tile_during <- function(tile, start_date, end_date) {
    .fi_during(
        fi = .tile_file_info(tile),
        start_date = start_date,
        end_date = end_date
    )
}

.tile_start_date <- function(tile) {
    fi <- .tile_file_info(tile)
    min(.fi_timeline(fi))
}

.tile_end_date <- function(tile) {
    fi <- .tile_file_info(tile)
    max(.fi_timeline(fi))
}

.tile_open_probs <- function(tile, labels, file) {
    # Open tile
    .tile_open(
        tile = tile,
        tile_class = .config_get("probs_cube_class"),
        labels = labels,
        band_name = .config_get("probs_cube_band"),
        file = file
    )
}

.tile_create_probs <- function(tile,
                               labels,
                               out_file,
                               block_files,
                               multicores) {
    # Create tile and template
    probs_tile <- .tile_create(
        tile = tile,
        tile_class = .config_get("probs_cube_class"),
        labels = labels,
        band_name = .config_get("probs_cube_band"),
        data_type = .config_get("probs_cube_data_type"),
        missing_value = .config_get("probs_cube_missing_value"),
        out_file = out_file
    )
    # Merge blocks
    tryCatch({
        .raster_merge(
            files = block_files,
            out_file = out_file,
            format = "GTiff",
            data_type = .config_get("probs_cube_data_type"),
            multicores = multicores,
            overwrite = FALSE
        )
    }, error = function(e) {
        unlink(out_file)
        stop(e$message, call. = FALSE)
    })
    # If everything goes well, delete block files
    unlink(block_files)
    return(probs_tile)
}

.tile_open <- function(tile,
                       tile_class,
                       labels = NULL,
                       band_name,
                       file) {
    # Create file_info and template
    tile[["file_info"]][[1]] <- .fi_open(
        tile = tile,
        band_name = band_name,
        start_date = .tile_start_date(tile),
        end_date = .tile_end_date(tile),
        labels = labels,
        file = file
    )
    # If there are labels, include them
    if (!is.null(labels)) {
        tile <- tibble::add_column(
            tile,
            labels = list(labels),
            .before = "file_info"
        )
    }
    # Set tile class
    class(tile) <- tile_class
    return(tile)
}

.tile_create <- function(tile,
                         tile_class,
                         labels = NULL,
                         band_name,
                         data_type,
                         missing_value,
                         out_file) {
    # Create file_info and template
    tile[["file_info"]][[1]] <- .fi_create(
        tile = tile,
        band_name = band_name,
        start_date = .tile_start_date(tile),
        end_date = .tile_end_date(tile),
        nlayers = if (is.null(labels)) 1 else length(labels),
        data_type = data_type,
        missing_value = missing_value,
        out_file = out_file
    )
    # If there are labels, include them
    if (!is.null(labels)) {
        tile <- tibble::add_column(
            tile,
            labels = list(labels),
            .before = "file_info"
        )
    }
    # Set tile class
    class(tile) <- tile_class
    return(tile)
}

.tile_file_info <- function(tile) {
    tile[["file_info"]][[1]]
}

`.tile_file_info<-` <- function(tile, value) {
    tile[["file_info"]][[1]] <- value
    tile
}

.tile_read_block <- function(tile,
                             bands,
                             block,
                             impute_fn,
                             filter_fn,
                             ml_model) {
    # Get file_info
    fi <- .tile_file_info(tile)
    # Get cloud values
    cloud_mask <- .fi_read_block(
        fi = fi,
        band = .source_cloud(),
        block = block
    )
    # Prepare cloud_mask
    if (!is.null(cloud_mask)) {
        cloud_mask <- .values_cloud_mask(
            values = cloud_mask,
            interp_values = .tile_cloud_interp_values(tile),
            is_bit_mask = .tile_cloud_bit_mask(tile)
        )
    }
    # Prepare bands
    bands <- setdiff(bands, .source_cloud())
    names(bands) <- bands
    # Get file_info
    fi <- .tile_file_info(tile)
    # Get bands values
    values_bands <- purrr::map(bands, function(band) {
        values <- .fi_read_block(fi = fi, band = band, block = block)
        # Retrieve the normalization stats from the model
        stats_quant_02 <- .ml_model_stats_quant2_band(
            ml_model = ml_model,
            band = band
        )
        stats_quant_98 <- .ml_model_stats_quant98_band(
            ml_model = ml_model,
            band = band
        )
        # Preprocess values
        .values_preprocess_bands(
            values = values,
            cloud_mask = cloud_mask,
            missing_value = .tile_band_missing_value(tile = tile, band = band),
            minimum_value = .tile_band_minimum_value(tile = tile, band = band),
            maximum_value = .tile_band_maximum_value(tile = tile, band = band),
            scale_factor = .tile_band_scale_factor(tile = tile, band = band),
            offset_value = .tile_band_offset_value(tile = tile, band = band),
            impute_fn = impute_fn,
            filter_fn = filter_fn,
            stats_quant_02 = stats_quant_02,
            stats_quant_98 = stats_quant_98
        )
    })
    values_bands <- do.call(cbind, values_bands)
    colnames(values_bands) <- .ml_model_attr_names(ml_model)
    values_bands
}


#---- fi api ----

.fi_open <- function(tile,
                     band_name,
                     start_date,
                     end_date,
                     labels,
                     file) {
    r_obj <- .raster_open_rast(file)
    # Check number of labels
    if (!is.null(labels)) {
        .check_that(
            x = .raster_nlayers(r_obj) == length(labels),
            local_msg = "number of layers does not match labels",
            msg = "invalid 'file' or 'labels' parameter"
        )
    }
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
        path = file
    )
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

.fi_timeline <- function(fi) {
    unique(as.Date(fi[["date"]]))
}

.fi_during <- function(fi, start_date, end_date) {
    !.fi_is_empty(
        .fi_temporal_filter(
            fi = ,
            start_date = start_date,
            end_date = end_date
        )
    )
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

.fi_is_empty <- function(fi) {
    nrow(fi) == 0
}

.fi_read_block <- function(fi, band, block) {
    files <- fi[["path"]][fi[["band"]] == band]
    if (length(files) == 0) return(NULL)
    .raster_read_stack(
        files = files,
        block = block
    )
}

#---- values ----
.values_preprocess_bands <- function(values,
                                     cloud_mask,
                                     missing_value,
                                     minimum_value,
                                     maximum_value,
                                     scale_factor,
                                     offset_value,
                                     impute_fn,
                                     filter_fn,
                                     stats_quant_02,
                                     stats_quant_98) {
    # Correct NA, minimum, maximum, and missing values
    values[values < minimum_value] <- NA
    values[values > maximum_value] <- NA
    values[values == missing_value] <- NA
    # Change the points under cloud mask to NA
    if (!is.null(cloud_mask)) {
        values[cloud_mask] <- NA
    }
    # Remove NA pixels
    if (!is.null(impute_fn) && any(is.na(values))) {
        values <- impute_fn(values)
    }
    values <- scale_factor * values + offset_value
    # Filter the data
    if (!(is.null(filter_fn))) {
        values <- filter_fn(values)
    }
    # Normalize the data
    if (!is.null(stats_quant_02) && !is.null(stats_quant_98)) {
        values <- normalize_data(values, stats_quant_02, stats_quant_98)
    }
    return(values)
}

.values_cloud_mask <- function(values, interp_values, is_bit_mask) {
    if (is_bit_mask) {
        matrix(
            bitwAnd(values, sum(2^interp_values)) > 0,
            nrow = length(values)
        )
    } else {
        values %in% interp_values
    }
}

.values_preprocess_probs <- function(values) {
    # convert probabilities matrix to INT2U
    scale_factor <- .config_get("probs_cube_scale_factor")
    round(round(1 / scale_factor) * values, digits = 0)
}

.values_save_job <- function(values, job, out_file, data_type) {
    # create a new raster
    r_obj <- .raster_new_rast(
        nrows = job[["nrows"]],
        ncols = job[["ncols"]],
        xmin = job[["xmin"]],
        xmax = job[["xmax"]],
        ymin = job[["ymin"]],
        ymax = job[["ymax"]],
        nlayers = ncol(values),
        crs = job[["crs"]]
    )
    # copy values
    r_obj <- .raster_set_values(
        r_obj = r_obj,
        values = values
    )
    # write the probabilities to a raster file
    .raster_write_rast(
        r_obj = r_obj,
        file = out_file,
        format = "GTiff",
        data_type = data_type,
        overwrite = TRUE
    )
}

#---- ml_model ----

.ml_model_stats <- function(ml_model) {
    environment(ml_model)[["stats"]]
}

.ml_model_stats_quant2_band <- function(ml_model, band) {
    stats <- .ml_model_stats(ml_model)
    if (is.null(stats)) return(NULL)
    stats[2, band]
}

.ml_model_stats_quant98_band <- function(ml_model, band) {
    stats <- .ml_model_stats(ml_model)
    if (is.null(stats)) return(NULL)
    stats[3, band]
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

#---- samples ----
.samples_bands <- function(samples) {
    setdiff(names(samples[["time_series"]][[1]]), "Index")
}

.samples_labels <- function(samples) {
    sort(unique(samples[["label"]]))
}
