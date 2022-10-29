sits_mosaic_cubes <- function(cube,
                              roi,
                              res,
                              crs,
                              multicores = 2,
                              output_dir,
                              progress = TRUE, ...) {

    UseMethod("sits_mosaic_cubes", cube)
}

sits_mosaic_cubes.class_cube <- function(cube,
                                         roi = NULL,
                                         res = NULL,
                                         crs = 4326,
                                         multicores = 2,
                                         output_dir,
                                         version = "v1",
                                         progress = TRUE) {
    # Pre-conditions
    .check_is_raster_cube(cube)
    .check_cube_is_class_cube(cube)
    .check_res(res)
    .check_output_dir(output_dir)
    .check_multicores(multicores)
    .check_progress(progress)

    # Spatial filter
    if (!is.null(roi)) {
        roi <- .roi_as_sf(roi)
        cube <- .cube_filter_spatial(cube = cube, roi = roi)
    }
    .sits_parallel_start(workers = multicores, log = FALSE)
    on.exit(.sits_parallel_stop(), add = TRUE)
    # Process each tile in parallel
    cube <- .jobs_map_parallel_dfr(cube, function(tile) {
        tile_cropped <- .crop_tile(
            tile = tile, res = res, roi = roi,
            crs = crs, version = version,
            output_dir = output_dir
        )
        # Return a cropped tile
        tile_cropped
    }, progress = progress)
    .cube_mosaic_tiles(
        cube = cube, crs = crs, multicores = multicores,
        output_dir = output_dir, version = version
    )
}

.write_roi <- function(roi, output_file, quiet, ...) {
    sf::st_write(obj = roi, dsn = output_file, quiet = quiet, ...)
    output_file
}

.cube_mosaic_tiles <- function(cube, crs, multicores, output_dir, version) {
    # Generate a vrt file
    vrt_file <- tempfile(fileext = ".vrt")
    .gdal_buildvrt(
        file = vrt_file, base_files = sapply(cube$file_info, `[[`, "path"),
        quiet = TRUE
    )
    file_paths <- .cube_foreach_tile(cube = cube, fn = .tile_paths)
    # Create mosaic file name
    base_tile <- .tile(cube)
    out_file <- .file_mosaic_name(
        tile = base_tile, band = .tile_bands(base_tile),
        version = version,
        output_dir = output_dir
    )
    # Create template block for mask
    .gdal_template_from_file(
        base_file = vrt_file,
        file = out_file,
        nlayers = 1,
        miss_value = 255,
        data_type = "INT1U"
    )
    # .gdal_template_from_bbox(
    #     file = out_file,
    #     res = c(.tile_xres(base_tile), .tile_yres(base_tile)),
    #     bbox = .bbox(cube),
    #     miss_value = 255,
    #     data_type = "INT1U"
    # )
    # Copy values from mask cube into mask template
    .gdal_merge_into(
        file = out_file,
        base_files = vrt_file,
        multicores = multicores
    )
    # Create COG overviews
    conf_cog <- .conf("gdal_presets", "cog")
    .gdal_addo(
        base_file = out_file,
        method = conf_cog[["method"]],
        overviews = conf_cog[["overviews"]]
    )
    # Create tile based on template
    base_tile <- .tile_derived_from_file(
        file = out_file, band = .tile_bands(base_tile),
        base_tile = base_tile, derived_class = .tile_derived_class(base_tile),
        labels = .tile_labels(base_tile),
        update_bbox = TRUE
    )
    # Update tile name
    .tile_name(base_tile) <- "MOSAIC"
    return(base_tile)
}

.crop_tile <- function(tile, res, roi, crs, version, output_dir) {
    # Get all paths and expand
    file <- .tile_paths(tile)
    # Create output file name
    out_file <- .file_crop_name(
        tile = tile, band = .tile_bands(tile),
        version = version, output_dir = output_dir
    )
    # Resume feature
    if (.raster_is_valid(out_file)) {
        message("Recovery: file '", out_file, "' already exists.")
        message("(If you want to produce a new cropped image, please ",
                "change 'version' or 'output_dir' parameter)")
        tile <- .tile_eo_from_files(
            files = out_file, fid = .fi_fid(.fi(tile)),
            bands = .fi_bands(.fi(tile)), date = .tile_start_date(tile),
            base_tile = tile, update_bbox = TRUE
        )
        return(tile)
    }
    fix_image_proj <- TRUE
    if (fix_image_proj) {
        out_file_fixed <- paste0(out_file, "_fixed.tif")
        .gdal_fix_image(
            file = file,
            out_file = out_file_fixed,
            crs = .crs(tile),
            multicores = 1
        )
        file <- out_file_fixed
    }
    if (!is.null(roi)) {
        # Get roi and tile extent as sf
        roi_obj <- .roi_as_sf(roi = roi, as_crs = .cube_crs(tile))
        bbox_obj <- .bbox_as_sf(bbox = .bbox(tile), as_crs = .cube_crs(tile))

        is_tile_in_roi <- sf::st_contains(
            x = roi_obj,
            y = bbox_obj,
            sparse = FALSE
        )
        if (all(c(is_tile_in_roi))) {
            .gdal_rep_image(
                file = file,
                out_file = out_file,
                crs = crs,
                multicores = 1
            )
            tile <- .tile_class_from_file(
                file = out_file, band = "class", base_tile = tile
            )
            return(tile)
        }
        # Write roi in a temporary file
        roi <- .write_roi(
            roi = roi,
            output_file = tempfile(fileext = ".shp"),
            quiet = TRUE
        )
    }
    # Crop tile image
    out_file <- .gdal_crop_image(
        file = file, out_file = out_file, roi = roi, crs = crs, multicores = 1
    )
    # Delete temporary roi file
    # TODO: improve this
    unlink(.file_path(.file_sans_ext(roi), ".*"))
    # Update asset metadata
    update_bbox <- if (is.null(roi) && is.null(res)) FALSE else TRUE
    tile <- .tile_eo_from_files(
        files = out_file, fid = .fi_fid(.fi(tile)),
        bands = .fi_bands(.fi(tile)), date = .tile_start_date(tile),
        base_tile = tile, update_bbox = update_bbox
    )
    tile
}

.gdal_crop_image <- function(file, out_file, roi, crs, multicores) {

    band_conf <- .conf_derived_band(
        derived_class = "class_cube", band = "class"
    )
    gdal_params <- list(
        "-of" = .conf("gdal_presets", "image", "of"),
        "-co" = .conf("gdal_presets", "image", "co"),
        "-wo" = paste0("NUM_THREADS=", multicores),
        "-t_srs" = .as_crs(crs),
        "-multi" = TRUE,
        "-cutline" = roi,
        "-srcnodata" = .miss_value(band_conf),
        "-overwrite" = TRUE
    )
    .gdal_warp(
        file = out_file, base_files = file,
        params = gdal_params, quiet = TRUE
    )
    out_file
}

.gdal_template_from_bbox <- function(file, res, bbox, miss_value, data_type) {
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
            "-b" = rep(1, 1),
            #"-tr" = list(res, res),
            #"-outsize" = list(.ncols(block), .nrows(block)),
            "-scale" = list(0, 1, miss_value, miss_value),
            "-a_srs" = .crs(bbox),
            "-a_ullr" = list(
                .xmin(bbox), .ymax(bbox), .xmax(bbox), .ymin(bbox)
            ),
            "-a_nodata" = miss_value,
            "-co" = .conf("gdal_presets", "image", "co")
        ),
        quiet = TRUE
    )
}

.gdal_rep_image <- function(file, out_file, crs, multicores) {
    band_conf <- .conf_derived_band(
        derived_class = "class_cube", band = "class"
    )
    gdal_params <- list(
        "-of" = .conf("gdal_presets", "image", "of"),
        "-co" = .conf("gdal_presets", "image", "co"),
        "-wo" = paste0("NUM_THREADS=", multicores),
        "-t_srs" = .as_crs(crs),
        "-srcnodata" = .miss_value(band_conf),
        "-overwrite" = TRUE
    )
    .gdal_warp(
        file = out_file, base_files = file,
        params = gdal_params, quiet = TRUE
    )
    out_file
}

.gdal_fix_image <- function(file, out_file, crs, multicores) {
    band_conf <- .conf_derived_band(
        derived_class = "class_cube", band = "class"
    )
    gdal_params <- list(
        "-of" = .conf("gdal_presets", "image", "of"),
        "-co" = .conf("gdal_presets", "image", "co"),
        "-wo" = paste0("NUM_THREADS=", multicores),
        "-t_srs" = .as_crs(crs),
        "-tr" = list(10, 10),
        "-tap" = TRUE,
        "-srcnodata" = .miss_value(band_conf)
    )
    .gdal_warp(
        file = out_file, base_files = file,
        params = gdal_params, quiet = TRUE
    )
    out_file
}
