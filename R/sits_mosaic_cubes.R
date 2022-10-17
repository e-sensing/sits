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
    # precondition - cube
    .check_is_sits_cube(cube)
    # precondition - res
    .check_res(res)
    # precondition - output dir
    .check_output_dir(output_dir)
    # precondition - multicores
    .check_multicores(multicores)
    # precondition - progress
    .check_lgl_type(progress)
    # Spatial filter
    if (!is.null(roi)) {
        roi <- .roi_as_sf(roi)
        cube <- .cube_filter_spatial(cube = cube, roi = roi)
    }
    .sits_parallel_start(workers = multicores, log = FALSE)
    on.exit(.sits_parallel_stop(), add = TRUE)
    # TODO: Change assets to tile, thus makes more sense tiles
    # call that are done in code below
    # Create assets as jobs
    assets <- .cube_create_assets(cube)
    # Process each tile sequentially
    assets <- .jobs_map_parallel_dfr(assets, function(asset) {
        local_asset <- .crop_asset(
            asset = asset, res = res, roi = roi,
            crs = crs, output_dir = output_dir
        )
        # Return local tile
        local_asset
    }, progress = progress)
    .cube_mosaic_assets(
        assets = assets, crs = crs, multicores = multicores,
        output_dir = output_dir, version = version)
}

.transform_roi <- function(roi, crs) {
    sf::st_transform(x = roi, crs = crs)
}

.write_roi <- function(roi, output_file) {
    sf::st_write(obj = roi, dsn = output_file)
    output_file
}

.cube_mosaic_assets <- function(assets, crs, multicores, output_dir, version) {
    # Recreating cube structure
    cube <- .cube_merge_assets(assets)
    # Generate a vrt file
    vrt_file <- tempfile(fileext = ".vrt")
    .gdal_buildvrt(
        file = vrt_file, base_files = sapply(cube$file_info, `[[`, "path"),
        params = list("-allow_projection_difference" = TRUE), quiet = TRUE
    )
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
    # Copy values from mask cube into mask template
    .gdal_merge_into(
        file = out_file,
        base_files = vrt_file,
        multicores = 1
    )
    # TODO: set quiet to TRUE
    # overviews
    .gdal_addo(
        base_file = out_file, method = "NEAREST", overviews = c(2, 4, 8, 16)
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

.crop_asset <- function(asset, res, roi, crs, output_dir) {
    # Get all paths and expand
    file <- path.expand(.fi_paths(.fi(asset)))
    if (!is.null(roi)) {
        roi_obj <- .transform_roi(roi = roi, crs = .cube_crs(asset))
        bbox_obj <- .bbox_as_sf(.tile(asset), as_crs = .cube_crs(asset))
        is_tile_contained <- sf::st_contains(
            x = roi_obj,
            y = bbox_obj,
            sparse = FALSE
        )
        if (apply(is_tile_contained, 1, all)) {
            return(asset)
        }
        # TODO: set quiet to TRUE
        roi <- .write_roi(
            roi = roi,
            output_file = tempfile(fileext = ".shp")
        )
    }
    # Create a list of user parameters as gdal format
    gdal_params <- list(
        "-of" = "GTiff",
        "-co" = .conf("gdal_presets", "image", "co"),
        "-wo" = paste0("NUM_THREADS=", 2),
        "-multi" = TRUE,
        "-ot" = "Byte",
        "-cutline" = roi
    )
    # Create output file
    out_file <- file.path(output_dir, .file_base(file))
    # Resume feature
    if (.raster_is_valid(out_file)) {
        message("Recovery: file '", out_file, "' already exists.")
        message("(If you want to produce a new image, please ",
                "change 'output_dir' parameter)")
        asset <- .tile_eo_from_files(
            files = out_file, fid = .fi_fid(.fi(asset)),
            bands = .fi_bands(.fi(asset)), date = .tile_start_date(asset),
            base_tile = asset, update_bbox = TRUE
        )
        return(asset)
    }
    # crop asset
    out_file <- .crop_fn(file, out_file, gdal_params)
    # Update asset metadata
    update_bbox <- if (is.null(roi) && is.null(res)) FALSE else TRUE
    asset <- .tile_eo_from_files(
        files = out_file, fid = .fi_fid(.fi(asset)),
        bands = .fi_bands(.fi(asset)), date = .tile_start_date(asset),
        base_tile = asset, update_bbox = update_bbox
    )
    asset
}

.crop_fn <- function(file, out_file, gdal_params) {
    gdal_params[c("src_dataset", "dst_dataset")] <- list(file, out_file)
    .gdal_warp(
        file = out_file, base_files = file, params = gdal_params, quiet = TRUE
    )
    out_file
}
