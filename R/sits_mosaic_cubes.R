sits_mosaic_cubes <- function(cube,
                              bands = NULL,
                              start_date = NULL,
                              end_date = NULL,
                              roi = NULL,
                              res = NULL,
                              crs = 4326,
                              multicores = 2,
                              output_dir,
                              progress = TRUE) {

    # precondition - cube
    # TODO: add s3 para cada cubo
    # TODO: pensar em quais cubos serão aceitos? o pessoal da
    # embrapa fazem o mosaico dos cubos class, mas seria
    # interessante implementar para os cubos de observação da
    # terra
    .check_is_sits_cube(cube)
    # precondition - bands
    # TODO: create check
    .check_bands(bands)
    # precondition - res
    .check_res(res)
    # precondition - output dir
    output_dir <- path.expand(output_dir)
    .check_output_dir(output_dir)
    # precondition - multicores
    .check_multicores(multicores)
    # precondition - progress
    .check_lgl_type(progress)
    # Filter input bands
    cube <- .cube_filter_bands(cube = cube, bands = bands)
    # TODO: fazer filtro da data inicial e final caso sejam fornecidos
    # os parametros de start_date e end_date
    # Prepare parallel processing
    .sits_parallel_start(workers = multicores, log = FALSE)
    on.exit(.sits_parallel_stop(), add = TRUE)
    # Create assets as jobs
    assets <- .cube_create_assets(cube)
    # Process each tile sequentially
    assets <- .jobs_map_parallel_dfr(assets, function(asset) {
        local_asset <- .crop_asset(
            asset = asset, res = res, roi = roi, crs = crs,
            output_dir = output_dir, progress = progress
        )
        # Return local tile
        local_asset
    }, progress = progress)
    .cube_mosaic_assets()
}

.cube_mosaic_assets <- function() {
    # TODO: mosaicar cada data/banda de todos os tiles
    # TODO: colocar como nome do tile: "mosaic"
    # TODO: usar o comando sf::gdaladdo para criar os overviews do COG
    # TODO: criar uma função na API da .gdal
}

.crop_asset <- function(asset, res, roi, crs, output_dir, progress) {
    # Get all paths and expand
    file <- path.expand(.fi_paths(.fi(asset)))
    # Create a list of user parameters as gdal format
    gdal_params <- .gdal_format_params(asset = asset, roi = roi, res = res)
    # Create output file
    out_file <- file.path(output_dir, .file_base(file))
    # Resume feature
    if (.raster_is_valid(out_file)) {
        # # Callback final tile classification
        # .callback(process = "tile_classification", event = "recovery",
        #           context = environment())
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
    do.call(
        what = gdalUtilities::gdalwarp,
        args = gdal_params
    )
    out_file
}


