.summary_tile_information <- function(tile) {
    # print the basic tile information
    cat("class       : ", class(tile)[1], "\n")
    cat("dimensions  : ",
        .tile_nrows(tile), ", ",
        .tile_ncols(tile), "  (nrow, ncol)\n", sep = "")
    cat("resolution  : ",
        .tile_xres(tile), ", ",
        .tile_yres(tile), "  (x, y)\n", sep = "")
    cat("extent      : ",
        .xmin(tile), ", ",
        .xmax(tile), ", ",
        .ymin(tile), ", ",
        .ymax(tile),
        "  (xmin, xmax, ymin, ymax)\n", sep = "")
    cat("coord ref   : ", .crs_wkt_to_proj4(tile$crs), "\n")
    return(invisible(NULL))
}
.summary_derived_cube <- function(object,
                                  tile = object$tile[[1]],
                                  only_stats = FALSE) {

    # get sample size
    sample_size <- .conf("summary_sample_size")
    # filter the tile to be processed
    tile <- .summary_check_tile(object, tile)
    # get the bands
    band <- sits_bands(tile)
    .check_num(
        x = length(band),
        min = 1,
        max = 1,
        is_integer = TRUE,
        msg = "invalid cube - more than one probs band")
    # extract the file paths
    files <- .tile_paths(tile)

    # print the base information (if requested)
    if (!only_stats)
        .summary_tile_information(tile)
    # read the files with terra
    r <- terra::rast(files)
    # get the a sample of the values
    values <- r |>
        terra::spatSample(size = sample_size, na.rm = TRUE)
    # scale the values
    band_conf <- .tile_band_conf(tile, band)
    scale <- .scale(band_conf)
    offset <- .offset(band_conf)
    sum <- summary(values * scale + offset)
    colnames(sum) <- sits_labels(tile)
    sum
}
.summary_check_tile <- function(object, tile) {
    # only one tile at a time
    .check_chr_parameter(tile)
    # is tile inside the cube?
    .check_chr_contains(
        x = object$tile,
        contains = tile,
        case_sensitive = FALSE,
        discriminator = "one_of",
        can_repeat = FALSE,
        msg = "tile is not included in the cube"
    )
    # filter the tile to be processed
    tile <- .cube_filter_tiles(cube = object, tiles = tile)
    return(tile)
}
