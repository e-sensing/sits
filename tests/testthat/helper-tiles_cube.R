# Two tiles of the same size (.check_block_size() takes one per cube) cut
# side by side from the mod13q1 cube, so a block in the wrong tile has
# another bbox and other values. The reference is the CPU path.
gpu_tiles_cube <- function(root) {
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = system.file("extdata/raster/mod13q1", package = "sits"),
        progress = FALSE
    )
    split_x <- cube[["xmin"]] + 127L * .tile_xres(cube)
    crop <- function(xmin, xmax, name) {
        dir <- file.path(root, paste0("input_", name))
        dir.create(dir, recursive = TRUE)
        roi <- sf::st_as_sfc(sf::st_bbox(
            c(xmin = xmin, xmax = xmax,
              ymin = cube[["ymin"]], ymax = cube[["ymax"]]),
            crs = sf::st_crs(cube[["crs"]])
        ))
        # gdalwarp warns that it moves -3000 to -3001, the same in both
        tile <- suppressWarnings(sits_cube_copy(
            cube, roi = roi, multicores = 1L, output_dir = dir,
            progress = FALSE
        ))
        tile[["tile"]] <- name
        tile
    }
    dplyr::bind_rows(
        crop(cube[["xmin"]], split_x, "A"),
        crop(split_x, split_x + 127L * .tile_xres(cube), "B")
    )
}

# The cube of gpu_tiles_cube() with tile B in WGS 84, so the tiles have two
# CRSs; B keeps the size of A, as .check_block_size() needs. Not UTM south:
# float32 block coordinates round its northings (dev too).
gpu_tiles_cube_two_crs <- function(root) {
    cube <- gpu_tiles_cube(root)
    dir_b <- file.path(root, "input_B_wgs84")
    dir.create(dir_b)
    for (file in .fi(cube[2L, ])[["path"]]) {
        r <- terra::rast(file)
        wgs84 <- terra::project(r, "EPSG:4326", method = "near")
        terra::project(
            r, terra::rast(terra::ext(wgs84), nrows = nrow(r),
                           ncols = ncol(r), crs = terra::crs(wgs84)),
            method = "near", datatype = "INT2S",
            filename = file.path(dir_b, basename(file))
        )
    }
    tile_b <- sits_cube(
        source = "BDC", collection = "MOD13Q1-6.1", data_dir = dir_b,
        progress = FALSE
    )
    tile_b[["tile"]] <- "B"
    dplyr::bind_rows(cube[1L, ], tile_b)
}
