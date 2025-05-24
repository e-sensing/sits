sits_roi_to_tiles <- function(roi, grid_system = "mgrs") {
    .check_roi(roi)
    grid_system <- toupper(grid_system)
    .check_grid_system(grid_system)
    roi <- .roi_as_sf(roi)
    tiles_filtered <- .grid_filter_tiles(
        grid_system = grid_system, tiles = NULL, roi = roi
    )
    tiles_intersection <- sf::st_intersection(
        x = tiles_filtered, y = roi
    )
    tiles_intersection[["area"]] <- sf::st_area(tiles_intersection)
    tiles_intersection[["cover_percentage"]] <- sf::st_area(tiles_filtered)
    tiles_intersection[, c("tile_id", "epsg", "cover_percentage")]
}
