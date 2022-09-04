

#
# cube -> cube
#
.cube_spatial_filter <- function(cube, roi) {

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
    bbox <- .bbox_as_sf(tile)
}

.tile_temporal_filter <- function(tile, start_date, end_date) {
    UseMethod(".tile_temporal_filter")
}

.tile_temporal_filter.raster_cube <- function(tile, start_date, end_date) {
    .fi(tile) <- .fi_temporal_filter(
        .fi(tile),
        start_date = start_date,
        end_date = end_date
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

.fi_temporal_filter <- function(fi, start_date, end_date) {
    if (fi[["band"]] %in% .config_get("sits_results_bands")) {
        class(fi) <- unique(c("class_cube", class(fi)))
    } else {
        class(fi) <- unique(c("eo_cube", class(fi)))
    }
    UseMethod(".fi_temporal_filter", fi)
}

.fi_temporal_filter.eo_cube <- function(fi, start_date, end_date) {
    dplyr::filter(
        fi,
        !!start_date <= .data[["date"]],
        .data[["date"]] <= !!end_date
    )
}

.fi_temporal_filter.class_cube <- function(fi, start_date, end_date) {
    dplyr::filter(
        fi,
        !!start_date <= .data[["start_date"]],
        .data[["start_date"]] <= !!end_date,
        !!start_date <= .data[["end_date"]],
        .data[["end_date"]] <= !!end_date,
    )
}
