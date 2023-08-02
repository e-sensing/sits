#' @title Create all MGRS Sentinel-2 tiles
#' @name .s2tile_open
#' @keywords internal
#' @noRd
#' @return a simple feature containing all Sentinel-2 tiles
.s2tile_open <- function() {
    # define dummy local variables to stop warnings
    epsg <- xmin <- ymin <- xmax <- ymax <- NULL

    # open ext_data tiles.rds file
    s2_file <- system.file("extdata/s2-tiles/tiles.rds", package = "sits")
    s2_tb2 <- readRDS(s2_file)

    # creates a list of simple features
    s2_sf_lst <- unique(s2_tb2$epsg) |>
        purrr::map(function(x) {
            dplyr::filter(s2_tb2, epsg == x) |>
                dplyr::mutate(
                    xmax = xmin + 109800,
                    ymax = ymin + 109800
                ) |>
                dplyr::rowwise() |>
                dplyr::mutate(geom = sf::st_as_sfc(sf::st_bbox(
                    c(xmin = xmin,
                      ymin = ymin,
                      xmax = xmax,
                      ymax = ymax)
                ))) |>
                dplyr::ungroup()
        })

    # transform each sf to WGS84 and merge them into a single one sf object
    s2_sf_lst |>
        purrr::map_dfr(function(df) {
            df |>
                sf::st_as_sf(
                    sf_column_name = "geom",
                    crs = paste0("EPSG:", df$epsg[[1]])
                ) |>
                sf::st_segmentize(10980) |>
                sf::st_transform(crs = 4326)
        })
}
