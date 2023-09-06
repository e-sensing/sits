#' @title Create all MGRS Sentinel-2 tiles
#' @name .s2tile_open
#' @keywords internal
#' @noRd
#' @return a simple feature containing all Sentinel-2 tiles
.s2tile_open <- function(roi) {
    # define dummy local variables to stop warnings
    epsg <- xmin <- ymin <- xmax <- ymax <- NULL

    # open ext_data tiles.rds file
    s2_file <- system.file("extdata/s2-tiles/tiles.rds", package = "sits")
    s2_tb <- readRDS(s2_file)

    # create a sf of points
    points_sf <-
        unique(s2_tb$epsg) |>
        purrr::map_dfr(function(epsg) {
            tiles <- s2_tb |>
                dplyr::filter(epsg == {{epsg}})

            sfc <- matrix(c(tiles$xmin, tiles$ymin), ncol = 2) |>
                sf::st_multipoint(dim = "XY") |>
                sf::st_sfc(crs = epsg) |>
                sf::st_transform(crs = 4326)

            sf::st_sf(geom = sfc)
        }) |>
        sf::st_cast("POINT")

    # change roi to 1.5 degree to west and south
    roi <- .roi_as_sf(roi) |>
        .bbox() |>
        dplyr::mutate(
            xmin = xmin - 1.5,
            ymax = ymax - 1.5
        ) |>
        .bbox_as_sf()

    # filter points
    s2_tb <- s2_tb[.intersects(points_sf, roi), ]

    # creates a list of simple features
    s2_sf_lst <- unique(s2_tb$epsg) |>
        purrr::map(function(x) {
            dplyr::filter(s2_tb, epsg == {{x}}) |>
                dplyr::mutate(
                    xmax = xmin + 109800,
                    ymax = ymin + 109800,
                    crs = paste0("EPSG:", {{x}})
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
