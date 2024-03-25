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
    epsg_lst <- unique(s2_tb$epsg)
    points_sf <- sf::st_cast(purrr::map_dfr(epsg_lst, function(epsg) {
        tiles <- dplyr::filter(s2_tb, epsg == {{epsg}})
        sfc <- matrix(c(tiles$xmin, tiles$ymin), ncol = 2) |>
            sf::st_multipoint(dim = "XY") |>
            sf::st_sfc(crs = epsg) |>
            sf::st_transform(crs = "EPSG:4326")
        sf::st_sf(geom = sfc)
    }), "POINT")

    # change roi to 1.5 degree to west and south
    roi <- .bbox_as_sf(
        dplyr::mutate(
            .bbox(.roi_as_sf(roi, as_crs = "EPSG:4326")),
            xmin = xmin - 1.5,
            ymin = ymin - 1.5
        ))

    # filter points
    s2_tb <- s2_tb[.intersects(points_sf, roi), ]

    # creates a list of simple features
    epsg_lst <- unique(s2_tb$epsg)
    s2_sf_lst <- purrr::map(epsg_lst, function(epsg) {
        dplyr::filter(s2_tb, epsg == {{epsg}}) |>
            dplyr::mutate(
                xmax = xmin + 109800,
                ymax = ymin + 109800,
                crs = paste0("EPSG:", {{epsg}})
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
    s2_tiles <- purrr::map_dfr(s2_sf_lst, function(s2_sf) {
        s2_sf <- sf::st_as_sf(
            x = s2_sf,
            sf_column_name = "geom",
            crs = paste0("EPSG:", s2_sf$epsg[[1]])
        )
        sf::st_transform(
            x = sf::st_segmentize(s2_sf, 10980),
            crs = "EPSG:4326"
        )
    })

    return(s2_tiles)
}
#' @title Convert MGRS tile information to ROI in WGS84
#' @name .s2_mgrs_to_roi
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @description
#' Takes a list of MGRS tiles and produces a ROI covering them
#'
#' @param  tiles                Character vector with names of MGRS tiles
#' @return roi                  Valid ROI to use in other SITS functions
#'
.s2_mgrs_to_roi <- function(tiles){

    # read the MGRS data set
    mgrs_tiles <- readRDS(system.file("extdata/s2-tiles/tiles.rds",
                                      package = "sits"))
    # check tiles names are valid
    .check_chr_within(
        x = tiles,
        within = mgrs_tiles$tile_id,
        msg = "invalid MGRS tiles"
    )
    # select MGRS tiles
    tiles_selected <- dplyr::filter(mgrs_tiles, .data[["tile_id"]] %in% !!tiles)

    # obtain a list of sf objects
    bbox_dfr <- slider::slide_dfr(tiles_selected, function(tile){
        xmin <- as.double(tile$xmin)
        xmax <- xmin + 109800
        ymin <- as.double(tile$ymin)
        ymax <- ymin + 109800
        bbox <- sf::st_bbox(c("xmin" = xmin, "ymin" = ymin,
                              "xmax" = xmax, "ymax" = ymax), crs = sf::st_crs(tile$epsg))
        bbox_ll <- bbox |>
            sf::st_as_sfc() |>
            sf::st_transform(crs = "EPSG:4326") |>
            sf::st_bbox()

        ll <- c("lon_min" = bbox_ll[["xmin"]], "lat_min" = bbox_ll[["ymin"]],
                "lon_max" = bbox_ll[["xmax"]], "lat_max" = bbox_ll[["ymax"]])
        return(ll)
    })
    roi <- c("lon_min" = min(bbox_dfr[["lon_min"]]),
             "lat_min" = min(bbox_dfr[["lat_min"]]),
             "lon_max" = max(bbox_dfr[["lon_max"]]),
             "lat_max" = max(bbox_dfr[["lat_max"]])
    )
    return(roi)
}
