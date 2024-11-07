#' @title Create all MGRS Sentinel-2 tiles
#' @name .grid_filter_mgrs
#' @keywords internal
#' @noRd
#' @return a simple feature containing all Sentinel-2 tiles
.grid_filter_mgrs <- function(grid_system, roi, tiles) {
    # check
    .check_roi_tiles(roi, tiles)
    # define dummy local variables to stop warnings
    epsg <- xmin <- ymin <- xmax <- ymax <- NULL

    # get system grid path
    grid_path <- system.file(
        .conf("grid_systems", grid_system, "path"), package = "sits"
    )
    s2_tb <- readRDS(grid_path)

    if (is.character(tiles)) {
        s2_tb <- dplyr::filter(s2_tb, .data[["tile_id"]] %in% tiles)
    } else {
        # create a sf of points
        epsg_lst <- unique(s2_tb[["epsg"]])
        points_sf <- sf::st_as_sf(.map_dfr(epsg_lst, function(epsg) {
            tiles <- dplyr::filter(s2_tb, epsg == {{epsg}})
            sfc <- matrix(c(tiles[["xmin"]], tiles[["ymin"]]), ncol = 2) |>
                sf::st_multipoint(dim = "XY") |>
                sf::st_sfc(crs = epsg) |>
                sf::st_transform(crs = "EPSG:4326")
            sf::st_sf(geom = sfc)
        }))
        points_sf <- sf::st_cast(points_sf, "POINT")
        # heuristic to determine neighboring tiles using an ROI as one
        # tile is a maximum of 1 degree away from the other, 1.5 is
        # enough to intersect the neighborhood
        roi_search <- .bbox_as_sf(
            dplyr::mutate(
                .bbox(.roi_as_sf(roi, as_crs = "EPSG:4326")),
                xmin = xmin - 1.5,
                ymin = ymin - 1.5
            ))
        # filter points
        s2_tb <- s2_tb[.intersects(points_sf, roi_search), ]
    }

    # creates a list of simple features
    epsg_lst <- unique(s2_tb[["epsg"]])
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
    s2_tiles <- sf::st_as_sf(.map_dfr(s2_sf_lst, function(s2_sf) {
        s2_sf <- sf::st_as_sf(
            x = s2_sf,
            sf_column_name = "geom",
            crs = paste0("EPSG:", s2_sf[["epsg"]][[1]])
        )
        sf::st_transform(
            x = sf::st_segmentize(s2_sf, 10980),
            crs = "EPSG:4326"
        )
    }))

    # if roi is given, filter tiles by desired roi
    if (.has(roi))
        s2_tiles <- s2_tiles[.intersects(s2_tiles, .roi_as_sf(roi)), ]

    return(s2_tiles)
}

.grid_filter_bdc <- function(grid_system, roi, tiles) {
    # check
    .check_roi_tiles(roi, tiles)

    # get system grid path
    grid_path <- system.file(
        .conf("grid_systems", grid_system, "path"), package = "sits"
    )
    # open ext_data tiles.rds file
    bdc_tiles <- readRDS(grid_path)

    # define dummy local variables to stop warnings
    proj <- xmin <- ymin <- xmax <- ymax <- NULL

    if (.has(tiles)) {
        bdc_tiles <- bdc_tiles[bdc_tiles[["tile_id"]] %in% tiles, ]
    }

    # Get xres and yres
    xres <- .conf("grid_systems", grid_system, "xres")
    yres <- .conf("grid_systems", grid_system, "yres")

    # Get nrows and ncols
    nrows <- .conf("grid_systems", grid_system, "nrows")
    ncols <- .conf("grid_systems", grid_system, "ncols")

    # Create tiles geometry
    crs <- unique(bdc_tiles[["crs"]])
    bdc_tiles <- dplyr::mutate(
        bdc_tiles,
        xmax = xmin + xres * nrows,
        ymax = ymin + yres * ncols,
        crs = crs
    ) |>
        dplyr::rowwise() |>
        dplyr::mutate(geom = sf::st_as_sfc(sf::st_bbox(
            c(xmin = xmin,
              ymin = ymin,
              xmax = xmax,
              ymax = ymax)
        ))) |>
        sf::st_as_sf(crs = crs)

    # Just to ensure that we will reproject less data
    if (.has(roi)) {
        roi <- .roi_as_sf(roi, as_crs = .vector_crs(bdc_tiles))
        bdc_tiles <- bdc_tiles[.intersects(bdc_tiles, roi), ]
    }

    # Transform each sf to WGS84 and merge them into a single one sf object
    bdc_tiles <- sf::st_transform(
        x = bdc_tiles,
        crs = "EPSG:4326"
    )
    return(bdc_tiles)
}

.grid_filter_tiles <- function(grid_system, roi, tiles) {
    switch(
        grid_system,
        "MGRS" = .grid_filter_mgrs(grid_system, roi, tiles),
        "BDC_LG_V2" = , "BDC_MD_V2" = ,
        "BDC_SM_V2" = .grid_filter_bdc(grid_system, roi, tiles)
    )
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
.s2_mgrs_to_roi <- function(tiles) {
    .check_set_caller(".s2_mgrs_to_roi")
    # read the MGRS data set
    mgrs_tiles <- readRDS(
        system.file("extdata/s2-tiles/tiles.rds", package = "sits")
    )
    # check tiles names are valid
    .check_chr_within(
        x = tiles,
        within = mgrs_tiles[["tile_id"]]
    )
    # select MGRS tiles
    tiles_selected <- dplyr::filter(mgrs_tiles, .data[["tile_id"]] %in% !!tiles)

    # obtain a list of sf objects
    bbox_dfr <- slider::slide_dfr(tiles_selected, function(tile) {
        xmin <- as.double(tile[["xmin"]])
        xmax <- xmin + 109800
        ymin <- as.double(tile[["ymin"]])
        ymax <- ymin + 109800
        bbox <- sf::st_bbox(
            c(xmin = xmin,
              ymin = ymin,
              xmax = xmax,
              ymax = ymax),
            crs = sf::st_crs(tile[["epsg"]])
        )
        bbox_ll <- bbox |>
            sf::st_as_sfc() |>
            sf::st_transform(crs = "EPSG:4326") |>
            sf::st_bbox()

        ll <- c(
            lon_min = bbox_ll[["xmin"]],
            lat_min = bbox_ll[["ymin"]],
            lon_max = bbox_ll[["xmax"]],
            lat_max = bbox_ll[["ymax"]]
        )
        return(ll)
    })
    roi <- c(
        lon_min = min(bbox_dfr[["lon_min"]]),
        lat_min = min(bbox_dfr[["lat_min"]]),
        lon_max = max(bbox_dfr[["lon_max"]]),
        lat_max = max(bbox_dfr[["lat_max"]])
    )
    return(roi)
}
