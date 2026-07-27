#' @title Read grid tiles table
#' @name .grid_read_tiles
#' @keywords internal
#' @noRd
#' @param grid_system  Grid system name.
#' @return A tibble with grid tile metadata.
.grid_read_tiles <- function(grid_system) {
    grid_path <- system.file(
        .conf("grid_systems", grid_system, "path"),
        package = "sits"
    )
    readRDS(grid_path)
}

#' @title Compute tile width and height for a grid system
#' @name .grid_tile_size
#' @keywords internal
#' @noRd
#' @param grid_system  Grid system name.
#' @return Named list with tile width and height in metres (`x`, `y`).
.grid_tile_size <- function(grid_system) {
    list(
        x = .conf("grid_systems", grid_system, "xres") *
            .conf("grid_systems", grid_system, "ncols"),
        y = .conf("grid_systems", grid_system, "yres") *
            .conf("grid_systems", grid_system, "nrows")
    )
}

#' @title Filter grid tiles by ROI using tile-origin points
#' @name .grid_filter_points
#' @keywords internal
#' @noRd
#' @param tiles_tb  Tiles tibble with an `epsg` column.
#' @param roi       Region of interest in WGS84.
#' @param buffer    Buffer in degrees around the ROI.
#' @return Filtered tiles tibble.
.grid_filter_points <- function(tiles_tb, roi, buffer) {
    epsg <- xmin <- ymin <- NULL
    epsg_lst <- unique(tiles_tb[["epsg"]])
    # Avoid 'no visible for global variable' warning in check
    .env <- NULL
    points_sf <- sf::st_as_sf(.map_dfr(epsg_lst, function(epsg) {
        tiles <- dplyr::filter(tiles_tb, .data[["epsg"]] == .env$epsg)
        sfc <- matrix(c(tiles[["xmin"]], tiles[["ymin"]]), ncol = 2L) |>
            sf::st_multipoint(dim = "XY") |>
            sf::st_sfc(crs = epsg) |>
            sf::st_transform(crs = "EPSG:4326")
        sf::st_sf(geom = sfc)
    }))
    points_sf <- sf::st_cast(points_sf, "POINT")
    # heuristic to determine neighboring tiles using an ROI as one
    # tile is a maximum of 1 degree away from the other, the buffer is
    # enough to intersect the neighborhood
    roi_search <- .bbox_as_sf(
        dplyr::mutate(
            .bbox(.roi_as_sf(roi, as_crs = "EPSG:4326")),
            xmin = xmin - buffer,
            ymin = ymin - buffer
        )
    )
    tiles_tb[.intersects(points_sf, roi_search), ]
}

#' @title Build grid tile polygons in native CRS
#' @name .grid_tiles_as_sf_list
#' @keywords internal
#' @noRd
#' @param tiles_tb     Tiles tibble with tile_id, xmin, ymin, and crs/epsg.
#' @param grid_system  Grid system name.
#' @return A list of tibbles with a native CRS `geom` column.
.grid_tiles_as_sf_list <- function(tiles_tb, grid_system) {
    tile_size <- .grid_tile_size(grid_system)
    epsg <- xmin <- ymin <- xmax <- ymax <- NULL
    if ("epsg" %in% names(tiles_tb)) {
        tiles_tb[["crs"]] <- paste0("EPSG:", tiles_tb[["epsg"]])
    }
    crs_values <- unique(tiles_tb[["crs"]])
    # Avoid 'no visible for global variable' warning in check
    .env <- NULL
    purrr::map(crs_values, function(crs_value) {
        dplyr::filter(tiles_tb, .data[["crs"]] == .env$crs_value) |>
            dplyr::mutate(
                xmax = xmin + tile_size[["x"]],
                ymax = ymin + tile_size[["y"]]
            ) |>
            dplyr::rowwise() |>
            dplyr::mutate(geom = sf::st_as_sfc(sf::st_bbox(
                c(
                    xmin = xmin,
                    ymin = ymin,
                    xmax = xmax,
                    ymax = ymax
                )
            ))) |>
            dplyr::ungroup()
    })
}

#' @title Reproject grid tile polygons to WGS84
#' @name .grid_tiles_to_wgs84
#' @keywords internal
#' @noRd
#' @param tiles_sf_lst  List of tibbles returned by .grid_tiles_as_sf_list.
#' @param grid_system   Grid system name.
#' @return An sf object in EPSG:4326.
.grid_tiles_to_wgs84 <- function(tiles_sf_lst, grid_system) {
    nrows <- .conf("grid_systems", grid_system, "nrows")
    sf::st_as_sf(.map_dfr(tiles_sf_lst, function(tiles_sf) {
        tiles_sf <- sf::st_as_sf(
            x = tiles_sf,
            sf_column_name = "geom",
            crs = tiles_sf[["crs"]][[1L]]
        )
        sf::st_transform(
            x = sf::st_segmentize(x = tiles_sf, dfMaxLength = nrows),
            crs = "EPSG:4326"
        )
    }))
}

#' @title Create all MGRS Sentinel-2 tiles
#' @name .grid_filter_mgrs
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @keywords internal
#' @noRd
#' @return a simple feature containing all Sentinel-2 tiles
.grid_filter_mgrs <- function(grid_system, roi, tiles) {
    # check
    .check_roi_tiles(roi, tiles)
    # define dummy local variables to stop warnings
    epsg <- xmin <- ymin <- xmax <- ymax <- NULL

    s2_tb <- .grid_read_tiles(grid_system)

    if (.has(tiles)) {
        s2_tb <- s2_tb[s2_tb[["tile_id"]] %in% tiles, ]
    } else {
        s2_tb <- .grid_filter_points(s2_tb, roi, buffer = 1.5)
    }

    s2_tiles <- .grid_tiles_to_wgs84(
        .grid_tiles_as_sf_list(s2_tb, grid_system),
        grid_system
    )

    # if roi is given, filter tiles by desired roi
    if (.has(roi)) {
        s2_tiles <- s2_tiles[.intersects(s2_tiles, .roi_as_sf(roi)), ]
    }
    # return s2 tiles
    s2_tiles
}
#' @title Filter data in the Brazil Data Cube grid system
#' @name .grid_filter_bdc
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @keywords internal
#' @noRd
#' @param grid_system     Grid system in use (BDC)
#' @param roi             Region of interest
#' @param tiles           Tiles to be retrieved
#' @return                Tiles from the BDC system

.grid_filter_bdc <- function(grid_system, roi, tiles) {
    # check
    .check_roi_tiles(roi, tiles)

    bdc_tiles <- .grid_read_tiles(grid_system)

    # define dummy local variables to stop warnings
    xmin <- ymin <- xmax <- ymax <- NULL

    if (.has(tiles)) {
        bdc_tiles <- bdc_tiles[bdc_tiles[["tile_id"]] %in% tiles, ]
    }

    # Build tile polygons in native CRS
    bdc_tiles <- sf::st_as_sf(.map_dfr(
        .grid_tiles_as_sf_list(bdc_tiles, grid_system),
        function(bdc_sf) {
            sf::st_as_sf(
                x = bdc_sf,
                sf_column_name = "geom",
                crs = bdc_sf[["crs"]][[1L]]
            )
        }
    ))

    # Just to ensure that we will reproject less data
    if (.has(roi)) {
        roi <- suppressWarnings(.roi_as_sf(roi, as_crs = .vector_crs(bdc_tiles)))
        bdc_tiles <- suppressWarnings(bdc_tiles[.intersects(bdc_tiles, roi), ])
    }
    # Transform each sf to WGS84 and merge them into a single one sf object
    suppressWarnings(sf::st_transform(x = bdc_tiles, crs = "EPSG:4326"))
}
#' @title Filter data in the AlphaEarth (AEF) grid system
#' @name .grid_filter_aef
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @keywords internal
#' @noRd
#' @param grid_system     Grid system in use (ALPHAEARTH)
#' @param roi             Region of interest
#' @param tiles           Tiles to be retrieved
#' @return                Tiles from the AlphaEarth system
#'
#' @description
#' The AlphaEarth Foundations tile footprint is rebuilt here on the fly from the
#' grid origins, using the tile size configured for the ALPHAEARTH grid system.
.grid_filter_aef <- function(grid_system, roi, tiles) {
    # check tiles
    .check_roi_tiles(roi, tiles)

    # define dummy local variables to stop warnings
    epsg <- xmin <- ymin <- xmax <- ymax <- NULL

    aef_tb <- .grid_read_tiles(grid_system)

    # filter by id
    if (.has(tiles)) {
        aef_tb <- aef_tb[aef_tb[["tile_id"]] %in% tiles, ]
    } else {
        aef_tb <- .grid_filter_points(aef_tb, roi, buffer = 1.0)
    }

    aef_tiles <- .grid_tiles_to_wgs84(
        .grid_tiles_as_sf_list(aef_tb, grid_system),
        grid_system
    )

    # if roi is given, filter tiles by desired roi
    if (.has(roi)) {
        aef_tiles <- aef_tiles[.intersects(aef_tiles, .roi_as_sf(roi)), ]
    }

    # return aef tiles
    aef_tiles
}
#' @title Filter tiles in different grid system
#' @name .grid_filter_tiles
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @keywords internal
#' @noRd
#' @param grid_system     Grid system in use
#' @param roi             Region of interest
#' @param tiles           Tiles to be retrieved
#' @return                Tiles in the desired grid system
.grid_filter_tiles <- function(grid_system, roi, tiles) {
    switch(grid_system,
        "MGRS" = .grid_filter_mgrs(grid_system, roi, tiles),
        "ALPHAEARTH" = .grid_filter_aef(grid_system, roi, tiles),
        "BDC_LG_V2" = ,
        "BDC_MD_V2" = ,
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
    mgrs_tiles <- .grid_read_tiles("MGRS")
    # check tiles names are valid
    .check_chr_within(
        x = tiles,
        within = mgrs_tiles[["tile_id"]]
    )
    # select MGRS tiles
    tiles_selected <- dplyr::filter(mgrs_tiles, .data[["tile_id"]] %in% !!tiles)

    # obtain a list of sf objects
    tile_size <- .grid_tile_size("MGRS")
    bbox_dfr <- slider::slide_dfr(tiles_selected, function(tile) {
        xmin <- as.double(tile[["xmin"]])
        xmax <- xmin + tile_size[["x"]]
        ymin <- as.double(tile[["ymin"]])
        ymax <- ymin + tile_size[["y"]]
        bbox <- sf::st_bbox(
            c(
                xmin = xmin,
                ymin = ymin,
                xmax = xmax,
                ymax = ymax
            ),
            crs = sf::st_crs(tile[["epsg"]])
        )
        bbox_ll <- bbox |>
            sf::st_as_sfc() |>
            sf::st_transform(crs = "EPSG:4326") |>
            sf::st_bbox()
        # return tile box in lat/long as a row of a data frame
        c(
            lon_min = bbox_ll[["xmin"]],
            lat_min = bbox_ll[["ymin"]],
            lon_max = bbox_ll[["xmax"]],
            lat_max = bbox_ll[["ymax"]]
        )
    })
    # return the absolute bbox of the set of tiles
    c(
        lon_min = min(bbox_dfr[["lon_min"]]),
        lat_min = min(bbox_dfr[["lat_min"]]),
        lon_max = max(bbox_dfr[["lon_max"]]),
        lat_max = max(bbox_dfr[["lat_max"]])
    )
}
