#' @title Convert MGRS tile information to ROI in WGS84
#' @name sits_mgrs_to_roi
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@gmail.com}
#'
#' @description
#' Takes a list of MGRS tiles and produces a ROI covering them
#'
#' @param  tiles                Character vector with names of MGRS tiles
#' @return roi                  Valid ROI to use in other SITS functions
#'
#' @export
sits_mgrs_to_roi <- function(tiles) {
    .conf("messages", "sits_mgrs_to_roi")
    sits_tiles_to_roi(tiles = tiles, grid_system = "MGRS")
}

#' @title Convert MGRS tile information to ROI in WGS84
#' @name sits_tiles_to_roi
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@gmail.com}
#'
#' @description
#' Takes a list of MGRS tiles and produces a ROI covering them
#'
#' @param  tiles                Character vector with names of MGRS tiles
#' @param  grid_system          Grid system to be used
#' @return roi                  Valid ROI to use in other SITS functions
#'
#'
#' @export
sits_tiles_to_roi <- function(tiles, grid_system = "MGRS") {
    # retrieve the ROI
    roi <- .grid_filter_tiles(
        grid_system = grid_system,
        roi = NULL,
        tiles = tiles
    )
    sf::st_bbox(roi)
}

#' @title Given a ROI, find MGRS tiles intersecting it.
#' @name sits_roi_to_mgrs
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description
#' Takes a a ROI and produces a list of MGRS tiles intersecting it
#'
#' @param roi                  Valid ROI to use in other SITS functions
#' @return tiles                Character vector with names of MGRS tiles
#' @note
#'      To define a \code{roi} use one of:
#'      \itemize{
#'        \item{A path to a shapefile with polygons;}
#'        \item{A \code{sfc} or \code{sf} object from \code{sf} package;}
#'        \item{A \code{SpatExtent} object from \code{terra} package;}
#'        \item{A named \code{vector} (\code{"lon_min"},
#'             \code{"lat_min"}, \code{"lon_max"}, \code{"lat_max"}) in WGS84;}
#'        \item{A named \code{vector} (\code{"xmin"}, \code{"xmax"},
#'              \code{"ymin"}, \code{"ymax"}) with XY coordinates.}
#'       }
#'
#'      Defining a region of interest using \code{SpatExtent} or XY values not
#'      in WGS84 requires the \code{crs} parameter to be specified.
#'      \code{sits_regularize()} function will crop the images
#'      that contain the region of interest()
#' @examples
#' if (sits_run_examples()) {
#' # Defining a ROI
#' roi <- c(
#'   lon_min = -64.037,
#'   lat_min = -9.644,
#'   lon_max = -63.886,
#'   lat_max = -9.389
#' )
#' # Finding tiles
#' tiles <- sits_roi_to_mgrs(roi)
#' }
#' @export
sits_roi_to_mgrs <- function(roi) {
    sits_roi_to_tiles(roi = roi, grid_system = "MGRS")
}
#' @title Find tiles of a given ROI and Grid System
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @name sits_roi_to_tiles
#'
#' @description Given an ROI and grid system, this function finds the
#' intersected tiles and returns them as an SF object.
#'
#' @param roi         Region of interest (see notes below).
#' @param crs         Coordinate Reference System (CRS) of the roi.
#'                    (see details below).
#' @param grid_system Grid system to be used for the output images.
#'                    (Default is "MGRS")
#'
#' @note
#'      To define a \code{roi} use one of:
#'      \itemize{
#'        \item{A path to a shapefile with polygons;}
#'        \item{A \code{sfc} or \code{sf} object from \code{sf} package;}
#'        \item{A \code{SpatExtent} object from \code{terra} package;}
#'        \item{A named \code{vector} (\code{"lon_min"},
#'             \code{"lat_min"}, \code{"lon_max"}, \code{"lat_max"}) in WGS84;}
#'        \item{A named \code{vector} (\code{"xmin"}, \code{"xmax"},
#'              \code{"ymin"}, \code{"ymax"}) with XY coordinates.}
#'       }
#'
#'      Defining a region of interest using \code{SpatExtent} or XY values not
#'      in WGS84 requires the \code{crs} parameter to be specified.
#'      \code{sits_regularize()} function will crop the images
#'      that contain the region of interest().
#'
#'      The \code{grid_system} parameter allows the user to
#'      reproject the files to a grid system which is
#'      different from that used in the ARD image collection of
#'      the could provider. Currently, the package supports
#'      the use of MGRS grid system and those used by the Brazil
#'      Data Cube ("BDC_LG_V2" "BDC_MD_V2" "BDC_SM_V2").
#'
#'@examples
#' if (sits_run_examples()) {
#' # Defining a ROI
#' roi <- c(
#'   lon_min = -64.037,
#'   lat_min = -9.644,
#'   lon_max = -63.886,
#'   lat_max = -9.389
#' )
#' # Finding tiles
#' tiles <- sits_roi_to_tiles(roi, grid_system = "MGRS")
#' }
#' @return A \code{sf} object with the intersect tiles with three columns
#' tile_id, epsg, and the percentage of coverage area.
sits_roi_to_tiles <- function(roi, crs = NULL, grid_system = "MGRS") {
    # Pre-conditions
    grid_system <- toupper(grid_system)
    .check_grid_system(grid_system)
    # Find the intersected tiles
    roi <- .roi_as_sf(roi, default_crs = crs)
    # Add a small buffer when a single point is provided
    if (length(sf::st_geometry_type(roi))  == 1 &&
        sf::st_geometry_type(roi) == "POINT") {
        roi <- sf::st_buffer(roi, dist = 0.00001)
    }
    tiles <- .grid_filter_tiles(
        grid_system = grid_system, roi = roi, tiles = NULL
    )
    # columns to select
    cols_to_select <- "tile_id"
    # Compute the coverage area
    if (all(sf::st_geometry_type(roi) %in% c("POLYGON", "MULTIPOLYGON"))) {
        inter_tile <- .intersection(tiles, roi)
        tiles[["coverage_percentage"]] <- .as_dbl(round(
            (.area(inter_tile) / .area(tiles)) * 100, digits = 2
        ))
        cols_to_select <- c(cols_to_select, "coverage_percentage")
    }
    # Return sf object with filtered columns
    tiles[, cols_to_select]
}
