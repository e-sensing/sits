#' @title Points accessors
#' @noRd
#'
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @description
#' These functions are accessors of `point` fields in a object tibble.
#' Getters functions returns the respective field values with the expected
#' data type. Setters functions convert value to expected data type and
#' store it in respective fields on a given object. If value has no length
#' it is removed from the object.
#'
#' `.lon()` and `.lat()` get/set, respectively, `"longitude"`
#' and `"latitude"` fields.
#'
#' @param x Object to get/set field value.
#' @param value Value to set on object field.
#'
#' @examples
#' if (sits_run_examples()) {
#'     x <- c(longitude = "123")
#'     .lon(x) # 123 as number
#'     x <- list(longitude = 1:10)
#'     .lat(x) <- 11:20
#'     x # with 'longitude' and 'latitude' fields
#' }
#'
NULL

#' @title Get \code{'longitude'} field.
#' @noRd
.lon <- function(x) {
    .as_dbl(.compact(x[["longitude"]]))
}

#' @title Set \code{'longitude'} field as numeric.
#' @noRd
`.lon<-` <- function(x, value) {
    x[["longitude"]] <- .as_dbl(value)
    x
}

#' @title Get \code{'latitude'} field.
#' @noRd
.lat <- function(x) {
    .as_dbl(.compact(x[["latitude"]]))
}

#' @title Set \code{'latitude'} field as numeric.
#' @noRd
`.lat<-` <- function(x, value) {
    x[["latitude"]] <- .as_dbl(value)
    x
}

#' @title Point API
#' @noRd
#'
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @description
#' A point represents a dimensionless geographical location in a given
#' projection. A \code{point} is any \code{list} or \code{tibble}
#' containing \code{longitude} and \code{latitude} fields. A \code{point} may
#' contains multiple entries.
#'
#' @param x Any object to extract a \code{point}.
#' @param ... Additional parameters.
#' @param point A \code{point}.
#' @param crs The point CRS. If not informed, default CRS is \code{'EPSG:4326'}.
#' @param as_crs A CRS to project \code{point}.
#'
#' @examples
#' if (sits_run_examples()) {
#'     x <- list(a = 0, z = 0)
#'     .point(x) # NULL
#'     x <- list(a = 0, longitude = 1:3, b = 2:4, latitude = 2, z = 0)
#'     .point(x)
#'     .point_as_sf(x) # 3 features
#'     .point_as_sf(x, as_crs = "EPSG:3857") # reprojected features
#' }
NULL

# point fields
.point_cols <- c("longitude", "latitude")

#' @title Does vector \code{x} has \code{point} fields?
#' @returns \code{.has_point()}: \code{logical}.
#' @noRd
.has_point <- function(x) {
    all(.point_cols %in% names(x))
}

#' @title Is vector \code{x} a \code{point} object?
#' @returns \code{.is_point()}: \code{logical}.
#' @noRd
.is_point <- function(x) {
    setequal(names(x), c(.point_cols, "crs"))
}

#' @title Extract a \code{point} from any given \code{vector}.
#' @returns \code{.point()}: \code{point}.
#' @noRd
.point <- function(x, crs = NULL, as_crs = NULL) {
    if (!.has_point(x)) {
        return(NULL)
    }
    if (.has_not(crs)) crs <- "EPSG:4326"
    # Create point
    point <- .common_size(longitude = .lon(x), latitude = .lat(x), crs = crs)
    # Project to CRS
    if (.has(as_crs)) {
        point <- .point_as_sf(point = point, as_crs = as_crs)
    }
    # Return point
    point
}

#' @title Convert a \code{point} into a \code{sf} point object.
#' @returns \code{.point_as_sf()}: \code{sf}.
#' @noRd
.point_as_sf <- function(point, as_crs = NULL) {
    # Check for valid point
    .check_point(point)
    # Convert to sf object and return it
    geom <- sf::st_as_sf(
        x = point,
        coords = c("longitude", "latitude"),
        crs = .crs(point)
    )
    # Project CRS
    if (.has(as_crs)) {
        geom <- sf::st_transform(geom, crs = as_crs)
    }
    # Return geom
    geom
}
