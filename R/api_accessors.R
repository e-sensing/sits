#' @title Bouding box accessors
#' @noRd
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description
#' These functions are accessors of raster data and bbox structures.
#' Getters functions returns the respective field values with the expected
#' data type. Setters functions convert value to expected data type and
#' store it in respective fields on a given object. If value has no length
#' and the vector is not atomic, it is removed from the object.
#'
#' @examples
#' if (sits_run_examples()) {
#'     x <- c(xmax = "123")
#'     .xmax(x) # 123 as number
#'     x <- list(xmin = 1, xmax = 2, ymin = 3, ymax = 4)
#'     .crs(x) <- 4326
#'     x # with 'crs' field
#'     .as_crs(3857) # EPSG:3857
#' }
#' @title Returns the smallest X coordinate
#' @name .xmin
#' @description Returns the smallest X coordinate of a raster data structure
#'              (e.g, data cube, tile, chunk)
#'              If there many rasters inside a cube, return the overall min
#' @noRd
#' @param x    A raster data structure.
#' @returns    Smallest X coord
.xmin <- function(x) {
    .as_dbl(.compact(x[["xmin"]]))
}
#' @title Assigns the smallest X coordinate
#' @name `.xmin<-`
#' @description Assigns the smallest X coordinate of a raster data structure
#'              (e.g, data cube, tile, chunk)
#' @noRd
#' @param x     A raster data structure.
#' @param value A value
#' @returns     New smallest X coord
`.xmin<-` <- function(x, value) {
    x[["xmin"]] <- .as_dbl(value)
    x
}
#' @title Returns the largest X coordinate
#' @name .xmax
#' @description Returns the largest X coordinate of a raster data structure
#'              (e.g, data cube, tile, chunk)
#'              If there many rasters inside a cube, return the overall min
#' @noRd
#' @param x    A raster data structure.
#' @returns    Largest X coord
.xmax <- function(x) {
    .as_dbl(.compact(x[["xmax"]]))
}
#' @title Assigns the largest X coordinate
#' @name `.xmax<-`
#' @description Assigns the largest X coordinate of a raster data structure
#'              (e.g, data cube, tile, chunk)
#' @noRd
#' @param x     A raster data structure.
#' @param value A value
#' @returns     New smallest X coord
`.xmax<-` <- function(x, value) {
    x[["xmax"]] <- .as_dbl(value)
    x
}
#' @title Returns the smallest Y coordinate
#' @name .ymin
#' @description Returns the smallest Y coordinate of a raster data structure
#'              (e.g, data cube, tile, chunk)
#'              If there many rasters inside a cube, return the overall min
#' @noRd
#' @param x    A raster data structure.
#' @returns    Smallest Y coord
.ymin <- function(x) {
    .as_dbl(.compact(x[["ymin"]]))
}
#' @title Assigns the smallest Y coordinate
#' @name `.ymin<-`
#' @description Assigns the smallest Y coordinate of a raster data structure
#'              (e.g, data cube, tile, chunk)
#' @noRd
#' @param x     A raster data structure.
#' @param value A value
#' @returns     New smallest Y coord
`.ymin<-` <- function(x, value) {
    x[["ymin"]] <- .as_dbl(value)
    x
}
#' @title Returns the largest Y coordinate
#' @name .ymax
#' @description Returns the largest Y coordinate of a raster data structure
#'              (e.g, data cube, tile, chunk)
#'              If there many rasters inside a cube, return the overall min
#' @noRd
#' @param x    A raster data structure.
#' @returns    Largest Y coord
.ymax <- function(x) {
    .as_dbl(.compact(x[["ymax"]]))
}
#' @title Assigns the largest Y coordinate
#' @name `.ymax<-`
#' @description Assigns the largest Y coordinate of a raster data structure
#'              (e.g, data cube, tile, chunk)
#' @noRd
#' @param x     A raster data structure.
#' @param value A value
#' @returns     New smallest Y coord
`.ymax<-` <- function(x, value) {
    x[["ymax"]] <- .as_dbl(value)
    x
}
#' @title Transform value to valid CRS
#' @name .as_crs
#' @description Given an input, tries to transform it to a valid CRS
#' @noRd
#' @param x     input value
#' @returns     Valid CRS
.as_crs <- function(x) {
    if (.has(x)) {
        if (is.character(x)) {
            .compact(x)
        } else if (is.numeric(x)) {
            paste0("EPSG:", .compact(x))
        }
    } else {
        stop("invalid crs value")
    }
}
#' @title Return CRS from a spatial data structure
#' @name .crs
#' @description Given an spatial data structure,
#'              tries to obtain a valid CRS
#' @noRd
#' @param x     Raster data structure
#' @returns     Valid CRS
.crs <- function(x) {
    .as_crs(x[["crs"]])
}
#' @title Assigns CRS from a spatial data structure
#' @name `.crs<-`
#' @description Given an spatial data structure and a valid CRS,
#'              assign the CRS to the structure
#' @noRd
#' @param x        Raster data structure
#' @param value    Valid CRS
#' @returns        Updated CRS to the raster data structure
`.crs<-` <- function(x, value) {
    x[["crs"]] <- .as_crs(value)
    x
}
#' @title Resolution accessors
#' @noRd
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description
#' These functions are read-only accessors of chunk fields
#' `.xres()` and `.yres()` computes, respectively, horizontal and vertial
#' spatial resolution from chunks. The values are computed as:
#' * xres = (xmax - xmin) / ncols
#' * yres = (ymax - ymin) / nrows
#'
#' @examples
#' if (sits_run_examples()) {
#'     x <- c(nrows = 100, ymin = 1, ymax = 10)
#'     .yres(x) # 0.09
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     modis_cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6",
#'         data_dir = data_dir,
#'         delim = "_"
#'     )
#'     .xres(.fi(modis_cube))
#'     .yres(.fi(modis_cube))
#' }
#' @title Returns the horizontal resolution
#' @name .xres
#' @description Returns the horizontal resolution of a raster data structure
#'              (e.g, data cube, tile, chunk)
#' @noRd
#' @param x    A raster data structure.
#' @returns    Horizontal resolution
.xres <- function(x) {
    (.xmax(x) - .xmin(x)) / .ncols(x)
}
#' @title Returns the vertical resolution
#' @name .yres
#' @description Returns the vectical resolution of a raster data structure
#'              (e.g, data cube, tile, chunk)
#' @noRd
#' @param x    A raster data structure.
#' @returns    Vertical resolution
.yres <- function(x) {
    (.ymax(x) - .ymin(x)) / .nrows(x)
}
