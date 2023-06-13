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
#' x <- c(xmax = "123")
#' .xmax(x) # 123 as number
#' x <- list(xmin = 1, xmax = 2, ymin = 3, ymax = 4)
#' .crs(x) <- 4326
#' x # with 'crs' field
#' .as_crs(3857) # EPSG:3857
#' }

.xmin <- function(x) {
    .as_dbl(.compact(x[["xmin"]]))
}
`.xmin<-` <- function(x, value) {
    x[["xmin"]] <- .as_dbl(value)
    x
}
.xmax <- function(x) {
    .as_dbl(.compact(x[["xmax"]]))
}
`.xmax<-` <- function(x, value) {
    x[["xmax"]] <- .as_dbl(value)
    x
}
.ymin <- function(x) {
    .as_dbl(.compact(x[["ymin"]]))
}
`.ymin<-` <- function(x, value) {
    x[["ymin"]] <- .as_dbl(value)
    x
}
.ymax <- function(x) {
    .as_dbl(.compact(x[["ymax"]]))
}
`.ymax<-` <- function(x, value) {
    x[["ymax"]] <- .as_dbl(value)
    x
}
.as_crs <- function(x) {
    if (.has(x)) {
        if (is.character(x))
            .compact(x)
        else if (is.numeric(x))
            paste0("EPSG:", .compact(x))
        else if (is.na(x))
            NA_character_
        else
            stop("invalid crs value")
    }
}
.crs <- function(x) {
    .as_crs(x[["crs"]])
}
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
#' x <- c(nrows = 100, ymin = 1, ymax = 10)
#' .yres(x) # 0.09
#' data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#' modis_cube <- sits_cube(
#'   source = "BDC",
#'   collection = "MOD13Q1-6",
#'   data_dir = data_dir,
#'   delim = "_"
#' )
#' .xres(.fi(modis_cube))
#' .yres(.fi(modis_cube))
#' }
NULL

.xres <- function(x) {
    (.xmax(x) - .xmin(x)) / .ncols(x)
}
.yres <- function(x) {
    (.ymax(x) - .ymin(x)) / .nrows(x)
}

#' @title Block accessors
#' @noRd
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description
#' These functions are accessors of block fields in a vector.
#' Getters functions returns the respective field values with the expected
#' data type. Setters functions convert value to expected data type and
#' store it in respective fields on a given object. If value has no length
#' and the vector is not atomic, it is removed from the object.
#'
#' @examples
#' if (sits_run_examples()) {
#' x <- list(nrows = 123)
#' .nrows(x)
#' .ncols(x) <- 234
#' x
#' }
NULL

.col <- function(x) {
    .as_int(.compact(x[["col"]]))
}
`.col<-` <- function(x, value) {
    x[["col"]] <- .as_int(value)
    x
}
.row <- function(x) {
    .as_int(.compact(x[["row"]]))
}
`.row<-` <- function(x, value) {
    x[["row"]] <- .as_int(value)
    x
}
.ncols <- function(x) {
    .as_int(.compact(x[["ncols"]]))
}
`.ncols<-` <- function(x, value) {
    x[["ncols"]] <- .as_int(value)
    x
}
.nrows <- function(x) {
    .as_int(.compact(x[["nrows"]]))
}
`.nrows<-` <- function(x, value) {
    x[["nrows"]] <- .as_int(value)
    x
}
