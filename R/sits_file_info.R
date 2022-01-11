#' @title Functions to work with file info tibble
#' @name file_info_functions
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param  cube         input data cube
#' @param  bands        bands to be filtered
#'
#' @return vector with requested information
NULL

#' @rdname file_info_functions
#'
#' @details
#' Return the file info for a cube with a single tile
#' Filter by bands if required
#'
.file_info <- function(cube, bands = NULL) {

    # pre-condition - one tile at a time
    .check_num(nrow(cube), min = 1, max = 1, is_integer = TRUE,
               msg = "process one tile at a time for file_info")

    # get the file info associated with the tile
    file_info <- cube$file_info[[1]]

    # check bands
    if (!is.null(bands)) {
        .cube_bands_check(cube, bands = bands)
        file_info <- file_info[file_info$band %in% bands, ]
    }

    return(file_info)
}
#' @rdname file_info_functions
#'
#' @details
#' Return number of rows for a given tile
#' Throws an error if rows are not equal
#'
.file_info_nrows <- function(cube){

    file_info <- .file_info(cube)
    nrows <- unique(file_info[["nrows"]])

    .check_num(length(nrows), min = 1, max = 1, is_integer = TRUE,
               msg = "wrong nrows parameter in file_info")
    return(nrows)

}
#' @rdname file_info_functions
#'
#' @details
#' Returns number of cols for a given tile
#' Throws an error if cols are not equal
.file_info_ncols <- function(cube){

    file_info <- .file_info(cube)
    ncols <- unique(file_info[["ncols"]])

    .check_num(length(ncols), min = 1, max = 1, is_integer = TRUE,
               msg = "wrong ncols parameter in file_info")
    return(ncols)
}
#' @rdname file_info_functions
#'
#' @details
#' Returns a single path to a file
#' Throws an error if there is more than one path
.file_info_path <- function(cube){

    file_info <- .file_info(cube)
    path <- file_info$path[[1]]
    .check_num(length(path), min = 1, max = 1, is_integer = TRUE,
               msg = "wrong path parameter in file_info")

    return(path)
}
#' @rdname file_info_functions
#'
#' @details
#' Returns the X resolution for a single tiled cube
#' Throws an error if resolution is not unique
.file_info_xres <- function(cube){

    file_info <- .file_info(cube)
    xres <- unique(file_info[["xres"]])

    .check_num(length(xres), min = 1, is_integer = TRUE,
               msg = "wrong xres parameter in file_info")
    return(xres)
}
#' @rdname file_info_functions
#'
#' @details
#' Returns the Y resolution for a single tiled cube
#' Throws an error if resolution is not unique
.file_info_yres <- function(cube){

    file_info <- .file_info(cube)
    yres <- unique(file_info[["yres"]])

    .check_num(length(yres), min = 1, is_integer = TRUE,
               msg = "wrong yres parameter in file_info")
    return(yres)
}
#' @rdname file_info_functions
#'
#' @details
#' Returns the file ids for a single tiled cube
.file_info_fids <- function(cube){

    file_info <- .file_info(cube)
    fids <- unique(file_info[["fid"]])

    .check_num(length(fids), min = 1, is_integer = TRUE,
               msg = "wrong fid parameter in file_info")
    return(fids)
}
#' @rdname file_info_functions
#'
#' @details
#' Returns timeline  for a single tiled cube
.file_info_timeline <- function(cube){

    file_info <- .file_info(cube)
    timeline <- unique(lubridate::as_date(file_info$date))

    .check_num(length(timeline), min = 1, is_integer = TRUE,
               msg = "wrong timeline parameter in file_info")
    return(timeline)
}
#' @rdname file_info_functions
#'
#' @details
#' Returns timeline  for a single tiled cube
.file_info_timeline_wtss <- function(cube){

    file_info <- .file_info(cube)
    timeline <- unique(lubridate::as_date(file_info$date[[1]]))

    .check_num(length(timeline), min = 1, is_integer = TRUE,
               msg = "wrong timeline parameter in file_info")
    return(timeline)
}
#' @rdname file_info_functions
#'
#' @details
#' Returns start date  for a single tiled cube
#' Throws an error if cube is not a processed one
.file_info_start_date <- function(cube){

    .check_chr_contains(
        x = class(cube),
        contains = .config_get("sits_s3_classes_proc"),
        discriminator = "any_of",
        msg = paste0("Cube is not one of ",.config_get("sits_s3_classes_proc"))
    )

    file_info <- .file_info(cube)

    .check_chr_within(
        x = c("start_date", "end_date"),
        within = colnames(file_info),
        msg = "invalid file_info for cube")

    start_date <- unique(lubridate::as_date(file_info$start_date))

    .check_num(length(start_date), min = 1, max = 1,
               msg = "wrong start_date parameter in file_info")
    return(start_date)
}
#' @rdname file_info_functions
#'
#' @details
#' Returns start date  for a single tiled cube
#' Throws an error if cube is not a processed one
.file_info_end_date <- function(cube){

    .check_chr_contains(
        x = class(cube),
        contains = .config_get("sits_s3_classes_proc"),
        discriminator = "any_of",
        msg = paste0("Cube is not one of ",.config_get("sits_s3_classes_proc"))
    )

    file_info <- .file_info(cube)
    .check_chr_within(
        x = c("start_date", "end_date"),
        within = colnames(file_info),
        msg = "invalid file_info for cube")

    end_date <- unique(lubridate::as_date(file_info$end_date))

    .check_num(length(end_date), min = 1, max = 1,
               msg = "wrong end_date parameter in file_info")
    return(end_date)
}
#' @rdname file_info_functions
#'
#' @details
#' Returns the bands for the cube
#' Throws an error bands are NULL
.file_info_bands <- function(cube){

    file_info <- .file_info(cube)
    bands <- unique(unlist(file_info$band))

    .check_num(length(bands), min = 1, is_integer = TRUE,
               msg = "wrong bands parameter in file_info")

    return(bands)
}
