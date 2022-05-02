#' @title Functions to work with file info tibble
#' @name file_info_functions
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param cube    Input data cube.
#' @param bands   Bands to be filtered
#' @param dates   Dates to be filtered.
#' @param fid     Feature id (fid) to be filtered.
#'
#' @return        Vector with requested information.
NULL

#' @rdname file_info_functions
#'
#' @details
#' Return the file info for a cube with a single tile
#' Filter by bands if required
#'
.file_info <- function(cube,
                       bands = NULL,
                       fid = NULL,
                       start_date = NULL,
                       end_date = NULL) {

    # pre-condition - one tile at a time
    .check_num(nrow(cube),
               min = 1, max = 1, is_integer = TRUE,
               msg = "process one tile at a time for file_info"
    )

    # get the file info associated with the tile
    file_info <- cube[["file_info"]][[1]]

    # check bands
    if (!is.null(bands)) {
        .cube_bands_check(cube, bands = bands)
        file_info <- file_info[file_info[["band"]] %in% bands, ]
    }

    # filter fid
    if (!is.null(fid)) {
        fids <- .file_info_fids(cube)
        .check_chr_within(paste0(fid),
                          within = paste0(fids),
                          msg = "invalid fid value"
        )
        file_info <- file_info[file_info[["fid"]] == fid, ]
    }

    if (!is.null(start_date)) {
        cube_start_date <- sort(.file_info_timeline(cube))[[1]]


        .check_that(start_date >= cube_start_date, msg = "invalid start date")

        file_info <- file_info[file_info[["date"]] >= start_date, ]
    }

    if (!is.null(end_date)) {
        cube_end_date <- sort(.file_info_timeline(cube), decreasing = TRUE)[[1]]

        .check_that(end_date < cube_end_date, msg = "invalid end date")

        file_info <- file_info[file_info[["date"]] < end_date, ]
    }

    if (.is_token_expired(cube)) {
        file_info <- .file_info_token_generator(cube)
    }

    return(file_info)
}

.is_token_expired <- function(cube) {

    file_info <- cube[["file_info"]][[1]]
    n_tries <- min(
        nrow(file_info[["path"]]),
        ceiling(nrow(file_info[["path"]]) * 0.1)
    )

    href_paths <- sample(file_info[["path"]], n_tries)

    is_paths_valid <- purrr::map_lgl(href_paths, function(href) {
        tryCatch({
            .raster_open_rast(href)
            return(TRUE)
        },
        error = function(e) return(FALSE)
        )
    })

    return(all(is_paths_valid))
}

.file_info_token_generator <- function(cube) {

    source_cube <- tolower(.cube_source(cube))
    class(source_cube) <- source_cube

    UseMethod(".file_info_token_generator", cube)
}

.file_info_token_generator.mspc <- function(cube) {

    file_info <- cube[["file_info"]][[1]]

    token_endpoint <- .config_get(c("sources", .cube_source(cube), "token_url"))

    url <- paste0(token_endpoint, "/", tolower(.cube_collection(cube)))

    tryCatch({
        res_content <- httr::content(httr::GET(url), encoding = "UTF-8")
    },
    error = function(e) {
        stop(paste("Request error. %s", e$message))
    })

    #token_parsed <- httr::parse_url(paste0("?", res_content[["token"]]))
    file_info[["path"]] <- purrr::map_chr(file_info[["path"]], function(path) {

        # url_parsed <- httr::parse_url(path)
        # url_parsed[["query"]] <- modifyList(
        #     url_parsed[["query"]],
        #     token_parsed[["query"]]
        # )
        #
        # new_path <- httr::build_url(url_parsed)
        # remove token
        url_parsed <- strsplit(x = path, split = "?", fixed = TRUE)[[1]][[1]]
        paste0(url_parsed, "?", res_content[["token"]])
    })

    file_info
}


.file_info_token_generator.default <- function(cube) {

    # no caso do bdc o usuario remove o token do cubo??
    stop("invalid cube paths")
}

#' @rdname file_info_functions
#'
#' @details
#' Return number of rows for a given tile
#' Throws an error if rows are not equal
#'
.file_info_nrows <- function(cube, bands = NULL) {
    file_info <- .file_info(cube, bands = bands)
    nrows <- unique(file_info[["nrows"]])

    .check_num(length(nrows),
               min = 1, max = 1, is_integer = TRUE,
               msg = "wrong nrows parameter in file_info"
    )
    return(nrows)
}
#' @rdname file_info_functions
#'
#' @details
#' Returns number of cols for a given tile
#' Throws an error if cols are not equal
.file_info_ncols <- function(cube, bands = NULL) {
    file_info <- .file_info(cube, bands = bands)
    ncols <- unique(file_info[["ncols"]])

    .check_num(length(ncols),
               min = 1, max = 1, is_integer = TRUE,
               msg = "wrong ncols parameter in file_info"
    )
    return(ncols)
}
#' @rdname file_info_functions
#'
#' @details
#' Returns a single path to a file
#' Throws an error if there is more than one path
.file_info_path <- function(cube) {
    file_info <- .file_info(cube)
    path <- file_info[["path"]][[1]]
    .check_chr_type(path, msg = "wrong path parameter in file_info")

    return(path)
}

#' @rdname file_info_functions
#'
#' @details
#' Returns a single path to a file
#' Throws an error if there is more than one path
.file_info_paths <- function(cube, bands = NULL) {
    file_info <- .file_info(cube, bands = bands)

    paths <- file_info[["path"]]

    .check_chr_type(paths, msg = "wrong paths type in file_info")

    return(paths)
}

#' @rdname file_info_functions
#'
#' @details
#' Returns the X resolution for a single tiled cube
#' Throws an error if resolution is not unique
.file_info_xres <- function(cube, bands = NULL) {
    file_info <- .file_info(cube, bands = bands)
    xres <- unique(file_info[["xres"]])

    .check_num(length(xres),
               min = 1, is_integer = TRUE,
               msg = "wrong xres in file_info"
    )
    return(xres)
}
#' @rdname file_info_functions
#'
#' @details
#' Returns the Y resolution for a single tiled cube
#' Throws an error if resolution is not unique
.file_info_yres <- function(cube, bands = NULL) {
    file_info <- .file_info(cube, bands = bands)
    yres <- unique(file_info[["yres"]])

    .check_num(length(yres),
               min = 1, is_integer = TRUE,
               msg = "wrong yres in file_info"
    )
    return(yres)
}
#' @rdname file_info_functions
#'
#' @details
#' Returns the file ids for a single tiled cube
.file_info_fids <- function(cube) {
    file_info <- .file_info(cube)
    fids <- unique(file_info[["fid"]])

    .check_num(length(fids),
               min = 1, is_integer = TRUE,
               msg = "wrong fid in file_info"
    )
    return(fids)
}
#' @rdname file_info_functions
#'
#' @details
#' Returns timeline  for a single tiled cube
.file_info_timeline <- function(cube) {
    file_info <- .file_info(cube)
    timeline <- unique(lubridate::as_date(file_info[["date"]]))

    .check_num(length(timeline),
               min = 1, is_integer = TRUE,
               msg = "wrong timeline in file_info"
    )
    return(timeline)
}
#' @rdname file_info_functions
#'
#' @details
#' Returns start date  for a single tiled cube
#' Throws an error if cube is not a processed one
.file_info_start_date <- function(cube) {
    .check_chr_contains(
        x = class(cube),
        contains = .config_get("sits_s3_classes_proc"),
        discriminator = "any_of",
        msg = paste0("Cube is not one of ", .config_get("sits_s3_classes_proc"))
    )

    file_info <- .file_info(cube)

    .check_chr_within(
        x = c("start_date", "end_date"),
        within = colnames(file_info),
        msg = "invalid file_info for cube"
    )

    start_date <- unique(lubridate::as_date(file_info[["start_date"]]))
    .check_num(length(start_date),
               min = 1, max = 1,
               msg = "wrong start_date parameter in file_info"
    )
    return(start_date)
}
#' @rdname file_info_functions
#'
#' @details
#' Returns start date  for a single tiled cube
#' Throws an error if cube is not a processed one
.file_info_end_date <- function(cube) {
    .check_chr_contains(
        x = class(cube),
        contains = .config_get("sits_s3_classes_proc"),
        discriminator = "any_of",
        msg = paste0("Cube is not one of ", .config_get("sits_s3_classes_proc"))
    )

    file_info <- .file_info(cube)
    .check_chr_within(
        x = c("start_date", "end_date"),
        within = colnames(file_info),
        msg = "invalid file_info for cube"
    )

    end_date <- unique(lubridate::as_date(file_info[["end_date"]]))

    .check_num(length(end_date),
               min = 1, max = 1,
               msg = "wrong end_date parameter in file_info"
    )
    return(end_date)
}
#' @rdname file_info_functions
#'
#' @details
#' Returns the bands for the cube
#' Throws an error bands are NULL
.file_info_bands <- function(cube) {
    file_info <- .file_info(cube)
    bands <- unique(unlist(file_info[["band"]]))

    .check_num(length(bands),
               min = 1, is_integer = TRUE,
               msg = "wrong bands parameter in file_info"
    )

    return(bands)
}
