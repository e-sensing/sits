#' @title  Summarize data cubes
#' @method summary raster_cube
#' @name summary.raster_cube
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description This is a generic function. Parameters depend on the specific
#' type of input.
#'
#' @param  object      Object of classes "raster_cube".
#' @param  ...         Further specifications for \link{summary}.
#' @param  tile        Tile to be summarized
#' @param  date        Date to be summarized
#' @param  only_stats  Show only the statistics? (TRUE/FALSE)
#' @param  sample_size Number of sample used to build statistics
#'
#' @return A summary of the data cube.
#'
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#'
#' if (sits_run_examples()) {
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6",
#'         data_dir = data_dir
#'     )
#' summary(cube)
#' }
#'
#' @export
summary.raster_cube <- function(
        object, ...,
        tile = object$tile[[1]],
        date = NULL,
        only_stats = FALSE,
        sample_size = 100000
){
    # only one tile at a time
    .check_chr_parameter(tile)
    # is tile inside the cube?
    .check_chr_contains(
        x = object$tile,
        contains = tile,
        case_sensitive = FALSE,
        discriminator = "one_of",
        can_repeat = FALSE,
        msg = "tile is not included in the cube"
    )
    # filter the tile to be processed
    tile <- .cube_filter_tiles(cube = object, tiles = tile)
    if (purrr::is_null(date))
        date <- .tile_timeline(tile)[[1]]
    # only one date at a time
    .check_that(length(date) == 1,
                msg = "only one date per plot is allowed")
    # is this a valid date?
    date <- as.Date(date)
    .check_that(date %in% .tile_timeline(tile),
                msg = "date is not contained in the cube timeline")

    # extract the chosen date
    tile <- .tile_filter_dates(tile, dates = date)
    # get the bands
    bands <- sits_bands(tile)
    if ("CLOUD" %in% bands)
        tile <- .tile_filter_bands(tile, bands[bands != "CLOUD"])
    # extract the file paths
    files <- .tile_paths(tile)

    # read the files with terra
    r <- terra::rast(files)
    # print the base information (if requested)
    if (!only_stats) {
        cat("class       : ", class(tile)[1], "\n")
        cat("dimensions  : ",
            .tile_nrows(tile), ", ",
            .tile_ncols(tile), "  (nrow, ncol)\n", sep = "" )
        cat("resolution  : ",
            .tile_xres(tile), ", ",
            .tile_yres(tile), "  (x, y)\n", sep = "" )
        cat("extent      : ",
            .xmin(tile), ", ",
            .xmax(tile), ", ",
            .ymin(tile), ", ",
            .ymax(tile),
            "  (xmin, xmax, ymin, ymax)\n", sep = "" )
        cat("coord ref   : ", terra:::.name_or_proj4(r), "\n")
    }
    # get the a sample of the values
    values <- r %>%
        terra::spatSample(size = sample_size, na.rm = TRUE) %>%
        tibble::as_tibble()
    # set names as the band names
    bands <- bands[bands != "CLOUD"]
    # extract statistics
    band_conf <- .tile_band_conf(tile, bands[[1]])
    scale <- .scale(band_conf)
    offset <- .offset(band_conf)
    # rescale values
    # get summary
    sum <- summary(values*scale + offset)
    # change names
    colnames(sum) <- bands
    # print statistics
    d <- as.character(lubridate::as_date(date))
    cat("Statistics for tile ", tile$tile, " date ", d, "\n")
    sum
}
#' @title  Summarize data cubes
#' @method summary probs_cube
#' @name summary.probs_cube
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description This is a generic function. Parameters depend on the specific
#' type of input.
#' @param  object    Object of class "probs_cube"
#' @param ...        Further specifications for \link{summary}.
#' @param  only_stats  Show only the statistics? (TRUE/FALSE)
#' @param  sample_size Number of sample used to build statistics
#'
#' @return A summary of a probability cube
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#'
#' @export
summary.probs_cube <- function(
       object, ...,
       tile = object$tile[[1]],
       only_stats = FALSE,
       sample_size = 100000
){
    # only one tile at a time
    .check_chr_parameter(tile)
    # is tile inside the cube?
    .check_chr_contains(
        x = object$tile,
        contains = tile,
        case_sensitive = FALSE,
        discriminator = "one_of",
        can_repeat = FALSE,
        msg = "tile is not included in the cube"
    )
    # filter the tile to be processed
    tile <- .cube_filter_tiles(cube = object, tiles = tile)
    # get the bands
    band <- sits_bands(tile)
    .check_num(
        x = length(band),
        min = 1,
        max = 1,
        is_integer = TRUE,
        msg = "invalid cube - more than one probs band")
    # extract the file paths
    files <- .tile_paths(tile)

    # read the files with terra
    r <- terra::rast(files)
    # print the base information (if requested)
    if (!only_stats) {
        cat("class       : ", class(tile)[1], "\n")
        cat("dimensions  : ",
            .tile_nrows(tile), ", ",
            .tile_ncols(tile), "  (nrow, ncol)\n", sep = "" )
        cat("resolution  : ",
            .tile_xres(tile), ", ",
            .tile_yres(tile), "  (x, y)\n", sep = "" )
        cat("extent      : ",
            .xmin(tile), ", ",
            .xmax(tile), ", ",
            .ymin(tile), ", ",
            .ymax(tile),
            "  (xmin, xmax, ymin, ymax)\n", sep = "" )
        cat("coord ref   : ", terra:::.name_or_proj4(r), "\n")
    }
    # get the a sample of the values
    values <- r %>%
        terra::spatSample(size = sample_size, na.rm = TRUE)
    # scale the values
    band_conf <- .tile_band_conf(tile, band)
    scale <- .scale(band_conf)
    offset <- .offset(band_conf)
    sum <- summary(values*scale + offset)
    colnames(sum) <- sits_labels(tile)
    cat("Statistics for tile ", tile$tile, "\n")
    sum
}
#' @title  Summarize data cubes
#' @method summary variance_cube
#' @name summary.variance_cube
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description This is a generic function. Parameters depend on the specific
#' type of input.
#' @param  object    Object of class "probs_cube"
#' @param ...        Further specifications for \link{summary}.
#' @param  only_stats  Show only the statistics? (TRUE/FALSE)
#' @param  sample_size Number of sample used to build statistics
#'
#' @return A summary of a probability cube
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#'
#' @export
summary.variance_cube <- function(
        object, ...,
        tile = object$tile[[1]],
        only_stats = FALSE,
        sample_size = 100000
){
    # only one tile at a time
    .check_chr_parameter(tile)
    # is tile inside the cube?
    .check_chr_contains(
        x = object$tile,
        contains = tile,
        case_sensitive = FALSE,
        discriminator = "one_of",
        can_repeat = FALSE,
        msg = "tile is not included in the cube"
    )
    # filter the tile to be processed
    tile <- .cube_filter_tiles(cube = object, tiles = tile)
    # get the bands
    band <- sits_bands(tile)
    .check_num(
        x = length(band),
        min = 1,
        max = 1,
        is_integer = TRUE,
        msg = "invalid cube - more than one probs band")
    # extract the file paths
    files <- .tile_paths(tile)

    # read the files with terra
    r <- terra::rast(files)
    # print the base information (if requested)
    if (!only_stats) {
        cat("class       : ", class(tile)[1], "\n")
        cat("dimensions  : ",
            .tile_nrows(tile), ", ",
            .tile_ncols(tile), "  (nrow, ncol)\n", sep = "" )
        cat("resolution  : ",
            .tile_xres(tile), ", ",
            .tile_yres(tile), "  (x, y)\n", sep = "" )
        cat("extent      : ",
            .xmin(tile), ", ",
            .xmax(tile), ", ",
            .ymin(tile), ", ",
            .ymax(tile),
            "  (xmin, xmax, ymin, ymax)\n", sep = "" )
        cat("coord ref   : ", terra:::.name_or_proj4(r), "\n")
    }
    # get the a sample of the values
    values <- r %>%
        terra::spatSample(size = sample_size, na.rm = TRUE)
    # scale the values
    band_conf <- .tile_band_conf(tile, band)
    scale <- .scale(band_conf)
    offset <- .offset(band_conf)
    sum <- summary(values*scale + offset)
    colnames(sum) <- sits_labels(tile)
    cat("Statistics for tile ", tile$tile, "\n")
    sum
}
#' @title  Summarize data cubes
#' @method summary class_cube
#' @name summary.class_cube
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description This is a generic function. Parameters depend on the specific
#' type of input.
#' @param  object    Object of class "class_cube"
#' @param ...        Further specifications for \link{summary}.
#' @param  only_stats  Show only the statistics? (TRUE/FALSE)
#' @param  sample_size Number of sample used to build statistics
#'
#' @return A summary of a classified cube
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#'
#' @export
#'
summary.class_cube <- function(
        object, ...,
        tile = object$tile[[1]],
        only_stats = FALSE,
        sample_size = 100000
){
    # only one tile at a time
    .check_chr_parameter(tile)
    # is tile inside the cube?
    .check_chr_contains(
        x = object$tile,
        contains = tile,
        case_sensitive = FALSE,
        discriminator = "one_of",
        can_repeat = FALSE,
        msg = "tile is not included in the cube"
    )
    # filter the tile to be processed
    tile <- .cube_filter_tiles(cube = object, tiles = tile)
    # get the bands
    band <- sits_bands(tile)
    .check_num(
        x = length(band),
        min = 1,
        max = 1,
        is_integer = TRUE,
        msg = "invalid cube - more than one class band")
    # extract the file paths
    files <- .tile_paths(tile)

    # read the files with terra
    r <- terra::rast(files)
    # print the base information (if requested)
    if (!only_stats) {
        cat("class       : ", class(tile)[1], "\n")
        cat("dimensions  : ",
            .tile_nrows(tile), ", ",
            .tile_ncols(tile), "  (nrow, ncol)\n", sep = "" )
        cat("resolution  : ",
            .tile_xres(tile), ", ",
            .tile_yres(tile), "  (x, y)\n", sep = "" )
        cat("extent      : ",
            .xmin(tile), ", ",
            .xmax(tile), ", ",
            .ymin(tile), ", ",
            .ymax(tile),
            "  (xmin, xmax, ymin, ymax)\n", sep = "" )
        cat("coord ref   : ", terra:::.name_or_proj4(r), "\n")
    }
    # get the a sample of the values
    class_areas <- r %>%
        terra::expanse(unit = "km", byValue = TRUE)
    # create a tibble
    areas <-  class_areas[,3]
    # get t
    cat("Class Areas for tile ", tile$tile, "\n")
    data.frame(class = sits_labels(tile), area_km2  = signif(areas, 4))
}
