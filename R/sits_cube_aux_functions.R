#' @title Creates the description of a data cube
#' @name .cube_create
#' @keywords internal
#'
#' @description Print information and save metadata about a data cube.
#'
#' @param source             Source of data
#' @param collection         Image collection
#' @param satellite          Name of satellite
#' @param sensor             Name of sensor
#' @param tile               Tile of the image collection
#' @param xmin               Spatial extent (xmin).
#' @param ymin               Spatial extent (ymin).
#' @param xmax               Spatial extent (xmax).
#' @param ymax               Spatial extent (ymin).
#' @param crs                CRS for cube (EPSG code or PROJ4 string).
#' @param file_info          Tibble with information about files
#'
#' @return  A tibble containing a data cube
#'
.cube_create <- function(source,
                         collection = NA_character_,
                         satellite,
                         sensor,
                         tile = NA_character_,
                         xmin,
                         xmax,
                         ymin,
                         ymax,
                         crs,
                         labels = NULL,
                         file_info = NULL) {


    # create a tibble to store the metadata (mandatory parameters)
    cube <- tibble::tibble(
        source = source,
        collection = collection,
        satellite = satellite,
        sensor = sensor,
        tile  = tile,
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax,
        crs = crs
    )
    # if there are labels, include them
    if (!purrr::is_null(file_info)) {
        cube <- tibble::add_column(cube, labels = list(labels))
    }

    if (!purrr::is_null(file_info)) {
        cube <- tibble::add_column(cube, file_info = list(file_info))
    }

    class(cube) <- unique(c("sits_cube", class(cube)))

    return(cube)
}


#' @title Given a labelled cube, return the band information
#' @name .cube_area_freq
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param cube           Metadata about a data cube
#' @return               Frequency of each label in the data cube
#'
.cube_area_freq <- function(cube) {

    # set caller to show in errors
    .check_set_caller(".cube_area_freq")

    # precondition
    .check_that(
        x = inherits(cube, "classified_image"),
        msg = "requires a labelled cube"
    )

    # retrieve the r object associated to the labelled cube
    file_info <- .cube_file_info(cube)

    # open first raster
    r_obj <- .raster_open_rast(file_info$path[[1]])

    # retrieve the frequency
    freq <- tibble::as_tibble(.raster_freq(r_obj))

    return(freq)
}

#' @rdname cube_functions
.cube_bands <- function(cube, ...,
                        add_cloud = TRUE) {

    bands <- sits_bands(cube)

    # post-condition
    .check_chr(bands, min_len = 1, is_named = FALSE,
               msg = "inconsistent 'bands' information")

    if (!add_cloud)
        bands <- bands[bands != .source_cloud()]

    return(bands)
}

#' @rdname cube_functions
.cube_bands_check <- function(cube, bands, ...,
                              add_cloud = TRUE) {

    # all bands are upper case
    .check_chr_within(bands,
                      within = .cube_bands(cube = cube,
                                                  add_cloud = add_cloud),
                      case_sensitive = FALSE,
                      msg = "invalid 'bands' parameter")

    return(invisible(NULL))
}
#' @title Auxiliary functions for sits cubes
#' @keywords internal
#'
#' @name cube_functions
#'
#' @description Auxiliary functions for extracting information from sits cubes.
#'
#' @param cube  Data cube from where data is to be retrieved.
#' @param tile  Tiles from the collection to be included in the data cube
#' @param bands Bands to be included.
#' @param ....  Additional parameters to be included.
#'
#' @return a \code{vector} for get attributes functions and NULL or error for
#' check parameters functions.
NULL

#' @rdname cube_functions
.cube_band_missing_value <- function(cube, band) {

    # pre-condition
    .check_chr(band, len_min = 1, len_max = 1,
               msg = "invalid 'band' parameter")

    .check_chr_within(band,
                      within = .cube_bands(cube = cube, add_cloud = FALSE),
                      discriminator = "one_of",
                      case_sensitive = FALSE,
                      msg = "invalid 'band' parameter")

    # bands names are upper case
    band <- toupper(band)

    mv <- .config_get(key = c("sources", .cube_source(cube = cube),
                               "collections", .cube_collection(cube = cube),
                               "bands", band, "missing_value"))

    # post-condition
    .check_num(mv, len_min = 1, len_max = 1,
               msg = "invalid 'missing_value' value")

    return(mv)
}

#' @rdname cube_functions
.cube_band_minimum_value <- function(cube, band) {

    # pre-condition
    .check_chr(band, len_min = 1, len_max = 1,
               msg = "invalid 'band' parameter")

    .check_chr_within(band,
                      within = .cube_bands(cube = cube, add_cloud = FALSE),
                      discriminator = "one_of",
                      case_sensitive = FALSE,
                      msg = "invalid 'band' parameter")

    # bands names are upper case
    band <- toupper(band)

    mv <- .config_get(key = c("sources", .cube_source(cube = cube),
                               "collections", .cube_collection(cube = cube),
                               "bands", band, "minimum_value"))

    # post-condition
    .check_num(mv, len_min = 1, len_max = 1,
               msg = "invalid 'minimum_value' value")

    return(mv)
}

#' @rdname cube_functions
.cube_band_maximum_value <- function(cube, band) {

    # pre-condition
    .check_chr(band, len_min = 1, len_max = 1,
               msg = "invalid 'band' parameter")

    .check_chr_within(band,
                      within = .cube_bands(cube = cube, add_cloud = FALSE),
                      discriminator = "one_of",
                      case_sensitive = FALSE,
                      msg = "invalid 'band' parameter")

    # bands names are upper case
    band <- toupper(band)

    mv <- .config_get(key = c("sources", .cube_source(cube = cube),
                               "collections", .cube_collection(cube = cube),
                               "bands", band, "maximum_value"))

    # post-condition
    .check_num(mv, len_min = 1, len_max = 1,
               msg = "invalid 'maximum_value' value")

    return(mv)
}

#' @rdname cube_functions
.cube_band_scale_factor <- function(cube, band) {

    # pre-condition
    .check_chr(band, len_min = 1, len_max = 1,
               msg = "invalid 'band' parameter")

    .check_chr_within(band,
                      within = .cube_bands(cube = cube, add_cloud = FALSE),
                      discriminator = "one_of",
                      case_sensitive = FALSE,
                      msg = "invalid 'band' parameter")

    # bands names are upper case
    band <- toupper(band)

    sf <- .config_get(key = c("sources", .cube_source(cube = cube),
                               "collections", .cube_collection(cube = cube),
                               "bands", band, "scale_factor"))

    # post-condition
    .check_num(sf, allow_zero = FALSE, len_min = 1, len_max = 1,
               msg = "invalid 'scale_factor' value")

    return(sf)
}

#' @rdname cube_functions
.cube_band_offset_value <- function(cube, band) {

    # pre-condition
    .check_chr(band, len_min = 1, len_max = 1,
               msg = "invalid 'band' parameter")

    .check_chr_within(band,
                      within = .cube_bands(cube = cube, add_cloud = FALSE),
                      discriminator = "one_of",
                      case_sensitive = FALSE,
                      msg = "invalid 'band' parameter")

    # bands names are upper case
    band <- toupper(band)

    ov <- .config_get(key = c("sources", .cube_source(cube = cube),
                               "collections", .cube_collection(cube = cube),
                               "bands", band, "offset_value"))

    # post-condition
    .check_num(ov, len_min = 1, len_max = 1,
               msg = "invalid 'offset_value' value")

    return(ov)
}

#' @rdname cube_functions
.cube_band_resolution <- function(cube, band) {

    # pre-condition
    .check_chr(band, len_min = 1, len_max = 1,
               msg = "invalid 'band' parameter")

    .check_chr_within(band,
                      within = .cube_bands(cube = cube, add_cloud = TRUE),
                      discriminator = "one_of",
                      case_sensitive = FALSE,
                      msg = "invalid 'band' parameter")

    # bands names are upper case
    band <- toupper(band)

    res_band <- .config_get(key = c("sources", .cube_source(cube = cube),
                                    "collections", .cube_collection(cube = cube),
                                    "bands", band, "resolution"))

    # post-condition
    .check_num(res_band, min = 0, allow_zero = FALSE, len_min = 1,
               msg = "invalid 'resolution' value")

    return(res_band)
}
#' @rdname cube_functions
.cube_check <- function(cube) {

    return(inherits(cube, "sits_cube"))
}

#' @rdname cube_functions
.cube_collection <- function(cube) {

    col <- unique(cube[["collection"]])

    # post-condition
    .check_chr(col, allow_empty = FALSE, len_min = 1, len_max = 1,
               msg = "invalid collection value")

    return(col)
}


#' @title Given a band, return a set of values for chosen location
#' @name .cube_extract
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description          Given a data cube, retrieve the time series
#'                       of XY locations
#'
#' @param cube           Metadata about a data cube
#' @param band_cube      Name of the band to the retrieved
#' @param xy             Matrix with XY location
.cube_extract <- function(cube, band_cube, xy) {


    # set caller to show in errors
    .check_set_caller(".cube_extract")

    # precondition 2
    .check_chr_within(
        x = band_cube,
        within = sits_bands(cube),
        discriminator = "one_of",
        msg = paste("band", band_cube,
                    "is not available in the cube")
    )

    # filter the files that contain the band
    band <- dplyr::filter(cube$file_info[[1]], band == band_cube)

    # create a stack object
    r_obj <- .raster_open_stack(band$path)

    # extract the values
    values <- .raster_extract(r_obj, xy)

    # is the data valid?
    .check_that(
        x = nrow(values) == nrow(xy),
        msg = "error in retrieving data"
    )
    return(values)
}
#' @title Get cube file info
#' @name .cube_file_info
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param  cube         input data cube
#' @param  bands        bands to be filtered
#'
#' @return A file_info tibble
.cube_file_info <- function(cube, bands = NULL) {

    # check bands
    if (!is.null(bands))
        .cube_bands_check(cube, bands = bands)

    # get the first tile
    file_info <- cube$file_info[[1]]

    # return filter
    if (!purrr::is_null(bands))
        file_info <- file_info[file_info$band %in% bands, ]

    return(file_info)
}

#' @rdname cube_functions
.cube_has_cloud <- function(cube) {

    .source_cloud() %in% .cube_bands(cube = cube, add_cloud = TRUE)
}

#' @rdname cube_functions
#'
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description          verify is the cube is a regular one
#'
.cube_is_regular <- function(cube){

    # check if the resolutions are unique
    res_cube.lst <- slider::slide(cube, function(row){
       return(unique(.cube_file_info(row)[["res"]]))
    })
    if (length(unique(unlist(res_cube.lst))) != 1)
        return(FALSE)

    # check if timelines are unique
    timelines <- slider::slide(cube, function(row){
        return(sits_timeline(row))
    })
    # function to test timelines
    all_same <- function(x) length(unique(x)) == 1
    return(all_same(timelines))
}

#' @rdname cube_functions
.cube_labels <- function(cube) {

    labs <- unique(cube[["labels"]])

    # post-condition
    .check_lst(labs, min_len = 1, max_len = 1,
               is_named = FALSE,
               msg = "invalid cube 'labels' value")

    labs <- unlist(labs, use.names = FALSE)

    # post-condition
    .check_chr(labs, allow_na = TRUE, allow_empty = FALSE,
               len_min = 1, allow_null = TRUE,
               msg = "invalid cube 'labels' value")

    return(labs)
}
#' @title Determine the block spatial parameters of a given cube
#' @name .cube_params_block
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Based on the R object associated to a raster object,
#'                 determine its parameters
#' @param cube     A valid cube
#' @param block    A block insider the cube
#' @return A tibble with the cube parameters
.cube_params_block <- function(cube, block) {

    res <- .cube_resolution(cube)
    # compute new Y extent
    ymax  <-  cube$ymax - (block[["first_row"]] - 1) * res
    ymin  <-  ymax - block[["nrows"]] * res

    # compute new X extent
    xmin  <-  cube$xmin + (block[["first_col"]] - 1) * res
    xmax  <-  xmin + block[["ncols"]] * res

    # prepare result
    params <- tibble::tibble(
        nrows = block[["nrows"]],
        ncols = block[["ncols"]],
        xmin  = xmin,
        xmax  = xmax,
        ymin  = ymin,
        ymax  = ymax,
        crs   = cube$crs
    )

    return(params)
}
#' @title Create a set of RasterLayer objects to store
#' data cube classification results (only the probs)
#' @name .cube_probs_create
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Take a tibble containing metadata about a data cube
#' containing time series and create a
#' set of RasterLayers to store the classification result.
#' Each RasterLayer corresponds to one time step.
#' The time steps are specified in a list of dates.
#'
#' @param  tile              input tile (subset of a data cube).
#' @param  samples           samples used for training the classification model.
#' @param  sub_image         bounding box of the ROI
#' @param  output_dir        prefix of the output files.
#' @param  version           version of the output files
#' @return                   output data cube
#'
.cube_probs_create <- function(tile, samples, sub_image,
                               output_dir, version) {

    # set caller to show in errors
    .check_set_caller(".cube_probs_create")

    # ensure metadata tibble exists
    .check_that(x = nrow(tile) == 1,
                msg = "accepts only one tile at a time")

    # get the timeline of of the data cube
    timeline <- lubridate::as_date(sits_timeline(tile))
    start_date <- as.Date(timeline[1])
    end_date <- as.Date(timeline[length(timeline)])

    # labels come from samples
    labels <- sits_labels(samples)

    # copy the cube information
    probs_cube <- tile

    # output filename
    file_name <- paste0(output_dir, "/",
                        tile$satellite, "_",
                        tile$sensor,"_",
                        tile$tile,"_",
                        start_date,"_",
                        end_date,"_",
                        "probs", "_",
                        version, ".tif")

    # set the file information
    file_info <- tibble::tibble(
        band       = "probs",
        res        =  .cube_resolution(tile),
        start_date = start_date,
        end_date   = end_date,
        path       = file_name
    )

    # set the metadata for the probability cube
    probs_cube <- .cube_create(
        source     = tile$source,
        collection = tile$collection,
        satellite  = tile$satellite,
        sensor     = tile$sensor,
        tile       = tile$tile,
        xmin       = sub_image[["xmin"]],
        xmax       = sub_image[["xmax"]],
        ymin       = sub_image[["ymin"]],
        ymax       = sub_image[["ymax"]],
        crs        = tile$crs,
        labels     = labels,
        file_info  = file_info
    )

    class(probs_cube) <- c("probs_cube", "raster_cube", class(probs_cube))
    return(probs_cube)
}
#' @title Clone a probs or label cube with a new extension
#' @name .cube_probs_label
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  cube              input data cube
#' @param  ext               file extension
#' @param  output_dir        prefix of the output files
#' @param  version           version of the output files
#' @return                   output data cube
.cube_probs_label <- function(cube, ext, output_dir, version) {

    # copy the cube information
    cube_clone <- cube

    file_info_in <- .cube_file_info(cube)

    cube_clone$file_info <- file_info_in %>%
        slider::slide(function(row) {
            tibble::tibble(
                band = toupper(ext),
                start_date = row$start_date,
                end_date   = row$end_date,
                res        = .cube_resolution(cube),
                path = paste0(output_dir, "/",
                              cube$satellite[[1]], "_",
                              cube$sensor[[1]],"_",
                              cube$tile[[1]],"_",
                              row$start_date,"_",
                              row$end_date,"_",
                              ext, "_",
                              version, ".tif")
            )
        })

    return(cube_clone)
}
#' @rdname cube_functions
.cube_resolution <- function(cube, ..., bands = NULL) {

    file_info <- .cube_file_info(cube, bands)
    res <- unique(file_info$res)

    # post-condition
    .check_length(res,
                  len_min = 1,
                  len_max = 1,
                  msg = "Cube has bands with more than one resolution")

    return(res)
}

#' @rdname cube_functions
.cube_s3class <- function(cube) {

    unique(c(.source_s3class(source = .cube_source(cube = cube)),
             class(cube)))
}

#' @rdname cube_functions
.cube_size <- function(cube, ..., bands = NULL) {
    # get the file information
    file_info <- .cube_file_info(cube, bands)
    # get the file resolution
    res <- unique(file_info$res)
    # post-condition
    .check_length(res,
                  len_min = 1, len_max = 1,
                  msg = "cube has bands of different sizes")

    # obtain the size by querying the raster data
    r_obj <- .raster_open_rast(file_info$path[[1]])

    size <- c(
        nrows = .raster_nrows(r_obj),
        ncols = .raster_ncols(r_obj)
    )
    .check_num(size[["nrows"]], min = 1, allow_null = FALSE,
               is_integer = TRUE, msg = "Invalid number of rows")
    .check_num(size[["ncols"]], min = 1, allow_null = FALSE,
               is_integer = TRUE, msg = "Invalid number of columns")

    return(size)
}
#' @rdname cube_functions
.cube_size_max <- function(cube, ..., bands = NULL) {
    # get the file information
    file_info <- .cube_file_info(cube, bands)
    # get the file resolution
    res_min <- min(file_info$res)

    # select only files with the smallest resolution
    file_info <- dplyr::filter(file_info, res == res_min)

    # obtain the size by querying the raster data
    r_obj <- .raster_open_rast(file_info$path[[1]])

    size <- c(
        nrows = .raster_nrows(r_obj),
        ncols = .raster_ncols(r_obj)
    )
    .check_num(size[["nrows"]], min = 1, allow_null = FALSE,
               is_integer = TRUE, msg = "Invalid number of rows")
    .check_num(size[["ncols"]], min = 1, allow_null = FALSE,
               is_integer = TRUE, msg = "Invalid number of columns")

    return(size)
}
#' @title Get cube source
#' @name .cube_source
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param  cube input data cube
#'
#' @return A character string
.cube_source <- function(cube) {

    # set caller to show in errors
    .check_set_caller(".cube_source")

    src <- unique(cube$source)

    .check_length(x = src,
                  len_max = 1,
                  msg = "cube has different sources.")

    return(src)
}


#' @rdname cube_functions
.cube_timeline <- function(cube) {

    timeline <- unique(cube[["file_info"]][["date"]])

    # post-condition
    # check if all tiles have same timeline
    .check_lst(timeline, min_len = 1, max_len = 1,
               msg = "invalid cube timeline values")

    # simplify
    timeline <- unlist(timeline)

    return(timeline)
}

#' @rdname cube_functions
.cube_tiles <- function(cube) {

    tiles <- unique(cube[["tile"]])

    # post-condition
    .check_chr(tiles, allow_empty = FALSE, len_min = nrow(cube),
               len_max = nrow(cube),
               msg = "invalid cube 'tile' values")

    return(tiles)
}

#' @rdname cube_functions
.cube_tile_check <- function(cube, tile) {

    .check_that(
        is.numeric(tile) || is.character(tile),
        local_msg = "'tile' parameter must be numeric or character"
    )

    .check_length(tile, len_min = 1, len_max = nrow(cube),
                  "invalid 'tile' parameter")

    if (is.numeric(tile)) {
        .check_num(tile, min = 1, max = nrow(cube),
                   len_min = 1, is_integer = TRUE,
                   msg = "invalid 'tile' parameter"
        )
    } else if (is.character(tile)) {
        .check_chr_within(tile,
                          within = .cube_tiles(cube = cube),
                          discriminator = "one_of",
                          msg = "invalid 'tile' parameter"
        )
    }

    return(invisible(tile))
}

#' @rdname cube_functions
.cube_tile_get_fields <- function(cube, tile, fields = NULL) {

    # pre-condition
    .cube_tile_check(cube = cube, tile = tile)

    .check_chr_within(fields,
                      within = names(cube),
                      discriminator = "any_of",
                      msg = "invalid 'fields' parameter")

    if (is.numeric(tile))
        result <- c(cube[tile, fields])
    else
        result <- c(cube[which(.cube_tiles(cube = cube) %in% tile), fields])

    # post-condition
    .check_lst(result, min_len = length(fields), max_len = length(fields),
               msg = "invalid 'fields' parameter")

    return(result)
}

#' @rdname cube_functions
.cube_tile_crs <- function(cube, ...,
                           tile = 1) {

    crs <- .cube_tile_get_fields(cube = cube,  tile = tile, fields = "crs")

    # simplify
    crs <- unlist(res, use.names = FALSE)

    # post-condition
    .check_chr(crs, allow_empty = FALSE, len_min = 1, len_max = 1,
               "invalid tile 'crs' value")

    return(crs)
}

#' @rdname cube_functions
.cube_tile_bbox <- function(cube, ...,
                            tile = 1) {

    bbox <- .cube_tile_get_fields(cube = cube, tile = tile,
                                 fields = c("xmin", "ymin", "xmax", "ymax"))

    # post-condition
    .check_lst(bbox, min_len = 4, max_len = 4, fn_check = .check_num,
               len_min = 1, len_max = 1,
               msg = "invalid tile 'bbox' value")

    return(bbox)
}



