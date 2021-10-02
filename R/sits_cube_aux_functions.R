#' @title Creates the description of a data cube
#' @name .sits_cube_create
#' @keywords internal
#'
#' @description Print information and save metadata about a data cube.
#'
#' @param name               Name of the data cube (mandatory)
#' @param source             Source of data
#' @param collection         Image collection
#' @param satellite          Name of satellite
#' @param sensor             Name of sensor
#' @param tile               Tile of the image collection
#' @param bands              Vector with the names of the bands.
#' @param labels             Vector with labels (only for classified data).
#' @param nrows              Number of rows in the cube.
#' @param ncols              Number of columns in the cube.
#' @param xmin               Spatial extent (xmin).
#' @param ymin               Spatial extent (ymin).
#' @param xmax               Spatial extent (xmax).
#' @param ymax               Spatial extent (ymin).
#' @param xres               Spatial resolution (x dimension).
#' @param yres               Spatial resolution (y dimension).
#' @param crs                CRS for cube (EPSG code or PROJ4 string).
#' @param file_info          Tibble with information about stacks (for stacks)
#'
#' @return  A tibble containing a data cube
#'
.sits_cube_create <- function(name,
                              source,
                              collection = NA,
                              satellite,
                              sensor,
                              tile = NA,
                              bands,
                              labels = NA,
                              nrows,
                              ncols,
                              xmin,
                              xmax,
                              ymin,
                              ymax,
                              xres,
                              yres,
                              crs,
                              file_info = NULL) {


    # create a tibble to store the metadata (mandatory parameters)
    cube <- tibble::tibble(
        name = name,
        source = source,
        collection = collection,
        satellite = satellite,
        sensor = sensor,
        tile  = tile,
        bands = list(bands),
        labels = list(labels),
        nrows = nrows,
        ncols = ncols,
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax,
        xres = xres,
        yres = yres,
        crs = crs
    )

    if (!purrr::is_null(file_info)) {
          cube <- tibble::add_column(cube, file_info = list(file_info))
      }

    return(cube)
}



#' @title Create a set of RasterLayer objects to store
#' data cube classification results (only the probs)
#' @name .sits_cube_probs
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
.sits_cube_probs <- function(tile, samples, sub_image,
                             output_dir, version) {

    # set caller to show in errors
    .check_set_caller(".sits_cube_probs")

    # ensure metadata tibble exists
    .check_that(x = nrow(tile) == 1,
                msg = "accepts only one tile at a time")

    # set the name of the output cube
    name <- paste0(tile$name, "_probs")

    # get the timeline of of the data cube
    timeline <- lubridate::as_date(sits_timeline(tile))
    start_date <- as.Date(timeline[1])
    end_date <- as.Date(timeline[length(timeline)])

    # labels come from samples
    labels <- sits_labels(samples)

    # define the file names for the classified images
    file_name <- paste0(output_dir, "/", name, "_",
                        start_date, "_", end_date, "_", version, ".tif"
    )

    # define the band name
    band <- "PROBS"
    # get the file information
    file_info <- tibble::tibble(
        band = band,
        start_date = start_date,
        end_date = end_date,
        path = file_name
    )

    # set the metadata for the probability cube
    probs_cube <- .sits_cube_create(
        name = name,
        source = "PROBS",
        collection = "PROBS",
        satellite = tile$satellite,
        sensor = tile$sensor,
        bands = band,
        labels = labels,
        nrows = unname(sub_image["nrows"]),
        ncols = unname(sub_image["ncols"]),
        xmin = unname(sub_image["xmin"]),
        xmax = unname(sub_image["xmax"]),
        ymin = unname(sub_image["ymin"]),
        ymax = unname(sub_image["ymax"]),
        xres = tile$xres,
        yres = tile$yres,
        crs = tile$crs,
        file_info = file_info
    )

    class(probs_cube) <- .cube_s3class(probs_cube)
    return(probs_cube)
}

#' @title Clone a data cube
#' @name .sits_cube_clone
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  cube              input data cube
#' @param  name              name of the new cube
#' @param  ext               file extension
#' @param  output_dir        prefix of the output files
#' @param  version           version of the output files
#' @return                   output data cube
.sits_cube_clone <- function(cube, name, ext, output_dir, version) {

    # copy the cube information
    cube_clone <- cube
    cube_clone$name <- name

    # update the cube information
    cube_clone$file_info <-
        purrr::map(seq_along(cube_clone$file_info), function(i) {

            newb <- paste0(cube_clone$file_info[[i]]$band, ext)
            newp <- paste0(output_dir, "/", name[[i]], "_", newb,
                           "_", version, ".tif")
            cube_clone$file_info[[i]]$band <- newb
            cube_clone$file_info[[i]]$path <- newp

            return(cube_clone$file_info[[i]])
        })

    class(cube_clone) <- class(cube)
    return(cube_clone)
}
#' @title Get cube source
#' @name .sits_cube_source
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param  cube input data cube
#'
#' @return A character string
.sits_cube_source <- function(cube) {

    # set caller to show in errors
    .check_set_caller(".sits_cube_source")

    res <- unique(cube$source)

    .check_length(x = res,
                  len_max = 1,
                  msg = "cube has different sources.")

    return(res)
}

#' @title Get cube file info
#' @name .sits_cube_file_info
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param  cube         input data cube
#' @param  bands        bands to be filtered
#'
#' @return A file_info tibble
.sits_cube_file_info <- function(cube, bands = NULL) {

    # check bands
    if (!is.null(bands))
        .cube_bands_check(cube, bands = bands)

    # get the first tile
    file_info <- cube$file_info[[1]]

    # return filter
    return(file_info[file_info$band %in% bands, ])
}
#' @title Fix cube name uniqueness
#' @name .sits_cube_fix_name
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param  cube         input data cube
#'
#' @return A data cube
.sits_cube_fix_name <- function(cube) {

    # set caller to show in errors
    .check_set_caller(".sits_cube_fix_name")

    # check if each tile (row) in cube
    if (nrow(cube) == 1) {

        return(cube)
    }

    # check for uniqueness of tiles
    if (any(!is.na(cube$tile))) {

        .check_length(
            x = cube$tile,
            len_max = length(unique(cube$tile)),
            len_min = length(unique(cube$tile)),
            msg = "tiles must have unique identifiers")
    }

    # check for uniqueness of cube names
    if (length(cube$name) != length(unique(cube$name))) {

        if (all(!is.na(cube$tile))) {

            cube$name <- paste0(cube$name, "_", cube$tile)
        } else {

            cube$name <- paste0(cube$name, "_",
                                formatC(seq_len(nrow(cube)),
                                        flag = "0",
                                        width = log(nrow(cube), 10) + 1))
        }
    }
    return(cube)
}
