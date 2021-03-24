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
    # ensure metadata tibble exists
    assertthat::assert_that(
        nrow(tile) == 1,
        msg = ".sits_cube_probs: accepts only one tile at a time"
    )

    # set the name of the output cube
    name <- paste0(tile$name,"_probs")

    # get the timeline of of the data cube
    timeline <- lubridate::as_date(sits_timeline(tile))
    start_date = as.Date(timeline[1])
    end_date = as.Date(timeline[length(timeline)])

    # labels come from samples
    labels <- sits_labels(samples)

    # define the file names for the classified images
    file_name <- paste0(output_dir, "/", name, "_",
                        start_date, "_", end_date,"_", version, ".tif"
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
        collection = NA,
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

    class(probs_cube) <- c("probs_cube", "raster_cube", class(probs_cube))
    return(probs_cube)
}

#' @title Return a file associated to a data cube, given an index
#' @name .sits_cube_file
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description     Given a data cube and an index, retrieve the file
#' @param cube      Metadata about a data cube
#' @param index     Index for file to be retrived
#' @return          Name of file
.sits_cube_file <- function(cube, index = 1) {
    assertthat::assert_that(
        index <= length(cube$file_info[[1]]$path),
        msg = ".sits_cube_file: index is out of range"
    )
    return(cube$file_info[[1]]$path[index])
}

#' @title Return all file associated to a data cube
#' @name .sits_cube_files
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Given a data cube and an index, retrieve the files
#' @param cube     Metadata about a data cube
#' @return         Vector of files
.sits_cube_files <- function(cube) {
    return(cube$file_info[[1]]$path)
}


#' @title Return all labels associated to a data cube
#' @name .sits_cube_labels
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Given a data cube, retrieve the ;abels
#' @param cube     Metadata about a data cube
#' @return         Vector of labels
.sits_cube_labels <- function(cube) {
    return(cube$labels[[1]])
}



#' @title Check that the requested bands exist in the cube
#' @name .sits_cube_bands_check
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param cube          Metadata about a data cube
#' @param bands         Requested bands of the data sample
#' @return              Checked bands (cube bands if bands are NULL)
#'
.sits_cube_bands_check <- function(cube, bands = NULL) {
    # check the bands are available
    cb_bands <- sits_bands(cube)
    if (purrr::is_null(bands)) {
          bands <- cb_bands
      } else {
        bands <- toupper(bands)
        assertthat::assert_that(
            all(bands %in% cb_bands),
            msg = paste(".sits_cube_bands_check: bands are not available",
                        "in the cube")
        )
    }
    return(bands)
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
      purrr::map(cube_clone$file_info, function(file_info) {

        newb <- paste0(file_info$band, ext)
        newp <- paste0(output_dir, "/", name, "_", newb, "_", version, ".tif")
        file_info$band <- newb
        file_info$path <- newp

        return(file_info)
      })

    class(cube_clone) <- class(cube)
    return(cube_clone)
}
#' @title Extract a temporal interval from the cube to select
#' only the images to be used for classification
#'
#' @name .sits_cube_sub_interval
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param cube           Metadata about a data cube tile
#' @param samples        Samples used for training
#' @param start_date     Starting date for the classification
#' @param end_date       End date for the classification
#' @return          Name of file
.sits_cube_sub_interval <- function(cube, samples, start_date, end_date){
    assertthat::assert_that(
        nrow(cube) == 1,
        msg = "sits_cube_sub_interval: accepts one tile only")

    # get the cube timeline
    cube_timeline <- sits_timeline(cube)

    # if start and end dates are not provided, use the samples
    if (purrr::is_null(start_date) || purrr::is_null(end_date)) {
        # start date of the first sample
        start_date <- samples$start_date[[1]]
        # end date of the first sample
        end_date <- samples$end_date[[1]]

        message(paste0("No start/end date for classification provided", "\n",
                       "Using period from ", start_date, " to ", end_date, "\n",
                       "inferred from the samples"))
    }
    # verify if the dates are part of the cube timeline
    assertthat::assert_that(
        as.Date(start_date) <= cube_timeline[length(cube_timeline)],
        msg = paste(".sits_cube_sub_interval: start_date is not",
                    "inside the cube timeline")
    )
    assertthat::assert_that(
        as.Date(end_date) >= cube_timeline[1],
        msg = paste(".sits_cube_sub_interval: end_date is not inside the",
                    "cube timeline")
    )
    return(cube)
}
#' @title Update informations of a data cube
#' @name .sits_cube_update_raster_params
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param  cube              input data cube
#' @return                   output data cube
.sits_cube_update_raster_params <- function(cube) {

    res <- slider::slide_dfr(cube, function(tile) {
        layer_obj <- suppressWarnings(
            raster::raster(tile$file_info[[1]]$path[[1]])
        )
        dplyr::mutate(tile,
                      nrows = raster::nrow(layer_obj),
                      ncols = raster::ncol(layer_obj),
                      xmin  = raster::xmin(layer_obj),
                      xmax  = raster::xmax(layer_obj),
                      ymin  = raster::ymin(layer_obj),
                      ymax  = raster::ymax(layer_obj),
                      xres  = raster::xres(layer_obj),
                      yres  = raster::yres(layer_obj))
    })

    return(res)
}
#' @title Get cube source
#' @name .sits_cube_source
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param  cube              input data cube
#'
#' @return A character string
.sits_cube_source <- function(cube) {

    res <- unique(cube$source)

    assertthat::assert_that(
        length(res) == 1,
        msg = ".sits_cube_source: cube has different sources."
    )

    return(res)
}
