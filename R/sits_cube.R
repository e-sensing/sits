#' @title Defines a data cube
#' @name sits_cube
#'
#' @description Defines a cube to retrieve data. Cubes are associated to
#' data services. The following services are available:
#' \itemize{
#'  \item{"WTSS": }{Web Time Series Service - used to get time series}
#'  \item{"EOCUBES": }{EOCUBES service - used for cloud processing of data cubes}
#'  \item{"RASTER": }{Raster Brick files, local or remote}
#'  \item{"STACK": }{Raster Stack files, local or remote}
#' }
#'
#'
#' @param service           Name of the data service.
#' @param provider          Name of the service provider.
#' @param name              Name of the image collection.
#' @param tiles_names       A string with tile names to be filtered.
#' @param geom              A \code{sfc} object to filter tiles that intersects the given geometry.
#' @param from              A date value to filter cube's layers by date.
#' @param to                A date value to filter cube's layers by date.
#' @param timeline          Vector with the timeline of the collection.
#' @param bands             Vector of bands.
#' @param scale_factors     Vector with the scale factor for each band.
#' @param missing_values    Vector of missing values for each band.
#' @param minimum_values    Vector of minimum values for each band.
#' @param maximum_values    Vector of maximum values for each band.
#' @param files             Vector of file names for each band (only for raster data).
#'
#' @seealso To see the available values for the parameters above use \code{\link{sits_services}}, \code{\link{sits_config}} or \code{\link{sits_show_config}}.
#' @examples
#' \donttest{
#' # Example 1. Retrieve information about a WTSS collection
#' cube.tb <- sits_cube(service = "WTSS", name = "MOD13Q1")
#'
#' # Example 2. Create a raster cube with metadata
#' # read a raster file and put it into a vector
#' files <- c(system.file("extdata/raster/mod13q1/sinop-crop-ndvi.tif", package = "sits"))
#'
#' # create a raster cube file based on the information about the files
#' raster.tb <- sits_cube(service = "RASTER", name  = "Sinop-crop",
#'              timeline = timeline_modis_392, bands = "ndvi", files = files)
#' }
#' @export
sits_cube <- function(service        = "RASTER",
                          provider       = NULL,
                          name           = NULL,
                          tiles_names    = NULL,
                          geom           = NULL,
                          from           = NULL,
                          to             = NULL,
                          timeline       = NULL,
                          bands          = NULL,
                          missing_values = NULL,
                          scale_factors  = NULL,
                          minimum_values = NULL,
                          maximum_values = NULL,
                          files          = NA) {


    # backward compatibility
    if (service == "WTSS-INPE")
        service <- "WTSS"
    # pre-condition
    .sits_check_service(service)

    if (service == "WTSS") {

        # pre-condition
        if (any(!is.na(files))) {
            msg <- paste0("inconsistent specification of cube parameters - files should
                          be provided only when service is RASTER")
            .sits_log_error(msg)
            message(msg)
            return(NULL)
        }

        serverURL  <- .sits_get_server(service, provider)
        tryCatch({
            # obtains information about the available cubes
            wtss.obj   <- wtss::WTSS(serverURL)

        }, error = function(e){
            msg <- paste0("WTSS service not available at URL ", serverURL)
            .sits_log_error(msg)
            message(msg)
        })

        # create a cube
        cube.tb <- .sits_cube_wtss(wtss.obj, service, name, bands)
    }
    else if (service == "SATVEG") {

        # pre-condition
        if (any(!is.na(files))) {
            msg <- paste0("inconsistent specification of cube parameters - files should
                          be provided only when service is RASTER")
            .sits_log_error(msg)
            message(msg)
            return(NULL)
        }

        cube.tb <- .sits_cube_satveg(name, timeline, bands)
    }
    else if (service == "EOCUBES") {

        # pre-condition
        if (any(!is.na(files))) {
            msg <- paste0("inconsistent specification of cube parameters - files should
                          be provided only when service is RASTER")
            .sits_log_error(msg)
            message(msg)
            return(NULL)
        }

        tryCatch({
            remote_name  <- .sits_get_server(service)

            # obtains information about the available cubes
            remote.obj   <- EOCubes::remote(name = remote_name)

        }, error = function(e){
            msg <- paste0("EOCubes remote not available at name ", remote_name)
            .sits_log_error(msg)
            message(msg)
        })

        # create a cube
        cube.tb <- .sits_cube_eocubes(remote.obj, service, name, bands, tiles_names, geom, from, to)
    }
    else if (service == "RASTER") {

        # append "vsicurl" prefix for all web files
        web_files <- grepl(pattern = "^[^:/].+://.+$", x = files)
        files[web_files] <- paste("/vsicurl", files[web_files], sep = "/")[web_files]

        # verify if all files are reacheable
        r <- suppressWarnings(rgdal::GDALinfo(files, silent = FALSE))
        ensurer::ensure_that(r, all(!purrr::is_null(.)),
                             err_desc = "sits_cube: raster files cannot be accessed")

        cube.tb <- .sits_cube_raster(name, timeline, bands, scale_factors, missing_values,
                                     minimum_values, maximum_values, files)
    } else if (service == "STACK") {

        files <- lapply(files, function(band) {
            # append "vsicurl" prefix for all web files
            web_files <- grepl(pattern = "^[^:/].+://.+$", x = band)
            band[web_files] <- paste("/vsicurl", band[web_files], sep = "/")[web_files]
            band
        })

        cube.tb <- .sits_cube_stack(name, timeline, bands, scale_factors, missing_values,
                                    minimum_values, maximum_values, files)
    }
    return(cube.tb)
}



#' @title Provides information about one cube of the WTSS service
#' @name .sits_cube_wtss
#'
#' @description Uses the WTSS services to print information and save metadata about a
#' chosen cube.
#'
#' @param wtss.obj   R WTSS object associated to the service.
#' @param service    Name of the service.
#' @param name       Name of the cube.
#' @param bands      Name of the bands.
.sits_cube_wtss <- function(wtss.obj, service, name, bands) {
    # obtains information about the available cubes
    cubes.vec    <- wtss::listCoverages(wtss.obj)

    # is the cube in the list of cubes?
    ensurer::ensure_that(name, (.) %in% cubes.vec,
                         err_desc = ".sits_cube_wtss: cube is not available in the WTSS server")

    # describe the cube based on the WTSS API
    cov.lst    <- wtss::describeCoverage(wtss.obj, name)
    cov        <- cov.lst[[name]]

    # temporal extent
    timeline.lst <- list(cov$timeline)

    # retrieve information about the bands
    band_info <- cov$attributes

    attr <- as.data.frame(band_info)
    bands.vec <- as.vector(attr[,"name"])

    # verify if requested bands is in provided bands
    if (!purrr::is_null(bands)) {
        ensurer::ensure_that(bands.vec, all(bands %in% .),
                             err_desc = ".sits_cube_WTSS: requested band not provided by WTSS service.")
    } else bands <- bands.vec

    b <- bands.vec %in% bands
    bands.vec <- bands.vec[b]

    missing_values.vec <- as.vector(attr[,"missing_value"])[b]
    names(missing_values.vec) <- bands.vec
    scale_factors.vec  <- as.vector(attr[,"scale_factor"])[b]
    names(scale_factors.vec) <- bands.vec
    minimum_values.vec <- as.vector(attr[,"valid_range"][["min"]])[b]
    names(minimum_values.vec) <- bands.vec
    maximum_values.vec <- as.vector(attr[,"valid_range"][["max"]])[b]
    names(maximum_values.vec) <- bands.vec

    # Spatial extent
    xmin <- cov$spatial_extent$xmin
    ymin <- cov$spatial_extent$ymin
    xmax <- cov$spatial_extent$xmax
    ymax <- cov$spatial_extent$ymax

    # Spatial resolution
    xres <- cov$spatial_resolution$x
    yres <- cov$spatial_resolution$y

    # Size (rows and cols)
    nrows <- cov$dimension$y$max_idx - cov$dimensions$y$min_idx + 1
    ncols <- cov$dimension$x$max_idx - cov$dimensions$x$min_idx + 1

    # Projection CRS
    crs <- cov$crs$proj4

    #labels
    labels.vec <- c("NoClass")

    # create a tibble to store the metadata
    cube.tb <- .sits_create_cube(list(wtss.obj), name, service,
                                         bands.vec, labels.vec, scale_factors.vec,
                                         missing_values.vec, minimum_values.vec,
                                         maximum_values.vec, timeline.lst,
                                         nrows, ncols, xmin, xmax, ymin, ymax,
                                         xres, yres, crs, files.vec = NA)

    # return the tibble with cube info
    return(cube.tb)
}

#' @title Provides information about one cube of the SATVEG time series service
#' @name .sits_cube_satveg
#'
#' @description Creates a tibble with metadata about a given cube.
#'
#' @param name       Name of the cube.
#' @param timeline   Timeline of the cube.
#' @param bands      Bands of the cube.
.sits_cube_satveg <- function(name, timeline, bands) {

    service <- "SATVEG"
    # get the bands
    bands.vec <- .sits_get_bands(service, name)

    # check if requested bands are in provided bands
    if (!purrr::is_null(bands)) {
        ensurer::ensure_that(bands.vec, all(bands %in% .),
                             err_desc = ".sits_cube_SATVEG: requested band not provided by SATVEG service.")
    } else bands <- bands.vec

    # select requested bands
    b <- bands.vec %in% bands
    bands.vec <- bands.vec[b]

    # the data in unlabelled
    labels.vec <- c("NoClass")

    # get the timeline
    if (purrr::is_null(timeline))
        timeline.lst <- list(.sits_satveg_timeline())
    else
        timeline.lst <- list(timeline)

    # get the size of the cube
    size <- .sits_get_size(service, name)
    nrows <- as.integer(size["nrows"])
    ncols <- as.integer(size["ncols"])

    # get the bounding box of the cube
    bbox <- .sits_get_bbox(service, name)
    xmin <-  as.numeric(bbox["xmin"])
    xmax <-  as.numeric(bbox["xmax"])
    ymin <-  as.numeric(bbox["ymin"])
    ymax <-  as.numeric(bbox["ymax"])

    # get the resolution of the product
    res  <- .sits_get_resolution(service, name)
    xres <-  as.numeric(res["xres"])
    yres <-  as.numeric(res["yres"])

    # get the CRS projection
    crs <- .sits_get_projection(service, name)
    # get scale factors, missing values and minimum values
    scale_factors.vec  <- .sits_get_scale_factors(service, name, bands.vec)
    names(scale_factors.vec) <- bands.vec
    missing_values.vec <- .sits_get_missing_values(service, name, bands.vec)
    names(missing_values.vec) <- bands.vec
    minimum_values.vec <- .sits_get_minimum_values(service, bands.vec)
    names(minimum_values.vec) <- bands.vec
    maximum_values.vec <- .sits_get_maximum_values(service, bands.vec)
    names(maximum_values.vec) <- bands.vec

    # create a tibble to store the metadata
    cube.tb <- .sits_create_cube(r_objs.lst = NA, name, service, bands.vec, labels.vec, scale_factors.vec,
                                 missing_values.vec, minimum_values.vec, maximum_values.vec,
                                 timeline.lst, nrows, ncols, xmin, xmax, ymin, ymax,
                                 xres, yres, crs, files.vec = NA)

    return(cube.tb)
}

#' @title Uses the EOCUBES service to provide information about a data cube
#' @name .sits_cube_eocubes
#'
#' @description Creates a tibble with metadata about a data cube.
#'
#' @param remote.obj Remote object.
#' @param service    Service string.
#' @param name       Name of the cube
#' @param bands      Bands of the cube.
#' @param tiles      Filter tiles by prefix name.
#' @param geom       Geometry to filter tiles.
#' @param from       Start date to be filtered.
#' @param to         End date to be filtered.
.sits_cube_eocubes <- function(remote.obj, service, name, bands, tiles, geom, from, to) {

    # obtains information about the available cubes
    cubes.vec    <- names(EOCubes::list_cubes(remote.obj))

    # is the cube in the list of cubes?
    ensurer::ensure_that(name, (.) %in% cubes.vec,
                         err_desc = ".sits_cube_EOCUBES: cube is not available in the EOCubes remote")

    # describe the cube
    cub.obj <- EOCubes::cube(name = name, remote = remote.obj)

    # filter cube
    cub.obj <- EOCubes::cube_filter(cube = cub.obj,
                                    tiles = EOCubes::tiles_which(cub.obj, prefix = tiles, geom = geom),
                                    from = from, to = to)

    # verify if the filter returned tiles
    ensurer::ensure_that(cub.obj, length(EOCubes::list_tiles(.)) > 0,
                         err_desc = ".sits_cube_EOCUBES: cube filter returned no tile.")

    # temporal extent
    timeline.lst <- list(EOCubes::cube_dates_info(cub.obj))

    # retrieve information about the bands
    attr <- EOCubes::cube_bands_info(cub.obj)

    bands.vec <- EOCubes::cube_bands(cub.obj)

    # verify if requested bands is in provided bands
    if (purrr::is_null(bands))
        bands <- bands.vec
    ensurer::ensure_that(bands.vec, all(bands %in% .),
                         err_desc = ".sits_cube_EOCUBES: requested band not provided by EOCubes remote.")

    b <- match(bands, bands.vec)
    bands.vec <- bands.vec[b]

    missing_values.vec <- attr$fill[b]
    scale_factors.vec  <- attr$scale[b]
    minimum_values.vec <- attr$min[b]
    maximum_values.vec <- attr$max[b]

    # Spatial extent
    cub_bbox <- EOCubes::cube_bbox(cub.obj)
    xmin <- cub_bbox[1] # xmin
    ymin <- cub_bbox[2] # ymin
    xmax <- cub_bbox[3] # xmax
    ymax <- cub_bbox[4] # ymax

    # Spatial resolution
    raster_info <- EOCubes::cube_raster_info(cub.obj)
    xres <- raster_info$resolution$x
    yres <- raster_info$resolution$y

    # Size (rows and cols)
    nrows <- raster_info$size$y
    ncols <- raster_info$size$x

    # Projection CRS
    crs <- EOCubes::cube_crs(cub.obj)

    #labels
    labels.vec <- c("NoClass")

    # create a tibble to store the metadata
    cube.tb <- .sits_create_cube(list(cub.obj), name, service,
                                         bands.vec, labels.vec, scale_factors.vec,
                                         missing_values.vec, minimum_values.vec,
                                         maximum_values.vec, timeline.lst,
                                         nrows, ncols, xmin, xmax, ymin, ymax,
                                         xres, yres, crs, files.vec = NA)

    # return the tibble with cube info
    return(cube.tb)}


#' @title Create a data cube based on a set of Raster Bricks
#' @name .sits_cube_raster
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description  This function creates a tibble containing the metadata for
#'               a set of spatio-temporal raster files, organized as a set of "Raster Bricks".
#'               These files should be of the same size and
#'               projection. Each raster brick file should contain one band
#'               per time step. Different bands are archived in different raster files.
#'
#' @param  name                  Name of the data cube.
#' @param  timeline.vec          Vector of dates with the timeline of the bands.
#' @param  bands.vec             Vector of bands contained in the Raster Brick set (in the same order as the files).
#' @param  scale_factors.vec     Vector of scale factors (one per band).
#' @param  missing_values.vec    Vector of missing values (one per band).
#' @param  minimum_values.vec    Minimum values for each band (only for raster data).
#' @param  maximum_values.vec    Maximum values for each band (only for raster data).
#' @param  files.vec             Vector with the file paths of the raster files.
#' @return A tibble with metadata information about a raster data set.
.sits_cube_raster <- function(name,  timeline.vec, bands.vec, scale_factors.vec,
                              missing_values.vec, minimum_values.vec, maximum_values.vec, files.vec) {
    ensurer::ensure_that(bands.vec, length(.) == length(files.vec),
                         err_desc = "sits_cubeRaster: number of bands does not match number of files")
    ensurer::ensure_that(name, !purrr::is_null(.),
                         err_desc = "sits_cubeRaster: name of the coverega must be provided")
    ensurer::ensure_that(bands.vec, !purrr::is_null(.),
                         err_desc = "sits_cubeRaster - bands must be provided")
    ensurer::ensure_that(files.vec, !purrr::is_null(.),
                         err_desc = "sits_cubeRaster - files must be provided")

    # get the timeline
    if (purrr::is_null(timeline.vec))
        timeline.vec <- lubridate::as_date(.sits_get_timeline(service = "RASTER", name = name))

    # set the labels
    labels.vec <- c("NoClass")

    # create a list to store the raster objects
    brick.lst <- purrr::pmap(list(files.vec, bands.vec),
                             function(file, band) {
                                 # create a raster object associated to the file
                                 raster.obj <- raster::brick(file)
                                 # find out how many layers the object has
                                 n_layers   <-  raster.obj@file@nbands
                                 # check that there are as many layers as the length of the timeline
                                 ensurer::ensure_that(n_layers, (.) == length(timeline.vec),
                                                      err_desc = "duration of timeline is not matched by number of layers in raster")
                                 # add the object to the raster object list
                                 return(raster.obj)
                             })

    cube.tb <- .sits_create_raster_cube(brick.lst, service = "RASTER", name, list(timeline.vec),
                                        bands.vec, labels.vec, scale_factors.vec, missing_values.vec = missing_values.vec,
                                        minimum_values.vec, maximum_values.vec, files.vec)

    return(cube.tb)
}

#' @title Create a data cube based on a set of Raster Stacks
#' @name .sits_cube_stack
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description  This function creates a tibble containing the metadata for
#'               a set of spatio-temporal raster files, organized as a set of "Raster Bricks".
#'               These files should be of the same size and
#'               projection. Each raster brick file should contain one band
#'               per time step. Different bands are archived in different raster files.
#'
#' @param  name                  Name of the cube file.
#' @param  timeline.vec          Vector of dates with the timeline of the bands.
#' @param  bands.vec             Vector of bands contained in the Raster Brick set (in the same order as the files).
#' @param  scale_factors.vec     Vector of scale factors (one per band).
#' @param  missing_values.vec    Vector of missing values (one per band).
#' @param  minimum_values.vec    Minimum values for each band (only for raster data).
#' @param  maximum_values.vec    Maximum values for each band (only for raster data).
#' @param  files.lst             List of vectors with the file paths of the raster files.
#' @return A tibble with metadata information about a data cube.
.sits_cube_stack <- function(name, timeline.vec, bands.vec, scale_factors.vec,
                             missing_values.vec, minimum_values.vec, maximum_values.vec, files.lst)
{
    ensurer::ensure_that(bands.vec, length(.) == length(files.lst),
                         err_desc = "sits_cube_STACK: number of bands does not match number of files")
    ensurer::ensure_that(name, !purrr::is_null(.),
                         err_desc = "sits_cube_STACK: name of the coverega must be provided")
    ensurer::ensure_that(bands.vec, !purrr::is_null(.),
                         err_desc = "sits_cube_STACK - bands must be provided")
    ensurer::ensure_that(files.lst, !purrr::is_null(.),
                         err_desc = "sits_cube_STACK - files must be provided")

    # get the timeline
    if (purrr::is_null(timeline.vec))
        timeline.vec <- lubridate::as_date(.sits_get_timeline(service = "RASTER", name = name))

    # set the labels
    labels.vec <- c("NoClass")

    # create a list to store the raster objects
    stck.obj <- purrr::pmap(list(files.lst, bands.vec),
                             function(files, band) {
                                 # create a raster object associated to the file
                                 raster.obj <- raster::stack(files, quick = TRUE)
                                 # find out how many layers the object has
                                 n_layers   <-  length(files)
                                 # check that there are as many layers as the length of the timeline
                                 ensurer::ensure_that(n_layers, (.) == length(timeline.vec),
                                                      err_desc = "duration of timeline is not matched by number of layers in raster")
                                 # add the object to the raster object list
                                 return(raster.obj)
                             })

    cube.tb <- .sits_create_stack_cube(stck.obj, service = "RASTER",
                                       name, list(timeline.vec), bands.vec, labels.vec,
                                       scale_factors.vec, missing_values.vec, minimum_values.vec,
                                       maximum_values.vec, files.lst)

    return(cube.tb)
}

#' @title Create a set of RasterLayer objects to store data cube classification results
#' @name .sits_cube_classified
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a tibble containing metadata about a data cube
#' containing time series (each Brick has information for one band) and creates a
#' set of RasterLayers to store the classification result. Each RasterLayer corresponds
#' to one time step. The time steps are specified in a list of dates.
#'
#' @param  raster.tb         Tibble with metadata about the input data cube.
#' @param  samples.tb        Samples used for training the classification model.
#' @param  file              Generic name of the files that will contain the RasterLayers.
#' @param  interval          Classification interval.
#' @return A tibble with metadata about the output RasterLayer objects.
.sits_cube_classified <- function(raster.tb, samples.tb, file, interval){
    # ensure metadata tibble exists
    ensurer::ensure_that(raster.tb, NROW(.) > 0,
                         err_desc = ".sits_classify_cube: need a valid metadata for cube")

    # get the timeline of observations (required for matching dates)
    timeline <- raster.tb[1,]$timeline[[1]][[1]]

    # what is the reference start date?
    ref_start_date <- lubridate::as_date(samples.tb[1,]$start_date)
    # what is the reference end date?
    ref_end_date  <- lubridate::as_date(samples.tb[1,]$end_date)

    # produce the breaks used to generate the output rasters
    subset_dates.lst <- sits_match_timeline(timeline, ref_start_date, ref_end_date, interval)

    scale_factors_class   <- rep(1, length(subset_dates.lst))
    missing_values_class  <- rep(-9999, length(subset_dates.lst))
    minimum_values_class  <- rep(0, length(subset_dates.lst))
    maximum_values_class  <- rep(length(unique(samples.tb$label)), length(subset_dates.lst))

    # labels come from samples.tb
    labels <- sits_labels(samples.tb)$label

    # lists that store the content of the raster layers (classified values)
    rasters_class   <- vector("list", length = length(subset_dates.lst))
    bands_class     <- vector(length = length(subset_dates.lst))
    files_class     <- vector(length = length(subset_dates.lst))

    # lists that store the content of the raster bricks
    rasters_probs    <- vector("list", length = length(subset_dates.lst))
    bands_probs     <- vector(length = length(subset_dates.lst))
    files_probs     <- vector(length = length(subset_dates.lst))
    n_layers_probs  <- length(labels)

    scale_factors_probs   <- rep(0.001,  length(subset_dates.lst))
    missing_values_probs  <- rep(-9999,  length(subset_dates.lst))
    minimum_values_probs  <- rep(0, length(subset_dates.lst))
    maximum_values_probs  <- rep(1, length(subset_dates.lst))

    timeline_rasters <- vector("list", length = length(subset_dates.lst))

    # loop through the list of dates and create list of raster layers to be created
    for (i in 1:length(subset_dates.lst)) {

        # define the timeline for the raster data sets
        start_date <- subset_dates.lst[[i]][1]
        end_date   <- subset_dates.lst[[i]][2]
        timeline_rasters[[i]] <- c(start_date, end_date)

        # reference raster object to clone from
        r_obj <- raster.tb[1,]$r_objs[[1]][[1]]

        # creation of raster layers
        rasters_class[[i]] <- raster::raster(r_obj)
        raster::dataType(rasters_class[[i]]) <- "INT1U"

        # define the filename for the classified image
        filename <- .sits_raster_filename(file, start_date, end_date)
        rasters_class[[i]]@file@name <- filename
        files_class[i] <- filename
        bands_class[i] <- paste0("class_",lubridate::year(start_date),"_",lubridate::month(start_date),
                                 "_",lubridate::year(end_date),"_",lubridate::month(end_date))

        # creation of raster bricks
        rasters_probs[[i]] <- raster::brick(r_obj, nl = n_layers_probs )
        raster::dataType(rasters_probs[[i]]) <- "INT2U"

        # define the filename for the classified image
        file_probs <- paste0(file, "_probs")
        filename <- .sits_raster_filename(file_probs, start_date, end_date)
        rasters_probs[[i]]@file@name <- filename
        files_probs[i] <- filename
        bands_probs[i] <- paste0("probs_",lubridate::year(start_date),"_",lubridate::month(start_date),
                                 "_",lubridate::year(start_date),"_",lubridate::month(end_date))
    }

    # get the name of the cube
    name   <-  paste0(raster.tb[1,]$name, "-class")

    # create a new RasterLayer for a defined period and generate the associated metadata
    cube.tb <- .sits_create_raster_cube(rasters_class, service            = "RASTER", name,
                                        timeline_rasters, bands_class, labels,
                                        scale_factors_class, missing_values_class,
                                        minimum_values_class, maximum_values_class, files_class)

    # get the name of the cube
    name   <-  paste0(raster.tb[1,]$name, "-prob")

    cube_probs.tb <- .sits_create_raster_cube(rasters_probs, service = "RASTER",
                                              name, timeline_rasters, bands_probs, labels,
                                              scale_factors_probs, missing_values_probs,
                                              minimum_values_probs, maximum_values_probs, files_probs)

    cube.tb <- dplyr::bind_rows(cube.tb, cube_probs.tb)

    # join rows to a single row
    cube.tb <- dplyr::as_tibble(lapply(cube.tb, list))

    return(cube.tb)
}

#' @title Creates a tibble with information about a data cube based on Raster objects
#' @name .sits_create_raster_cube
#'
#' @description Creates a tibble with metadata about a given cube.
#'
#' @param raster.lst               List of Raster objects associated with the raster cubes.
#' @param service                  Time series service.
#' @param name                     Name of the cube.
#' @param timeline.lst             List of cube timelines.
#' @param bands.vec                Vector with names of bands.
#' @param labels.vec               Vector of labels for classified image.
#' @param scale_factors.vec        Vector of scale factors.
#' @param missing_values.vec       Vector of missing values.
#' @param minimum_values.vec       Vector of minimum values.
#' @param maximum_values.vec       Vector of maximum values.
#' @param files.vec                Vector of names of raster files where the data is stored.
.sits_create_raster_cube <- function(raster.lst,
                                         service,
                                         name,
                                         timeline.lst,
                                         bands.vec,
                                         labels.vec,
                                         scale_factors.vec,
                                         missing_values.vec,
                                         minimum_values.vec,
                                         maximum_values.vec,
                                         files.vec) {
    # associate an R raster object to the first element of the list of bricks
    r_obj <- raster.lst[[1]]

    # get the size of the cube
    nrows <- raster::nrow(r_obj)
    ncols <- raster::ncol(r_obj)

    # test if all bricks have the same size
    i <- 1
    while (length(raster.lst) > i) {
        i <- i + 1
        ensurer::ensure_that(nrows, (.) == raster::nrow(raster.lst[[i]]),
                             err_desc = "raster bricks/layers do not have the same number of rows")
        ensurer::ensure_that(ncols, (.) == raster::ncol(raster.lst[[i]]),
                             err_desc = "raster bricks/layers do not have the same number of cols")
    }
    # get the bounding box of the product
    xmin <- raster::xmin(r_obj)
    xmax <- raster::xmax(r_obj)
    ymin <- raster::ymin(r_obj)
    ymax <- raster::ymax(r_obj)

    # get the resolution of the product
    xres <- raster::xres(r_obj)
    yres <- raster::yres(r_obj)

    # test if all bricks have the same resolution
    i <- 1
    while (length(raster.lst) > i) {
        i <- i + 1
        ensurer::ensure_that(xres, (.) == raster::xres(raster.lst[[i]]),
                             err_desc = "raster bricks/layers have different xres")
        ensurer::ensure_that(yres, (.) == raster::yres(raster.lst[[i]]),
                             err_desc = "raster bricks/layers have different yres")
    }

    # if scale factors are not provided, try a best guess
    if (purrr::is_null(scale_factors.vec)) {
        message("Scale factors not provided - will use default values: please check they are valid")
        # try to guess what is the satellite
        satellite <- .sits_guess_satellite(r_obj)
        # retrieve the scale factors
        scale_factors.vec <- .sits_get_scale_factors("RASTER", satellite, bands.vec)
        # are the scale factors valid?
        ensurer::ensure_that(scale_factors.vec, !(purrr::is_null(.)),
                             err_desc = "Not able to obtain scale factors for raster data")
    }
    else
        names(scale_factors.vec) <- bands.vec

    # if missing_values are not provided, try a best guess
    if (purrr::is_null(missing_values.vec)) {
        message("Missing values not provided - will use default values: please check they are valid")
        # try to guess what is the satellite
        satellite      <- .sits_guess_satellite(r_obj)
        # try to retrieve the missing values
        missing_values.vec <- .sits_get_missing_values("RASTER", satellite, bands.vec)
        ensurer::ensure_that(missing_values.vec, !(purrr::is_null(.)),
                             err_desc = "Not able to obtain scale factors for raster data")
    }
    else
        names(missing_values.vec) <- bands.vec

    if (purrr::is_null(minimum_values.vec)) {
        minimum_values.vec <- .sits_get_minimum_values("RASTER", bands.vec)
    }

    if (purrr::is_null(maximum_values.vec)) {
        maximum_values.vec <- .sits_get_maximum_values("RASTER", bands.vec)
    }

    # preserve the names of the bands on the list of raster objects and in the files
    names(raster.lst) <- bands.vec
    names(files.vec)  <- bands.vec

    # get CRS
    crs = as.character(raster::crs(r_obj))

    # create a tibble to store the metadata
    cube.tb <- .sits_create_cube(raster.lst, name, service, bands.vec, labels.vec,
                                 scale_factors.vec, missing_values.vec,
                                 minimum_values.vec, maximum_values.vec, timeline.lst,
                                 nrows, ncols, xmin, xmax, ymin, ymax,
                                 xres, yres, crs, files.vec)

    return(cube.tb)
}

#' @title Creates a tibble with information about a set of raster bricks
#' @name .sits_create_stack_cube
#'
#' @description Creates a tibble with metadata about a given cube.
#'
#' @param raster.lst               List of Raster objects associated with the raster cubes.
#' @param service                  Time series service.
#' @param name                     Name of the cube.
#' @param timeline.lst             List of cube timelines.
#' @param bands.vec                Vector with names of bands.
#' @param labels.vec               Vector of labels for classified image.
#' @param scale_factors.vec        Vector of scale factors.
#' @param missing_values.vec       Vector of missing values.
#' @param minimum_values.vec       Vector of minimum values.
#' @param maximum_values.vec       Vector of maximum values.
#' @param files.lst                List of vectors containing raster files where the data is stored.
.sits_create_stack_cube <- function(raster.lst,
                                        service,
                                        name,
                                        timeline.lst,
                                        bands.vec,
                                        labels.vec,
                                        scale_factors.vec,
                                        missing_values.vec,
                                        minimum_values.vec,
                                        maximum_values.vec,
                                        files.lst) {

    # associate an R raster object to the first element of the list of stacks
    r_obj <- raster.lst[[1]]

    # get the size of the cube
    nrows <- raster::nrow(r_obj)
    ncols <- raster::ncol(r_obj)

    # test if all stacks have the same size
    i <- 1
    while (length(raster.lst) > i) {
        i <- i + 1
        ensurer::ensure_that(nrows, (.) == raster::nrow(raster.lst[[i]]),
                             err_desc = "raster stack/layers do not have the same number of rows")
        ensurer::ensure_that(ncols, (.) == raster::ncol(raster.lst[[i]]),
                             err_desc = "raster stack/layers do not have the same number of cols")
    }
    # get the bounding box of the product
    xmin <- raster::xmin(r_obj)
    xmax <- raster::xmax(r_obj)
    ymin <- raster::ymin(r_obj)
    ymax <- raster::ymax(r_obj)

    # get the resolution of the product
    xres <- raster::xres(r_obj)
    yres <- raster::yres(r_obj)

    # test if all bricks have the same resolution
    i <- 1
    while (length(raster.lst) > i) {
        i <- i + 1
        ensurer::ensure_that(xres, (.) == raster::xres(raster.lst[[i]]),
                             err_desc = "raster stacks/layers have different xres")
        ensurer::ensure_that(yres, (.) == raster::yres(raster.lst[[i]]),
                             err_desc = "raster stacks/layers have different yres")
    }
    # if scale factors are not provided, try a best guess
    if (purrr::is_null(scale_factors.vec)) {
        message("Scale factors not provided - will use default values: please check they are valid")
        # try to guess what is the satellite
        satellite <- .sits_guess_satellite(r_obj)
        # retrieve the scale factors
        scale_factors.vec <- .sits_get_scale_factors("RASTER", satellite, bands.vec)
        # are the scale factors valid?
        ensurer::ensure_that(scale_factors.vec, !(purrr::is_null(.)),
                             err_desc = "Not able to obtain scale factors for raster data")
    }
    else
        names(scale_factors.vec) <- bands.vec

    # if missing_values are not provided, try a best guess
    if (purrr::is_null(missing_values.vec)) {
        message("Missing values not provided - will use default values: please check they are valid")
        # try to guess what is the satellite
        satellite      <- .sits_guess_satellite(r_obj)
        # try to retrieve the missing values
        missing_values.vec <- .sits_get_missing_values("RASTER", satellite, bands.vec)
        ensurer::ensure_that(missing_values.vec, !(purrr::is_null(.)),
                             err_desc = "Not able to obtain scale factors for raster data")
    } else
        names(missing_values.vec) <- bands.vec

    if (purrr::is_null(minimum_values.vec)) {
        minimum_values.vec <- .sits_get_minimum_values("RASTER", bands.vec)
    }

    if (purrr::is_null(maximum_values.vec)) {
        maximum_values.vec <- .sits_get_maximum_values("RASTER", bands.vec)
    }

    # preserve the names of the bands on the list of raster objects and in the files
    names(raster.lst) <- bands.vec
    names(files.lst)  <- bands.vec

    # get CRS
    crs = as.character(raster::crs(r_obj))

    # create a tibble to store the metadata
    cube.tb <- .sits_create_cube(raster.lst,
                                         name = name,
                                         service = service,
                                         bands.vec = bands.vec,
                                         labels.vec = labels.vec,
                                         scale_factors.vec = scale_factors.vec,
                                         missing_values.vec = missing_values.vec,
                                         minimum_values.vec = minimum_values.vec,
                                         maximum_values.vec = maximum_values.vec,
                                         timeline.lst = timeline.lst,
                                         nrows = nrows, ncols = ncols,
                                         xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                                         xres = xres, yres = yres, crs = crs,
                                         files.vec = files.lst)

    return(cube.tb)
}
#' @title Creates the description of a data cube
#' @name .sits_create_cube
#'
#' @description Uses the configuration file to print information and save metadata about a
#' data cube.
#'
#' @param r_objs.lst         List of raster objects contained in the cube.
#' @param name               Name of the data cube.
#' @param service            Name of the web service that has provided metadata about the cube.
#' @param bands.vec          Vector with the names of the bands.
#' @param labels.vec         Vector with labels (only valid for classified data).
#' @param scale_factors.vec  Vector with scale factor for each band.
#' @param missing_values.vec Vector with missing values for each band.
#' @param minimum_values.vec Vector with minimum values for each band.
#' @param maximum_values.vec Vector with maximum values for each band.
#' @param timeline.lst       List with vectors of valid timelines for each band.
#' @param nrows              Number of rows in the cube.
#' @param ncols              Number of columns in the cube.
#' @param xmin               Spatial extent (xmin).
#' @param ymin               Spatial extent (ymin).
#' @param xmax               Spatial extent (xmax).
#' @param ymax               Spatial extent (ymin).
#' @param xres               Spatial resolution (x dimension).
#' @param yres               Spatial resolution (y dimension).
#' @param crs                CRS for cube.
#' @param files.vec          Vector with associated files.
.sits_create_cube <- function(r_objs.lst, name, service,
                              bands.vec, labels.vec, scale_factors.vec, missing_values.vec,
                              minimum_values.vec, maximum_values.vec, timeline.lst,
                              nrows, ncols, xmin, xmax, ymin, ymax,
                              xres, yres, crs, files.vec) {
    # create a tibble to store the metadata
    cube.tb <- tibble::tibble(r_objs         = list(r_objs.lst),
                              name           = name,
                              service        = service,
                              bands          = list(bands.vec),
                              labels         = list(labels.vec),
                              scale_factors  = list(scale_factors.vec),
                              missing_values = list(missing_values.vec),
                              minimum_values = list(minimum_values.vec),
                              maximum_values = list(maximum_values.vec),
                              timeline       = list(timeline.lst),
                              nrows          = nrows,
                              ncols          = ncols,
                              xmin           = xmin,
                              xmax           = xmax,
                              ymin           = ymin,
                              ymax           = ymax,
                              xres           = xres,
                              yres           = yres,
                              crs            = crs,
                              files          = list(files.vec))

    return(cube.tb)
}
#' @title Try a best guess for the type of sensor/satellite
#' @name .sits_guess_satellite
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Based on the projection, tries to guess what is the satellite.
#'
#' @param r_obj         A raster object.
#' @return Name of the satellite (or sensor).
.sits_guess_satellite <- function(r_obj) {
    # get the CRS projection
    crs <- as.character(raster::crs(r_obj))

    # if the projection is UTM, guess it's a LANDSAT data set
    if (stringr::str_detect(crs, "utm")) {
        message("Data in UTM projection - assuming LANDSAT-compatible images")
        satellite <- "LANDSAT"
    }
    # if the projection is sinusoidal, guess it's a MODIS data set
    else if (stringr::str_detect(crs, "sinu")) {
        message("Data in Sinusoidal projection - assuming MODIS-compatible images")
        satellite <- "MODIS"
    }
    else {
        message("Cannot guess what is the satellite")
        satellite <- "UNKNOWN"
    }

    return(satellite)
}

#' @title Define a filename associated to one classified raster layer
#' @name .sits_raster_filename
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Creates a filename for a raster layer with associated temporal information,
#'                 given a basic filename.
#'
#' @param file          Original file name (without temporal information).
#' @param start_date    Starting date of the time series classification.
#' @param end_date      End date of the time series classification.
#' @return Name of the classification file for the required interval.
.sits_raster_filename <- function(file, start_date, end_date){
    file_base <- tools::file_path_sans_ext(file)
    y1 <- lubridate::year(start_date)
    m1 <- lubridate::month(start_date)
    y2 <- lubridate::year(end_date)
    m2 <- lubridate::month(end_date)

    file_name <- paste0(file_base, "_", y1, "_", m1, "_", y2, "_", m2, ".tif")

    return(file_name)
}

#' @title Provides information about one coverage used to retrieve data
#' @name sits_coverage
#'
#' @description Defines a coverage to retrieve data. Coverages are associated to
#' data services. The following services are available:
#' \itemize{
#'  \item{"WTSS": }{Web Time Series Service - used to get time series}
#'  \item{"EOCUBES": }{EOCUBES service - used for cloud processing of data cubes}
#'  \item{"RASTER": }{Raster Brick files, local or remote}
#'  \item{"STACK": }{Raster Stack files, local or remote}
#' }
#'
#'
#' @param service           Name of the data service.
#' @param provider          Name of the service provider.
#' @param name              Name of the image collection.
#' @param tiles_names       A string with tile names to be filtered.
#' @param geom              A \code{sfc} object to filter tiles that intersects the given geometry.
#' @param from              A date value to filter cube's layers by date.
#' @param to                A date value to filter cube's layers by date.
#' @param timeline          Vector with the timeline of the collection.
#' @param bands             Vector of bands.
#' @param scale_factors     Vector with the scale factor for each band.
#' @param missing_values    Vector of missing values for each band.
#' @param minimum_values    Vector of minimum values for each band.
#' @param maximum_values    Vector of maximum values for each band.
#' @param files             Vector of file names for each band (only for raster data).
#'
#' @seealso To see the available values for the parameters above use \code{\link{sits_services}}, \code{\link{sits_config}} or \code{\link{sits_show_config}}.
#' @examples
#' \donttest{
#' # Example 1. Retrieve information about a WTSS collection
#' coverage.tb <- sits_coverage(service = "WTSS", name = "MOD13Q1")
#'
#' # Example 2. Create a raster coverage with metadata
#' # read a raster file and put it into a vector
#' files <- c(system.file("extdata/raster/mod13q1/sinop-crop-ndvi.tif", package = "sits"))
#'
#' # create a raster coverage file based on the information about the files
#' raster.tb <- sits_coverage(service = "RASTER", name  = "Sinop-crop",
#'              timeline = timeline_modis_392, bands = "ndvi", files = files)
#' }
#' @export
sits_coverage <- function(service        = "RASTER",
                          provider       = NULL,
                          name           = NULL,
                          tiles_names    = NULL,
                          geom           = NULL,
                          from           = NULL,
                          to             = NULL,
                          timeline       = NULL,
                          bands          = NULL,
                          missing_values = NULL,
                          scale_factors  = NULL,
                          minimum_values = NULL,
                          maximum_values = NULL,
                          files          = NA) {

    # backward compatibility
    cube.tb <- sits_cube(service, provider, name, tiles_names, geom, from, to, timeline,
                         bands, missing_values, scale_factors, minimum_values,
                         maximum_values, files)

    return (cube.tb)
}
