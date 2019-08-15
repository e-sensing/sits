#' @title Defines a data cube
#' @name sits_cube
#'
#' @description Defines a cube to retrieve data. Cubes are associated to
#' data services. The following services are available:
#' \itemize{
#'  \item{"WTSS": }{Web Time Series Service - used to get time series}
#'  \item{"EOCUBES": }{EOCUBES service - used for cloud processing of data cubes}
#'  \item{"AWS": }{AWS service - used for cloud processing of data cubes}
#'  \item{"LOCALHOST": }{Raster Brick files, local}
#'  \item{"STACK": }{Raster Stack files, local}
#' }
#'
#'
#' @param service           Name of the data service.
#' @param URL               URL of the service provider.
#' @param satellite         Name of satellite
#' @param sensor            Name of sensor
#' @param name              Name of the data cube in the remote service.
#' @param tiles_names       A string with tile names to be filtered (for EOCUBES service)
#' @param geom              An \code{sf} object to filter tiles (for EOCUBES service)
#' @param from              Starting date for the cube to be extracted
#' @param to                End date for the cube to be extracted
#' @param timeline          Vector with the timeline of the collection (only for local files)
#' @param bands             Vector of bands.
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
#' raster.tb <- sits_cube(name  = "Sinop-crop",
#'              timeline = timeline_modis_392, bands = "ndvi", files = files)
#' }
#' @export
sits_cube <- function(service            = "LOCALHOST",
                      URL            = NULL,
                      name           = NULL,
                      satellite      = NULL,
                      sensor         = NULL,
                      tiles_names    = NULL,
                      geom           = NULL,
                      from           = NULL,
                      to             = NULL,
                      timeline       = NULL,
                      bands          = NULL,
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


        tryCatch({
            if (purrr::is_null(URL))
                URL <- .sits_server(service)
            # obtains information about the available cubes
            wtss.obj   <- wtss::WTSS(URL)

        }, error = function(e){
            msg <- paste0("WTSS service not available at URL ", URL)
            .sits_log_error(msg)
            message(msg)
        })

        # create a cube
        cube.tb <- .sits_cube_wtss(wtss.obj, service, URL, name, bands)
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

        cube.tb <- .sits_cube_satveg(name)
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
            URL  <- .sits_server(service)

            # obtains information about the available cubes
            remote.obj   <- EOCubes::remote(name = URL)

        }, error = function(e){
            msg <- paste0("EOCubes remote not available at name ", name)
            .sits_log_error(msg)
            message(msg)
        })

        # create a cube
        cube.tb <- .sits_cube_eocubes(remote.obj, service, URL, name, bands, tiles_names, geom, from, to)
    }
    else if (service == "STACK") {

        files <- lapply(files, function(band) {
            # append "vsicurl" prefix for all web files
            web_files <- grepl(pattern = "^[^:/].+://.+$", x = band)
            band[web_files] <- paste("/vsicurl", band[web_files], sep = "/")[web_files]
            band
        })
        # get the URL of the provider
        provider <- urltools::suffix_extract(urltools::domain(files[1]))$host

        # create a raster data cube
        cube.tb <- .sits_cube_raster(service, URL, satellite, sensor, name, timeline, bands, files)
    }
    else {

        # are there webfiles?
        if (all(grepl("http", c(files[1])))) {
            # append "vsicurl" prefix for all web files if it is not there
            if (!grepl("vsicurl", c(files[1])))
                files <- paste("/vsicurl", files, sep = "/")
            # verify if all files are reacheable
            r <- suppressWarnings(rgdal::GDALinfo(files, silent = FALSE))
            ensurer::ensure_that(r, all(!purrr::is_null(.)),
                                 err_desc = "sits_cube: raster files cannot be accessed")

            # get the URL of the provider
            URL <- urltools::domain(files[1])
            if (grep("aws", URL))
                service <- "AWS"
        }
        if (purrr::is_null(URL))
            URL <- .sits_server(service)

        cube.tb <- .sits_cube_raster(service, URL, satellite, sensor, name, timeline, bands, files)
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
#' @param URL        URL of the service provider.
#' @param name       Name of the cube.
#' @param bands      Name of the bands.
.sits_cube_wtss <- function(wtss.obj, service, URL, name, bands) {
    # obtains information about the available cubes
    cubes.vec    <- wtss::listCoverages(wtss.obj)

    # is the cube in the list of cubes?
    ensurer::ensure_that(name, (.) %in% cubes.vec,
                         err_desc = ".sits_cube_wtss: cube is not available in the WTSS server")

    # describe the cube based on the WTSS API
    cov.lst    <- wtss::describeCoverage(wtss.obj, name)
    cov        <- cov.lst[[name]]

    # retrieve the satellite associated to the product
    satellite <- .sits_satellite_product(name)
    # retrieve the sensor associated to the product
    sensor <- .sits_sensor_product(name)
    # temporal extent
    timeline <- lubridate::as_date(cov$timeline)

    # retrieve information about the bands
    band_info <- cov$attributes

    attr <- as.data.frame(band_info)
    bands_wtss <- as.vector(attr[,"name"])

    # verify if requested bands is in provided bands
    if (!purrr::is_null(bands)) {
        ensurer::ensure_that(bands_wtss, all(bands %in% .),
                             err_desc = ".sits_cube_WTSS: requested band not provided by WTSS service.")
    } else
    bands <- bands_wtss

    b <- bands_wtss %in% bands
    bands_wtss <- bands_wtss[b]

    missing_values <- as.vector(attr[,"missing_value"])[b]
    names(missing_values) <- bands_wtss
    scale_factors  <- as.vector(attr[,"scale_factor"])[b]
    names(scale_factors)  <- bands_wtss
    minimum_values <- as.vector(attr[,"valid_range"][["min"]])[b]
    names(minimum_values) <- bands_wtss
    maximum_values <- as.vector(attr[,"valid_range"][["max"]])[b]
    names(maximum_values) <- bands_wtss

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
    labels<- c("NoClass")

    # create a tibble to store the metadata
    cube_wtss <- .sits_create_cube(service, URL, satellite, sensor, name, bands_wtss, labels,
                                   scale_factors, missing_values, minimum_values, maximum_values,
                                   list(timeline), nrows, ncols, xmin, xmax, ymin, ymax,
                                   xres, yres, crs)

    # return the tibble with cube info
    return(cube_wtss)
}

#' @title Provides information about one cube of the SATVEG time series service
#' @name .sits_cube_satveg
#'
#' @description Creates a tibble with metadata about a given cube.
#'
#' @param name       Name of the cube.
.sits_cube_satveg <- function(name) {

    service   <- "SATVEG"
    satellite <- "TERRA"
    sensor    <- "MODIS"
    # get the bands
    bands <- .sits_bands_service(service, name)

    # the data in unlabelled
    labels <- c("NoClass")

    # get scale factors, missing values and minimum values
    scale_factors  <- .sits_scale_factors(sensor,  bands)
    missing_values <- .sits_missing_values(sensor, bands)
    minimum_values <- .sits_minimum_values(sensor, bands)
    maximum_values <- .sits_maximum_values(sensor, bands)

    # get the timeline
    timeline <- lubridate::as_date(.sits_satveg_timeline())

    # get the size of the cube
    size <- .sits_size_service(service, name)
    nrows <- as.integer(size["nrows"])
    ncols <- as.integer(size["ncols"])

    # get the bounding box of the cube
    bbox <- .sits_bbox_service(service, name)
    xmin <-  as.numeric(bbox["xmin"])
    xmax <-  as.numeric(bbox["xmax"])
    ymin <-  as.numeric(bbox["ymin"])
    ymax <-  as.numeric(bbox["ymax"])

    # get the resolution of the product
    res  <- .sits_resolution(sensor)
    xres <-  as.numeric(res["xres"])
    yres <-  as.numeric(res["yres"])

    # get the CRS projection
    crs <- .sits_projection_service(service, name)


    URL <- .sits_providers(service)

    # create a tibble to store the metadata
    cube_satveg <- .sits_create_cube(service, URL, satellite, sensor, name, bands, labels,
                                 scale_factors, missing_values, minimum_values, maximum_values,
                                 list(timeline), nrows, ncols, xmin, xmax, ymin, ymax,
                                 xres, yres, crs)

    return(cube_satveg)
}

#' @title Uses the EOCUBES service to provide information about a data cube
#' @name .sits_cube_eocubes
#'
#' @description Creates a tibble with metadata about a data cube.
#'
#' @param remote.obj Remote object.
#' @param service    Name of the data service.
#' @param URL        URL of the service provider.
#' @param name       Name of the cube
#' @param bands      Bands of the cube.
#' @param tiles      Filter tiles by prefix name.
#' @param geom       Geometry to filter tiles.
#' @param from       Start date to be filtered.
#' @param to         End date to be filtered.
.sits_cube_eocubes <- function(remote.obj, service, URL, name, bands, tiles, geom, from, to) {

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
    timeline <- lubridate::as_date(c(EOCubes::cube_dates_info(cub.obj)$from,
                                     EOCubes::cube_dates_info(cub.obj)$to))

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

    missing_values <- attr$fill[b]
    scale_factors  <- attr$scale[b]
    minimum_values <- attr$min[b]
    maximum_values <- attr$max[b]

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
    labels <- c("NoClass")

    # temporary fix until EOCubes is fully implemented
    satellite <- "TERRA"
    sensor    <- "MODIS"

    # create a tibble to store the metadata
    cube <- .sits_create_cube(service, URL, satellite, sensor, name, bands.vec, labels,
                              scale_factors, missing_values, minimum_values, maximum_values,
                              list(timeline), nrows, ncols, xmin, xmax, ymin, ymax,
                              xres, yres, crs)

    # return the tibble with cube info
    return(cube)}


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
#' @param  service               Name of the data service.
#' @param  URL                   URL of the service provider.
#' @param  satellite             Name of satellite
#' @param  sensor                Name of sensor
#' @param  name                  Name of the data cube.
#' @param  timeline              Vector of dates with the timeline of the bands.
#' @param  bands                 Vector of bands contained in the Raster Brick set (in the same order as the files).
#' @param  files                 Vector with the file paths of the raster files.
#' @return A tibble with metadata information about a raster data set.
.sits_cube_raster <- function(service, URL, satellite, sensor, name,
                              timeline, bands,  files) {

    ensurer::ensure_that(bands, length(.) == length(files),
                         err_desc = "sits_cubeRaster: number of bands does not match number of files")
    ensurer::ensure_that(name, !purrr::is_null(.),
                         err_desc = "sits_cubeRaster: name of the coverege must be provided")
    ensurer::ensure_that(files, !purrr::is_null(.),
                         err_desc = "sits_cubeRaster - files must be provided")

    # try to guess which is the satellite
    if (purrr::is_null(satellite)) {
        satellite <- .sits_guess_satellite(.sits_files_robj(files))
        message(paste0("satellite information not provided - assuming ", satellite))
    }
    # is the satellite supported by SITS?
    ensurer::ensure_that(satellite, (.) %in% .sits_satellites(),
                         err_desc = "satellite not supported by SITS - please edit configuration file")

    # try to guess which is the sensor
    if (purrr::is_null(sensor)) {
        sensor <- .sits_guess_sensor(satellite)
        message(paste0("sensor information not provided - assuming ", sensor))
    }
    # is the sensor supported by SITS?
    ensurer::ensure_that(sensor, (.) %in% .sits_sensors_satellite(satellite),
                         err_desc = "sensor not supported by SITS - please edit configuration file")

    # guess the timeline if it is not provided
    if (purrr::is_null(timeline))
        timeline <- .sits_guess_timeline(service, name)

    # transform the timeline to date format
    timeline <- lubridate::as_date(timeline)

    # set the labels
    labels <- c("NoClass")

    # obtain the parameters
    params <- .sits_raster_params(.sits_files_robj(files))

    # get scale factors
    scale_factors  <- .sits_scale_factors(sensor, bands)
    # get missing values
    missing_values <- .sits_missing_values(sensor, bands)
    # get minimum values
    minimum_values <- .sits_minimum_values(sensor, bands)
    # get maximum values
    maximum_values <- .sits_maximum_values(sensor, bands)


    # create a tibble to store the metadata
    cube <- .sits_create_cube(service, URL, satellite, sensor, name, bands, labels, scale_factors,
                              missing_values, minimum_values, maximum_values, list(timeline),
                              nrows = params$nrows, ncols = params$ncols,
                              xmin  = params$xmin, xmax  = params$xmax, ymin  = params$ymin, ymax  = params$ymax,
                              xres  = params$xres, yres  = params$yres, crs   = params$crs,
                              files)
    return(cube)
}


#' @title Create a set of RasterLayer objects to store data cube classification results (only the probs)
#' @name .sits_cube_classified
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a tibble containing metadata about a data cube
#' containing time series (each Brick has information for one band) and creates a
#' set of RasterLayers to store the classification result. Each RasterLayer corresponds
#' to one time step. The time steps are specified in a list of dates.
#'
#' @param  cube              Tibble with metadata about the input data cube.
#' @param  samples           Samples used for training the classification model..
#' @param  interval          Classification interval.
#' @param  output_dir        Prefix of the output files.
#' @return A tibble with metadata about the output RasterLayer objects.
.sits_cube_classified <- function(cube, samples, interval, output_dir){
    # ensure metadata tibble exists
    ensurer::ensure_that(cube, NROW(.) > 0,
                         err_desc = ".sits_classify_cube: need a valid metadata for cube")

    # get the timeline of of the data cube
    timeline <- lubridate::as_date(sits_timeline(cube))

    # Get the reference start date and end date from the samples
    ref_start_date <- lubridate::as_date(samples[1,]$start_date)
    ref_end_date   <- lubridate::as_date(samples[1,]$end_date)

    # produce the breaks used to generate the output rasters
    subset_dates <- sits_match_timeline(timeline, ref_start_date, ref_end_date, interval)

    # how many objects are to be created?
    n_objs <- length(subset_dates)

    # labels come from samples.tb
    labels <- sits_labels(samples)$label

    # create vectors and lists to store the content of the probabilities
    rasters    <- vector("list", length = n_objs)
    bands      <- vector(length = n_objs)
    files      <- vector(length = n_objs)
    n_layers   <- length(labels)
    timelines  <- vector("list", length = n_objs)

    # set scale factors, missing values, minimum and maximum values for probs
    scale_factors   <- rep(0.001,  n_objs)
    missing_values  <- rep(-9999,  n_objs)
    minimum_values  <- rep(0.0,    n_objs)
    maximum_values  <- rep(1.0,    n_objs)

    # loop through the list of dates and create list of raster layers to be created
    for (i in 1:n_objs) {

        # define the timeline for the raster data sets
        start_date     <- subset_dates[[i]][1]
        end_date       <- subset_dates[[i]][2]
        timelines[[i]] <- timeline[lubridate::as_date(timeline) >= start_date &
                                        lubridate::as_date(timeline) <= end_date]

        # define the filename for the classified image
        files[i] <- .sits_raster_filename(output_dir, cube$name, "probs", start_date, end_date)
        bands[i] <- .sits_class_band_name(cube$name, "probs", start_date, end_date)
    }

    params <- .sits_raster_params(.sits_cube_robj(cube))
    # get the name of the cube
    name   <-  paste0(cube[1,]$name, "_probs")
    # set the metadata for the probability cube
    cube_probs <- .sits_create_cube(service = "LOCALHOST", URL     = "http://127.0.0.1",
                                    satellite = cube$satellite, sensor = cube$sensor,
                                    name, bands, labels, scale_factors,
                                    missing_values, minimum_values, maximum_values,
                                    timelines, nrows = params$nrows, ncols = params$ncols,
                                    xmin  = params$xmin, xmax  = params$xmax,
                                    ymin  = params$ymin, ymax  = params$ymax,
                                    xres  = params$xres, yres  = params$yres, crs   = params$xmin,
                                    files = files )


    return(cube_probs)
}
#' @title Create a set of RasterLayer objects to store data cube classification results (labelled classes)
#' @name .sits_cube_labelled
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a tibble containing metadata about a data cube wuth
#' classification probabilites and and creates a
#' set of RasterLayers to store the classification result. Each RasterLayer corresponds
#' to one time step. The time steps are specified in a list of dates.
#'
#' @param  cube_probs        Tibble with metadata about the input data cube (probability).
#' @param  smoothing         (optional) smoothing method to be applied ("none", "bayesian", "majority")
#' @param  output_dir        Output directory where to put the files
#' @return A tibble with metadata about the output RasterLayer objects.
.sits_cube_labelled <- function(cube_probs, smoothing, output_dir){

    # labels come from the input cube
    labels <- .sits_cube_labels(cube_probs)

    # how many objects are to be created?
    n_objs <- length(.sits_cube_files(cube_probs))

    # lists that store the content of the raster layers (classified values))
    bands     <- vector(length = n_objs)
    files     <- vector(length = n_objs)
    timelines <- vector("list", length = n_objs)

    # set scale factors, missing values, minimum and maximum values for labelled image
    scale_factors   <- rep(1, n_objs)
    missing_values  <- rep(-9999, n_objs)
    minimum_values  <- rep(1, n_objs)
    maximum_values  <- rep(length(labels), n_objs)

    # get the type of the cube
    type  <- paste0("class_", smoothing)
    # name of the cube
    name  <- paste0(cube_probs[1,]$name, "_", type)

    # loop through the list of dates and create list of raster layers to be created
    for (i in 1:n_objs) {

        # define the timeline for the raster data sets
        timelines[[i]] <- lubridate::as_date(sits_timeline(cube_probs, i))
        start_date     <- timelines[[i]][1]
        end_date       <- timelines[[i]][length(timelines[[i]])]

        # # define the filename for the classified image
        bands[i] <- .sits_class_band_name(cube_probs[1,]$name, type, start_date, end_date)
        files[i] <- .sits_raster_filename(output_dir, cube_probs[1,]$name, type, start_date, end_date)
    }


    # inherit the dimension parameters from probability cube
    params <- .sits_raster_params(.sits_cube_robj(cube_probs))
    # create a new RasterLayer for a defined period and generate the associated metadata
    cube_labels <- .sits_create_cube(service = "LOCALHOST", URL     = "http://127.0.0.1",
                                     satellite = cube_probs$satellite, sensor = cube_probs$sensor,
                                     name, bands, labels,
                                     scale_factors, missing_values, minimum_values, maximum_values,
                                     timelines, nrows = params$nrows, ncols = params$ncols,
                                     xmin  = params$xmin, xmax  = params$xmax,
                                     ymin  = params$ymin, ymax  = params$ymax,
                                     xres  = params$xres, yres  = params$yres, crs   = params$crs,
                                     files = files)

    return(cube_labels)
}

#' @title Creates the description of a data cube
#' @name .sits_create_cube
#'
#' @description Uses the configuration file to print information and save metadata about a
#' data cube.
#'
#' @param service            Name of the web service that has provided metadata about the cube.
#' @param URL                URL of the provider
#' @param satellite          Name of satellite
#' @param sensor             Name of sensor
#' @param name               Name of the data cube.
#' @param bands              Vector with the names of the bands.
#' @param labels             Vector with labels (only valid for classified data).
#' @param scale_factors      Vector with scale factor for each band.
#' @param missing_values     Vector with missing values for each band.
#' @param minimum_values     Vector with minimum values for each band.
#' @param maximum_values     Vector with maximum values for each band.
#' @param timelines          List with vectors of valid timelines for each band.
#' @param nrows              Number of rows in the cube.
#' @param ncols              Number of columns in the cube.
#' @param xmin               Spatial extent (xmin).
#' @param ymin               Spatial extent (ymin).
#' @param xmax               Spatial extent (xmax).
#' @param ymax               Spatial extent (ymin).
#' @param xres               Spatial resolution (x dimension).
#' @param yres               Spatial resolution (y dimension).
#' @param crs                CRS for cube.
#' @param files              Vector with associated files.
#'
.sits_create_cube <- function(service, URL, satellite, sensor, name, bands, labels,
                              scale_factors, missing_values, minimum_values, maximum_values,
                              timelines, nrows, ncols, xmin, xmax, ymin, ymax, xres, yres, crs,
                              files = NULL) {


    # create a tibble to store the metadata
    cube.tb <- tibble::tibble(service        = service,
                              URL            = URL,
                              satellite      = satellite,
                              sensor         = sensor,
                              name           = name,
                              bands          = list(bands),
                              labels         = list(labels),
                              scale_factors  = list(scale_factors),
                              missing_values = list(missing_values),
                              minimum_values = list(minimum_values),
                              maximum_values = list(maximum_values),
                              timeline       = list(timelines),
                              nrows          = nrows,
                              ncols          = ncols,
                              xmin           = xmin,
                              xmax           = xmax,
                              ymin           = ymin,
                              ymax           = ymax,
                              xres           = xres,
                              yres           = yres,
                              crs            = crs,
                              files          = list(files))

    return(cube.tb)
}

#' @title Determine the cube params to write in the metadata
#' @name .sits_raster_params
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Based on the R object associated to a raster object, determine its params
#'
#' @param r_obj    An R object associated to a Raster (Layer, Brick or Stack)
#' @return A tibble with the cube params
.sits_raster_params <- function(r_obj) {

    params.tb <- tibble::tibble(
        nrows = raster::nrow(r_obj),
        ncols = raster::ncol(r_obj),
        xmin  = raster::xmin(r_obj),
        xmax  = raster::xmax(r_obj),
        ymin  = raster::ymin(r_obj),
        ymax  = raster::ymax(r_obj),
        xres  = raster::xres(r_obj),
        yres  = raster::yres(r_obj),
        crs   = as.character(raster::crs(r_obj))
    )
    return(params.tb)
}
#' @title Define a name for classified band
#' @name .sits_class_band_name
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Creates a name for a raster layer with associated temporal information,
#'
#' @param name           Original cube name (without temporal information).
#' @param type           Type of output
#' @param start_date     Starting date of the time series classification.
#' @param end_date       End date of the time series classification.
#' @return Name of the classification file for the required interval.
.sits_class_band_name <- function(name, type, start_date, end_date){
    y1 <- lubridate::year(start_date)
    m1 <- lubridate::month(start_date)
    y2 <- lubridate::year(end_date)
    m2 <- lubridate::month(end_date)

    band_name <- paste0(name, "_", type, "_", y1, "_", m1, "_", y2, "_", m2)

    return(band_name)
}

#' @title Define a filename associated to one classified raster layer
#' @name .sits_raster_filename
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Creates a filename for a raster layer with associated temporal information,
#'                 given a basic filename.
#' @param output_dir     Output directory
#' @param name           Original cube name (without temporal information).
#' @param type           Type of output
#' @param start_date    Starting date of the time series classification.
#' @param end_date      End date of the time series classification.
#' @return Name of the classification file for the required interval.
.sits_raster_filename <- function(output_dir, name, type, start_date, end_date){
    y1 <- lubridate::year(start_date)
    m1 <- lubridate::month(start_date)
    y2 <- lubridate::year(end_date)
    m2 <- lubridate::month(end_date)

    file_name <- paste0(output_dir,"/", name, "_", type, "_", y1, "_", m1, "_", y2, "_", m2, ".tif")

    return(file_name)
}
#' @title Find the bands associated to a cube
#' @name .sits_cube_bands
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Given a data cube, retrieves the bands
#'
#' @param cube          Metadata about a data cube
#' @return  Vector of bands available in the data cube
.sits_cube_bands <- function(cube) {
    return(cube$bands[[1]])
}

#' @title Find the web service associated to a cube
#' @name .sits_cube_service
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Given a data cube, informs the web services
#' @param cube     Metadata about a data cube
#' @return Name of web service
.sits_cube_service <- function(cube) {
    return(cube[1,]$service)
}

#' @title Return a file associated to a data cube, given an index
#' @name .sits_cube_file
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description     Given a data cube and an index, retrieve the file
#' @param cube      Metadata about a data cube
#' @param index     Index for file to be retrived
#' @return          Name of file
.sits_cube_file <- function(cube, index = 1) {
    ensurer::ensure_that(index, (.) <= length(cube$files[[1]]),
                         err_desc = ".sits_cube_file: files is not available - index is out of range")
    return(cube$files[[1]][index])
}
#' @title Return all file associated to a data cube
#' @name .sits_cube_files
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Given a data cube and an index, retrieve the files
#' @param cube     Metadata about a data cube
#' @return         Vector of files
.sits_cube_files <- function(cube) {
    return(cube$files[[1]])
}
#' @title Return all labels associated to a data cube
#' @name .sits_cube_labels
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Given a data cube, retrieve the ;abels
#' @param cube     Metadata about a data cube
#' @return         Vector of labels
.sits_cube_labels <- function(cube){
    return(cube$labels[[1]])
}

#' @title Return the timeline associated to a data cube, given an index
#' @name .sits_cube_timeline
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Given a data cube, retrieve the timeline
#' @param cube     Metadata about a data cube
#' @param index    Index of timeline list
#' @return         Vector of times for an index
.sits_cube_timeline <- function(cube, index = 1){
    ensurer::ensure_that(index, (.) <= length(cube$timeline[[1]]),
                         err_desc = ".sits_cube_timeline: index out of range")
    return(cube$timeline[[1]][[index]])
}

#' @title Given a file index, return the Raster object associated the indexed file
#' @name .sits_cube_robj
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description     Given a data cube, retrieve the timeline
#' @param cube      Metadata about a data cube
#' @param index     Index for file to be retrived
#' @return          Raster object associated to the indexed file
#'
.sits_cube_robj <- function(cube, index = 1){
    ensurer::ensure_that(index, (.) <= length(cube$files[[1]]),
                         err_desc = ".sits_cube_file: files is not available - index is out of range")

    return(raster::brick(cube$files[[1]][index]))
}

#' @title Return the Raster objects associated to a data cube
#' @name .sits_cube_all_robjs
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description     Given a data cube, retrieve the Raster objects
#' @param cube      Metadata about a data cube
#' @return          list of raster object associated to the indexed file
#'
.sits_cube_all_robjs <- function(cube){
    nfiles <- length(cube$files[[1]])
    robjs <- vector("list", nfiles )
    for (i in 1:nfiles)
        robjs[[i]] <- raster::brick(cube$files[[1]][i])
    return(robjs)
}

#' @title Retrieve the missing values for a data cube
#' @name .sits_cube_missing_values
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description     Given a data cube, retrieve the missing values
#' @param cube      Metadata about a data cube
#' @return          Vector of missing values.
.sits_cube_missing_values <- function(cube){
    return(cube$missing_values[[1]])
}

#' @title Retrieve the minimum values for a data cube
#' @name .sits_cube_minimum_values
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description     Given a data cube, retrieve the minimum values
#' @param cube      Metadata about a data cube
#' @return          Vector of minimum values.
.sits_cube_minimum_values <- function(cube){
    return(cube$minimum_values[[1]])
}
#' @title Retrieve the minimum values for a data cube
#' @name .sits_cube_maximum_values
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description     Given a data cube, retrieve the maximum values
#' @param cube      Metadata about a data cube
#' @return          Vector of maximum values.
.sits_cube_maximum_values <- function(cube){
    return(cube$maximum_value[[1]])
}

#' @title Retrieve the scale factors for a data cube
#' @name .sits_cube_scale_factors
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description     Given a data cube, retrieve the scale factors
#' @param cube      Metadata about a data cube
#' @return          Vector of scale factors
.sits_cube_scale_factors <- function(cube){
    return(cube$scale_factors[[1]])
}

#' @title Given a vector of files, return the Raster object associated with the first file
#' @name .sits_files_robj
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description     Given a vector of files and index, return the Raster object for the file
#' @param files     Vector of files
#' @return          Raster object associated to the indexed file
#'
.sits_files_robj <- function(files){
    return(raster::brick(files[1]))
}
