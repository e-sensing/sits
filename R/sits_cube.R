#' @title Defines a data cube
#' @name sits_cube
#'
#' @description Defines a cube to retrieve data. '
#' Cubes can be of the following types:
#' \itemize{
#'  \item{"WTSS": }{Web Time Series Service - used to get time series}
#'  \item{"SATVEG": }{ SATVEG Time Series Service - used to get time series}
#'  \item{"BRICK": }{Raster Brick files}
#' }
#'
#' @param type              Type of cube (one of "WTSS", "SATVEG", "BRICK")
#' @param name              Name of the data cube.
#' @param URL               URL of the service provider (for WTSS).
#' @param satellite         Name of satellite
#' @param sensor            Name of sensor
#' @param timeline          Vector with the timeline of the collection
#'                          (only for local files)
#' @param bands             Vector of bands.
#' @param files             Vector of file names for each band
#'                          (only for raster data).
#' @param service           Name of the data service (deprecated)
#' @param name              Name associated to local and remote data
#'                          (deprecated)
#'
#' @seealso To see the available values for the parameters above
#' use \code{\link{sits_config}} or \code{\link{sits_config_show}}.
#' @examples
#' \donttest{
#' # Example 1. Create a data cube based on a WTSS service
#' cube_wtss <- sits_cube(type = "WTSS",
#'           name = "MOD13Q1",
#'           URL = "http://www.esensing.dpi.inpe.br/wtss/")
#'
#' # Example 2. Create a data cube based on the SATVEG service
#' cube_satveg <- sits_cube(type = "SATVEG",
#'                          name = "terra")
#'
#' # Example 3. Create a raster cube based on bricks
#' # inform the files that make up a raster brick with 392 time instances
#' files <- c(system.file("extdata/raster/mod13q1/sinop-crop-ndvi.tif",
#'            package = "sits"))
#'
#' # create a raster cube file based on the information about the files
#' raster.tb <- sits_cube(type = "BRICK",
#'                        name      = "Sinop-crop",
#'                        satellite = "TERRA",
#'                        sensor    = "MODIS",
#'                        timeline  = timeline_modis_392,
#'                        bands     = "ndvi",
#'                        files     = files)
#'
#' }
#' @export
sits_cube <- function(type           = NULL,
                      name,
                      URL            = NULL,
                      satellite      = NULL,
                      sensor         = NULL,
                      timeline       = NULL,
                      bands          = NULL,
                      files          = NULL,
                      service        = NULL) {

    # test if type has been provided
    if(purrr::is_null(type) && !purrr::is_null(service)) {
        type <- toupper(service)
        message("sits_cube: use of service variable is deprecated, see docs")
    }
    type  <- toupper(type)
    types <- .sits_config_types()
    assertthat::assert_that(type %in% types,
            msg = paste0("sits_cube: required type not available\n",
                         "please use one of ", types))

    if (type == "WTSS") {
        # find the URL of the WTSS service, if not provided
        assertthat::assert_that(!purrr::is_null(URL),
                    msg = "sits_cube: WTSS service needs URL")

        # is the WTSS service working?
        coverages <- wtss::list_coverages(URL)
        assertthat::assert_that(!purrr::is_null(coverages),
                    msg = "sits_cube: WTSS service not responding")
        # is the cube in the list of cubes?
        assertthat::assert_that(name %in% coverages,
                    msg = paste0("sits_cube: ", name,
                                 " not available in the WTSS server"))

        # create a cube
        cube.tb <- .sits_wtss_cube(URL, name, bands)

    }
    else if (type == "SATVEG") {
        # check if SATVEG is working
        satveg_ok <- .sits_satveg_check()
        # if OK, go ahead a create a SATVEG cube
        if (satveg_ok)
            cube.tb <- .sits_satveg_cube(name)
    }
    else if (type == "BRICK"){
        assertthat::assert_that(!purrr::is_null(files),
                msg = "sits_cube: for type = BRICK, files must be provided")
        assertthat::assert_that(!purrr::is_null(satellite),
                msg = "sits_cube: for type = BRICK satelite must be provided")
        assertthat::assert_that(!purrr::is_null(sensor),
                msg = "sits_cube: for type = BRICK sensor must be provided")
        assertthat::assert_that(!purrr::is_null(bands),
                msg = "sits_cube: for type = BRICK bands must be provided")
        assertthat::assert_that(length(bands) == length(files),
                msg = "sits_cube: bands do not match files")
        assertthat::assert_that(!purrr::is_null(timeline),
                msg = "sits_cube: for type = BRICK timeline must be provided")

        # bands are lowercase
        bands <- tolower(bands)

        # is the satellite supported by SITS?
        satellite <- toupper(satellite)
        sats <- .sits_config_satellites()
        assertthat::assert_that(satellite %in% sats,
                msg = paste0("satellite not supported\n",
                             "Use one of ", sats))

        # is the sensor supported by SITS?
        sensor <- toupper(sensor)
        sensors <- .sits_config_sensors(satellite)
        assertthat::assert_that(sensor %in% sensors,
                msg = paste0("sensor not supported\n",
                             "Use one of ", sensors))

        # raster files
        # check if need to include "/vsicurl" to be read by GDAL
        files <- .sits_raster_check_webfiles(files)

        # check if the raster files can be read by GDAL
        .sits_raster_check_gdal_access(files)

        # check if the raster files are organised as bricks
        .sits_raster_check_bricks(files)

        # create a brick data cube
        cube.tb <- .sits_raster_cube(satellite = satellite,
                                     sensor    = sensor,
                                     name      = name,
                                     timeline  = timeline,
                                     bands     = bands,
                                     files     = files)
    }
    return(cube.tb)
}

#' @title Creates the description of a data cube
#' @name .sits_cube_create
#'
#' @description Print information and save metadata about a data cube.
#'
#' @param type               Web service with metadata about the cube.
#' @param URL                URL of the provider
#' @param satellite          Name of satellite
#' @param sensor             Name of sensor
#' @param name               Name of the data cube.
#' @param bands              Vector with the names of the bands.
#' @param labels             Vector with labels (only for classified data).
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
.sits_cube_create <- function(type, URL, satellite, sensor,
                              name, bands, labels,
                              scale_factors, missing_values,
                              minimum_values, maximum_values,
                              timelines, nrows, ncols, xmin,
                              xmax, ymin, ymax, xres, yres, crs,
                              files = NULL) {


    # create a tibble to store the metadata
    cube.tb <- tibble::tibble(type           = type,
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



#' @title Create a set of RasterLayer objects to store
#' data cube classification results (only the probs)
#' @name .sits_cube_classified
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Take a tibble containing metadata about a data cube
#' containing time series (each Brick has information for one band) and create a
#' set of RasterLayers to store the classification result.
#' Each RasterLayer corresponds to one time step.
#' The time steps are specified in a list of dates.
#'
#' @param  cube              Tibble with metadata about the input data cube.
#' @param  samples           Samples used for training the classification model.
#' @param  interval          Classification interval.
#' @param  output_dir        Prefix of the output files.
#' @param  version           Version of the output files
#' @return A tibble with metadata about the output RasterLayer objects.
.sits_cube_classified <- function(cube, samples, interval,
                                  output_dir, version) {
    # ensure metadata tibble exists
    assertthat::assert_that(NROW(cube) > 0,
        msg = ".sits_classify_cube: need a valid metadata for cube")

    # get the timeline of of the data cube
    timeline <- lubridate::as_date(sits_timeline(cube))

    # Get the reference start date and end date from the samples
    ref_start_date <- lubridate::as_date(samples[1,]$start_date)
    ref_end_date   <- lubridate::as_date(samples[1,]$end_date)

    # produce the breaks used to generate the output rasters
    subset_dates <- sits_timeline_match(timeline = timeline,
                                        ref_start_date = ref_start_date,
                                        ref_end_date = ref_end_date,
                                        interval = interval)

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

    # loop through the list of dates and create list of raster layers
    for (i in 1:n_objs) {

        # define the timeline for the raster data sets
        start_date     <- subset_dates[[i]][1]
        end_date       <- subset_dates[[i]][2]
        timelines[[i]] <- timeline[lubridate::as_date(timeline) >= start_date &
                                       lubridate::as_date(timeline) <= end_date]

        # define the filename for the classified image
        files[i] <- .sits_raster_filename(output_dir = output_dir,
                                          version = version,
                                          name = cube$name,
                                          type = "probs",
                                          start_date = start_date,
                                          end_date = end_date)
        bands[i] <- .sits_class_band_name(name = cube$name, type = "probs",
                                          start_date = start_date,
                                          end_date = end_date)
    }

    params <- .sits_raster_params(.sits_cube_robj(cube))
    # get the name of the cube
    name   <-  paste0(cube[1,]$name, "_probs")
    # set the metadata for the probability cube
    cube_probs <- .sits_cube_create(type            = "PROBS",
                                    URL             = "http://127.0.0.1",
                                    satellite       = cube$satellite,
                                    sensor          = cube$sensor,
                                    name            = name,
                                    bands           = bands,
                                    labels          = labels,
                                    scale_factors   = scale_factors,
                                    missing_values  = missing_values,
                                    minimum_values  = minimum_values,
                                    maximum_values  = maximum_values,
                                    timelines       = timelines,
                                    nrows           = params$nrows,
                                    ncols           = params$ncols,
                                    xmin            = params$xmin,
                                    xmax            = params$xmax,
                                    ymin            = params$ymin,
                                    ymax            = params$ymax,
                                    xres            = params$xres,
                                    yres            = params$yres,
                                    crs             = params$xmin,
                                    files           = files)

    class(cube_probs) <- c("probs_cube", class(cube_probs))
    return(cube_probs)
}
#' @title Create a set of RasterLayer objects
#'        to store data cube classification results (labelled classes)
#' @name .sits_cube_labelled
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a tibble containing metadata about a data cube wuth
#' classification probabilites and and creates a
#' set of RasterLayers to store the classification result. Each RasterLayer is
#' to one time step. The time steps are specified in a list of dates.
#'
#' @param  cube_probs        Metadata about the input data cube (probability).
#' @param  smoothing         (optional) smoothing method to be applied
#'                           ("none", "bayesian", "majority")
#' @param  output_dir        Output directory where to put the files
#' @param  version           Name of the version of the result
#' @return                   Metadata about the output RasterLayer objects.
.sits_cube_labelled <- function(cube_probs, smoothing, output_dir, version) {

    # labels come from the input cube
    labels <- .sits_cube_labels(cube_probs)

    # how many objects are to be created?
    n_objs <- length(.sits_cube_files(cube_probs))

    # lists that store the content of the raster layers (classified values))
    bands     <- vector(length = n_objs)
    files     <- vector(length = n_objs)
    timelines <- vector("list", length = n_objs)

    # set scale factors, missing values, minimum and maximum values
    scale_factors   <- rep(1, n_objs)
    missing_values  <- rep(-9999, n_objs)
    minimum_values  <- rep(1, n_objs)
    maximum_values  <- rep(length(labels), n_objs)

    # get the type of the cube
    type  <- paste0("class_", smoothing)
    # name of the cube
    name  <- paste0(cube_probs[1,]$name, "_", type)

    # loop through the list of dates and create list of raster layers
    for (i in 1:n_objs) {

        # define the timeline for the raster data sets
        timelines[[i]] <- lubridate::as_date(sits_timeline(cube_probs, i))
        start_date     <- timelines[[i]][1]
        end_date       <- timelines[[i]][length(timelines[[i]])]

        # # define the filename for the classified image
        bands[i] <- .sits_class_band_name(name = cube_probs[1,]$name,
                                          type = type,
                                          start_date = start_date,
                                          end_date = end_date)
        files[i] <- .sits_raster_filename(output_dir = output_dir,
                                          version = version,
                                          name = cube_probs[1,]$name,
                                          type = type,
                                          start_date = start_date,
                                          end_date = end_date)
    }


    # inherit the dimension parameters from probability cube
    params <- .sits_raster_params(.sits_cube_robj(cube_probs))
    # create a new RasterLayer for a defined period and generate metadata
    cube_labels <- .sits_cube_create(type           = "CLASSIFIED",
                                     URL            = "http://127.0.0.1",
                                     satellite      = cube_probs$satellite,
                                     sensor         = cube_probs$sensor,
                                     name           = name,
                                     bands          = bands,
                                     labels         = labels,
                                     scale_factors  = scale_factors,
                                     missing_values = missing_values,
                                     minimum_values = minimum_values,
                                     maximum_values = maximum_values,
                                     timelines      = timelines,
                                     nrows          = params$nrows,
                                     ncols          = params$ncols,
                                     xmin           = params$xmin,
                                     xmax           = params$xmax,
                                     ymin           = params$ymin,
                                     ymax           = params$ymax,
                                     xres           = params$xres,
                                     yres           = params$yres,
                                     crs            = params$crs,
                                     files          = files)

    class(cube_labels) <- c("classified_image", class(cube_labels))
    return(cube_labels)
}

#' @title Define a name for classified band
#' @name .sits_class_band_name
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Creates a name for a raster layer based on timeline
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

#' @title Check that the cube is valid
#' @name .sits_cube_check_validity
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description     Given a data cube, retrieve the scale factors
#' @param cube      Metadata about a data cube
#' @return          Boolean value
.sits_cube_check_validity <- function(cube){

    # check that the service is valid
    .sits_config_check(cube[1,]$type)

    check <- FALSE

    # check is WTSS service is working
    if (cube$type == "WTSS")
        check <- .sits_wtss_check(cube$URL)
    # check is SATVEG service is working
    else if (cube$type == "SATVEG")
        check <- .sits_satveg_check()
    # check if the raster files are organised as bricks
    else if (cube$type == "BRICK")
        check <- .sits_raster_check_bricks(cube$files[[1]])

    return(check)
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
    assertthat::assert_that(index <= length(cube$files[[1]]),
                   msg = ".sits_cube_file: index is out of range")
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
#' @name sits_cube_timeline
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Given a data cube, retrieve the timeline
#' @param cube     Metadata about a data cube
#' @param index    Index of timeline list
#' @return         Vector of times for an index
#' @export
sits_cube_timeline <- function(cube, index = 1){
    assertthat::assert_that(index <= length(cube$timeline[[1]]),
                         msg = ".sits_cube_timeline: index out of range")
    return(cube$timeline[[1]][[index]])
}

#' @title Given a file index, return the associated Raster object
#' @name .sits_cube_robj
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description     Given a data cube, retrieve the timeline
#' @param cube      Metadata about a data cube
#' @param index     Index for file to be retrived
#' @return          Raster object associated to the indexed file
#'
.sits_cube_robj <- function(cube, index = 1){
    assertthat::assert_that(index <= length(cube$files[[1]]),
                         msg = ".sits_cube_robj: index is out of range")
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
