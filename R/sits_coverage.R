#' @title Provides information about one coverage used to retrieve data
#' @name sits_coverage
#'
#' @description Defines a coverage to retrieve data. As default, it uses the metadata about a
#' chosen coverage stored in an yml configuration file.
#' \itemize{
#' \item{service: }{Name of time series service that provides the coverage (e.g., "WTSS", "SATVEG", "RASTER").}
#' \item{name: }{Name of the coverage (must be unique).}
#' \item{bands: }{Vector of bands.}
#' \item{scale_factor: }{Vector of scale factors.}
#' \item{missing_values: }{Vector of missing values.}
#' \item{minimum_values: }{Vector of minimum values.}
#' \item{maximum_values: }{Vector of maximum values.}
#' \item{timeline: }{The timelines of the coverage (more than one if data has been classified).}
#' \item{xmin: }{Spatial extent (xmin).}
#' \item{ymin: }{Spatial extent (ymin).}
#' \item{xmax: }{Spatial extent (xmax).}
#' \item{ymax: }{Spatial extent (ymin).}
#' \item{xres: }{Spatial resolution (x dimension).}
#' \item{yres: }{Spatial resolution (y dimension).}
#' \item{crs: }{Projection CRS.}
#' \item{files: }{Files associated with the coverage (in case of raster data).}
#' }
#'
#' @param service           Name of the time series service.
#' @param name              Name of the coverage.
#' @param timeline          Vector with the timeline of the coverage.
#' @param bands             Vector of bands.
#' @param scale_factors     Vector with the scale factor for each band.
#' @param missing_values    Vector of missing values for each band.
#' @param minimum_values    Vector of minimum values for each band.
#' @param maximum_values    Vector of maximum values for each band.
#' @param files             Vector of file names for each band (only for raster data).
#' @seealso To see the available values for the parameters above use \code{\link{sits_services}}, \code{\link{sits_config}} or \code{\link{sits_show_config}}.
#' @examples
#' \donttest{
#' # Example 1. Retrieve information about a WTSS coverage
#' coverage.tb <- sits_coverage(service = "WTSS-INPE", name = "MOD13Q1")
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
                          name,
                          timeline       = NULL,
                          bands          = NULL,
                          missing_values = NULL,
                          scale_factors  = NULL,
                          minimum_values = NULL,
                          maximum_values = NULL,
                          files          = NA) {
    # if no service is specified, but the names of files are provided,
    # assume we are dealing with raster data
    if (service == "RASTER") {
        r <- suppressWarnings(rgdal::GDALinfo(files, silent = FALSE))
        ensurer::ensure_that(r, all(!purrr::is_null(.)),
                                        err_desc = "sits_coverage: raster files cannot be accessed")
    }

    # pre-condition
    if (any(!is.na(files))) {
        if (all(file.exists(files)) && service != "RASTER") {
            msg <- paste0("inconsistent specification of coverage parameters - files should
                          be provided only when service is RASTER")
            .sits_log_error(msg)
            message(msg)
            return(NULL)
        }
    }

    # pre-condition
    .sits_check_service(service)
    # get the protocol associated with the service
    protocol <- .sits_get_protocol(service)

    if (protocol == "WTSS") {
        tryCatch({
            URL  <- .sits_get_server(service)
            # obtains information about the available coverages
            wtss.obj   <- wtss::WTSS(URL)

        }, error = function(e){
            msg <- paste0("WTSS service not available at URL ", URL)
            .sits_log_error(msg)
            message(msg)
        })

        # create a coverage
        coverage.tb <- .sits_coverage_WTSS(wtss.obj, service, name, bands)
    }
    else if (protocol == "SATVEG") {
        coverage.tb <- .sits_coverage_SATVEG(name, timeline, bands)
    }
    else
        coverage.tb <- .sits_coverage_raster(name = name,
                                             timeline.vec       = timeline,
                                             bands.vec          = bands,
                                             scale_factors.vec  = scale_factors,
                                             missing_values.vec = missing_values,
                                             minimum_values.vec = minimum_values,
                                             maximum_values.vec = maximum_values,
                                             files.vec          = files)
    return(coverage.tb)
}

#' @title Creates a coverage metadata
#' @name .sits_create_coverage
#'
#' @description Uses the configuration file to print information and save metadata about a
#' chosen coverage.
#'
#' @param r_objs.lst         List of raster objects contained in the coverage.
#' @param name               Name of the coverage.
#' @param service            Name of the time series service.
#' @param bands.vec          Vector with the names of the bands.
#' @param labels.vec         Vector with labels (only valid for classified data).
#' @param scale_factors.vec  Vector with scale factor for each band.
#' @param missing_values.vec Vector with missing values for each band.
#' @param minimum_values.vec Vector with minimum values for each band.
#' @param maximum_values.vec Vector with maximum values for each band.
#' @param timeline.lst       List with vectors of valid timelines for each band.
#' @param nrows              Number of rows in the coverage.
#' @param ncols              Number of columns in the coverage.
#' @param xmin               Spatial extent (xmin).
#' @param ymin               Spatial extent (ymin).
#' @param xmax               Spatial extent (xmax).
#' @param ymax               Spatial extent (ymin).
#' @param xres               Spatial resolution (x dimension).
#' @param yres               Spatial resolution (y dimension).
#' @param crs                CRS for coverage.
#' @param files.vec          Vector with associated files.
.sits_create_coverage <- function(r_objs.lst,
                                  name,
                                  service,
                                  bands.vec,
                                  labels.vec,
                                  scale_factors.vec,
                                  missing_values.vec,
                                  minimum_values.vec,
                                  maximum_values.vec,
                                  timeline.lst,
                                  nrows, ncols, xmin, xmax, ymin, ymax,
                                  xres, yres, crs, files.vec) {
    # create a tibble to store the metadata
    coverage.tb <- tibble::tibble(r_objs         = list(r_objs.lst),
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

    return(coverage.tb)
}

#' @title Provides information about one coverage of the WTSS service
#' @name .sits_coverage_wtss
#'
#' @description Uses the WTSS services to print information and save metadata about a
#' chosen coverage.
#'
#' @param wtss.obj   R WTSS object associated to the service.
#' @param service    Name of the service.
#' @param name       Name of the coverage.
#' @param bands      Name of the bands.
.sits_coverage_WTSS <- function(wtss.obj, service, name, bands) {
    # obtains information about the available coverages
    coverages.vec    <- wtss::listCoverages(wtss.obj)

    # is the coverage in the list of coverages?
    ensurer::ensure_that(name, (.) %in% coverages.vec,
                         err_desc = ".sits_coverage_wtss: coverage is not available in the WTSS server")

    # describe the coverage
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
                             err_desc = ".sits_coverage_WTSS: requested band not provided by WTSS service.")
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
    coverage.tb <- .sits_create_coverage(list(wtss.obj), name, service,
                                         bands.vec, labels.vec, scale_factors.vec,
                                         missing_values.vec, minimum_values.vec,
                                         maximum_values.vec, timeline.lst,
                                         nrows, ncols, xmin, xmax, ymin, ymax,
                                         xres, yres, crs, files.vec = NA)

    # return the tibble with coverage info
    return(coverage.tb)
}

#' @title Provides information about one coverage of the SATVEG time series service
#' @name .sits_coverage_satveg
#'
#' @description Creates a tibble with metadata about a given coverage.
#'
#' @param name       Name of the coverage.
#' @param timeline   Timeline of the coverage.
#' @param bands      Bands of the coverage.
.sits_coverage_SATVEG <- function(name, timeline, bands) {

    service <- "SATVEG"
    # get the bands
    bands.vec <- .sits_get_bands(service, name)

    # check if requested bands are in provided bands
    if (!purrr::is_null(bands)) {
        ensurer::ensure_that(bands.vec, all(bands %in% .),
                             err_desc = ".sits_coverage_SATVEG: requested band not provided by WTSS service.")
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

    # get the size of the coverage
    size <- .sits_get_size(service, name)
    nrows <- as.integer(size["nrows"])
    ncols <- as.integer(size["ncols"])

    # get the bounding box of the coverage
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
    coverage.tb <- .sits_create_coverage(r_objs.lst = NA,
                                         name, service,
                                         bands.vec, labels.vec, scale_factors.vec,
                                         missing_values.vec, minimum_values.vec, maximum_values.vec,
                                         timeline.lst, nrows, ncols, xmin, xmax, ymin, ymax,
                                         xres, yres, crs,
                                         files.vec = NA)

    return(coverage.tb)
}

#' @title Create a metadata tibble to store the description of a spatio-temporal raster dataset
#' @name .sits_coverage_raster
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description  This function creates a tibble containing the metadata for
#'               a set of spatio-temporal raster files, organized as a set of "Raster Bricks".
#'               These files should be of the same size and
#'               projection. Each raster brick file should contain one band
#'               per time step. Different bands are archived in different raster files.
#'
#' @param  name                  Name of the coverage file.
#' @param  timeline.vec          Vector of dates with the timeline of the bands.
#' @param  bands.vec             Vector of bands contained in the Raster Brick set (in the same order as the files).
#' @param  scale_factors.vec     Vector of scale factors (one per band).
#' @param  missing_values.vec    Vector of missing values (one per band).
#' @param  minimum_values.vec    Minimum values for each band (only for raster data).
#' @param  maximum_values.vec    Maximum values for each band (only for raster data).
#' @param  files.vec             Vector with the file paths of the raster files.
#' @return A tibble with metadata information about a raster data set.
.sits_coverage_raster <- function(name,
                                  timeline.vec,
                                  bands.vec,
                                  scale_factors.vec,
                                  missing_values.vec,
                                  minimum_values.vec,
                                  maximum_values.vec,
                                  files.vec) {
    ensurer::ensure_that(bands.vec, length(.) == length(files.vec),
                         err_desc = "sits_coverageRaster: number of bands does not match number of files")
    ensurer::ensure_that(name, !purrr::is_null(.),
                         err_desc = "sits_coverageRaster: name of the coverega must be provided")
    ensurer::ensure_that(bands.vec, !purrr::is_null(.),
                         err_desc = "sits_coverageRaster - bands must be provided")
    ensurer::ensure_that(files.vec, !purrr::is_null(.),
                         err_desc = "sits_coverageRaster - files must be provided")

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

    coverage.tb <- .sits_create_raster_coverage(raster.lst         = brick.lst,
                                                service            = "RASTER",
                                                name               = name,
                                                timeline.lst       = list(timeline.vec),
                                                bands.vec          = bands.vec,
                                                labels.vec         = labels.vec,
                                                scale_factors.vec  = scale_factors.vec,
                                                missing_values.vec = missing_values.vec,
                                                minimum_values.vec = minimum_values.vec,
                                                maximum_values.vec = maximum_values.vec,
                                                files.vec          = files.vec)

    return(coverage.tb)
}

#' @title Create a set of RasterLayer objects to store time series classification results
#' @name .sits_coverage_raster_classified
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a tibble containing metadata about a set of RasterBrick objects
#' containing time series (each Brick has information for one band) and creates a
#' set of RasterLayers to store the classification result. Each RasterLayer corresponds
#' to one time step. The time steps are specified in a list of dates.
#'
#' @param  raster.tb         Tibble with metadata about the input RasterBrick objects.
#' @param  samples.tb        Samples used for training the classification model.
#' @param  file              Generic name of the files that will contain the RasterLayers.
#' @param  interval          Classification interval.
#' @return A tibble with metadata about the output RasterLayer objects.
.sits_coverage_raster_classified <- function(raster.tb, samples.tb, file, interval){
    # ensure metadata tibble exists
    ensurer::ensure_that(raster.tb, NROW(.) > 0,
                         err_desc = "sits_classify_raster: need a valid metadata for coverage")

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

    # get the name of the coverage
    name   <-  paste0(raster.tb[1,]$name, "-class")
    # create a new RasterLayer for a defined period and generate the associated metadata
    coverage.tb <- .sits_create_raster_coverage(raster.lst         = rasters_class,
                                                service            = "RASTER",
                                                name               = name,
                                                timeline.lst       = timeline_rasters,
                                                bands.vec          = bands_class,
                                                labels.vec         = labels,
                                                scale_factors.vec  = scale_factors_class,
                                                missing_values.vec = missing_values_class,
                                                minimum_values.vec = minimum_values_class,
                                                maximum_values.vec = maximum_values_class,
                                                files.vec          = files_class)

    coverage_probs.tb <- .sits_create_raster_coverage(raster.lst         = rasters_probs,
                                                      service            = "RASTER",
                                                      name               = name,
                                                      timeline.lst       = timeline_rasters,
                                                      bands.vec          = bands_probs,
                                                      labels.vec         = labels,
                                                      scale_factors.vec  = scale_factors_probs,
                                                      missing_values.vec = missing_values_probs,
                                                      minimum_values.vec = minimum_values_probs,
                                                      maximum_values.vec = maximum_values_probs,
                                                      files.vec          = files_probs)

    coverage.tb <- dplyr::bind_rows(coverage.tb, coverage_probs.tb)

    return(coverage.tb)
}

#' @title Creates a tibble with information about a set of raster bricks
#' @name .sits_create_raster_coverage
#'
#' @description Creates a tibble with metadata about a given coverage.
#'
#' @param raster.lst               List of Raster objects associated with the raster coverages.
#' @param service                  Time series service.
#' @param name                     Name of the coverage.
#' @param timeline.lst             List of coverage timelines.
#' @param bands.vec                Vector with names of bands.
#' @param labels.vec               Vector of labels for classified image.
#' @param scale_factors.vec        Vector of scale factors.
#' @param missing_values.vec       Vector of missing values.
#' @param minimum_values.vec       Vector of minimum values.
#' @param maximum_values.vec       Vector of maximum values.
#' @param files.vec                Vector of names of raster files where the data is stored.
.sits_create_raster_coverage <- function(raster.lst,
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
    # get the size of the coverage
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
    coverage.tb <- .sits_create_coverage(raster.lst,
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
                                         files.vec = files.vec)

    return(coverage.tb)
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
