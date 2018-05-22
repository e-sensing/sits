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
#' @param  name                  name of the coverage file
#' @param  timeline.vec          vector of dates with the timeline of the bands
#' @param  bands.vec             vector of bands contained in the Raster Brick set (in the same order as the files)
#' @param  scale_factors.vec     vector of scale factors (one per band)
#' @param  missing_values.vec    vector of missing values (one per band)
#' @param  minimum_values.vec    minimum values for each band (only for raster data)
#' @param  files.vec             vector with the file paths of the raster files
#' @return raster.tb         tibble with metadata information about a raster data set
#'
.sits_coverage_raster <- function(name,
                                  timeline.vec,
                                  bands.vec,
                                  scale_factors.vec,
                                  missing_values.vec,
                                  minimum_values.vec,
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
#' @param  raster.tb         tibble with metadata about the input RasterBrick objects
#' @param  samples.tb        samples used for training the classification model
#' @param  file              generic name of the files that will contain the RasterLayers
#' @param  interval          classification interval
#' @return raster_layers.tb  tibble with metadata about the output RasterLayer objects
#'
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

    # labels come from samples.tb
    labels <- sits_labels(samples.tb)$label

    # lists that store the content of the raster layers (classified values)
    rasters_class   <- vector("list", length = length(subset_dates.lst))
    bands_class     <- vector( length = length(subset_dates.lst))
    files_class     <- vector(length = length(subset_dates.lst))

    # lists that store the content of the raster bricks
    rasters_probs    <- vector("list", length = length(subset_dates.lst))
    bands_probs     <- vector(length = length(subset_dates.lst))
    files_probs     <- vector(length = length(subset_dates.lst))
    n_layers_probs  <- length(labels)

    scale_factors_probs   <- rep(0.001,  length(subset_dates.lst))
    missing_values_probs  <- rep(-9999,  length(subset_dates.lst))
    minimum_values_probs  <- rep(0, length(subset_dates.lst))

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
        file_probs <- paste0(file, "_probs_")
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
                                                      files.vec          = files_probs)

    coverage.tb <- dplyr::bind_rows(coverage.tb, coverage_probs.tb)

    return(coverage.tb)
}
#' @title Creates a tibble with information about a set of raster bricks
#' @name .sits_create_raster_coverage
#'
#' @description creates a tibble with metadata about a given coverage
#'
#' @param raster.lst               list of Raster objects associated with the raster coverages
#' @param service                  time series service
#' @param name                     name of the coverage
#' @param timeline.lst             list - coverage timelines
#' @param bands.vec                vector - names of bands
#' @param labels.vec               vector - labels for classified image
#' @param scale_factors.vec        vector - scale factors
#' @param missing_values.vec       vector - missing values
#' @param minimum_values.vec       vector - minimum values
#' @param files.vec                vector - names of raster files where the data is stored
#'
.sits_create_raster_coverage <- function(raster.lst,
                                         service,
                                         name,
                                         timeline.lst,
                                         bands.vec,
                                         labels.vec,
                                         scale_factors.vec,
                                         missing_values.vec,
                                         minimum_values.vec,
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
    # preserve the names of the bands on the list of raster objects and in the files
    names(raster.lst) <- bands.vec
    names(files.vec)  <- bands.vec

    # get CRS
    crs = as.character(raster::crs(r_obj))

    # create a tibble to store the metadata
    coverage.tb <- .sits_create_coverage (raster.lst,
                                          name = name,
                                          service = service,
                                          bands.vec = bands.vec,
                                          labels.vec = labels.vec,
                                          scale_factors.vec = scale_factors.vec,
                                          missing_values.vec = missing_values.vec,
                                          minimum_values.vec = minimum_values.vec,
                                          timeline.lst = timeline.lst,
                                          nrows = nrows, ncols = ncols,
                                          xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                                          xres = xres, yres = yres, crs = crs,
                                          files.vec = files.vec)

    return(coverage.tb)
}

#' @title Find the time index of the blocks to be extracted for classification
#' @name .sits_get_time_index
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Obtains the indexes of the blocks to be extract for each time interval
#' associated with classification
#'
#' @param class_info.tb tibble with information required for classification
#' @return time_index   list with indexes of the input data set associated to each time interval
#'                      used for classification
#'
.sits_get_time_index <- function(class_info.tb) {
    # find the subsets of the input data
    dates_index.lst <- class_info.tb$dates_index[[1]]

    #retrieve the timeline of the data
    timeline <- class_info.tb$timeline[[1]]

    # retrieve the bands
    bands <- class_info.tb$bands[[1]]

    #retrieve the time index
    time_index.lst  <- .sits_time_index(dates_index.lst, timeline, bands)

    return(time_index.lst)

}
#' @title Obtain the names of the columns of the matrix to be classified
#' @name .sits_get_attr_names
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Obtains the names of the columns of the matrix to be classified
#'
#' @param class_info.tb    tibble with information required for classification
#' @return attr_names      vector with the names of the columns with the matrix to be classified
.sits_get_attr_names <- function(class_info.tb){

    # get information about the bands
    bands <- class_info.tb$bands[[1]]

    # find the subsets of the input data
    dates_index.lst <- class_info.tb$dates_index[[1]]

    # find the number of the samples
    nsamples <- dates_index.lst[[1]][2] - dates_index.lst[[1]][1] + 1

    # define the column names
    attr_names.lst <- bands %>%
        purrr::map(function(b){
            attr_names_b <- c(paste(c(rep(b, nsamples)), as.character(c(1:nsamples)), sep = ""))
            return(attr_names_b)
        })
    attr_names <- unlist(attr_names.lst)
    attr_names <- c("original_row", "reference", attr_names)

    return(attr_names)
}
#' @title Try a best guess for the type of sensor/satellite
#' @name .sits_guess_satellite
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Based on the projection, tries to guess what is the satellite
#'
#' @param r_obj         a raster object
#' @return satellite    name of the satellite (or sensor)
#'
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
#'                 given a basic filename
#'
#' @param file          original file name (without temporal information)
#' @param start_date    starting date of the time series classification
#' @param end_date      end date of the time series classification
#' @return file_name    name of the classification file for the required interval
#'
.sits_raster_filename <- function(file, start_date, end_date){


    file_base <- tools::file_path_sans_ext(file)
    y1 <- lubridate::year(start_date)
    m1 <- lubridate::month(start_date)
    y2 <- lubridate::year(end_date)
    m2 <- lubridate::month(end_date)

    file_name <- paste0(file_base,"_",y1,"_",m1,"_",y2,"_",m2,".tif")

    return(file_name)
}
#' @title Define the split of the data blocks for multicore processing
#' @name .sits_split_block_size
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description this functions defines the rows of the input data that will be
#' split to fit to be divided between the different cores
#'
#' @param data             data (data.table or matrix)
#' @param ncores           number of cores for processing
#' @return block_size.lst  list of pairs of positions (first row, last row) to be assigned to each core
#'
.sits_split_block_size <- function(data, ncores){

    # number of rows in the data
    nrows <- nrow(data)
    # find the number of rows per core
    step <- ceiling(nrows/ncores)

    # create a vector with the initial rows per block
    blocks <- seq(from = 1, to = nrows, by = step)

    # create a list to store the result
    block_size.lst <- vector("list", ncores)

    # fill the list with the initial and final row per block
    for (i in 1:length(blocks)) {
        block_size_start <- blocks[i]
        block_size_end   <- block_size_start + step - 1
        if (i == ncores )
            block_size_end <- nrows
        block_size.lst[[i]] <- c(block_size_start, block_size_end)
    }
    return(block_size.lst)
}

#' @title Split a data.table or a matrix for multicore processing
#' @name .sits_split_data
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description this functions splits a data.table into a list of chunks for multicore processing
#'
#' @param data             data (data.table or matrix)
#' @param ncores           number of cores for processing
#' @return block_size.lst  list of pairs of positions (first row, last row) to be assigned to each core
#'
.sits_split_data <- function(data, ncores){

    # number of rows in the data
    nrows <- nrow(data)
    # find the number of rows per core
    step <- ceiling(nrows/ncores)

    # create a vector with the initial rows per block
    blocks <- seq(from = 1, to = nrows, by = step)

    # create a list to store the result
    block.lst <- vector("list", ncores)

    # fill the list with the initial and final row per block
    for (i in 1:length(blocks)) {
        start <- blocks[i]
        end   <- start + step - 1
        if (i == ncores )
            end <- nrows
        block.lst[[i]] <- data[start:end,]
    }
    return(block.lst)
}


