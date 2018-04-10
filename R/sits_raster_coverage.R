#' @title Get a raster object from a raster classified coverage
#' @name .sits_get_robj
#' @description This function retrieves one or more raster layer objects stored in a raster coverage.
#'              It should be used to ensure that the raster objects are returned correctly.
#'
#' @param raster.tb  raster coverage (classified)
#' @param i          i-th element of the list to retrieve
#' @return r_obj     a raster layer with classification
#'
#
.sits_get_robj <- function(raster.tb, i) {

    ensurer::ensure_that(i, (.) <= nrow(raster.tb),
                         err_desc = "sits_get_raster: index of raster object cannot be retrieved")

    return(raster.tb[i,]$r_objs[[1]])
}
#' @title Create a set of RasterLayer objects to store time series classification results
#' @name .sits_create_classified_raster
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
#' @return raster_layers.tb  tibble with metadata about the output RasterLayer objects
#'
.sits_create_classified_raster <- function(raster.tb, samples.tb, file, interval){

    # ensure metadata tibble exists
    ensurer::ensure_that(raster.tb, NROW(.) > 0,
                         err_desc = "sits_classify_raster: need a valid metadata for coverage")

    # get the timeline of observations (required for matching dates)
    timeline <- raster.tb[1,]$timeline[[1]]

    # what is the reference start date?
    ref_start_date <- lubridate::as_date(samples.tb[1,]$start_date)
    # what is the reference end date?
    ref_end_date  <- lubridate::as_date(samples.tb[1,]$end_date)

    # produce the breaks used to generate the output rasters
    subset_dates.lst <- sits_match_timeline(timeline, ref_start_date, ref_end_date, interval)

    scale_factors <- c(1)
    missing_values <- c(-9999)
    minimum_values <- c(0.0)

    # there is one band only
    bands   <-  c("class")
    # labels come from samples.tb
    labels <- sits_labels(samples.tb)$label

    # loop through the list of dates and create list of raster layers to be created
    layer.lst <- subset_dates.lst %>%
        purrr::map(function(date_pair) {
            # create one raster layer per date pair
            r_obj <- raster.tb$r_objs[[1]][[1]]
            r_out <- raster::raster(r_obj)
            raster::dataType(r_out) <- "INT1U"

            # define the timeline for the classified image
            start_date <- date_pair[1]
            end_date   <- date_pair[2]
            timeline   <- c(start_date, end_date)

            # define the filename for the classified image
            filename <- .sits_raster_filename(file, start_date, end_date)
            r_out@file@name <- filename

            # get the name of the coverages
            name   <-  paste0(raster.tb[1,]$name, "-class-", start_date, "-", end_date)

            # create a new RasterLayer for a defined period and generate the associated metadata
            coverage.tb <- .sits_create_raster_coverage(raster.lst     = list(r_out),
                                                        service        = "RASTER",
                                                        name           = name,
                                                        timeline       = timeline,
                                                        bands          = bands,
                                                        labels         = labels,
                                                        scale_factors  = scale_factors,
                                                        missing_values = missing_values,
                                                        minimum_values = minimum_values,
                                                        files          = list(filename))

            return(coverage.tb)
        })

    # join all rows in a single tibble
    raster_class.tb <- dplyr::bind_rows(layer.lst)

    return(raster_class.tb)
}
#' @title Creates a tibble with information about a set of raster bricks
#' @name .sits_create_raster_coverage
#'
#' @description creates a tibble with metadata about a given coverage
#'
#' @param raster.lst        list of Raster objects associated with the raster coverages
#' @param service           time series service
#' @param name              name of the coverage
#' @param timeline          vector - coverage timeline
#' @param bands             vector - names of bands
#' @param labels            vector - labels for classified image
#' @param scale_factors     vector - scale factors
#' @param missing_values    vector - missing values
#' @param minimum_values    vector - minimum values
#' @param files             vector - names of raster files where the data is stored
#'
.sits_create_raster_coverage <- function(raster.lst,
                                         service,
                                         name,
                                         timeline,
                                         bands,
                                         labels = NULL,
                                         scale_factors,
                                         missing_values,
                                         minimum_values,
                                         files) {

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


    # if timeline is not provided, try a best guess
    if (purrr::is_null(timeline))
        timeline <- .sits_get_timeline("RASTER", "MOD13Q1")

    if (purrr::is_null(labels))
        labels <- c("Unclassified")

    # if scale factors are not provided, try a best guess
    if (purrr::is_null(scale_factors)) {
        message("Scale factors not provided - will use default values: please check they are valid")
        # try to guess what is the satellite
        satellite <- .sits_guess_satellite(r_obj)
        # retrieve the scale factors
        scale_factors <- .sits_get_scale_factors("RASTER", satellite, bands)
        # are the scale factors valid?
        ensurer::ensure_that(scale_factors, !(purrr::is_null(.)),
                             err_desc = "Not able to obtain scale factors for raster data")
    }
    else
        names(scale_factors) <- bands

    # if missing_values are not provided, try a best guess
    if (purrr::is_null(missing_values)) {
        message("Missing values not provided - will use default values: please check they are valid")
        # try to guess what is the satellite
        satellite      <- .sits_guess_satellite(r_obj)
        # try to retrieve the missing values
        missing_values <- .sits_get_missing_values("RASTER", satellite, bands)
        ensurer::ensure_that(missing_values, !(purrr::is_null(.)),
                             err_desc = "Not able to obtain scale factors for raster data")
    }
    else
        names(missing_values) <- bands

    if (purrr::is_null(minimum_values)) {
        minimum_values <- .sits_get_minimum_values("RASTER", bands)
    }

    # initiate writing
    # create a tibble to store the metadata
    coverage.tb <- tibble::tibble(r_objs         = list(raster.lst),
                                  name           = name,
                                  service        = service,
                                  bands          = list(bands),
                                  labels         = list(labels),
                                  scale_factors  = list(scale_factors),
                                  missing_values = list(missing_values),
                                  minimum_values = list(minimum_values),
                                  start_date     = as.Date(timeline[1]),
                                  end_date       = as.Date(timeline[length(timeline)]),
                                  timeline       = list(timeline),
                                  nrows          = nrows,
                                  ncols          = ncols,
                                  xmin           = xmin,
                                  xmax           = xmax,
                                  ymin           = ymin,
                                  ymax           = ymax,
                                  xres           = xres,
                                  yres           = yres,
                                  crs            = as.character(raster::crs(r_obj)),
                                  files          = list(files))

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
#' @title Define a reasonable block size to process a RasterBrick
#' @name .sits_raster_block_size
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Defines the size of the block of a Raster Brick to be read into memory.
#' The total pixels of a RasterBrick is given by combining the size of the timeline
#' with the number of rows and columns of the Brick. For example, a Raster Brick
#' with 500 rows and 500 columns and 400 time instances will have a total pixel size
#' of 800 Mb if pixels are 64-bit. I
#'
#' @param  nrows      number of rows in the image
#' @param  ncols      number of collumns in the image
#' @param  nblocks    number of blocks to be read
#' @return block      list with three attributes: n (number of blocks), rows (list of rows to begin),
#'                    nrows - number of rows to read at each iteration
#'
.sits_raster_block_size <- function(nrows, ncols, nblocks){


    # number of rows per block
    block_rows <- ceiling(nrows/nblocks)

    row.vec <- seq.int(from = 1, to = nrows, by = block_rows)
    nrows.vec <- rep.int(block_rows, length(row.vec))
    if (sum(nrows.vec) != nrows )
        nrows.vec[length(nrows.vec)] <- nrows - sum(nrows.vec[1:(length(nrows.vec) - 1)])

    # find out the size of the block in pixels
    size.vec <- nrows.vec * ncols

    # elements of the block list
    # n          number of blocks
    # row        starting row from the RasterBrick
    # nrow       Number of rows in the block extracted from the RasterBrick

    block <- list(n = nblocks, row = row.vec, nrows = nrows.vec, size = size.vec)

    return(block)

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
#' @param init_row         initial row of the data to be read
#' @param nrows            number of rows in the input data table
#' @param ncores           number of cores for processing
#' @return block_size.lst  list of pairs of positions (first row, last row) to be assigned to each core
#'
.sits_split_block_size <- function(init_row, nrows, ncores){

    # find the number of rows per core
    step <- ceiling(nrows/ncores)

    # create a vector with the initial rows per block
    blocks <- seq(from = init_row, to = nrows, by = step)

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
