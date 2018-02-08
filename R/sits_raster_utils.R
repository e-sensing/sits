#' @title Write a block of values in a RasterLayer
#' @name .sits_block_from_data
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a SITS tibble with label values and write a block inside a set of RasterLayers
#'
#' @param  pred.lst          Vector with list of predicted classes
#' @param  layers.lst        List of RasterLayers where label values are to be writtenn
#' @param  int_labels        Named vector with integers match the class labels
#' @param  init_row          Starting row of the output RasterLayer
#' @return raster_class.tb   Metadata with information on a set of RasterLayers

.sits_block_from_data <- function(pred.lst, layers.lst, int_labels, init_row) {

    # for each layer, write the values
    for (i in 1:length(layers.lst)) {
        values <- as.integer(int_labels[pred.lst[[i]]])
        layers.lst[[i]]  <- raster::writeValues(layers.lst[[i]], values, init_row)
        }
    return(layers.lst)
}
#' @title Classify a distances matrix extracted from raster using machine learning models
#' @name .sits_classify_block
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Returns a sits table with the results of the ML classifier.
#'
#' @param  data.mx         a matrix with data values
#' @param  class_info.tb   a tibble with the information on classification
#' @param  ml_model        a model trained by \code{\link[sits]{sits_train}}
#' @param  multicores      Number of threads to process the time series.
#' @return pred.lst        list with the predicted labels for each output time interval
.sits_classify_block <- function(data.mx, class_info.tb, ml_model = NULL, multicores = 1){

    ensurer::ensure_that(ml_model, !purrr::is_null(.),
                         err_desc = "sits-classify: please provide a machine learning model already trained")

    # find the subsets of the input data
    dates_index.lst <- class_info.tb$dates_index[[1]]

    # find the number of the samples
    nsamples <- dates_index.lst[[1]][2] - dates_index.lst[[1]][1] + 1

    #retrieve the timeline of the data
    timeline <- class_info.tb$timeline[[1]]

    # retrieve the bands
    bands <- class_info.tb$bands[[1]]

    #retrieve the time index
    time_index.lst  <- .sits_time_index(dates_index.lst, timeline, bands)

    # define the column names
    attr_names.lst <- bands %>%
        purrr::map(function(b){
            attr_names_b <- c(paste(c(rep(b, nsamples)), as.character(c(1:nsamples)), sep = ""))
            return(attr_names_b)
        })
    attr_names <- unlist(attr_names.lst)
    attr_names <- c("original_row", "reference", attr_names)


    # classify a block of data
    classify_block <- function(block.mx) {
        # create a list to get the predictions
        pred_block.lst <- list()
        for (t in 1:length(time_index.lst)) {
            # create an empty matrix to store the subset of the data
            values.mx <- matrix(nrow = nrow(block.mx), ncol = 0)
            idx <- time_index.lst[[t]]
            for (b in 1:length(bands)) {
                # retrieve the values used for classification
                values.mx <- cbind(values.mx, block.mx[,idx[(2*b - 1)]:idx[2*b]])
            }
            dist.tb <- data.frame("original_row" = rep(1,nrow(block.mx)) ,
                                  "reference" = rep("NoClass", nrow(block.mx)))
            dist.tb[,3:(nsamples*length(bands) + 2)] <- values.mx

            colnames(dist.tb) <- attr_names
            # classify the subset data
            pred_block.lst[[t]] <- .sits_predict(dist.tb, ml_model)
        }
        return(pred_block.lst)
    }

    join_blocks <- function(blocks.lst) {
        pred.lst <- list()
        # create a vector to store the prediction list for each time index
        for (t in 1:length(time_index.lst))
            pred.lst[[t]] <- vector()

        # join the blocks for each time index
        for (i in 1:length(blocks.lst))
            for (t in 1:length(time_index.lst))
                pred.lst[[t]] <- c(pred.lst[[t]], blocks.lst[[i]][[t]])
        return(pred.lst)
    }

    if (multicores > 1) {
        blocks.lst <- split.data.frame(data.mx, cut(1:nrow(data.mx), multicores, labels = FALSE))
        # apply parallel processing to the split data
        results <- parallel::mclapply(blocks.lst, classify_block, mc.cores = multicores)

        pred.lst <- join_blocks(results)
    }
    else
        pred.lst <- classify_block(data.mx)

    # check the result has the right dimension
    ensurer::ensure_that (pred.lst, all(sapply((.), length) == nrow(data.mx)),
                          err_desc = "sits_classify_raster - number of classified pixels is different
                                from number of input pixels")

    return(pred.lst)
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
#' @param  raster.tb         Tibble with metadata about the input RasterBrick objects
#' @param  samples.tb        The samples used for training the classification model
#' @param  file              Generic name of the files that will contain the RasterLayers
#' @return raster_layers.tb  Tibble with metadata about the output RasterLayer objects
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
    subset_dates.lst <- .sits_match_timeline(timeline, ref_start_date, ref_end_date, interval)

    # loop through the list of dates and create list of raster layers to be created
    layer.lst <- subset_dates.lst %>%
        purrr::map(function(date_pair) {
            # create one raster layer per date pair
            r_obj <- sits_get_raster(raster.tb, 1)
            r_out <- raster::raster(r_obj)

            # define the timeline for the classified image
            start_date <- date_pair[1]
            end_date   <- date_pair[2]
            timeline   <- c(start_date, end_date)

            band   <-  paste0("class-", start_date, "-", end_date)

            # define the filename for the classified image
            filename <- .sits_raster_filename(file, start_date, end_date)
            r_out@file@name <- filename

            layer <- tibble::tibble(layer.obj = list(r_out),
                                    band = band, filename = filename)

            return(layer)
        })

    # join all rows in a single tibble
    info_layers.tb <- dplyr::bind_rows(layer.lst)

    # get the name of the coverages
    name   <-  paste0(raster.tb[1,]$name, "-class")

    #define the bands, the scale factors and the missing values
    bands <- info_layers.tb$band

    scale_factors <- vector()
    scale_factors[bands] <- 1
    missing_values <- vector()
    missing_values[bands] <- -9999

    # get the filenames
    files <- info_layers.tb$filename

    # get the layer objects
    layer.lst <- info_layers.tb$layer.obj

    # create a new RasterLayer for a defined period and generate the associated metadata
    coverage.tb <- .sits_create_raster_coverage(raster.lst = layer.lst,
                                                service        = "RASTER",
                                                name           = name,
                                                timeline       = timeline,
                                                bands          = list(bands),
                                                scale_factors  = scale_factors,
                                                missing_values = missing_values,
                                                files          = files)


    return(coverage.tb)
}
#' @title Provides information about the coverages that make up a set of raster bricks
#' @name .sits_create_raster_coverage
#'
#' @description creates a tibble with metadata about a given coverage
#'
#' @param raster.lst        the list of Raster objects associated with the raster coverages
#' @param service           the time series service
#' @param name              the name of the coverage
#' @param timeline          the coverage timeline
#' @param bands             vector with names of bands
#' @param scale_factors     scale factors
#' @param missing_values    missing values
#' @param files             vector with names of raster files where the data is stored
#'
.sits_create_raster_coverage <- function(raster.lst,
                                         service,
                                         name,
                                         timeline,
                                         bands,
                                         scale_factors,
                                         missing_values,
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
    # get the CRS projection
    crs <- as.character(raster::crs(r_obj))

    # if timeline is not provided, try a best guess
    if (purrr::is_null(timeline))
        timeline <- .sits_get_timeline("RASTER", "MOD13Q1")

    # if scale factors are not provided, try a best guess
    if (purrr::is_null(scale_factors)) {
        msg <- paste0("Scale factors not provided - will use default values: please check they are valid")
        .sits_log_error(msg)
        # if the projection is UTM, guess it's a LANDSAT data set
        if (stringr::str_detect(crs, "utm")) {
            msg <- paste0("Data in UTM projection - assuming LANDSAT-compatible images")
            .sits_log_error(msg)
            scale_factors <- .sits_get_scale_factors("RASTER", "LANDSAT", bands)
        }
        # if the projection is sinusoidal, guess it's a MODIS data set
        if (stringr::str_detect(crs, "sinu")) {
            msg <-  paste0("Data in Sinusoidal projection - assuming MODIS-compatible images")
            .sits_log_error(msg)
            scale_factors <- .sits_get_scale_factors("RASTER", "MODIS", bands)
        }
        ensurer::ensure_that(scale_factors, !(purrr::is_null(.)),
               err_desc = "Not able to obtain scale factors for raster data")
    }
    # if missing_values are not provided, try a best guess
    if (purrr::is_null(missing_values)) {
        msg <- paste0("Missing values not provided - will use default values: please check they are valid")
        .sits_log_error(msg)
        # if the projection is UTM, guess it's a LANDSAT data set
        if (stringr::str_detect(crs, "utm")) {
            msg <- paste0("Data in UTM projection - assuming LANDSAT-compatible images")
            .sits_log_error(msg)
            missing_values <- .sits_get_missing_values("RASTER", "LANDSAT", bands)
        }
        # if the projection is sinusoidal, guess it's a MODIS data set
        if (stringr::str_detect(crs, "sinu")) {
            msg <-  paste0("Data in Sinusoidal projection - assuming MODIS-compatible images")
            .sits_log_error(msg)
            missing_values <- .sits_get_missing_values("RASTER", "MODIS", bands)
        }
        ensurer::ensure_that(missing_values, !(purrr::is_null(.)),
                             err_desc = "Not able to obtain scale factors for raster data")
    }

    # initiate writing
    # create a tibble to store the metadata
    coverage.tb <- tibble::tibble(r_objs         = list(raster.lst),
                                  name           = name,
                                  service        = service,
                                  bands          = list(bands),
                                  scale_factors  = list(scale_factors),
                                  missing_values = list(missing_values),
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
                                  crs            = crs,
                                  files          = list(files))

    return(coverage.tb)
}


#' @title Retrieve a block of values obtained from a RasterBrick
#' @name .sits_data_from_block
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a block of RasterBricks and retrieves a set of values
#'
#' @param  raster.tb      Metadata for a RasterBrick
#' @param  row            Starting row from the RasterBrick
#' @param  nrow           Number of rows in the block extracted from the RasterBrick
#' @param  adj_fun        Adjustment function to be applied to the data
#' @return data.mx        Matrix with data values
#'
.sits_data_from_block <- function(raster.tb, row, nrows, adj_fun) {

    # go element by element of the raster metadata tibble (each object points to a RasterBrick)
    values.lst <- list()
    # get the list of bricks
    bricks.lst <- sits_get_raster(raster.tb)
    # get the bands, scale factors and missing values
    bands <- unlist(raster.tb$bands)
    missing_values <- unlist(raster.tb$missing_values)
    scale_factors  <- unlist(raster.tb$scale_factors)
    i <- 0
    # go through all the bricks
    bricks.lst %>%
        purrr::map(function(r_brick) {
            # the raster::getValues function returns a matrix
            # the rows of the matrix are the pixels
            # the cols of the matrix are the layers
            values.mx   <- raster::getValues(r_brick, row = row, nrows = nrows)

            # get the associated band
            i <<- i + 1
            band <- bands[i]

            # update missing values to NA (this should be replaced by a fast linear interpolation)
            values.mx[values.mx == missing_values[band]] <- NA

            if (any(is.na(values.mx))) {
                # transpose matrix to replace NA values
                values.mx <- t(values.mx)
                values.mx <- zoo::na.approx(values.mx)
                values.mx <- t(values.mx)
            }
            # correct by the scale factor
            values.mx     <- values.mx*scale_factors[band]

            # adjust values to avoid negative pixel vales
            values.mx <-  adj_fun(values.mx)

            # Convert the matrix to a list of time series (all with values for a single band)
            values.lst[[length(values.lst) + 1]]  <<- values.mx
        })
    data.mx <- do.call(cbind, values.lst)
    return(data.mx)
}
#' @title Extract a time series from a ST raster data set
#' @name .sits_fromRaster
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Reads metadata about a raster data set to retrieve a set of
#' time series.
#'
#' @param coverage        A tibble with metadata describing a raster coverage
#' @param file            A CSV file with lat/long locations to be retrieve
#' @param longitude       double - the longitude of the chosen location
#' @param latitude        double - the latitude of the chosen location
#' @param start_date      date - the start of the period
#' @param end_date        date - the end of the period
#' @param label           string - the label to attach to the time series
#' @return data.tb        a SITS tibble with the time series
#'
.sits_fromRaster <- function(coverage,
                             file = NULL,
                             longitude = NULL,
                             latitude = NULL,
                             start_date = NULL,
                             end_date  = NULL,
                             label = "NoClass"){

    # ensure metadata tibble exists
    ensurer::ensure_that(coverage, NROW(.) >= 1,
                         err_desc = "sits_classify_raster: need a valid metadata for coverage")

    # get data based on CSV file
    if (!purrr::is_null(file) && tolower(tools::file_ext(file)) == "csv") {
        data.tb <- .sits_ts_fromRasterCSV(coverage, file)
    }

    if (!purrr::is_null(longitude) && !purrr::is_null(latitude)) {
        data.tb <- .sits_ts_fromRaster(coverage, longitude, latitude, label)
    }
    return(data.tb)
}

#' @title Define a reasonable block size to process a RasterBrick
#' @name .sits_raster_block_size
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Defines the size of the block of a Raster Brick to be read into memory.
#' The total pixels of a RasterBrick is given by combining the size of the timeline
#' with the number of rows and columns of the Brick. For example, a Raster Brick
#' with 500 rows and 500 columns and 400 time instances will have a total pixel size
#' of 250 Mb if pixels are 16-bit (about a GigaByte). If there are 4 bands to be processed together, there will be 4 Raster Bricks.
#' Thus, a block size of 250000 will use a l GB just to store the image data.
#'
#' As a rule of thumb, consider that for a typical MODIS data set such as MOD13Q1 there will be
#' about 23 time instances per year. In 20 years, this means about 460 instances.
#' In a small desktop with 8 GBytes, a block size of 250000 will use 1Gb of memory.
#' This is taken to be the default for small machines.
#' In a larger server, users should increase the block size for improved processing.
#'
#' @param  brick.tb   Metadata for a RasterBrick
#' @param  blocksize  Default size of the block (rows * cols)
#' @return block      list with three attributes: n (number of blocks), rows (list of rows to begin),
#'                    nrows - number of rows to read at each iteration
#'
.sits_raster_block_size <- function(brick.tb, blocksize = 250000){
    #' n          number of blocks
    #' row        starting row from the RasterBrick
    #' nrow       Number of rows in the block extracted from the RasterBrick

    # number of rows per block
    block_rows <- min(ceiling(blocksize/brick.tb$ncols), brick.tb$nrows)
    # number of blocks to be read
    nblocks <- ceiling(brick.tb$nrows/block_rows)

    row <- seq.int(from = 1, to = brick.tb$nrows, by = block_rows)
    nrows <- rep.int(block_rows, length(row))
    if (sum(nrows) != brick.tb$nrows )
        nrows[length(nrows)] <- brick.tb$nrows - sum(nrows[1:(length(nrows) - 1)])

    # find out the size of the block in pixels
    size <- nrows * brick.tb$ncols

    block <- list(n = nblocks, row = row, nrows = nrows, size = size)

    return(block)

}
#' @title Define a filename associated to one classified raster layer
#' @name .sits_raster_filename
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Creates a filename for a raster layer with associated temporal information,
#'                 given a basic filename
#'
#' @param file       The original file name (without temporal information)
#' @param start_date The starting date of the time series classification
#' @param end_date   The end date of the time series classification
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

#' @title Extract a time series for a set of Raster Layers
#' @name .sits_ts_fromRaster
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description  This function extracts a SITS time series from a set of
#'               Raster Layers whose metadata is stored in a tibble
#'               created by the sits_STraster function
#'
#' @param raster.tb        A tibble with metadata information about a raster data set
#' @param longitude        Longitude of the chosen location
#' @param latitude         Latitude of the chosen location
#' @param label            Label to attach to the time series
#' @return data.tb         SITS tibble with the time series
#'
#' @description This function creates a tibble to store the information
#' about a raster time series
#'
.sits_ts_fromRaster <- function(raster.tb, longitude, latitude, label = "NoClass"){

    # ensure metadata tibble exists
    ensurer::ensure_that(raster.tb, NROW(.) >= 1,
                         err_desc = "sits_ts_fromRasterXY: need a valid metadata for coverage")

    timeline <- raster.tb$timeline[[1]]

    ts.tb <- tibble::tibble(Index = timeline)

    # get the bands, scale factors and missing values
    bands <- unlist(raster.tb$bands)
    missing_values <- unlist(raster.tb$missing_values)
    scale_factors  <- unlist(raster.tb$scale_factors)
    nband <- 0

    # transform longitude and latitude to an sp Spatial Points* (understood by raster)
    st_point <- sf::st_point(c(longitude, latitude))
    ll_sfc <- sf::st_sfc(st_point, crs = "+init=epsg:4326")
    ll_sp <- sf::as_Spatial(ll_sfc)

    # An input raster brick contains several files, each corresponds to a band
    bricks.lst <- sits_get_raster(raster.tb)
    values.lst <- bricks.lst %>%
        purrr::map(function(r_brick) {
            # eack brick is a band
            nband <<- nband + 1
            # get the values of the time series
            values <- suppressWarnings(as.vector(raster::extract(r_brick, ll_sp)))
            # is the data valid?
            if (all(is.na(values))) {
                message("point outside the raster extent - NULL returned")
                return(NULL)
            }
            # create a tibble to store the values
            values.tb <- tibble::tibble(values)
            # find the names of the tibble column
            band <- bands[nband]
            names(values.tb) <- band
            # correct the values using the scale factor
            values.tb <- values.tb[,1]*scale_factors[band]
            return(values.tb)
        })

    ts.tb <- dplyr::bind_cols(ts.tb, values.lst)

    # create a list to store the time series coming from the set of Raster Layers
    ts.lst <- list()
    # transform the list into a tibble to store in memory
    ts.lst[[1]] <- ts.tb

    # create a tibble to store the WTSS data
    data.tb <- sits_tibble()
    # add one row to the tibble
    data.tb <- tibble::add_row(data.tb,
                               longitude    = longitude,
                               latitude     = latitude,
                               start_date   = as.Date(timeline[1]),
                               end_date     = as.Date(timeline[length(timeline)]),
                               label        = label,
                               coverage     = raster.tb$name,
                               time_series  = ts.lst
    )
    return(data.tb)
}

#' @title Extract a time series for a set of Raster Layers
#' @name .sits_ts_fromRasterXY
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description  This function extracts a SITS time series from a set of
#'               Raster Layers whose metadata is stored in a tibble
#'               created by the sits_STraster function
#'
#' @param raster.tb        A tibble with metadata information about a raster data set
#' @param xy               A matrix with X/Y coordinates
#' @param longitude        Longitude of the chosen location
#' @param latitude         Latitude of the chosen location
#' @param label            Label to attach to the time series
#' @return data.tb         SITS tibble with the time series
#'
#' @description This function creates a tibble to store the information
#' about a raster time series
#'

.sits_ts_fromRasterXY <- function(raster.tb, xy, longitude, latitude, label = "NoClass"){

    # ensure metadata tibble exists
    ensurer::ensure_that(raster.tb, NROW(.) >= 1,
                         err_desc = "sits_ts_fromRasterXY: need a valid metadata for coverage")

    timeline <- raster.tb$timeline[[1]]

    ts.tb <- tibble::tibble(Index = timeline)

    # get the bands, scale factors and missing values
    bands <- unlist(raster.tb$bands)
    missing_values <- unlist(raster.tb$missing_values)
    scale_factors  <- unlist(raster.tb$scale_factors)
    nband <- 0

    # An input raster brick contains several files, each corresponds to a band
    bricks.lst <- sits_get_raster(raster.tb)
    values.lst <- bricks.lst %>%
        purrr::map(function(r_brick) {
            # eack brick is a band
            nband <<- nband + 1
            # get the values of the time series
            values <- as.vector(raster::extract(r_brick, xy))
            # is the data valid?
            if (all(is.na(values)))
                return(NULL)
            # create a tibble to store the values
            values.tb <- tibble::tibble(values)
            # find the names of the tibble column
            band <- bands[nband]
            names(values.tb) <- band
            # correct the values using the scale factor
            values.tb <- values.tb[,1]*scale_factors[band]
            return(values.tb)
        })

    ts.tb <- dplyr::bind_cols(ts.tb, values.lst)

    # create a list to store the time series coming from the set of Raster Layers
    ts.lst <- list()
    # transform the list into a tibble to store in memory
    ts.lst[[1]] <- ts.tb

    # create a tibble to store the WTSS data
    data.tb <- sits_tibble()
    # add one row to the tibble
    data.tb <- tibble::add_row(data.tb,
                               longitude    = longitude,
                               latitude     = latitude,
                               start_date   = as.Date(timeline[1]),
                               end_date     = as.Date(timeline[length(timeline)]),
                               label        = label,
                               coverage     = raster.tb$name,
                               time_series  = ts.lst
    )
    return(data.tb)
}

#' @title Extract a time series for a set of Raster Layers
#' @name .sits_ts_fromRasterCSV
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description  This function extracts a SITS time series from a set of
#'               Raster Layers whose metadata is stored in a tibble
#'               created by the sits_STraster function
#'
#' @param raster.tb       A tibble with metadata describing a spatio-temporal data set
#' @param file            A CSV file with lat/long locations to be retrieved
#' @return data.tb         SITS tibble with the time series

.sits_ts_fromRasterCSV <- function(raster.tb, file) {

    # ensure metadata tibble exists
    ensurer::ensure_that(raster.tb, NROW(.) == 1,
                         err_desc = "sits_classify_raster: need a valid metadata for coverage")

    # configure the format of the CSV file to be read
    cols_csv <- readr::cols(id          = readr::col_integer(),
                            longitude   = readr::col_double(),
                            latitude    = readr::col_double(),
                            start_date  = readr::col_date(),
                            end_date    = readr::col_date(),
                            label       = readr::col_character())

    # read sample information from CSV file and put it in a tibble
    csv.tb <- readr::read_csv(file, col_types = cols_csv)
    # create a tibble for the time series
    data.tb <- sits_tibble()
    # create a tibble  to store the unread rows
    csv_unread.tb <- .sits_tibble_csv()
    # for each row of the input, retrieve the time series
    csv.tb %>%
        purrrlyr::by_row(function(r) {
            # convert to the projection coordinates
            xy <- .sits_latlong_to_proj(r$longitude, r$latitude, raster.tb$crs)

            if (!.sits_XY_inside_raster(xy, raster.tb)) {
                    csv_unread_row.tb <- tibble::tibble(
                        longitude  = r$longitude,
                        latitude   = r$latitude,
                        start_date = r$start_date,
                        end_date   = r$end_date,
                        label      = r$label)
                    csv_unread.tb <<- dplyr::bind_rows(csv_unread.tb, csv_unread_row.tb)
            }
            # read the time series
            row.tb <- .sits_ts_fromRasterXY(raster.tb, xy, r$longitude, r$latitude, r$label)
            # extract the time interval
            row.tb <- .sits_extract(row.tb, r$start_date, r$end_date)
            # put one more row in the outopur tibble
            data.tb <<- dplyr::bind_rows(data.tb, row.tb)
        })
    if (NROW(csv_unread.tb) > 0) {
        message("Some points could not be retrieved - see log file and csv_unread_file")
        .sits_log_CSV(csv_unread.tb, "unread_samples.csv")
    }



    return(data.tb)
}
