#' @title Write a block of values in a RasterLayer
#' @name .sits_block_from_data
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a SITS tibble with label values and write a block inside a set of RasterLayers
#'
#' @param  pred.lst          Vector with list of predicted classes
#' @param  raster_class.tb   Metadata of RasterLayers where label values are to be writtenn
#' @param  int_labels        Named vector with integers match the class labels
#' @param  init_row          Starting row of the output RasterLayer
#' @return raster_class.tb   Metadata with information on a set of RasterLayers

.sits_block_from_data <- function(pred.lst, raster_class.tb, int_labels, init_row) {

    # for each layer, write the values
    i <- 0
    raster_class.tb$r_objs.lst %>%
        purrr::map(function(layer){
            i <- i + 1
            values <- as.integer(int_labels[pred.lst[[i]]])
            layer  <- raster::writeValues(layer, values, init_row)

        })
    return(raster_class.tb)
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
        for (t in 1:length(time_index.lst)) {
            pred.lst[[t]] <- vector()
        }
        for (i in 1:length(blocks.lst)) {
            pred.lst <- c(pred.lst[[t]], blocks.lst[[i]][[t]])
        }
        return(pred.lst)
    }

    if (multicores > 1) {
        blocks.lst <- split.data.frame(data.mx, cut(1:nrow(data.mx),multicores, labels = FALSE))
        # apply parallel processing to the split data
        results <- parallel::mclapply(blocks.lst, classify_block, mc.cores = multicores)

        pred.lst <- join_blocks(results)
    }
    else
        pred.lst <- classify_block(data.mx)

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


    # loop through the list of dates
    raster.lst <- subset_dates.lst %>%
        purrr::map(function(date_pair) {
            # create one raster layer per date pair
            r_out <- raster::raster(raster.tb[1,]$r_objs.lst[[1]])

            # define the timeline for the classified image
            start_date <- date_pair[1]
            end_date   <- date_pair[2]
            timeline   <- c(start_date, end_date)

            # define the coverage name (must be unique)
            coverage   <- paste0(raster.tb$coverage, "-class-", start_date, "-", end_date)

            # define the filename for the classified image
            filename <- .sits_raster_filename(file, start_date, end_date)
            r_out@file@name <- filename

            #define the band and the scale factor
            band <- "class"
            scale_factor <- 1

            # create a new RasterLayer for a defined period and generate the associated metadata
            row.tb <- .sits_coverage_raster  (r_objs      = list(r_out),
                                              service     = "RASTER",
                                              product     = raster.tb$product,
                                              coverage    = coverage,
                                              timeline    = timeline,
                                              bands       = raster.tb$band_info[[1]]$name,
                                              files       = list(filename))


            # add the metadata information to the list
            return(row.tb)
        })
    # join all rows in a single tibble
    raster_layers.tb <- dplyr::bind_rows(raster.lst)

    return(raster_layers.tb)
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


    nband <- 0
    # go element by element of the raster metadata tibble (each object points to a RasterBrick)
    values.lst <- raster.tb$r_objs.lst %>%
        purrr::map(function(r_brick) {
            # the raster::getValues function returns a matrix
            # the rows of the matrix are the pixels
            # the cols of the matrix are the layers
            values.mx   <- raster::getValues(r_brick, row = row, nrows = nrows)

            # remove NA values
            values.mx[is.na(values.mx)] <- 0
            #values.mx   <- zoo::na.spline(values.mx)

            # correct by the scale factor
            nband        <- nband + 1
            scale_factor <- as.numeric(raster.tb$band_info[[1]][nband, "scale_factor"])
            values.mx    <- values.mx*scale_factor

            # adjust values to avoid negative pixel vales
            values.mx <-  adj_fun(values.mx)

            # Convert the matrix to a list of time series (all with values for a single band)
            return(values.mx)
        })
    data.mx <- do.call(cbind, values.lst)
    return(data.mx)
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
    ensurer::ensure_that(raster.tb, NROW(.) == 1,
                         err_desc = "sits_classify_raster: need a valid metadata for coverage")

    timeline <- raster.tb[1,]$timeline[[1]]

    ts.tb <- tibble::tibble(Index = timeline)

    nband <- 0
    # An input raster brick contains several files, each corresponds to a band
    values.lst <- raster.tb$r_objs.lst %>%
        purrr::map(function(r_brick) {
            # eack brick is a banc
            nband <- nband + 1
            # get the values of the time series
            values <- as.vector(raster::extract(r_brick, xy))
            # is the data valid?
            if (all(is.na(values)))
                return(NULL)
            # create a tibble to store the values
            values.tb <- tibble::tibble(values)
            # find the names of the tibble column
            names(values.tb) <- as.character(raster.tb$band_info[[1]][nband, "name"])
            # correct the values using the scale factor
            scale_factor <- as.numeric(raster.tb$band_info[[1]][nband, "scale_factor"])
            values.tb <- values.tb[,1]*scale_factor
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
                               coverage     = raster.tb$coverage,
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
