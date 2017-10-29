#' @title Write a block of values in a RasterLayer
#' @name .sits_block_from_data
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a SITS tibble with label values and write a block inside a set of RasterLayers
#'
#' @param  class.tb          Classified SITS tibble
#' @param  raster_class.tb   Metadata of RasterLayers where label values are to be writtenn
#' @param  int_labels        Named vector with integers match the class labels
#' @param  init_row          Starting row of the output RasterLayer
#' @return raster_class.tb   Metadata with information on a set of RasterLayers
#' @export

.sits_block_from_data <- function (class.tb, raster_class.tb, int_labels, init_row){

    # for each layer, write the values
    raster_class.tb %>%
        purrrlyr::by_row(function (layer){
            values          <- as.integer (int_labels [dplyr::filter (class.tb, from == layer$start_date)$predicted])
            layer$r_obj[[1]]  <- raster::writeValues(layer$r_obj[[1]], values, init_row)
        })

    return (raster_class.tb)
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
#' @param  patterns.tb       Tibble with list of patterns (used to store the classification labels)
#' @param  file              Generic name of the files that will contain the RasterLayers
#' @param  interval          Interval between classifications
#' @return raster_layers.tb  Tibble with metadata about the output RasterLayer objects
#'
#' @export
.sits_create_classified_raster <- function (raster.tb, patterns.tb, file, interval = "12 month"){
    # ensure metadata tibble exists
    .sits_test_tibble (raster.tb)

    # get the timeline of observations (required for matching dates)
    timeline <- raster.tb[1,]$timeline[[1]]
    # produce the breaks used to generate the output rasters
    subset_dates.lst <- .sits_match_timelines(timeline, patterns.tb[1,]$start_date, patterns.tb[1,]$end_date, interval)

    # create a list to store the results
    raster.lst <- list()

    # loop through the list of dates
    subset_dates.lst %>%
        purrr::map (function (date_pair) {
            # create one raster layer per date pair
            r_out <- raster::raster(raster.tb[1,]$r_obj[[1]])

            # define the timeline for the classified image
            start_date <- date_pair[1]
            end_date   <- date_pair[2]
            timeline   <- c(start_date, end_date)

            # define the filename for the classified image
            filename <- .sits_raster_filename(file, start_date, end_date)
            r_out@file@name <- filename

            #define the band and the scale factor
            band <- "class"
            scale_factor <- 1

            # create a new RasterLayer for a defined period and generate the associated metadata
            row.tb <- sits_tibble_raster (r_out, band, timeline, scale_factor)

            # store the labels of the classified image
            labels <- sits_labels(patterns.tb)$label
            row.tb$labels <- list(labels)

            # add the metadata information to the list
            raster.lst[[length(raster.lst) + 1 ]] <<- row.tb

        })
    # join all rows in a single tibble
    raster_layers.tb <- dplyr::bind_rows (raster.lst)

    return (raster_layers.tb)
}

#' @title Retrieve a list of distances from a block of values obtained from a RasterBrick
#' @name .sits_distances_from_block
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a block of RasterBricks and retrieves a set pf distances
#'
#' @param  raster.tb  Metadata for a RasterBrick
#' @param  row        starting row from the RasterBrick
#' @param  nrow       Number of rows in the block extracted from the RasterBrick
#' @param  size       Size of the block in pixels
#' @param  timeline   Timeline that contains the valid dates
#' @param  shift      Adjustment value to avoid negative pixel vales
#' @return ts.lst     List of time series (one per pixel of the block)
#'
.sits_distances_from_block <- function (raster.tb, row, nrows, size, timeline, shift = 3.0) {

    # create a list to store the time series extracted from the block
    distances.tb <- tibble::tibble(id = 1:size,)

    # go line by line of the raster metadata tibble (each line points to a RasterBrick)
    raster.tb %>%
        purrrlyr::by_row(function (r_brick){
            # the raster::getValues function returns a matrix
            # the rows of the matrix are the pixels
            # the cols of the matrix are the layers
            values.mx   <- raster::getValues(r_brick$r_obj[[1]], row = row, nrows = nrows)

            # remove NA values
            values.mx[is.na(values.mx)] <- 0
            #values.mx   <- zoo::na.spline(values.mx)

            # correct by the scale factor
            values.mx   <- values.mx*r_brick$scale_factor

            # adjust values to avoid negative pixel vales
            values.mx <-  values.mx + shift

            # Convert the matrix to a list of time series (all with values for a single band)
            distances.tb  <<- dplyr::bind_cols(distances.tb, tibble::as_tibble(values.mx))
            })

    return (distances.tb[,-1])
}

#' @title Define a reasonable block size to process a RasterBrick
#' @name .sits_raster_block_size
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a block of RasterBricks and builds a SITS tibble
#'
#' @param  brick.tb   Metadata for a RasterBrick
#' @param  blocksize  Default size of the block (rows * cols)
#' @return block      list with three attributes: n (number of blocks), rows (list of rows to begin),
#'                    nrows - number of rows to read at each iteration
#'
.sits_raster_block_size <- function (brick.tb, blocksize = 90000){
    #' n          number of blocks
    #' row        starting row from the RasterBrick
    #' nrow       Number of rows in the block extracted from the RasterBrick

    # number of rows per block
    block_rows <- min (ceiling(blocksize/brick.tb$ncols), brick.tb$nrows)
    # number of blocks to be read
    nblocks <- ceiling(brick.tb$nrows/block_rows)

    row <- seq.int(from = 1, to = brick.tb$nrows, by = block_rows)
    nrows <- rep.int(block_rows, length(row))
    if (sum(nrows) != brick.tb$nrows )
        nrows[length(nrows)] <- brick.tb$nrows - sum (nrows[1:(length(nrows) - 1)])

    # find out the size of the block in pixels
    size <- nrows * brick.tb$ncols

    block <- list(n = nblocks, row = row, nrows = nrows, size = size)

    return (block)

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
.sits_raster_filename <- function (file, start_date, end_date){


    file_base <- tools::file_path_sans_ext(file)
    y1 <- lubridate::year(start_date)
    m1 <- lubridate::month(start_date)
    y2 <- lubridate::year(end_date)
    m2 <- lubridate::month(end_date)

    file_name <- paste0(file_base,"_",y1,"_",m1,"_",y2,"_",m2,".tif")

    return (file_name)
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
#' @param coverage         Name of the coverage to be retrieved
#' @return data.tb         SITS tibble with the time series
#'
#' @description This function creates a tibble to store the information
#' about a raster time series
#'

.sits_ts_fromRasterXY <- function (raster.tb, xy, longitude, latitude, label = "NoClass", coverage = NULL){
    # ensure metadata tibble exists
    .sits_test_tibble (raster.tb)

    timeline <- raster.tb[1,]$timeline[[1]]

    ts.tb <- tibble::tibble (Index = timeline)

    raster.tb %>%
        purrrlyr::by_row (function (row){
            # obtain the Raster Layer object
            r_obj <- row$r_obj[[1]]
            # get the values of the time series
            values <- as.vector(raster::extract(r_obj, xy))
            values.tb <- tibble::tibble(values)
            names(values.tb) <- row$band
            # correct the values using the scale factor
            values.tb <- values.tb[,1]*row$scale_factor
            # add the column to the SITS tibble
            ts.tb <<- dplyr::bind_cols(ts.tb, values.tb)
        })

    # create a list to store the time series coming from the set of Raster Layers
    ts.lst <- list()
    # transform the zoo list into a tibble to store in memory
    ts.lst[[1]] <- ts.tb
    # set a name for the coverage
    if (purrr::is_null(coverage))
        coverage = tools::file_path_sans_ext(basename (raster.tb[1,]$name))
    # create a tibble to store the WTSS data
    data.tb <- sits_tibble()
    # add one row to the tibble
    data.tb <- tibble::add_row (data.tb,
                                longitude    = longitude,
                                latitude     = latitude,
                                start_date   = as.Date(timeline[1]),
                                end_date     = as.Date(timeline[length(timeline)]),
                                label        = label,
                                coverage     = coverage,
                                time_series  = ts.lst
    )
    return (data.tb)
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

.sits_ts_fromRasterCSV <- function (raster.tb, file){
    # ensure metadata tibble exists
    .sits_test_tibble (raster.tb)

    # configure the format of the CSV file to be read
    cols_csv <- readr::cols(id          = readr::col_integer(),
                            longitude   = readr::col_double(),
                            latitude    = readr::col_double(),
                            start_date  = readr::col_date(),
                            end_date    = readr::col_date(),
                            label       = readr::col_character())
    # read sample information from CSV file and put it in a tibble
    csv.tb <- readr::read_csv (file, col_types = cols_csv)
    # create the tibble
    data.tb <- sits_tibble()
    # for each row of the input, retrieve the time series
    csv.tb %>%
        purrrlyr::by_row( function (r){
            xy <- .sits_latlong_to_proj(r$longitude, r$latitude, raster.tb[1,]$crs)
            ensurer::ensure_that(xy, .sits_XY_inside_raster((.), raster.tb),
                                 err_desc = "lat-long point not inside raster")
            row.tb <- .sits_ts_fromRasterXY (raster.tb, xy, r$longitude, r$latitude, r$label)
            row.tb <- .sits_extract (row.tb, r$start_date, r$end_date)
            data.tb <<- dplyr::bind_rows (data.tb, row.tb)
        })
    return (data.tb)
}
#' @title Retrieve a set of time series from a block of values obtained from a RasterBrick
#' @name .sits_values_from_block
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a block of values obtained form a RasterBrick, whose metadata is
#' described by a raster tibble, and produce a list of time series for further processing
#'
#' @param  values.mx     Matrix of values obtained from a RasterBrick (rows are pixels, cols are layers in time)
#' @param  scale_factor  Scale factor to convert the values from [0..1]
#' @return ts.lst        List of time series (one per pixel)
#'
.sits_values_from_block <- function (values.mx, scale_factor){

    # create a time series to store the result
    #ts.lst <- list()
    # loop through all rows of the matrix (each row is a pixel)
    # for (i in 1:nrow(values.mx)){
    #     # the values of each row are layers (values in time for a RasterBrick)
    #     ts <- values.mx[i,]
    #     # interpolate missing values
    #     ts <- zoo::na.spline(ts)
    #     # create a tibble to store the values of the time series
    #     ts.tb <- tibble::tibble(ts)
    #     # give a name to the tibble taken from the metadata associated to each raster object
    #     names(ts.tb) <- tibble_row$band
    #     # correct the values by the scale factor provided by the metadata information
    #     ts.tb <- ts.tb[,1]*tibble_row$scale_factor
    #     # include the time series in the output list
    #     ts.lst [[length(ts.lst) + 1]] <- ts.tb
    # }
    values.mx <- zoo::na.spline(values.mx)
    values.mx <- values.mx$scale_factor
    return (tibble::as.tibble(values.mx))
}
