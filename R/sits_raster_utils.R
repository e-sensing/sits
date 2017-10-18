#' @title Write a block of values in a RasterLayer
#' @name .sits_block_from_data
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a SITS tibble with label values and write a block inside a set of RasterLayers
#'
#' @param  class.tb          Classified SITS tibble
#' @param  raster_class.tb   Metadata of RasterLayers where label values are to be writtenn
#' @param  labels            Labels of classes to be written to the RasterLayer
#' @param  row               Starting row of the output RasterLayer
#' @return raster_class.tb   Metadata with information on a set of RasterLayers
#' @export

.sits_block_from_data <- function (class.tb, raster_class.tb, labels, row){


    # create a named vector with integers match the class labels
    int_labels <- c(1:length(labels))
    names (int_labels) <- labels

    # for each layer, write the values
    for (i in 1:NROW(raster_class.tb)){
        layer           <- raster_class.tb[i,]$r_obj[[1]]
        start_date      <- raster_class.tb[i,]$start_date
        values          <- int_labels [dplyr::filter (class.tb, from == start_date)$predicted]
        raster_class.tb[i,]$r_obj[[1]]  <- raster::writeValues(layer, values, row)
    }
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

#' @title Retrieve a sits tibble with time series from a block of values obtained from a RasterBrick
#' @name .sits_data_from_block
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a block of RasterBricks and builds a SITS tibble
#'
#' @param  raster.tb  Metadata for a RasterBrick
#' @param  row        starting row from the RasterBrick
#' @param  nrow       Number of rows in the block extracted from the RasterBrick
#' @return data.tb    Sits tibble with data (time series)
#'
.sits_data_from_block <- function (raster.tb, row, nrows) {

    # find out the size of the block in pixels
    size <- nrows * raster.tb[1,]$ncols

    # use the timeline of the input raster
    timeline <- raster.tb[1,]$timeline[[1]]

    # create a list to store the time series extracted from the block
    ts.lst <- .sits_create_ts_list (timeline, size)

    # go line by line of the raster metadata tibble (each line points to a RasterBrick)
    raster.tb %>%
        purrrlyr::by_row(function (r_brick){
            # the raster::getValues function returns a matrix
            # the rows of the matrix are the pixels
            # the cols of the matrix are the layers
            values.mx      <- raster::getValues(r_brick$r_obj[[1]], row = row, nrows = nrows)

            # Convert the matrix to a list of time series (all with values for a single band)
            ts_band.lst    <- .sits_values_from_block (values.mx, r_brick, nrows)

            # Put all time series for this block in a larger list (that has all the bands)
            ts.lst <<- purrr::map2 (ts.lst, ts_band.lst, function (ts, ts_band){
                ts <- dplyr::bind_cols(ts, ts_band)
            })
        })
    # Convert the values of the time series for all bands inside the block into tibbles
    # create the tibble
    data.tb <- sits_tibble()
    # convert the time series from the raster block values to the tibble
    ts.lst %>%
        purrr::map (function (ts){
            data.tb <<- .sits_add_row (data.tb,
                                       start_date   = as.Date(timeline[1]),
                                       end_date     = as.Date(timeline[length(timeline)]),
                                       time_series  = list(ts))

        })
    return (data.tb)
}

#' @title Retrieve a list of time series from a block of values obtained from a RasterBrick
#' @name .sits_ts_from_block
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a block of RasterBricks and retrieves a time series
#'
#' @param  raster.tb  Metadata for a RasterBrick
#' @param  row        starting row from the RasterBrick
#' @param  nrow       Number of rows in the block extracted from the RasterBrick
#' @return ts.lst     List of time series (one per pixel of the block)
#'
.sits_ts_from_block <- function (raster.tb, row, nrows) {

    # find out the size of the block in pixels
    size <- nrows * raster.tb[1,]$ncols

    # use the timeline of the input raster
    timeline <- raster.tb[1,]$timeline[[1]]

    # create a list to store the time series extracted from the block
    ts.lst <- .sits_create_ts_list (timeline, size)

    # go line by line of the raster metadata tibble (each line points to a RasterBrick)
    raster.tb %>%
        purrrlyr::by_row(function (r_brick){
            # the raster::getValues function returns a matrix
            # the rows of the matrix are the pixels
            # the cols of the matrix are the layers
            values.mx      <- raster::getValues(r_brick$r_obj[[1]], row = row, nrows = nrows)

            # Convert the matrix to a list of time series (all with values for a single band)
            ts_band.lst    <- .sits_values_from_block (values.mx, r_brick, nrows)

            # Put all time series for this block in a larger list (that has all the bands)
            # ts.lst <<- purrr::map2 (ts.lst, ts_band.lst, function (ts, ts_band){
            #     ts <- dplyr::bind_cols(ts, ts_band)
            #})


        })
    # Retrun the values of the time series for all bands inside the block into tibbles

    return (ts.lst)
}
#' @title Retrieve a set of time series from a block of values obtained from a RasterBrick
#' @name .sits_values_from_block
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a block of values obtained form a RasterBrick, whose metadata is
#' described by a raster tibble, and produce a list of time series for further processing
#'
#' @param  values.mx   Matrix of values obtained from a RasterBrick (rows are pixels, cols are layers in time)
#' @param  tibble_row  Row from raster tibble that includes the metadata about the RasterBrick object
#' @param  nrow        Number of rows in the matrix
#' @return ts.lst      List of time series (one per pixel)
#'
.sits_values_from_block <- function (values.mx, tibble_row, nrow){

    # create a time series to store the result
    ts.lst <- list()
    # loop through all rows of the matrix (each row is a pixel)
    for (i in nrow(values.mx)){
        # the values of each row are layers (values in time for a RasterBrick)
        ts <- values.mx[i,]
        # interpolate missing values
        ts <- zoo::na.spline(ts)
        # create a tibble to store the values of the time series
        ts.tb <- tibble::tibble(ts)
        # give a name to the tibble taken from the metadata associated to each raster object
        names(ts.tb) <- tibble_row$band
        # correct the values by the scale factor provided by the metadata information
        ts.tb <- ts.tb[,1]*tibble_row$scale_factor
        # include the time series in the output list
        ts.lst [[length(ts.lst) + 1]] <- ts.tb
    }
    return (ts.lst)
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
