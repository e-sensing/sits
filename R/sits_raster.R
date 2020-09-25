#' @title Filter the time series values in the case of a matrix
#' @name .sits_raster_filter_data
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function filters a matrix.
#'
#' @param  values.mx      Matrix of values.
#' @param  filter         Filter function to apply to matrix.
#' @param  multicores     Number of cores.
#' @return Scaled integer matrix.
.sits_raster_filter_data <- function(values.mx, filter, multicores) {
    # scale the data set
    # auxiliary function to scale a block of data
    filter_matrix_block <- function(chunk) {
        filtered_block.mx <- filter(chunk)
    }
    # use multicores to speed up filtering
    if (multicores > 1) {
        chunk.lst <- .sits_raster_split_data(values.mx, multicores)
        rows.lst  <- parallel::mclapply(chunk.lst, filter_matrix_block,
                                        mc.cores = multicores)
        values.mx <- do.call(rbind, rows.lst)
        rm(chunk.lst)
        rm(rows.lst)
        gc()
    }
    else
        values.mx <- filter(values.mx)

    return(values.mx)
}


#' @title Preprocess a set of values retrived from a raster brick
#' @name  .sits_raster_preprocess_data
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  values.mx        Matrix of values retrieved from a brick.
#' @param  band             Band to be processed.
#' @param  missing_value    Missing value for the band.
#' @param  minimum_value    Minimum values for the band.
#' @param  scale_factor     Scale factor for each band (only for raster data).
#' @param  stats            Normalization parameters.
#' @param  filter           Smoothing filter to be applied.
#' @param  multicores       Number of cores to process the time series.
#' @return Matrix with pre-processed values.
.sits_raster_preprocess_data <- function(values.mx,
                                         band,
                                         missing_value,
                                         minimum_value,
                                         scale_factor,
                                         stats,
                                         filter,
                                         multicores) {
    # correct minimum value
    values.mx[is.na(values.mx)] <- minimum_value
    values.mx[values.mx <= minimum_value] <- minimum_value

    # scale the data set
    values.mx <- .sits_raster_scale_data(values.mx, scale_factor, multicores)

    # filter the data
    if (!(purrr::is_null(filter))) {
        values.mx <- .sits_raster_filter_data(values.mx, filter, multicores)
    }

    # normalize the data
    if (!purrr::is_null(stats)) {
        values.mx <- .sits_normalize_matrix(values.mx, stats, band, multicores)
    }

    return(values.mx)
}

#' @title Read a block of values retrived from a set of raster bricks
#' @name  .sits_raster_read_data
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  cube            input data cube.
#' @param  samples         tibble with samples.
#' @param  t_obj.lst       list of terra objects to be read
#' @param  extent          bounding box in (i,j) coordinates
#' @param  stats           normalization parameters.
#' @param  filter          smoothing filter to be applied.
#' @param  multicores      number of cores to process the time series.
#' @return A data.table with values for classification.
.sits_raster_read_data <- function(cube,
                                   samples,
                                   t_obj.lst,
                                   extent,
                                   stats,
                                   filter,
                                   multicores) {
    # get the bands in the same order as the samples
    bands   <- sits_bands(samples)
    n_bands <- length(bands)
    # get the missing values, minimum values and scale factors
    missing_values <- unlist(cube$missing_values)
    minimum_values <- unlist(cube$minimum_values)
    scale_factors  <- unlist(cube$scale_factors)

    # read the values from the raster bricks ordered by bands
    values.lst <- purrr::map2(bands, c(1:n_bands), function(band, b) {
            # read the values
            terra::readStart(t_obj.lst[[b]])
            values.mx    <- terra::readValues(x      = t_obj.lst[[b]],
                                              row    = extent["row"],
                                              nrows  = extent["nrows"],
                                              col    = extent["col"],
                                              ncols  = extent["ncols"],
                                              mat = TRUE)
            terra::readStop(t_obj.lst[[b]])

            # preprocess the input data
            values.mx <- .sits_raster_preprocess_data(values.mx,
                                                      band,
                                                      missing_values[band],
                                                      minimum_values[band],
                                                      scale_factors[band],
                                                      stats,
                                                      filter,
                                                      multicores)

            return(values.mx)
        })

    # create a data.table joining the values
    data_DT <- data.table::as.data.table(do.call(cbind,values.lst))

    # memory cleanup
    rm(values.lst)
    gc()

    # create two additional columns for prediction
    two_cols_DT <- data.table::data.table("original_row" = rep(1, nrow(data_DT)),
                                          "reference"    = rep("NoClass", nrow(data_DT)))

    # join the two columns with the data values
    data_DT <- data.table::as.data.table(cbind(two_cols_DT, data_DT))

    return(data_DT)
}

#' @title Scale the time series values in the case of a matrix
#' @name .sits_raster_scale_data
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Normalizes one band of the values read from a raster brick.
#'
#' @param  values.mx      Matrix of values.
#' @param  scale_factor   Scaling factor.
#' @param  multicores     Number of cores.
#' @return A scaled matrix.
.sits_raster_scale_data <- function(values.mx, scale_factor, multicores) {
    # scale the data set
    # auxiliary function to scale a block of data
    scale_block <- function(chunk, scale_factor) {
        scaled_block.mx <- scale_data(chunk, scale_factor)
    }
    # use multicores to speed up scaling
    if (multicores > 1) {
        chunk.lst <- .sits_raster_split_data(values.mx, multicores)
        rows.lst  <- parallel::mclapply(chunk.lst, scale_block,
                                        scale_factor, mc.cores = multicores)
        values.mx <- do.call(rbind, rows.lst)
        rm(chunk.lst)
        rm(rows.lst)
        gc()
    }
    else
        values.mx <- scale_data(values.mx, scale_factor)

    return(values.mx)
}

#' @title Scale the time series values in the case of a matrix
#' @name .sits_raster_scale_matrix_integer
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function transforms a numerical matrix into an integer one.
#'
#' @param  values.mx      Matrix of values.
#' @param  scale_factor   Scaling factor.
#' @param  multicores     Number of cores.
#' @return Scaled integer matrix.
.sits_raster_scale_matrix_integer <- function(values.mx,
                                              scale_factor,
                                              multicores) {
    # scale the data set
    # auxiliary function to scale a block of data
    scale_matrix_block <- function(chunk, scale_factor) {
        scaled_block.mx <- scale_matrix_integer(chunk, scale_factor)
    }
    # use multicores to speed up scaling
    if (multicores > 1) {
        chunk.lst <- .sits_raster_split_data(values.mx, multicores)
        rows.lst  <- parallel::mclapply(chunk.lst, scale_matrix_block,
                                        scale_factor, mc.cores = multicores)
        int_values.mx <- do.call(rbind, rows.lst)
        rm(chunk.lst)
        rm(rows.lst)
        gc()
    }
    else
        int_values.mx <- scale_matrix_integer(values.mx, scale_factor)

    return(int_values.mx)
}

#' @title Split a data.table or a matrix for multicore processing
#' @name .sits_raster_split_data
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function splits a data.table into a
#'              list of chunks for multicore processing.
#'
#' @param data             Data (data.table or matrix).
#' @param ncores           Number of cores for processing.
#' @return                 List of pairs of positions (first row, last row)
#'                         to be assigned to each core.
#'
.sits_raster_split_data <- function(data, ncores) {
    # number of rows in the data
    nrows <- nrow(data)
    # find the number of rows per core
    step <- ceiling(nrows/ncores)

    # create a vector with the initial rows per block
    blocks <- seq(from = 1, to = nrows, by = step)

    # fill the list with the initial and final row per block
    block.lst <- purrr::map2(blocks, 1:ncores, function(blk, i) {
        start <- blk
        end   <- start + step - 1
        if (i == ncores )
            end <- nrows
        return(data[start:end,])
    })
    return(block.lst)
}

#' @title Extract a time series from raster
#' @name .sits_raster_get_ts
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Retrieve a set of time series for a raster data cube.
#'
#' @param cube              Metadata describing a raster data cube.
#' @param points            tibble with points
#' @param bands             Bands to be retrieved.
#' @return                  A sits tibble with the time series.
.sits_raster_get_ts <- function(cube, points, bands){

    # ensure metadata tibble exists
    assertthat::assert_that(NROW(cube) >= 1,
                            msg = ".sits_raster_get_ts: need a valid metadata for data cube")

    names <- c("longitude", "latitude", "label")

    assertthat::assert_that(all(names %in% colnames(points)),
                            msg = ".sits_raster_get_ts: data input is not valid")
    # get XY
    xy.tb <- .sits_latlong_to_proj(points$longitude, points$latitude, cube$crs)
    # join lat-long with XY values in a single tibble
    points <- dplyr::bind_cols(points, xy.tb)
    # filter the points inside the data cube
    points <- dplyr::filter(points, X > cube$xmin & X < cube$xmax & Y > cube$ymin &
                                Y < cube$ymax)

    # are there points to be retrieved from the cube?
    if (nrow(points) == 0)
        return(NULL)

    # retain only xy inside the cube
    xy <- matrix(c(points$X, points$Y), nrow = nrow(points), ncol = 2)
    colnames(xy) <- c("X", "Y")
    # get the timeline
    timeline <- sits_timeline(cube)

    # get the scale factors and missing values
    missing_values <- unlist(cube$missing_values)
    scale_factors  <- unlist(cube$scale_factors)

    # Retrieve values on a band by band basis
    ts_bands.lst <- bands %>%
        purrr::map(function(band) {
            # create a tibble to store the data for each band
            ts_band.tb <- .sits_tibble()
            # get the values of the time series (terra object)
            t_obj <- .sits_cube_terra_obj_band(cube, band)
            values <- terra::extract(t_obj, xy)
            # terra includes an ID (remove it)
            values <- values[,-1]
            rm(t_obj)
            # is the data valid?
            assertthat::assert_that(nrow(values) > 0,
                                    msg = "sits_ts_from_raster_shp - no data retrieved")
            if (all(is.na(values))) {
                message("point outside the raster extent - NULL returned")
                return(NULL)
            }

            # each row of the values matrix is a spatial point
            for (i in 1:nrow(values)) {
                time_idx <- .sits_timeline_indexes(timeline = timeline,
                                                   start_date = lubridate::as_date(points$start_date[i]),
                                                   end_date   = lubridate::as_date(points$end_date[i]))
                # select the valid dates in the timeline
                timeline_row <- timeline[time_idx["start_idx"]:time_idx["end_idx"]]
                # get only valid values for the timeline
                values.vec <- as.vector(values[i, time_idx["start_idx"]:time_idx["end_idx"]])
                # correct the values using the scale factor
                values.vec <- values.vec*scale_factors[band]
                # create a tibble for each band
                ts.tb <- tibble::tibble(Index = timeline_row)
                # put the values in the time series tibble together t
                ts.tb$values <- values.vec
                colnames(ts.tb) <- c("Index", band)

                # insert a row on the tibble with the values for lat/long and the band
                ts_band.tb <- tibble::add_row(ts_band.tb,
                                              longitude    = dplyr::pull(points[i,"longitude"]),
                                              latitude     = dplyr::pull(points[i,"latitude"]),
                                              start_date   = timeline[time_idx["start_idx"]],
                                              end_date     = timeline[time_idx["end_idx"]],
                                              label        = as.character(dplyr::pull(points[i,"label"])),
                                              cube         = cube$name,
                                              time_series  = list(ts.tb)
                )
            }
            return(ts_band.tb)
        })

    # merge the bands
    data.tb <- .sits_tibble()
    l <- length(ts_bands.lst)
    for (i in 1:l) {
        data.tb <- sits_merge(data.tb, ts_bands.lst[[i]])
    }
    return(data.tb)
}

