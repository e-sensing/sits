#' @title Define a reasonable block size to process a RasterBrick
#' @name .sits_raster_blocks
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Defines the size of the block of a Raster Brick to be read into memory.
#' The total pixels of a RasterBrick is given by combining the size of the timeline
#' with the number of rows and columns of the Brick. For example, a Raster Brick
#' with 500 rows and 500 columns and 400 time instances will have a total pixel size
#' of 800 Mb if pixels are 64-bit.
#'
#' @param  cube            Input data cube.
#' @param  ml_model        Machine learning model.
#' @param  interval        Classification interval.
#' @param  memsize         Memory available for classification (in GB).
#' @param  multicores      Number of threads to process the time series.
#' @return List with three attributes: n (number of blocks), rows (list of rows to begin),
#' nrows (number of rows to read at each iteration).
.sits_raster_blocks <- function(cube, ml_model, interval, memsize, multicores){
    # number of bands
    nbands <-  length(.sits_cube_bands(cube))
    # number of rows and cols
    nrows <- cube[1,]$nrows
    ncols <- cube[1,]$ncols
    # timeline
    timeline <- sits_timeline(cube[1,])

    nblocks <- .sits_raster_blocks_estimate(ml_model, nbands, nrows, ncols, timeline, interval, memsize, multicores)

    # number of rows per block
    block_rows <- ceiling(nrows/nblocks)

    # initial row of each block
    row.vec <- seq.int(from = 1, to = nrows, by = block_rows)
    # number of rows in each block
    nrows.vec <- rep.int(block_rows, length(row.vec))
    # check that total number of rows is the same as the sum of all blocks
    # correct the last block for overflow
    if (sum(nrows.vec) != nrows )
        nrows.vec[length(nrows.vec)] <- nrows - sum(nrows.vec[1:(length(nrows.vec) - 1)])

    # find out the size of the block in pixels
    size.vec <- nrows.vec * ncols

    # elements of the block list
    # n          number of blocks
    # row        starting row from the RasterBrick
    # nrow       Number of rows in the block extracted from the RasterBrick
    # size       size of each block in pixels

    bs <- list(n = nblocks, row = row.vec, nrows = nrows.vec, size = size.vec)

    return(bs)
}
#' @title Estimate the number of blocks
#' @name .sits_raster_blocks_estimate
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Defines the number of blocks of a Raster Brick to be read into memory.
#'
#' @param  ml_model        Machine learning model.
#' @param  nbands          Number of bands.
#' @param  nrows           Number of rows per brick.
#' @param  ncols           Number of cols per brick.
#' @param  timeline        Timeline of the brick.
#' @param  interval        Classification interval.
#' @param  memsize         Memory available for classification (in GB).
#' @param  multicores      Number of threads to process the time series.
#' @return Number of blocks to be read.
.sits_raster_blocks_estimate <- function(ml_model, nbands, nrows, ncols, timeline, interval, memsize, multicores) {
    # total number of instances
    ninstances <- length(timeline)
    # number of instances per classification interval
    interval_dates <- lubridate::as.duration(lubridate::as_date(timeline) - lubridate::as_date(timeline[1])) > lubridate::as.duration(interval)
    if (any(interval_dates))
        ninterval <- which(interval_dates)[1] - 1
    else
        ninterval <- ninstances
    # number of bytes por pixel
    nbytes <-  8
    # estimated memory bloat
    bloat <- as.numeric(.sits_config_memory_bloat())
    # estimated processing bloat
    proc_bloat <- as.numeric(.sits_config_processing_bloat())
    if (proc_bloat == 0) proc_bloat <- multicores

    # single instance size
    single_data_size <- as.numeric(nrows)*as.numeric(ncols)*as.numeric(nbytes)
    # total size including all bricks
    bricks_data_size <- single_data_size*as.numeric(nbands)

    # estimated full size of the data
    full_data_size <- as.numeric(ninstances)*bricks_data_size

    # estimated size of memory required for scaling and normalization
    mem_required_scaling <- (full_data_size + as.numeric(.sits_mem_used()))*bloat

    .sits_log_debug(paste0("max memory required for scaling (GB) - ", round(mem_required_scaling/1e+09, digits = 3)))

    # number of labels
    nlabels <- length(sits_labels(environment(ml_model)$data)$label)
    # estimated size of the data for classification
    input_class_data_size <- as.numeric(ninterval)*bricks_data_size
    output_class_data_size <- as.numeric(nlabels)*single_data_size
    class_data_size <- input_class_data_size + output_class_data_size

    # memory required for processing depends on the model
    if ("keras_model" %in% class(ml_model) || "rfor_model" %in% class(ml_model))  {
        .sits_log_debug(paste0("keras and ranger run on multiple threads"))
        mem_required_processing <- (class_data_size + as.numeric(.sits_mem_used()))*proc_bloat
    }
    else {
        # test two different cases
        if (ninstances == ninterval) # one interval only
            mem_required_processing <- as.numeric(multicores)*(class_data_size + as.numeric(.sits_mem_used()))
        else
            mem_required_processing <- as.numeric(multicores)*(.sits_mem_used() + class_data_size + full_data_size)
    }
    .sits_log_debug(paste0("max memory required for processing (GB) - ", round(mem_required_processing/1e+09, digits = 3)))

    # number of passes to read the full data sets
    nblocks <- max(ceiling(mem_required_scaling/(memsize*1e+09)), ceiling(mem_required_processing/(memsize*1e+09)))

    return(nblocks)
}

#' @title Check if the raster files are on the web and include prefix for GDAL access
#' @name .sits_raster_check_webfiles
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param files         files associated to the raster data
#' @return files        Updated files with appropriate information for GDAL access
.sits_raster_check_webfiles <- function(files) {
    # are there webfiles?
    if (all(grepl("http", c(files[1])))) {
        # append "vsicurl" prefix for all web files if it is not there
        if (!grepl("vsicurl", c(files[1])))
            files <- paste("/vsicurl", files, sep = "/")
    }
    return(files)
}

#' @title Check if the raster files are accessible by GDAL
#' @name .sits_raster_check_gdal_access
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param files         files associated to the raster data
#' @return TRUE         true if filles are acessible
.sits_raster_check_gdal_access <- function(files){
    # verify if all files are reacheable
    r <- suppressWarnings(rgdal::GDALinfo(files, silent = FALSE))
    ensurer::ensure_that(r, all(!purrr::is_null(.)),
                         err_desc = "sits_cube: raster files cannot be accessed")
    return(TRUE)
}

#' @title Check if the raster files are bricks
#' @name .sits_raster_check_bricks
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param files         files associated to the raster data
#' @return TRUE         true if filles are acessible
.sits_raster_check_bricks <- function(files){
    # are the files bricks?
    tryCatch({
        brick <- raster::brick(files[1])
    }, error = function(e){
        msg <- paste0("Raster files are not bricks")
        .sits_log_error(msg)
        message(msg)
    })
    return(TRUE)
}

#' @title Check if the raster files are stacks
#' @name .sits_raster_check_stacks
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param files         files associated to the raster data
#' @return TRUE         true if filles are acessible
.sits_raster_check_stacks <- function(files){
    # are the files stacks?
    tryCatch({
        brick <- raster::stack(files[1])
    }, error = function(e){
        msg <- paste0("Raster files are not stacks")
        .sits_log_error(msg)
        message(msg)
    })
    return(TRUE)
}
#' @title Create a data cube based on a set of Raster Bricks
#' @name .sits_raster_cube
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
.sits_raster_cube <- function(service, URL, satellite, sensor, name,
                              timeline, bands,  files) {

    ensurer::ensure_that(bands, length(.) == length(files),
                         err_desc = "sits_raster_cube: number of bands does not match number of files")
    ensurer::ensure_that(name, !purrr::is_null(.),
                         err_desc = "sits_raster_cube: name of the coverege must be provided")
    ensurer::ensure_that(files, !purrr::is_null(.),
                         err_desc = "sits_raster_cube - files must be provided")

    ensurer::ensure_that(timeline, !purrr::is_null(.),
                         err_desc = "sits_raster_cube - timeline must be provided")

    # try to guess which is the satellite
    if (purrr::is_null(satellite)) {
        satellite <- .sits_raster_guess_satellite(.sits_raster_files_robj(service, files))
        message(paste0("satellite information not provided - assuming ", satellite))
    }
    # is the satellite supported by SITS?
    ensurer::ensure_that(satellite, (.) %in% .sits_config_satellites(),
                         err_desc = "satellite not supported by SITS - please edit configuration file")

    # try to guess which is the sensor
    if (purrr::is_null(sensor)) {
        sensor <- .sits_config_default_sensor(satellite)
        message(paste0("sensor information not provided - assuming ", sensor))
    }
    # is the sensor supported by SITS?
    ensurer::ensure_that(sensor, (.) %in% .sits_config_sensors(satellite),
                         err_desc = "sensor not supported by SITS - please edit configuration file")

    # transform the timeline to date format
    timeline <- lubridate::as_date(timeline)

    # set the labels
    labels <- c("NoClass")

    # obtain the parameters
    params <- .sits_raster_params(.sits_raster_files_robj(service, files))

    # get scale factors
    scale_factors  <- .sits_config_scale_factors(sensor, bands)
    # get missing values
    missing_values <- .sits_config_missing_values(sensor, bands)
    # get minimum values
    minimum_values <- .sits_config_minimum_values(sensor, bands)
    # get maximum values
    maximum_values <- .sits_config_maximum_values(sensor, bands)


    # create a tibble to store the metadata
    cube <- .sits_cube_create(service        = service,
                              URL            = URL,
                              satellite      = satellite,
                              sensor         = sensor,
                              name           = name,
                              bands          = bands,
                              labels         = labels,
                              scale_factors  = scale_factors,
                              missing_values = missing_values,
                              minimum_values = minimum_values,
                              maximum_values = maximum_values,
                              timelines      = list(timeline),
                              nrows = params$nrows,
                              ncols = params$ncols,
                              xmin  = params$xmin,
                              xmax  = params$xmax,
                              ymin  = params$ymin,
                              ymax  = params$ymax,
                              xres  = params$xres,
                              yres  = params$yres,
                              crs   = params$xmin,
                              files = files )
    return(cube)
}

#' @title Define a filename associated to one classified raster layer
#' @name .sits_raster_filename
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Creates a filename for a raster layer with associated temporal information,
#'                 given a basic filename.
#' @param output_dir     Output directory
#' @param version        Output version
#' @param name           Original cube name (without temporal information).
#' @param type           Type of output
#' @param start_date    Starting date of the time series classification.
#' @param end_date      End date of the time series classification.
#' @return Name of the classification file for the required interval.
.sits_raster_filename <- function(output_dir, version, name, type, start_date, end_date){
    y1 <- lubridate::year(start_date)
    m1 <- lubridate::month(start_date)
    y2 <- lubridate::year(end_date)
    m2 <- lubridate::month(end_date)

    file_name <- paste0(output_dir,"/", name, "_", type, "_", y1, "_", m1, "_", y2, "_", m2, "_", version,".tif")

    return(file_name)
}
#' @title Filter the time series values in the case of a matrix
#' @name .sits_raster_filter_data
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function filters a matrix.
#'
#' @param  values.mx      Matrix of values.
#' @param  filter         Filter function to apply to matrix.
#' @param  multicores     Number of cores.
#' @param  progress       Show progress bar? Default is TRUE.
#' @return Scaled integer matrix.
.sits_raster_filter_data <- function(values.mx, filter, multicores, progress = TRUE) {
    # scale the data set
    # auxiliary function to scale a block of data
    filter_matrix_block <- function(chunk) {
        filtered_block.mx <- filter(chunk)
    }
    # use multicores to speed up filtering
    if (multicores > 1) {
        chunk.lst <- .sits_raster_split_data(values.mx, multicores)
        rows.lst <- pbLapply(multicores, progress = progress, X = chunk.lst, FUN = filter_matrix_block)

        values.mx <- do.call(rbind, rows.lst)
        rm(chunk.lst)
        rm(rows.lst)
        gc()
    }
    else
        values.mx <- filter(values.mx)

    return(values.mx)
}
#' @title Try a best guess for the type of sensor/satellite
#' @name .sits_raster_guess_satellite
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Based on the projection, tries to guess what is the satellite.
#'
#' @param r_obj      The R object that describes the file
#' @return Name of the satellite .
.sits_raster_guess_satellite <- function(r_obj) {

    crs   <- as.character(raster::crs(r_obj))
    # if the projection is UTM, guess it's a LANDSAT data set
    if (stringr::str_detect(crs, "utm")) {
        satellite <- "LANDSAT"
    }
    # if the projection is sinusoidal, guess it's a TERRA data set
    else if (stringr::str_detect(crs, "sinu")) {
        satellite <- "TERRA"
    }
    else {
        satellite <- "UNKNOWN"
    }

    return(satellite)
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

#' @title Preprocess a set of values retrived from a raster brick
#' @name  .sits_raster_preprocess_data
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
.sits_raster_preprocess_data <- function(values.mx, band, missing_value, minimum_value, scale_factor,
                                         stats, filter, multicores){
    # correct minimum value
    values.mx[is.na(values.mx)] <- minimum_value
    values.mx[values.mx <= minimum_value] <- minimum_value

    # scale the data set
    values.mx <- .sits_raster_scale_data(values.mx, scale_factor, multicores)

    # normalize the data
    if (!purrr::is_null(stats)) {
        values.mx <- .sits_normalize_matrix(values.mx, stats, band, multicores)
    }

    if (!(purrr::is_null(filter))) {
        values.mx <- .sits_raster_filter_data(values.mx, filter, multicores)
    }
    return(values.mx)
}

#' @title Read a block of values retrived from a set of raster bricks
#' @name  .sits_raster_read_data
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  cube            Input data cube.
#' @param  samples         Tibble with samples.
#' @param  ml_model        Machine learning model.
#' @param  first_row       First row to start reading.
#' @param  n_rows_block    Number of rows in the block.
#' @param  stats           Normalization parameters.
#' @param  filter          Smoothing filter to be applied.
#' @param  multicores      Number of cores to process the time series.
#' @return A data.table with values for classification.
.sits_raster_read_data <- function(cube, samples, ml_model,
                                   first_row, n_rows_block,
                                   stats, filter, multicores) {
    # get the bands in the same order as the samples
    bands <- sits_bands(samples)
    n_bands <- length(bands)
    # get the missing values, minimum values and scale factors
    missing_values <- unlist(cube$missing_values)
    minimum_values <- unlist(cube$minimum_values)
    scale_factors  <- unlist(cube$scale_factors)

    ordered_bricks.lst <- purrr::map(1:n_bands, function(i) {
        ordered_bricks <- .sits_cube_robj(cube, i)
    })

    names(ordered_bricks.lst) <- bands

    # index to go through the bands vector
    b <- 0

    # read the values from the raster bricks ordered by bands
    values.lst <- ordered_bricks.lst %>%
        purrr::map(function(r_stack) {
            # getValues function returns a matrix
            # the rows of the matrix are the pixels
            # the cols of the matrix are the layers
            values.mx    <- raster::getValues(r_stack, first_row, n_rows_block)

            # proprocess the input data
            b <<- b + 1
            band <- bands[b]
            values.mx <- .sits_raster_preprocess_data(values.mx, band, missing_values[band], minimum_values[band], scale_factors[band],
                                               stats, filter, multicores)

            # save information about memory use for debugging later
            .sits_log_debug(paste0("Memory used after readGDAL - ", .sits_mem_used(), " GB"))
            .sits_log_debug(paste0("Read band ", b, " from rows ", first_row, "to ", (first_row + n_rows_block - 1)))

            return(values.mx)
        })
    # create a data.table joining the values
    data_DT <- data.table::as.data.table(do.call(cbind,values.lst))

    # memory cleanup
    rm(values.lst)
    gc()

    # create two additional columns for prediction
    size <- n_rows_block*cube[1,]$ncols
    two_cols_DT <- data.table::data.table("original_row" = rep(1,size),
                                          "reference"    = rep("NoClass", size))

    # join the two columns with the data values
    data_DT <- data.table::as.data.table(cbind(two_cols_DT, data_DT))

    # memory debug
    .sits_log_debug(paste0("Memory used after reading block - ", .sits_mem_used(), " GB"))

    return(data_DT)
}

#' @title Read a block of values retrived from a set of raster stacks
#' @name  .sits_raster_read_data_cubes
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  cube            Input data cube.
#' @param  samples         Tibble with samples.
#' @param  ml_model        Machine learning model.
#' @param  first_row       First row to start reading.
#' @param  n_rows_block    Number of rows in the block.
#' @param  stats           Normalization parameters.
#' @param  filter          Smoothing filter to be applied.
#' @param  multicores      Number of cores to process the time series.
#' @return A data.table with values for classification.
.sits_raster_read_data_cubes <- function(cube,
                                  samples,
                                  ml_model,
                                  first_row,
                                  n_rows_block,
                                  stats,
                                  filter,
                                  multicores) {
    # get the bands in the same order as the samples
    bands <- sits_bands(samples)
    n_bands <- length(bands)

    # get the missing values, minimum values and scale factors
    missing_values <- unlist(cube$missing_values)
    minimum_values <- unlist(cube$minimum_values)
    scale_factors  <- unlist(cube$scale_factors)

    ordered_bricks.lst <- purrr::map(1:n_bands, function(i) {
        ordered_bricks <- .sits_cube_robj(cube, i)
    })

    names(ordered_bricks.lst) <- bands

    # index to go through the bands vector
    b <- 0

    # read the values from the raster bricks ordered by bands
    values.lst <- ordered_bricks.lst %>%
        purrr::map(function(r_stack) {
            # getValues function returns a matrix
            # the rows of the matrix are the pixels
            # the cols of the matrix are the layers
            values.mx    <- raster::getValues(r_stack, first_row, n_rows_block)

            # proprocess the input data
            b <<- b + 1
            band <- bands[b]
            values.mx <- .sits_raster_preprocess_data(values.mx, band,
                                               missing_values[band], minimum_values[band],
                                               scale_factors[band],
                                               stats, filter, multicores)

            # save information about memory use for debugging later
            .sits_log_debug(paste0("Memory used after readGDAL - ", .sits_mem_used(), " GB"))
            .sits_log_debug(paste0("Read band ", b, " from rows ", first_row, "to ",
                                   (first_row + n_rows_block - 1)))

            return(values.mx)
        })
    # create a data.table joining the values
    data_DT <- data.table::as.data.table(do.call(cbind,values.lst))

    # memory cleanup
    rm(values.lst)
    gc()

    # create two additional columns for prediction
    size <- n_rows_block*cube[1,]$ncols
    two_cols_DT <- data.table::data.table("original_row" = rep(1,size),
                                          "reference"    = rep("NoClass", size))

    # join the two columns with the data values
    data_DT <- data.table::as.data.table(cbind(two_cols_DT, data_DT))

    # memory debug
    .sits_log_debug(paste0("Memory used after reading block - ", .sits_mem_used(), " GB"))

    return(data_DT)
}

#' @title Given a vector of files, and the name of the servic, return the raster object
#'        associated with the first file
#' @name .sits_raster_files_robj
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description     Given the vector of files and the name of the service, return the Raster object for the file
#' @param service   Name of the service (must be "BRICK" or "STACK!)
#' @param files     Vector of files
#' @return          Raster object associated to the first file
#'
.sits_raster_files_robj <- function(service, files){
    ensurer::ensure_that(service, (.) %in% c("BRICK", "STACK"),
                         err_desc = "Error in creating a data cube from raster files; data should be
                         BRICK or STACK")
    if (service == "BRICK")
        return(raster::brick(files[1]))
    if (service == "STACK")
        return(raster::stack(files[1]))
}

#' @title Scale the time series values in the case of a matrix
#' @name .sits_raster_scale_data
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function normalizes one band of the values read from a raster brick.
#'
#' @param  values.mx      Matrix of values.
#' @param  scale_factor   Scaling factor.
#' @param  multicores     Number of cores.
#' @param  progress       Show progress bar? Default is TRUE.
#' @return A scaled matrix.
.sits_raster_scale_data <- function(values.mx, scale_factor, multicores, progress = TRUE) {
    # scale the data set
    # auxiliary function to scale a block of data
    scale_block <- function(chunk) {
        scaled_block.mx <- scale_data(chunk, scale_factor)
    }
    # use multicores to speed up scaling
    if (multicores > 1) {
        chunk.lst <- .sits_raster_split_data(values.mx, multicores)
        rows.lst <- pbLapply(multicores, progress = progress, X = chunk.lst, FUN = scale_block)

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
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function transforms a numerical matrix into an integer one.
#'
#' @param  values.mx      Matrix of values.
#' @param  scale_factor   Scaling factor.
#' @param  multicores     Number of cores.
#' @param  progress       Show progress bar? Default is TRUE.
#' @return Scaled integer matrix.
.sits_raster_scale_matrix_integer <- function(values.mx, scale_factor, multicores, progress = TRUE) {
    # scale the data set
    # auxiliary function to scale a block of data
    scale_matrix_block <- function(chunk) {
        scaled_block.mx <- scale_matrix_integer(chunk, scale_factor)
    }
    # use multicores to speed up scaling
    if (multicores > 1) {
        chunk.lst <- .sits_raster_split_data(values.mx, multicores)
        rows.lst <- pbLapply(multicores, progress = progress, X = chunk.lst, FUN = scale_matrix_block)
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
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function splits a data.table into a list of chunks for multicore processing.
#'
#' @param data             Data (data.table or matrix).
#' @param ncores           Number of cores for processing.
#' @return List of pairs of positions (first row, last row) to be assigned to each core.
.sits_raster_split_data <- function(data, ncores){
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


#' @title Tests if an XY position is inside a ST Raster Brick
#' @name .sits_raster_xy_inside
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function compares an XY position to the extent of a RasterBrick
#'              described by a raster metadata tibble, and return TRUE if the point is
#'              inside the extent of the RasterBrick object.
#'
#' @param xy         XY extent compatible with the R raster package.
#' @param raster.tb  Tibble with metadata information about a raster data set.
#' @return TRUE if XY is inside the raster extent, FALSE otherwise.
.sits_raster_xy_inside <- function(xy, raster.tb){
    if(xy[1, "X"] < raster.tb[1, ]$xmin) return(FALSE)
    if(xy[1, "X"] > raster.tb[1, ]$xmax) return(FALSE)
    if(xy[1, "Y"] < raster.tb[1, ]$ymin) return(FALSE)
    if(xy[1, "Y"] > raster.tb[1, ]$ymax) return(FALSE)
    return(TRUE)
}

