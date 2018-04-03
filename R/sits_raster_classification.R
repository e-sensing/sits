#' @title Classify a set of spatio-temporal raster bricks using machine learning models
#' @name sits_classify_raster
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a set of spatio-temporal raster bricks, whose metadata is
#'              described by tibble (created by \code{\link[sits]{sits_coverage}}),
#'              a set of samples used for training a classification model,
#'              a prediction model (created by \code{\link[sits]{sits_train}}),
#'              and produces a classified set of RasterLayers. This function is similar to
#'               \code{\link[sits]{sits_classify}} which is applied to time series stored in a SITS tibble.
#'               There are two parameters for optimizing processing of large data sets. These
#'               parameters are "memsize" and "multicores". The "multicores" parameter defines the
#'               number of cores used for processing. The "memsize" parameter  controls
#'               the amount of memory available for classification.
#'
#'
#' @param  file            vector of file names to store the output (one file per classified year)
#' @param  raster.tb       tibble with information about a set of space-time raster bricks
#' @param  samples.tb      tibble with samples used for training the classification model
#' @param  ml_model        an R model trained by \code{\link[sits]{sits_train}}
#' @param  ml_method       an R machine learning method such as SVM, Random Forest or Deep Learning
#' @param  adj_fun         adjustment function to be applied to the data
#' @param  interval        interval between two sucessive classifications, expressed in months
#' @param  smoothing       (boolean) apply a Whittaker smoothing function?
#' @param  lambda          degree of smoothing of the Whittaker smoother (default = 0.5)
#' @param  differences     the order of differences of contiguous elements (default = 3)
#' @param  memsize         memory available for classification (in GB)
#' @param  multicores      number of threads to process the time series.
#' @param  verbose         logical: run function in verbose mode? (useful for working with big data sets)
#' @return raster_class.tb tibble with the metadata for the vector of classified RasterLayers
#'
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' data(samples_MT_ndvi)
#'
#' # read a raster file and put it into a vector
#' files  <- c(system.file ("extdata/raster/mod13q1/sinop-crop-ndvi.tif", package = "sits"))
#'
#' # define the timeline
#' data(timeline_modis_392)
#'
#' # create a raster metadata file based on the information about the files
#' #' # create a raster coverage file based on the information about the files
#' raster.tb <- sits_coverage(service = "RASTER", name  = "Sinop-crop",
#'              timeline = timeline_modis_392, bands = c("ndvi"), files = files)
#'
#' # classify the raster file
#' raster_class.tb <- sits_classify_raster (file = "./raster-class", raster.tb, samples_MT_ndvi,
#'    ml_method = sits_svm(), smoothing = TRUE, memsize = 1, multicores = 1)
#' }
#'
#' @export
sits_classify_raster <- function(file = NULL,
                                 raster.tb,
                                 samples.tb,
                                 ml_model  = NULL,
                                 ml_method  = sits_svm(),
                                 adj_fun    = sits_adjust(),
                                 interval   = "12 month",
                                 smoothing  = FALSE,
                                 lambda     = 0.5,
                                 differences = 3.0,
                                 memsize    = 4,
                                 multicores = 2,
                                 verbose    = FALSE){

    # ensure metadata tibble exists
    ensurer::ensure_that(raster.tb, NROW(.) > 0,
                         err_desc = "sits_classify_raster: need a valid metadata for coverage")

    # ensure patterns tibble exits
    .sits_test_tibble(samples.tb)

    # ensure that file name and prediction model are provided
    ensurer::ensure_that(file, !purrr::is_null(.),
                         err_desc = "sits-classify-raster: please provide name of output file")

    # estimate the amount of memory required
    # get the bands
    bands <-  raster.tb$bands[[1]]
    nbands <-  length(bands)

    # size of the timeline
    ntimes <- length(raster.tb$timeline[[1]])

    # estimated memory bloat
    bloat <- 20

    # calculate the blocksize as
    # blocksize = (available memory for use) / (20 * bands * times).
    # For example, for a server with 60 GB available, and an image with
    # 2 bands and 500 instances of time, the blocksize can be up to 3000000 (3e+06).
    # The factor of 20 is an estimate of how many bytes are used for each pixel,
    # since R stores everything in double precision and makes copies of data.

    roughsize <- (memsize*1e+09)/(as.numeric(nbands) * as.numeric(ntimes) * as.numeric(bloat))
    blocksize <- as.integer(trunc(roughsize/10000)*10000)

    # apply the smoothing function, if required
    if (smoothing) {
        samples.tb <- sits_whittaker(samples.tb, lambda = lambda, bands_suffix = "")
    }

    # set up the ML model
    if (purrr::is_null(ml_model))
        ml_model <- sits_train(samples.tb, ml_method = ml_method, adj_fun = adj_fun)

    # create the raster objects and their respective filenames
    raster_class.tb <- .sits_create_classified_raster(raster.tb, samples.tb, file, interval)

    # define the classification info parameters
    class_info.tb <- .sits_class_info(raster.tb, samples.tb, interval)

    # get the labels of the data
    labels <- sits_labels(samples.tb)$label

    # create a named vector with integers match the class labels
    int_labels <- c(1:length(labels))
    names(int_labels) <- labels

    # create a list with the output raster layers
    layers.lst <- unlist(raster_class.tb$r_objs)

    #initiate writing
    for (i in 1:length(layers.lst)) {
        layers.lst[[i]] <- raster::writeStart(layers.lst[[i]], layers.lst[[i]]@file@name, overwrite = TRUE)
    }
    # recover the input data by blocks for efficiency
    bs <- .sits_raster_block_size(raster_class.tb[1,], blocksize)

    # prepare the data required for classification
    time_index.lst <- .sits_get_time_index(class_info.tb)

    # get attribute names
    attr_names <- .sits_get_attr_names(class_info.tb)

    progress_bar <- NULL
    # create a progress bar
    if (bs$n > 1) {
        message("Classifying raster data")
        progress_bar <- utils::txtProgressBar(min = 0, max = bs$n, style = 3)
    }

    # read the input raster in blocks
    # classify the data

    for (i in 1:bs$n) {
        layers.lst <- .sits_classify_bigdata(raster.tb,
                                             layers.lst,
                                             time_index.lst,
                                             bands,
                                             attr_names,
                                             int_labels,
                                             bs$row[i],
                                             bs$nrows[i],
                                             adj_fun,
                                             ml_model,
                                             smoothing,
                                             lambda,
                                             differences,
                                             multicores,
                                             verbose)
        if (!purrr::is_null(progress_bar))
            utils::setTxtProgressBar(progress_bar, i)
    }
    # finish writing
    for (i in 1:length(layers.lst)) {
        layers.lst[[i]] <- raster::writeStop(layers.lst[[i]])
    }
    if (!purrr::is_null(progress_bar)) close(progress_bar)
    # update the raster objects
    raster_class.tb$r_objs <- layers.lst

    return(raster_class.tb)
}
#' @title Get a raster object from a raster classified coverage
#' @name sits_get_robj
#' @description This function retrieves one or more raster layer objects stored in a raster coverage.
#'              It should be used to ensure that the raster objects are returned correctly.
#'
#' @param raster.tb  raster coverage (classified)
#' @param i          i-th element of the list to retrieve
#' @return r_obj     a raster layer with classification
#'
#' @examples
#' \donttest{
#' # Define a raster Brick and retrieve the associated object
#' # define the file that has the raster brick
#' files  <- c(system.file ("extdata/raster/mod13q1/sinop-crop-ndvi.tif", package = "sits"))
#' # define the timeline
#' data(timeline_modis_392)
#' # create a raster metadata file based on the information about the files
#' raster_cov <- sits_coverage(files = files, name = "Sinop-crop",
#'                             timeline = timeline_modis_392, bands = c("ndvi"))
#'
#' # plot the first raster object with a selected color pallete
#' # get the first classified object
#' r.obj <- sits_get_robj(raster_class.tb,1)
#'
#' # make a title, define the colors and the labels)
#' title <- paste0("Classified image of part of SINOP-MT - 2000/2001")
#' colors <- c("#65AF72", "#d4d6ed", "#006400","#add8e6","#a0522d",
#'             "#a52a2a","#d2b48c", "#cd853f", "#ff8c00")
#' labels <- sits_labels(samples_MT_ndvi)$label
#'
#' sits_plot_raster(r.obj, title, labels, colors)
#' }
#'
#' @export
#
sits_get_robj <- function(raster.tb, i) {

    ensurer::ensure_that(i, (.) <= nrow(raster.tb),
                         err_desc = "sits_get_raster: index of raster object cannot be retrieved")

    return(raster.tb[i,]$r_objs[[1]])
}
#' @title Classify a raster chunk using machine learning models
#' @name .sits_classify_bigdata
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Classifies a block of data one interval at a time. Reads data from
#' a file using Rgdal, then cleans the data for NAs and missing values. The clean
#' data is store in a data table that has all the time instances for all pixels of
#' the block. The algorithm then classifies data on an year by year basis.
#' For each year, it extracts the sub-blocks for each band. It then breaks
#' the sub-block horizontally and divides its among the server cores available.
#' After all cores process their sub-blocks, it joins the result and then writes it
#' in the classified image for the corresponding year. Then, it starts again in the
#' next year to be classified.
#'
#' @param  raster.tb       tibble with metadata for a RasterBrick
#' @param  layers.lst      a list with the raster layer objects to be written
#' @param  time_index.lst  a list with the indexes to extract data for each time interval
#' @param  bands           vector with the names of the bands
#' @param  attr_names      vector with the attribute names
#' @param  int_labels      conversion vector from the labels to integer values
#' @param  init_row        starting row from the RasterBrick
#' @param  nrows           number of rows in the block extracted from the RasterBrick
#' @param  adj_fun         sdjustment function to be applied to the data
#' @param  ml_model        a model trained by \code{\link[sits]{sits_train}}
#' @param  smoothing       (boolean) apply whittaker smoothing?
#' @param  lambda          smoothing factor (default = 1.0)
#' @param  differences     the order of differences of contiguous elements (default = 3)
#' @param  multicores      number of threads to process the time series.
#' @param  verbose         run function in verbose mode? (useful for working with big data sets)
#' @return layer.lst       list  of the classified raster layers
#'
.sits_classify_bigdata <-  function(raster.tb,
                                    layers.lst,
                                    time_index.lst,
                                    bands,
                                    attr_names,
                                    int_labels,
                                    init_row,
                                    nrows,
                                    adj_fun,
                                    ml_model,
                                    smoothing,
                                    lambda,
                                    differences,
                                    multicores,
                                    verbose) {


    ensurer::ensure_that(ml_model, !purrr::is_null(.),
                         err_desc = "sits-classify: please provide a machine learning model already trained")

    # get the vector of bricks
    bricks.vec <- raster.tb$files[[1]]
    # get the bands, scale factors and missing values
    bands <- unlist(raster.tb$bands)
    missing_values <- unlist(raster.tb$missing_values)
    minimum_values <- unlist(raster.tb$minimum_values)
    scale_factors  <- unlist(raster.tb$scale_factors)

    # define the smoothing function
    whit <- function(ts) {
        E <- diag(length(ts))
        D <- diff(E, lag = 1, differences)
        B <- E + (lambda * crossprod(D))
        tsf <- solve(B, ts)
        return(tsf)
    }

    # set the offset and region to be read by GDAL
    offset <- c(init_row - 1, 0)
    ncols <- raster.tb$ncols
    region.dim <- c(nrows, ncols)
    # create a pointer to the years
    i <- 0
    if (verbose)
        .sits_log_debug(paste0("Memory used before readGDAL - ", .sits_mem_used(), " GB"))

    # go through all the bricks
    values.lst <- bricks.vec %>%
        purrr::map(function(r_brick) {
            # the readGDAL function returns a matrix
            # the rows of the matrix are the pixels
            # the cols of the matrix are the layers
            values.mx    <- as.matrix(suppressWarnings(rgdal::readGDAL(r_brick, offset, region.dim, silent = TRUE))@data)

            # get the associated band
            i <<- i + 1
            band <- bands[i]
            missing_value <- missing_values[band]
            minimum_value <- minimum_values[band]
            scale_factor  <- scale_factors[band]

            if (verbose) {
                message(paste0("Memory used after readGDAL - ", .sits_mem_used(), " GB"))
                message(paste0("Read band ", band, " from rows ", init_row, "to ", (init_row + nrows - 1)))
            }

            values.mx[is.na(values.mx)] <- minimum_value
            values.mx[values.mx <= minimum_value] <- minimum_value

            # values.mx <- preprocess_data(values.mx, minimum_value, scale_factor)
            values.mx <- values.mx * scale_factor

            values.mx <- adj_fun(values.mx)
            if (smoothing) {
                rows.lst <- lapply(seq_len(nrow(values.mx)), function(i) values.mx[i, ]) %>%
                    lapply(whit)
                values.mx <- do.call(rbind, rows.lst)
            }
            if (verbose) {
                message(paste0("Memory used after adj_fun - ", .sits_mem_used(), " GB"))
            }

            return(values.mx)
        })

    dist.tb <- data.table::as.data.table(do.call(cbind, values.lst))

    # memory cleanup
    if (verbose)
        message(paste0("Memory used after binding bricks  - ", .sits_mem_used(), " GB"))

    rm(values.lst)
    gc()
    if (verbose)
        message(paste0("Memory used after removing values - ", .sits_mem_used(), " GB"))

    # create a data table for better memory management
    size <- nrows*ncols
    two_cols.tb <- data.table::data.table("original_row" = rep(1,size),
                                          "reference" = rep("NoClass", size))

    dist.tb <- cbind(two_cols.tb, dist.tb)

    # memory debug
    if (verbose)
        message(paste0("Memory used after adding two first cols - ", .sits_mem_used(), " GB"))


    # iterate through time intervals
    for (t in 1:length(time_index.lst)) {
        idx <- time_index.lst[[t]]
        # for a given time index, build the data.table to be classified
        # build the classification matrix extracting the relevant columns
        selec <- logical(length = ncol(dist.tb))
        selec[1:2] <- TRUE
        for (b in 1:length(bands)) {
            i1 <- idx[(2*b - 1)] + 2
            i2 <- idx[2*b] + 2
            selec[i1:i2] <- TRUE
        }
        # retrieve the values used for classification
        dist1.tb <- dist.tb[,selec]
        # set the names of the columns of dist1.tb
        colnames(dist1.tb) <- attr_names

        # memory management
        if (verbose)
            message(paste0("Memory used after selecting data subset  - ", .sits_mem_used(), " GB"))

        # classify a block of data
        classify_block <- function(block.tb) {
            # predict the values for each time interval
            pred_block.vec <- .sits_predict(block.tb, ml_model)
            return(pred_block.vec)
        }

        # set up multicore processing
        if (multicores > 1) {
            # estimate the list for breaking a block
            block_size.lst <- .sits_split_block_size(nrow(dist1.tb), multicores)
            # divide the input matrix into blocks for multicore processing
            blocks.lst <- vector(mode = "list", length = multicores)
            for (b in 1:length(block_size.lst)) {
                bs <- block_size.lst[[b]]
                    # select a chunk of data for each core
                    block.tb <- dist1.tb[bs[1]:bs[2],]
                    blocks.lst[[b]] <- block.tb
            }
            # memory management
            if (verbose)
                message(paste0("Memory used after building chunks  - ", .sits_mem_used(), " GB"))
            #rm(dist1.tb)
            gc()
            if (verbose)
                message(paste0("Memory used before calling parallel processing - ", .sits_mem_used(), " GB"))

            # apply parallel processing to the split data and join the results
            pred.vec <- unlist(parallel::mclapply(blocks.lst, classify_block, mc.cores = multicores))

            # memory management
            if (verbose)
                message(paste0("Memory used after calling parallel processing - ", .sits_mem_used(), " GB"))
            rm(block_size.lst)
            rm(blocks.lst)
            gc()
            if (verbose)
                message(paste0("Memory used after removing blocks - ", .sits_mem_used(), " GB"))
        }
        else {
            # estimate the prediction vector
            pred.vec <- classify_block(dist1.tb)

            # memory management
            rm(dist1.tb)
            gc()
            if (verbose)
                message(paste0("Memory used after classification in one core - ", .sits_mem_used(), " GB"))
        }

        # check the result has the right dimension
        ensurer::ensure_that(pred.vec, length(.) == nrow(dist.tb),
                             err_desc = "sits_classify_raster - number of classified pixels is different
                                        from number of input pixels")

        # for each layer, write the predicted values

        layers.lst[[t]] <- raster::writeValues(layers.lst[[t]], as.integer(int_labels[pred.vec]), init_row)
        if (verbose)
            message(paste0("Processed year ", t, " starting from row ", init_row))

        # memory management
        rm(pred.vec)
        #rm(values)
        gc()
        if (verbose)
            message(paste0("Memory used after classification of year ", t, " - ", .sits_mem_used(), " GB"))
    }
    # memory management
    rm(dist.tb)
    gc()
    if (verbose) {
        message(paste0("Memory used after end of processing all years - ", .sits_mem_used(), " GB"))
        message(paste0("Processed block starting from ", init_row, " to ", (init_row + nrows - 1)))
    }

    return(layers.lst)
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
#'
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
#' @title Define a reasonable block size to process a RasterBrick
#' @name .sits_raster_block_size
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Defines the size of the block of a Raster Brick to be read into memory.
#' The total pixels of a RasterBrick is given by combining the size of the timeline
#' with the number of rows and columns of the Brick. For example, a Raster Brick
#' with 500 rows and 500 columns and 400 time instances will have a total pixel size
#' of 250 Mb if pixels are 16-bit (about a GigaByte). If there are 4 bands to be processed together, there will be 4 Raster Bricks.
#' Thus, a block size of 250000 will use a 1 GB just to store the image data.
#'
#' As a rule of thumb, consider that for a typical MODIS data set such as MOD13Q1 there will be
#' about 23 time instances per year. In 20 years, this means about 460 instances.
#' In a small desktop with 8 GBytes, a block size of 250000 will use 1Gb of memory.
#' This is taken to be the default for small machines.
#' In a larger server, users should increase the block size for improved processing.
#'
#' @param  brick.tb   metadata for a RasterBrick
#' @param  blocksize  default size of the block (rows * cols)
#' @return block      list with three attributes: n (number of blocks), rows (list of rows to begin),
#'                    nrows - number of rows to read at each iteration
#'
.sits_raster_block_size <- function(brick.tb, blocksize){


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

    # elements of the block list
    # n          number of blocks
    # row        starting row from the RasterBrick
    # nrow       Number of rows in the block extracted from the RasterBrick

    block <- list(n = nblocks, row = row, nrows = nrows, size = size)

    return(block)

}
#' @title Define the split of the data blocks for multicore processing
#' @name .sits_split_block_size
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description this functions defines the rows of the input data table that will be
#' split to fit to be divided between the different cores
#'
#' @param nrows            number of rows in the input data table
#' @param ncores           number of cores for processing
#' @return block_size.lst  list of pairs of positions (first row, last row) to be assigned to each core
#'
.sits_split_block_size <- function(nrows, ncores){

    # find the remainder and quotient
    quo <- nrows %/% ncores
    rem <- nrows %% ncores
    # c
    # create a list to store the result
    block_size.lst <- list(length = ncores)
    block_size_end = 0
    for (i in 1:(ncores)) {
        block_size_start <- block_size_end + 1
        block_size_end   <- block_size_start + quo - 1
        if (i == ncores )
            block_size_end <- block_size_end + rem
        block_size.lst[[i]] <- c(block_size_start, block_size_end)
    }
    return(block_size.lst)
}





