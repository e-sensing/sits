#' @title Classify a set of spatio-temporal raster bricks using multicore machines
#' @name sits_classify_multicores
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
#' @param  adj_val         adjustment value to be applied to the data
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
#' raster_class.tb <- sits_classify_multicores (file = "./raster-class", raster.tb, samples_MT_ndvi,
#'    ml_method = sits_svm(), smoothing = TRUE, memsize = 2, multicores = 2)
#' }
#'
#' @export
sits_classify_multicores <- function(file = NULL,
                                 raster.tb,
                                 samples.tb,
                                 ml_model  = NULL,
                                 ml_method  = sits_svm(),
                                 adj_val    = 3.0,
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

    # apply the smoothing function, if required
    if (smoothing) {
        samples.tb <- sits_whittaker(samples.tb, lambda = lambda, bands_suffix = "")
    }

    # set up the ML model
    if (purrr::is_null(ml_model))
        ml_model <- sits_train(samples.tb, ml_method = ml_method, adj_val = adj_val)

    # create the raster objects and their respective filenames
    raster_class.tb <- .sits_create_classified_raster(raster.tb, samples.tb, file, interval)

    # define the classification info parameters
    class_info.tb <- .sits_class_info(raster.tb, samples.tb, interval)

    # define the time indexes required for classification
    time_index.lst <- .sits_get_time_index(class_info.tb)

    # set attribute names
    attr_names <- .sits_get_attr_names(class_info.tb)

    # get the labels of the data
    labels <- sits_labels(samples.tb)$label

    # create a named vector with integers to match the class labels
    int_labels <- c(1:length(labels))
    names(int_labels) <- labels

    # classify the data
    raster_class.tb <- .sits_classify_bigdata_mc(raster.tb,
                                             raster_class.tb,
                                             time_index.lst,
                                             attr_names,
                                             int_labels,
                                             adj_val,
                                             ml_model,
                                             smoothing,
                                             lambda,
                                             differences,
                                             memsize,
                                             multicores,
                                             verbose)

    return(raster_class.tb)
}
#' @title Classify a raster chunk using machine learning models
#' @name .sits_classify_bigdata_mc
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Classifies a block of data using multicores. It breaks
#' the data into horizontal blocks and divides them between the available cores.
#'
#' Reads data from a file using Rgdal, then cleans the data for NAs and missing values. The clean
#' data is stored in a data table that has all the time instances for all pixels of
#' the block. The algorithm then classifies data on an year by year basis.
#' For each year, it extracts the sub-blocks for each band.
#'
#' After all cores process their blocks, it joins the result and then writes it
#' in the classified images for each corresponding year.
#'
#' @param  raster.tb       tibble with metadata for a RasterBrick
#' @param  raster_class.tb raster layer objects to be written
#' @param  time_index.lst  a list with the indexes to extract data for each time interval
#' @param  attr_names      vector with the attribute names
#' @param  int_labels      conversion vector from the labels to integer values
#' @param  adj_val         adjustment value to be applied to the data
#' @param  ml_model        a model trained by \code{\link[sits]{sits_train}}
#' @param  smoothing       (boolean) apply whittaker smoothing?
#' @param  lambda          smoothing factor (default = 1.0)
#' @param  differences     the order of differences of contiguous elements (default = 3)
#' @param  memsize         memory available for classification (in GB)
#' @param  multicores      number of threads to process the time series.
#' @param  verbose         run function in verbose mode? (useful for working with big data sets)
#' @return layer.lst       list  of the classified raster layers
#'
.sits_classify_bigdata_mc <-  function(raster.tb,
                                    raster_class.tb,
                                    time_index.lst,
                                    attr_names,
                                    int_labels,
                                    adj_val,
                                    ml_model,
                                    smoothing,
                                    lambda,
                                    differences,
                                    memsize,
                                    multicores,
                                    verbose) {


    ensurer::ensure_that(ml_model, !purrr::is_null(.),
                         err_desc = "sits-classify: please provide a machine learning model already trained")

    # define the smoothing function
    whit <- function(ts) {
        E <- diag(length(ts))
        D <- diff(E, lag = 1, differences)
        B <- E + (lambda * crossprod(D))
        tsf <- solve(B, ts)
        return(tsf)
    }

    # get the vector of bricks
    bricks.vec <- raster.tb$files[[1]]
    # get the bands, scale factors and missing values
    bands <- unlist(raster.tb$bands)
    missing_values <- unlist(raster.tb$missing_values)
    minimum_values <- unlist(raster.tb$minimum_values)
    scale_factors  <- unlist(raster.tb$scale_factors)

    # create a list with the output raster layers
    layers.lst <- unlist(raster_class.tb$r_objs)

    #initiate writing
    for (i in 1:length(layers.lst)) {
        layers.lst[[i]] <- raster::writeStart(layers.lst[[i]], layers.lst[[i]]@file@name, overwrite = TRUE)
    }

    # estimate the full data size - number of bands, rows and cols, time instances and bloat
    nbands <-  length(bands)
    nrows <- raster.tb[1,]$nrows
    ncols <- raster.tb[1,]$ncols
    # size of the timeline
    ntimes <- time_index.lst[[length(time_index.lst)]][2] - time_index.lst[[1]][1] + 1
    # number of bytes por pixel
    nbytes <-  8
    # estimated memory bloat
    bloat <- 2

    # calculate the estimated size of the data
    full_data_size <- as.numeric(nrows*ncols*ntimes*nbands*nbytes*bloat) + as.numeric(pryr::mem_used())

    # number of passes to read the full data sets
    nblocks <- ceiling(full_data_size/(memsize*1e+09))

    # divide the input data in blocks
    bs <- .sits_raster_block_size(nrows, ncols, nblocks)

    # function to process blocks
    process_block <- function(block_info) {

        # set the offset and region to be read by GDAL
        offset <- c(block_info[1] - 1, 0)
        block_nrows  <- block_info[2] - block_info[1] + 1
        region.dim <- c(block_nrows, ncols)

        # set a pointer to the bands
        i <- 0

        # read the values from the raster bricks
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

                # correct minimum value
                values.mx[is.na(values.mx)] <- minimum_value
                values.mx[values.mx <= minimum_value] <- minimum_value

                # values.mx <- preprocess_data(values.mx, minimum_value, scale_factor)
                # scale the data set
                values.mx <- scale_data(values.mx, scale_factor, adj_val)

                if (smoothing) {
                    rows.lst <- lapply(seq_len(nrow(values.mx)), function(i) values.mx[i, ]) %>%
                        lapply(whit)
                    values.mx <- do.call(rbind, rows.lst)
                }
                return(values.mx)
            })
        # create a data table with all the values from the bands
        dist.tb <- data.table::as.data.table(do.call(cbind, values.lst))

        # clean memory
        rm(values.lst)
        gc()

        # include two new columns in the data table
        size <- block_nrows*ncols
        two_cols.tb <- data.table::data.table("original_row" = rep(1,size),
                                              "reference" = rep("NoClass", size))
        dist.tb <- cbind(two_cols.tb, dist.tb)

        pred_vec.lst <- vector("list",  length(time_index.lst))

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

            # estimate the prediction vector for the time instance
            pred_vec.lst[[t]] <- ml_model(dist1.tb)

            # check the result has the right dimension
            ensurer::ensure_that(pred_vec.lst[[t]], length(.) == nrow(dist1.tb),
                                 err_desc = "not enough memory for classification - please increase memory or reduce number of cores")
        }
        # memory management
        rm(dist1.tb)
        gc()
        return(pred_vec.lst)
    }

    for (i in 1:bs$n) {
        # define the how data blocks are split for multicore processing
        block_size.lst <- .sits_split_block_size(bs$row[i], bs$nrows[i], multicores)

        if (multicores > 1) {
            # apply parallel processing to the split data and join the results
            pred.lst <- parallel::mclapply(block_size.lst, process_block, mc.cores = multicores)

            # create a list to gather the results
            pred_cls <- vector("list", length = length(time_index.lst))

            for (t in 1:length(time_index.lst))
                for (c in 1:multicores)
                pred_cls[[t]] <- append(pred_cls[[t]], pred.lst[[c]][t])
        }

        else
            # estimate the prediction vector using one core only
            pred_cls <- process_block(block_size.lst[[1]])

        # for each time instance, get the prediction values to be written
        for (t in 1:length(time_index.lst))
            layers.lst[[t]] <- raster::writeValues(layers.lst[[t]], as.integer(int_labels[unlist(pred_cls[[t]])]), bs$row[i])

        if (verbose)
                message(paste0("Processed year ", t, " starting from row ", bs$row[i]))
        # memory management
        rm(pred_cls)
        gc()
        if (verbose)
            message(paste0("Memory used after classification of year ", t, " - ", .sits_mem_used(), " GB"))
    }

    if (verbose) {
        message(paste0("Memory used after end of processing all years - ", .sits_mem_used(), " GB"))
        message(paste0("Processed block starting from ", bs$row[i], " to ", (bs$row[i] + bs$nrows[i] - 1)))
    }

    # finish writing
    for (i in 1:length(layers.lst)) {
        layers.lst[[i]] <- raster::writeStop(layers.lst[[i]])
    }

    # update the raster objects
    raster_class.tb$r_objs <- layers.lst

    return(raster_class.tb)
}







