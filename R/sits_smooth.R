#' @title Smooth probability cubes with spatial predictors
#'
#' @name  sits_smooth
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Takes a set of classified raster layers with probabilities,
#'              whose metadata is]created by \code{\link[sits]{sits_cube}},
#'              and applies a smoothing function. There are three options,
#'              defined by the "type" parameter:
#' \itemize{
#'    \item{"bayes": }{Use a bayesian smoother}
#'    \item{"gaussian": }{Use a gaussian smoother}
#'    \item{"bilateral: }{Use a bilateral smoother}
#'
#' }
#'
#' @param  cube              Probability data cube
#' @param  type              Type of smoothing
#' @param  ...               Parameters for specific functions
#' @param  window_size       Size of the neighbourhood.
#' @param  smoothness        Estimated variance of logit of class probabilities
#'                           (Bayesian smoothing parameter). It can be either
#'                           a matrix or a scalar.
#' @param  covar             a logical argument indicating if a covariance
#'                           matrix must be computed as the prior covariance
#'                           for bayesian smoothing.
#' @param  sigma             Standard deviation of the spatial Gaussian kernel
#'                           (for gaussian and bilateral smoothing)
#' @param  tau               Standard deviation of the class probs value
#'                           (for bilateral smoothing)
#' @param  multicores        Number of cores to run the smoothing function
#' @param  memsize           Maximum overall memory (in GB) to run the
#'                           smoothing.
#' @param  output_dir        Output directory for image files
#' @param  version           Version of resulting image
#'                           (in the case of multiple tests)
#'
#' @return A tibble with metadata about the output raster objects.
#'
#' @references K. Schindler, "An Overview and Comparison of Smooth Labeling
#'             Methods for Land-Cover Classification",
#'             IEEE Transactions on Geoscience and Remote Sensing,
#'             50 (11), 4534-4545, 2012 (for gaussian and bilateral smoothing)
#'
#' @examples
#' \dontrun{
#' # Retrieve the samples for Mato Grosso
#' # select band "ndvi"
#'
#' samples_ndvi <- sits_select(samples_modis_4bands, bands = "NDVI")
#'
#' # select a random forest model
#' rfor_model <- sits_train(samples_ndvi, sits_rfor(num_trees = 500))
#'
#' # create a data cube based on the information about the files
#' data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#' cube <- sits_cube(
#'     source = "BDC",
#'     collection = "MOD13Q1-6",
#'     data_dir = data_dir,
#'     delim = "_",
#'     parse_info = c("X1", "X2", "tile", "band", "date")
#' )
#'
#' # classify the raster image
#' probs_cube <- sits_classify(cube,
#'     ml_model = rfor_model,
#'     output_dir = tempdir(),
#'     memsize = 4, multicores = 2
#' )
#'
#' # smooth the result with a bayesian filter
#' bayes_cube <- sits_smooth(probs_cube,
#'      type = "bayes", output_dir = tempdir()
#' )
#'
#' # smooth the result with a gaussian filter
#' gauss_cube <- sits_smooth(probs_cube,
#'     type = "gaussian", output_dir = tempdir()
#' )
#'
#' # smooth the result with a bilateral filter
#' bil_cube <- sits_smooth(probs_cube,
#'     type = "bilateral", output_dir = tempdir()
#' )
#' }
#'
#' @export
#'
sits_smooth <- function(cube, type = "bayes", ...) {

    # set caller to show in errors
    .check_set_caller("sits_smooth")

    # check if cube has probability data
    .check_that(
        x = inherits(cube, "probs_cube"),
        msg = "input is not probability cube"
    )

    # define the class of the smoothing
    class(type) <- c(type, class(type))

    UseMethod("sits_smooth", type)
}

#' @rdname sits_smooth
#'
#' @export
#'
sits_smooth.bayes <- function(cube, type = "bayes", ...,
                              window_size = 5,
                              smoothness = 20,
                              covar = FALSE,
                              multicores = 2,
                              memsize = 4,
                              output_dir = ".",
                              version = "v1") {

    # precondition 1 - check if cube has probability data
    .check_that(
        x = inherits(cube, "probs_cube"),
        msg = "input is not probability cube"
    )

    # precondition 2 - test window size
    .check_that(
        x = window_size %% 2 != 0,
        msg = "window_size must be an odd number"
    )

    # find out how many labels exist
    n_labels <- length(sits_labels(cube[1, ]))

    # precondition 3 - test variance
    if (is.matrix(smoothness)) {
        .check_that(
            x = (nrow(smoothness) == ncol(smoothness)) &&
                (ncol(smoothness) == n_labels),
            msg = paste("smoothness must be square matrix of",
                        "the same length as the number of labels")
        )
    } else {
        .check_num(x = smoothness,
                   min = 1,
                   len_max = 1,
                   allow_zero = FALSE,
                   msg = "smoothness must be greater than 1")
        smoothness <- diag(smoothness, nrow = n_labels, ncol = n_labels)
    }

    # precondition 4 - multicores
    .check_num(x = multicores,
               len_max = 1,
               min = 1,
               allow_zero = FALSE,
               msg = "multicores must be at least 1")

    # precondition 5 - memory
    .check_num(x = memsize,
               len_max = 1,
               min = 1,
               allow_zero = FALSE,
               msg = "memsize must be positive")

    # precondition 6 - output dir
    .check_file(x = output_dir,
                msg = "invalid output dir")

    # precondition 7 - version
    .check_chr(x = version,
               len_min = 1,
               msg = "invalid version")

    # create a window
    window <- matrix(1, nrow = window_size, ncol = window_size)

    # retrieve the scale factor
    mult_factor <- round(1 / .config_get("probs_cube_scale_factor"))

    # Bayesian smoother to be executed by workers cluster
    .do_bayes <- function(chunk) {

        data <- .raster_get_values(r_obj = chunk)

        # fix probabilities
        maxprob <- mult_factor - ncol(data) + 1
        data[data == 0] <- 1
        data[data > maxprob] <- maxprob

        # compute logit
        logit <- log(data / (rowSums(data) - data))

        # process Bayesian
        data <- bayes_smoother(m = logit,
                               m_nrow = .raster_nrows(chunk),
                               m_ncol = .raster_ncols(chunk),
                               w = window,
                               sigma = smoothness,
                               covar_sigma0 = covar)

        # calculate the Bayesian probability for the pixel
        data <- exp(data) * mult_factor / (exp(data) + 1)

        # create cube smooth
        res <- .raster_rast(r_obj = chunk,
                            nlayers = .raster_nlayers(chunk))

        # copy values
        res <- .raster_set_values(r_obj = res,
                                  values = data)

        return(res)
    }

    # compute which block size is many tiles to be computed
    block_size <- .smth_estimate_block_size(
        cube = cube,
        multicores = multicores,
        memsize = memsize
    )

    # start parallel processes
    .sits_parallel_start(workers = multicores, log = FALSE)
    on.exit(.sits_parallel_stop())

    # process each brick layer (each time step) individually
    tiles_blocks_lst <- slider::slide(cube, function(tile) {

        # create metadata for raster cube
        tile_bayes <- .cube_derived_create(
            cube       = tile,
            cube_class = "probs_cube",
            band_name  = "bayes",
            labels     = .cube_labels(tile),
            start_date = .file_info_start_date(tile),
            end_date   = .file_info_end_date(tile),
            bbox       = .cube_tile_bbox(tile),
            output_dir = output_dir,
            version    = version
        )

        # open probability file
        in_file <- .file_info_path(tile)

        # retrieve the file to be written
        out_file <- .file_info_path(tile_bayes)

        #
        # .smth_map_layer <- function(cube,
        #                             cube_out,
        #                             overlapping_y_size = 0,
        #                             func,
        #                             func_args = NULL,
        #                             multicores = 1,
        #                             memsize = 1,
        #                             gdal_datatype,
        #                             gdal_options, ...)
        # file_blocks <- .sits_smooth_map_layer(
        #     cube = tile,
        #     cube_out = tile_bayes,
        #     overlapping_y_size =
        #         ceiling(window_size / 2) - 1,
        #     func = .do_bayes,
        #     multicores = multicores,
        #     memsize = memsize,
        #     gdal_datatype = .raster_gdal_datatype(.config_get("probs_cube_data_type")),
        #     gdal_options = .config_gtiff_default_options()
        # )

        # overlapping pixels
        overlapping_y_size <- ceiling(window_size / 2) - 1

        # precondition - overlapping rows must be non negative
        .check_num(
            x = overlapping_y_size,
            min = 0,
            msg = "overlaping rows must be non negative"
        )

        # for now, only vertical blocks are allowed, i.e. 'x_blocks' is 1
        blocks <- .smth_compute_blocks(
            img_y_size = .cube_size(cube)["nrows"],
            block_y_size = block_size[["block_y_size"]],
            overlapping_y_size = overlapping_y_size)

        # process blocks in parallel
        block_files_lst <- .sits_parallel_map(blocks, function(block, in_file, func, args) {

            # open brick
            b <- .raster_open_rast(in_file)

            # create extent
            blk_overlap <- list(first_row = block$r1,
                                nrows = block$r2 - block$r1 + 1,
                                first_col = 1,
                                ncols = .raster_ncols(b))

            # crop adding overlaps
            chunk <- .raster_crop(r_obj = b, block = blk_overlap)

            # process it
            raster_out <- do.call(func, args = c(list(chunk = chunk), args))

            # create extent
            blk_no_overlap <- list(first_row = block$o1,
                                   nrows = block$o2 - block$o1 + 1,
                                   first_col = 1,
                                   ncols = .raster_ncols(raster_out))

            # crop removing overlaps
            raster_out <- .raster_crop(raster_out, block = blk_no_overlap)

            # export to temp file
            block_file <- tempfile(tmpdir = dirname(.file_info(cube_out)$path),
                                   fileext = ".tif")

            # save chunk
            .raster_write_rast(
                r_obj = raster_out,
                file = block_file,
                format = "GTiff",
                data_type = .raster_data_type(.config_get("probs_cube_data_type")),
                gdal_options = .config_gtiff_default_options(),
                overwrite = TRUE
            )

            return(block_file)
        })

        block_files <- unlist(block_files_lst)

        return(invisible(cube_out))



        file_blocks <- .sits_smooth_map_layer(
            cube = tile,
            cube_out = tile_bayes,
            overlapping_y_size =
                ceiling(window_size / 2) - 1,
            func = .do_bayes,
            multicores = multicores,
            memsize = memsize,
            gdal_datatype = .raster_gdal_datatype(.config_get("probs_cube_data_type")),
            gdal_options = .config_gtiff_default_options()
        )

        return(file_blocks)
    })


    # process each brick layer (each time step) individually
    cube_bayes <- .sits_parallel_map(seq_along(tiles_blocks_lst), function(i) {

        # get tile from cube
        tile <- cube[i,]

        # create metadata for raster cube
        tile_bayes <- .cube_derived_create(
            cube       = tile,
            cube_class = "probs_cube",
            band_name  = "bayes",
            labels     = .cube_labels(tile),
            start_date = .file_info_start_date(tile),
            end_date   = .file_info_end_date(tile),
            bbox       = .cube_tile_bbox(tile),
            output_dir = output_dir,
            version    = version
        )

        # prepare output filename
        out_file <- .file_info_path(tile_bayes)

        # if file exists skip it (resume feature)
        if (file.exists(out_file))
            return(NULL)

        tmp_blocks <- tiles_blocks_lst[[i]]

        # apply function to blocks
        on.exit(unlink(tmp_blocks))

        # merge to save final result
        suppressWarnings(
            .raster_merge(
                in_files = tmp_blocks,
                out_file = out_file,
                format   = "GTiff",
                gdal_datatype =
                    .raster_gdal_datatype(.config_get("probs_cube_data_type")),
                gdal_options =
                    .config_gtiff_default_options(),
                overwrite = TRUE
            )
        )

        return(tile_bayes)
    })

    # bind rows
    cube_bayes <- dplyr::bind_rows(cube_bayes)

    class(cube_bayes) <- class(cube)

    return(cube_bayes)
}

#' @rdname sits_smooth
#'
#' @export
#'
sits_smooth.gaussian <- function(cube, type = "gaussian", ...,
                                 window_size = 5,
                                 sigma = 1,
                                 multicores = 2,
                                 memsize = 4,
                                 output_dir = ".",
                                 version = "v1") {

    # precondition 1 - check if cube has probability data
    .check_that(
        x = inherits(cube, "probs_cube"),
        msg = "input is not probability cube"
    )

    # precondition 2 - test window size
    .check_that(
        x = window_size %% 2 != 0,
        msg = "window_size must be an odd number"
    )

    # prediction 3 - test variance
    .check_num(x = sigma,
               len_max = 1,
               min = 1,
               allow_zero = FALSE,
               msg = "smoothness must be positive")

    # precondition 4 - multicores
    .check_num(x = multicores,
               len_max = 1,
               min = 1,
               allow_zero = FALSE,
               msg = "multicores must be at least 1")

    # precondition 5 - memsize
    .check_num(x = memsize,
               len_max = 1,
               min = 1,
               allow_zero = FALSE,
               msg = "memsize must be positive")

    # precondition 6 - output dir
    .check_file(x = output_dir,
                msg = "invalid output dir")

    # precondition 7 - version
    .check_chr(x = version,
               len_min = 1,
               msg = "invalid version")

    # create output window
    gauss_kernel <- .sits_smooth_gauss_kernel(window_size = window_size,
                                              sigma = sigma)

    # retrieve the scale factor
    scale_factor <- round(1 / .config_get("probs_cube_scale_factor"))
    mult_factor <- 1 / scale_factor

    # Gaussian smoother to be executed by workers cluster
    .do_gauss <- function(chunk) {

        # scale probabilities
        data <- .raster_get_values(r_obj = chunk) * scale_factor

        # process Gaussian smoother
        data <- kernel_smoother(m = data,
                                m_nrow = .raster_nrows(chunk),
                                m_ncol = .raster_ncols(chunk),
                                w = gauss_kernel,
                                normalised = TRUE)

        # create cube smooth
        res <- .raster_rast(r_obj = chunk,
                            nlayers = .raster_nlayers(chunk))

        # copy values
        res <- .raster_set_values(r_obj = res,
                                  values = data * mult_factor)
        return(res)
    }

    # process each brick layer (each time step) individually
    cube_gauss <- slider::slide_dfr(cube, function(tile) {

        # create metadata for Gauss smoothed raster cube
        tile_gauss <- .cube_derived_create(
            cube       = tile,
            cube_class = "probs_cube",
            band_name  = "gauss",
            labels     = .cube_labels(tile),
            start_date = .file_info_start_date(tile),
            end_date   = .file_info_end_date(tile),
            bbox       = .cube_tile_bbox(tile),
            output_dir = output_dir,
            version    = version
        )

        .sits_smooth_map_layer(
            cube = tile,
            cube_out = tile_gauss,
            overlapping_y_size =
                ceiling(window_size / 2) - 1,
            func = .do_gauss,
            multicores = multicores,
            memsize = memsize,
            gdal_datatype = .raster_gdal_datatype(.config_get("probs_cube_data_type")),
            gdal_options = .config_gtiff_default_options()
        )

        return(tile_gauss)
    })

    class(cube_gauss) <- class(cube)

    return(cube_gauss)
}

#' @rdname sits_smooth
#'
#' @export
#'
sits_smooth.bilateral <- function(cube,
                                  type = "bilateral",
                                  ...,
                                  window_size = 5,
                                  sigma = 8,
                                  tau = 0.1,
                                  multicores = 2,
                                  memsize = 4,
                                  output_dir = ".",
                                  version = "v1") {

    # precondition 1 - check if cube has probability data
    .check_that(
        x = inherits(cube, "probs_cube"),
        msg = "input is not probability cube"
    )

    # precondition 2 - test window size
    .check_that(
        x = window_size %% 2 != 0,
        msg = "window_size must be an odd number"
    )

    # prediction 3 - test variance
    .check_num(x = sigma,
               len_max = 1,
               min = 1,
               allow_zero = FALSE,
               msg = "smoothness must be positive")

    # precondition 4 - multicores
    .check_num(x = multicores,
               len_max = 1,
               min = 1,
               allow_zero = FALSE,
               msg = "multicores must be at least 1")

    # precondition 5 - memsize
    .check_num(x = memsize,
               len_max = 1,
               min = 1,
               allow_zero = FALSE,
               msg = "memsize must be positive")

    # precondition 6 - output dir
    .check_file(x = output_dir,
                msg = "invalid output dir")

    # precondition 7 - version
    .check_chr(x = version,
               len_min = 1,
               msg = "invalid version")

    # create output window
    gauss_kernel <- .sits_smooth_gauss_kernel(window_size = window_size,
                                              sigma = sigma)

    # retrieve the scale factor
    scale_factor <- round(1 / .config_get("probs_cube_scale_factor"))
    mult_factor <- 1 / scale_factor

    # Gaussian smoother to be executed by workers cluster
    .do_bilateral <- function(chunk) {

        # scale probabilities
        data <- .raster_get_values(r_obj = chunk) * scale_factor

        # process bilateral smoother
        data <- bilateral_smoother(m = data,
                                   m_nrow = .raster_nrows(chunk),
                                   m_ncol = .raster_ncols(chunk),
                                   w = gauss_kernel,
                                   tau = tau)

        # create cube smooth
        res <- .raster_rast(r_obj = chunk,
                            nlayers = .raster_nlayers(chunk))

        # copy values
        res <- .raster_set_values(r_obj = res,
                                  values = data * mult_factor)

        return(res)
    }

    # process each brick layer (each time step) individually
    cube_bilat <- slider::slide_dfr(cube, function(tile) {

        # create metadata for bilateral smoothed raster cube
        tile_bilat <- .cube_derived_create(
            cube       = tile,
            cube_class = "probs_cube",
            band_name  = "bilat",
            labels     = .cube_labels(tile),
            start_date = .file_info_start_date(tile),
            end_date   = .file_info_end_date(tile),
            bbox       = .cube_tile_bbox(tile),
            output_dir = output_dir,
            version    = version
        )

        .sits_smooth_map_layer(
            cube = tile,
            cube_out = tile_bilat,
            overlapping_y_size =
                ceiling(window_size / 2) - 1,
            func = .do_bilateral,
            multicores = multicores,
            memsize = memsize,
            gdal_datatype = .raster_gdal_datatype(.config_get("probs_cube_data_type")),
            gdal_options = .config_gtiff_default_options()
        )

        return(tile_bilat)
    })

    class(cube_bilat) <- class(cube)

    return(cube_bilat)
}


#' @title Estimate the number of blocks to run .sits_split_cluster
#' @name .smth_estimate_block_size
#' @keywords internal
#'
#' @param cube         input data cube
#' @param multicores   number of processes to split up the data
#' @param memsize      maximum overall memory size (in GB)
#'
#' @return  returns a list with following information:
#'             - multicores theoretical upper bound;
#'             - block x_size (horizontal) and y_size (vertical)
#'
.smth_estimate_block_size <- function(cube, multicores, memsize) {

    # set caller to show in errors
    .check_set_caller(".smth_estimate_block_size")

    # precondition 1 - check if cube has probability data
    .check_that(
        x = inherits(cube, "probs_cube"),
        msg = "input is not probability cube"
    )
    size <- .cube_size(cube)
    n_layers <- length(cube$labels[[1]])
    bloat_mem <- .config_processing_bloat()
    n_bytes <- 8

    # total memory needed to do all work in GB
    needed_memory <-  1E-09 * size[["ncols"]] * size[["nrows"]] * n_layers * bloat_mem * n_bytes

    # minimum block size
    min_block_x_size <- size["ncols"] # for now, only vertical blocking
    min_block_y_size <- 1

    # compute factors
    memory_factor <- needed_memory / memsize
    blocking_factor <- size[["ncols"]] / min_block_x_size * size[["nrows"]] / min_block_y_size

    # stop if blocking factor is less than memory factor!
    # reason: the provided memory is not enough to process the data by
    # breaking it into small chunks
    .check_that(
        x = memory_factor <= blocking_factor,
        msg = "provided memory not enough to run the job"
    )

    # update multicores to the maximum possible processes given the available
    # memory and blocking factor
    multicores <- min(floor(blocking_factor / memory_factor), multicores)

    # compute blocking allocation that maximizes the
    # block / (memory * multicores) ratio, i.e. maximize parallel processes
    # and returns the following information:
    # - multicores theoretical upper bound;
    # - block x_size (horizontal) and y_size (vertical)
    blocks <- list(
        # theoretical max_multicores = floor(blocking_factor / memory_factor),
        block_x_size = floor(min_block_x_size),
        block_y_size = min(floor(blocking_factor / memory_factor / multicores),
                           size[["nrows"]])
    )

    return(blocks)
}


# function to compute blocks grid
.smth_compute_blocks <- function(img_y_size,
                                 block_y_size,
                                 overlapping_y_size) {

    r1 <- ceiling(seq(1, img_y_size - 1, by = block_y_size))
    r2 <- c(r1[-1] - 1, img_y_size)
    ovr_r1 <- c(1, c(r1[-1] - overlapping_y_size))
    ovr_r2 <- c(r2[-length(r2)] + overlapping_y_size, img_y_size)

    # define each block as a list element
    mapply(list,
           r1 = ovr_r1,
           r2 = ovr_r2,
           o1 = r1 - ovr_r1 + 1,
           o2 = r2 - ovr_r1 + 1,
           SIMPLIFY = FALSE)
}


# function that run on workers...
.smth_worker_fun <- function(block, in_file, func, args) {

    # open brick
    b <- .raster_open_rast(in_file)

    # create extent
    blk_overlap <- list(first_row = block$r1,
                        nrows = block$r2 - block$r1 + 1,
                        first_col = 1,
                        ncols = .raster_ncols(b))

    # crop adding overlaps
    chunk <- .raster_crop(r_obj = b, block = blk_overlap)

    # process it
    raster_out <- do.call(func, args = c(list(chunk = chunk), args))

    # create extent
    blk_no_overlap <- list(first_row = block$o1,
                           nrows = block$o2 - block$o1 + 1,
                           first_col = 1,
                           ncols = .raster_ncols(raster_out))

    # crop removing overlaps
    raster_out <- .raster_crop(raster_out, block = blk_no_overlap)

    # export to temp file
    filename <- tempfile(tmpdir = dirname(.file_info(cube_out)$path),
                         fileext = ".tif")

    # save chunk
    .raster_write_rast(
        r_obj = raster_out,
        file = filename,
        format = "GTiff",
        data_type = .raster_data_type(.config_get("probs_cube_data_type")),
        gdal_options = .config_gtiff_default_options(),
        overwrite = TRUE
    )

    return(filename)
}

#' @title Parallel processing of classified images
#' @name .sits_smooth_map_layer
#' @keywords internal
#'
#' @description Process chunks of raster bricks individually in parallel.
#'
#' @param cube              Probability data cube tile
#' @param cube_out          Output probability data cube
#' @param overlapping_rows  number of overlapping rows of each chunk.
#' @param func              a function that receives RasterBrick and
#'                          returns any Raster*.
#' @param args              additional arguments to pass to \code{fun} function.
#' @param multicores        Number of process to run the Bayesian smoothing in
#'                          snow subprocess.
#' @param memsize           Maximum overall memory (in GB) to run the Bayesian
#'                          smoothing.
#' @param ...               optional arguments to merge final raster
#'
#' @return  Output data cube
#'
.smth_map_layer <- function(cube,
                            cube_out,
                            overlapping_y_size = 0,
                            func,
                            func_args = NULL,
                            multicores = 1,
                            memsize = 1,
                            gdal_datatype,
                            gdal_options, ...) {

    # set caller to show in errors
    .check_set_caller(".sits_smooth_map_layer")

    # pre-condition - one tile at a time
    .check_num(nrow(cube), min = 1, max = 1, is_integer = TRUE,
               msg = "process one tile only")

    # precondition - check if cube has probability data
    .check_that(
        x = inherits(cube, "probs_cube"),
        msg = "input is not probability cube"
    )

    # precondition - overlapping rows must be non negative
    .check_num(
        x = overlapping_y_size,
        min = 0,
        msg = "overlaping rows must be non negative"
    )

    # open probability file
    in_file <- .file_info_path(cube)

    # retrieve the file to be written
    out_file <- .file_info_path(cube_out)

    # compute how many tiles to be computed
    block_size <- .sits_smooth_blocks_size_estimate(cube = cube,
                                                    multicores = multicores,
                                                    memsize = memsize)

    # for now, only vertical blocks are allowed, i.e. 'x_blocks' is 1
    blocks <- .sits_compute_blocks(
        img_y_size = .cube_size(cube)["nrows"],
        block_y_size = block_size[["block_y_size"]],
        overlapping_y_size = overlapping_y_size)

    # process blocks in parallel
    block_files_lst <- .sits_parallel_map(blocks, function(block, in_file, func, args) {

        # open brick
        b <- .raster_open_rast(in_file)

        # create extent
        blk_overlap <- list(first_row = block$r1,
                            nrows = block$r2 - block$r1 + 1,
                            first_col = 1,
                            ncols = .raster_ncols(b))

        # crop adding overlaps
        chunk <- .raster_crop(r_obj = b, block = blk_overlap)

        # process it
        raster_out <- do.call(func, args = c(list(chunk = chunk), args))

        # create extent
        blk_no_overlap <- list(first_row = block$o1,
                               nrows = block$o2 - block$o1 + 1,
                               first_col = 1,
                               ncols = .raster_ncols(raster_out))

        # crop removing overlaps
        raster_out <- .raster_crop(raster_out, block = blk_no_overlap)

        # export to temp file
        block_file <- tempfile(tmpdir = dirname(.file_info(cube_out)$path),
                               fileext = ".tif")

        # save chunk
        .raster_write_rast(
            r_obj = raster_out,
            file = block_file,
            format = "GTiff",
            data_type = .raster_data_type(.config_get("probs_cube_data_type")),
            gdal_options = .config_gtiff_default_options(),
            overwrite = TRUE
        )

        return(block_file)
    })

    block_files <- unlist(block_files_lst)

    return(invisible(cube_out))
}
