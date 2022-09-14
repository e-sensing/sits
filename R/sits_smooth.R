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
#'                           (for bilateral smoothing)
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
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#' @examples
#' if (sits_run_examples()) {
#'     # select a set of samples
#'     samples_ndvi <- sits_select(samples_modis_4bands, bands = c("NDVI"))
#'     # create a ResNet model
#'     torch_model <- sits_train(samples_ndvi, sits_resnet())
#'     # plot the model
#'     plot(torch_model)
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6",
#'         data_dir = data_dir,
#'         delim = "_",
#'         parse_info = c("X1", "tile", "band", "date")
#'     )
#'     # classify a data cube
#'     probs_cube <- sits_classify(data = cube, ml_model = torch_model)
#'     # plot the probability cube
#'     plot(probs_cube)
#'     # smooth the probability cube using Bayesian statistics
#'     bayes_cube <- sits_smooth(probs_cube)
#'     # plot the smoothed cube
#'     plot(bayes_cube)
#'     # label the probability cube
#'     label_cube <- sits_label_classification(bayes_cube)
#'     # plot the labelled cube
#'     plot(label_cube)
#' }
#' @export
#'
sits_smooth <- function(cube, type = "bayes", ...) {

    # set caller to show in errors
    .check_set_caller("sits_smooth")

    .check_require_packages("parallel")

    # check if cube has probability data
    .check_is_probs_cube(cube)

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

    # precondition - test window size
    .check_window_size(window_size)
    # precondition - covar
    .check_lgl_type(
        x = covar
    )
    # precondition - multicores
    .check_multicores(multicores)
    # precondition - memsize
    .check_memsize(memsize)
    # precondition - output dir
    .check_output_dir(output_dir)
    # precondition - version
    .check_version(version)
    # find out how many labels exist
    nlabels <- length(.tile_labels(.tile(cube)))
    # precondition - smoothness
    .check_smoothness(smoothness, nlabels)

    if (!is.matrix(smoothness)) {
        smoothness <- diag(smoothness, nrow = nlabels, ncol = nlabels)
    }

    # create a window
    window <- matrix(1, nrow = window_size, ncol = window_size)

    # Get job size
    job_size <- .raster_file_blocksize(
        .raster_open_rast(.fi_path(.fi(.tile(cube))))
    )
    # Check minimum memory needed to process one block
    overlap <- ceiling(window_size / 2) - 1
    job_memsize <- .jobs_memsize(
        job_size = job_size, npaths = length(.tile_labels(.tile(cube))),
        nbytes = 8, proc_bloat = .config_processing_bloat(), overlap = overlap
    )

    # Check if memsize is above minimum needed to process one block
    .check_that(
        x = job_memsize < memsize,
        local_msg = paste("minimum memsize needed is", job_memsize, "GB"),
        msg = "provided 'memsize' is insufficient for processing"
    )
    # Update multicores parameter
    multicores <- .jobs_max_multicores(
        job_memsize = job_memsize, memsize = memsize, multicores = multicores
    )

    # Prepare parallel processing
    .sits_parallel_start(workers = multicores, log = FALSE)
    on.exit(.sits_parallel_stop(), add = TRUE)

    # Smoothing
    # Process each tile sequentially
    probs_cube <- .cube_foreach_tile(cube, function(tile) {

        # Smooth the data
        probs_tile <- .sits_smooth_tile(
            tile = tile, overlap = overlap, smoothness = smoothness,
            covar = covar, window = window, smooth_fn = .smooth_bayes, memsize = memsize,
            multicores = multicores, output_dir = output_dir, version = version
        )

        return(probs_tile)
    })

    return(probs_cube)
}

.sits_smooth_tile <- function(tile,
                              overlap,
                              smoothness,
                              covar,
                              window,
                              smooth_fn,
                              memsize,
                              multicores,
                              output_dir,
                              version) {

    # Output file
    out_file <- .file_derived_name(
        tile = tile, band = "bayes", version = version,
        output_dir = output_dir
    )
    # Resume feature
    if (file.exists(out_file)) {
        # # Callback final tile classification
        # .callback(process = "tile_classification", event = "recovery",
        #           context = environment())
        message("Recovery: tile '", tile[["tile"]], "' already exists.")
        message("(If you want to produce a new image, please ",
                "change 'output_dir' or 'version' parameters)")
        probs_tile <- .tile_probs_from_file(
            file = out_file,
            band = "bayes",
            base_tile = tile,
            labels = .tile_labels(tile)
        )
        return(probs_tile)
    }
    # Create jobs
    # Get job size
    job_size <- .raster_file_blocksize(
        .raster_open_rast(.fi_path(.fi(tile)))
    )
    # Compute how many jobs to process
    jobs <- .jobs_create(
        job_size = job_size, block_overlap = overlap,
        ncols = .tile_ncols(tile), nrows = .tile_nrows(tile),
        xmin = .xmin(tile), xmax = .xmax(tile),
        ymin = .ymin(tile), ymax = .ymax(tile),
        crs = .crs(tile)
    )
    # Process jobs in parallel
    block_files <- .jobs_parallel_chr(jobs, function(job) {
        # Get job block
        block <- .block(job)
        # Output overlap file name
        overlap_block_file <- .file_block_name(
            pattern = .file_pattern(file = out_file, suffix = "_overlap"),
            block = block,
            output_dir = output_dir
        )
        # Output file name
        block_file <- .file_block_name(
            pattern = .file_pattern(file = out_file),
            block = block,
            output_dir = output_dir
        )
        # Resume processing in case of failure
        if (file.exists(block_file)) {
            unlink(overlap_block_file)
            # Try to open the file
            r_obj <- .try({
                .raster_open_rast(block_file)
            },
            .default = {
                unlink(block_file)
                NULL
            })
            # If file can be opened, check if the result is correct
            # this file will not be processed again
            if (!is.null(r_obj)) {
                # Verify if the raster is corrupted
                valid_block <- .try({
                    .raster_get_values(r_obj)
                    # Return value
                    TRUE
                },
                .default = {
                    unlink(block_file)
                    # Return value
                    FALSE
                })
                if (valid_block) {
                    return(block_file)
                }
            }
        }

        # Read and preprocess values
        values <- .sits_derived_data_read(
            tile = tile, band = .tile_bands(tile), block = block
        )
        # Avoid memory bloat
        original_nrows <- nrow(values)

        # Apply the labeling function to values
        values <- smooth_fn(
            values,
            block,
            window,
            smoothness,
            covar
        )

        # Are the results consistent with the data input?
        .check_that(
            x = nrow(values) == original_nrows,
            msg = paste(
                "number of rows of class matrix is different",
                "from number of input pixels"
            )
        )
        # Prepare probability to be saved
        conf_band <- .conf_derived_band(
            derived_class = "probs_cube", band = "bayes"
        )
        offset <- .band_offset(conf_band)
        if (!is.null(offset) && offset != 0) {
            values <- values - offset
        }
        scale <- .band_scale(conf_band)
        if (!is.null(scale) && scale != 1) {
            values <- values / scale
        }

        # Prepare and save results as raster
        .raster_write_block(
            file = overlap_block_file,
            block = block,
            bbox = .bbox(job),
            values = values,
            data_type = .band_data_type(conf_band)
        )

        # Create extent to crop overlaps
        blk_no_overlap <- .jobs_remove_overlap(job)

        # Save chunk
        # Crop removing overlaps
        .raster_crop(
            r_obj = .raster_open_rast(overlap_block_file),
            file = block_file,
            data_type = .raster_data_type(
                .config_get("probs_cube_data_type")
            ),
            overwrite = TRUE,
            block = blk_no_overlap
        )

        gc()
        on.exit(unlink(overlap_block_file), add = TRUE)
        return(block_file)
    })

    # Merge into template
    .raster_merge_blocks(
        base_file = .file_info_path(tile),
        block_files = block_files,
        out_file = out_file,
        data_type = .config_get("probs_cube_data_type"),
        missing_value = .config_get("probs_cube_missing_value"),
        multicores = multicores
    )
    on.exit(unlink(block_files), add = TRUE)
    return(invisible(block_files))
}

.smooth_bayes <- function(data, block, window, smoothness, covar) {

    # compute logit
    logit <- log(data / (rowSums(data) - data))

    # process Bayesian
    data <- bayes_smoother(
        m = logit,
        m_nrow = block[["nrows"]],
        m_ncol = block[["ncols"]],
        w = window,
        sigma = smoothness,
        covar_sigma0 = covar
    )

    # calculate the Bayesian probability for the pixel
    data <- exp(data) / (exp(data) + 1)

    return(data)
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

    # precondition - window size
    .check_window_size(window_size)
    # prediction - variance
    .check_num_parameter(sigma, exclusive_min = 0)
    # prediction - tau
    .check_num_parameter(tau, exclusive_min = 0)
    # precondition - multicores
    .check_multicores(multicores)
    # precondition - memsize
    .check_memsize(memsize)
    # precondition - output dir
    .check_output_dir(output_dir)
    # precondition - version
    .check_version(version)

    # calculate gauss kernel
    gauss_kernel <- function(window_size, sigma) {

        w_center <- ceiling(window_size / 2)
        w_seq <- seq_len(window_size)
        x <- stats::dnorm(
            (abs(rep(w_seq, each = window_size) - w_center)^2 +
                 abs(rep(w_seq, window_size) - w_center)^2)^(1 / 2),
            sd = sigma
        ) / stats::dnorm(0)
        matrix(x / sum(x), nrow = window_size, byrow = TRUE)
    }

    gs_matrix <- gauss_kernel(window_size, sigma)

    # retrieve the scale factor
    scale_factor <- .config_get("probs_cube_scale_factor")
    mult_factor <- round(1 / scale_factor)

    # Gaussian smoother to be executed by workers cluster
    .do_bilateral <- function(chunk) {

        # scale probabilities
        data <- .raster_get_values(r_obj = chunk) * scale_factor

        # process bilateral smoother
        data <- bilateral_smoother(
            m = data,
            m_nrow = .raster_nrows(chunk),
            m_ncol = .raster_ncols(chunk),
            w = gs_matrix,
            tau = tau
        )

        # create cube smooth
        res <- .raster_rast(
            r_obj = chunk,
            nlayers = .raster_nlayers(chunk)
        )

        # copy values
        res <- .raster_set_values(
            r_obj = res,
            values = data * mult_factor
        )

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
    blocks_tile_lst <- slider::slide(cube, function(tile) {

        # create metadata for raster cube
        tile_new <- .cube_derived_create(
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

        # prepare output filename
        out_file <- .file_info_path(tile_new)

        # if file exists skip it (resume feature)
        if (file.exists(out_file)) {

            if (.cube_is_equal_bbox(tile_new)) {
                message(paste0(
                    "Recovery mode: smoothed image file found in '",
                    dirname(out_file), "' directory. ",
                    "(If you want a new smoothing, please ",
                    "change the directory in the 'output_dir' or the ",
                    "value of 'version' parameter)"
                ))
                return(NULL)
            }
        }

        # overlapping pixels
        overlapping_y_size <- ceiling(window_size / 2) - 1

        # get cube size
        size <- .cube_size(tile)

        # for now, only vertical blocks are allowed, i.e. 'x_blocks' is 1
        blocks <- .smth_compute_blocks(
            xsize = size[["ncols"]],
            ysize = size[["nrows"]],
            block_y_size = block_size[["block_y_size"]],
            overlapping_y_size = overlapping_y_size
        )

        # open probability file
        in_file <- .file_info_path(tile)

        # process blocks in parallel
        block_files_lst <- .sits_parallel_map(blocks, function(block) {

            # open brick
            b <- .raster_open_rast(in_file)

            # crop adding overlaps
            temp_chunk_file <- .create_chunk_file(
                output_dir = output_dir,
                pattern = "chunk_bilat_overlap_",
                ext = ".tif"
            )
            chunk <- .raster_crop(
                r_obj = b,
                file = temp_chunk_file,
                data_type = .raster_data_type(
                    .config_get("probs_cube_data_type")
                ),
                overwrite = TRUE,
                block = block
            )
            # Delete temp file
            on.exit(unlink(temp_chunk_file), add = TRUE)

            # process it
            raster_out <- .do_bilateral(chunk = chunk)

            # Create extent
            blk_no_overlap <- list(
                col = block$crop_col,
                row = block$crop_row,
                ncols = block$crop_ncols,
                nrows = block$crop_nrows
            )

            block_file <- .smth_filename(
                tile = tile_new,
                output_dir = output_dir,
                block = block
            )

            # Save chunk
            .raster_crop(
                r_obj = raster_out,
                file = block_file,
                data_type = .raster_data_type(
                    .config_get("probs_cube_data_type")
                ),
                overwrite = TRUE,
                block = blk_no_overlap
            )

            return(block_file)
        })

        block_files <- unlist(block_files_lst)

        return(invisible(block_files))
    })


    # process each brick layer (each time step) individually
    result_cube <- .sits_parallel_map(seq_along(blocks_tile_lst), function(i) {

        # get tile from cube
        tile <- cube[i, ]

        # create metadata for raster cube
        tile_new <- .cube_derived_create(
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

        # prepare output filename
        out_file <- .file_info_path(tile_new)

        # if file exists skip it (resume feature)
        if (file.exists(out_file)) {
            return(tile_new)
        }

        blocks_path <- blocks_tile_lst[[i]]

        # Join predictions
        if (is.null(blocks_path)) {
            return(NULL)
        }

        # Merge into template
        .raster_merge_blocks(
            base_file = .file_info_path(tile),
            block_files = blocks_path,
            out_file = out_file,
            data_type = .config_get("probs_cube_data_type"),
            missing_value = .config_get("probs_cube_missing_value"),
            multicores = 1
        )

        # Remove blocks
        on.exit(unlink(blocks_path), add = TRUE)

        return(tile_new)
    })

    # bind rows
    result_cube <- dplyr::bind_rows(result_cube)

    class(result_cube) <- class(cube)

    return(result_cube)
}
