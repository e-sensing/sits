#' @title Smooth probability cubes with spatial predictors
#'
#' @name  sits_smooth
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Takes a set of classified raster layers with probabilities,
#'              whose metadata is]created by \code{\link[sits]{sits_cube}},
#'              and applies a smoothing function. There are two options,
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
#' @param  window_size       Size of the neighborhood.
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
#'     # create a ResNet model
#'     torch_model <- sits_train(samples_modis_ndvi, sits_resnet(epochs = 20))
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
sits_smooth <- function(cube,
                        type = "bayes",
                        ...,
                        window_size = 9,
                        memsize = 4,
                        multicores = 2,
                        output_dir = getwd(),
                        version = "v1",
                        progress = TRUE) {

    # Check if cube has probability data
    .check_is_probs_cube(cube)
    # Check memsize
    .check_memsize(memsize)
    # Check multicores
    .check_multicores(multicores)
    # Check output dir
    output_dir <- path.expand(output_dir)
    .check_output_dir(output_dir)
    # Check version
    .check_version(version)

    # Check memory and multicores
    # Get block size
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(cube)))
    # Overlapping pixels
    overlap <- ceiling(window_size / 2) - 1
    # Check minimum memory needed to process one block
    job_memsize <- .jobs_memsize(
        job_size = .block_size(block = block, overlap = overlap),
        # npaths = input(nlayers) + output(nlayers)
        npaths = length(.tile_labels(cube)) * 2,
        nbytes = 8,
        proc_bloat = .conf("processing_bloat")
    )
    # Update multicores parameter
    multicores <- .jobs_max_multicores(
        job_memsize = job_memsize,
        memsize = memsize,
        multicores = multicores
    )
    # Update block parameter
    block <- .jobs_optimal_block(
        job_memsize = job_memsize,
        block = block,
        image_size = .tile_size(.tile(cube)),
        memsize = memsize,
        multicores = multicores
    )
    # Prepare parallel processing
    .sits_parallel_start(workers = multicores, log = FALSE)
    on.exit(.sits_parallel_stop(), add = TRUE)

    # Call the smooth method
    .smooth_use_method(
        cube = cube,
        type = type,
        block = block,
        window_size = window_size,
        memsize = memsize,
        multicores = multicores,
        output_dir = output_dir,
        version = version, ...
    )
}

#---- internal functions ----

.smooth_tile <- function(tile, band, overlap, smooth_fn, output_dir,
                         version, progress) {
    # Output file
    out_file <- .file_derived_name(
        tile = tile, band = band, version = version,
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
            file = out_file, band = band, base_tile = tile,
            labels = .tile_labels(tile), update_bbox = FALSE
        )
        return(probs_tile)
    }
    # Create chunks as jobs
    chunks <- .tile_chunks_create(tile = tile, overlap = overlap)
    # Process jobs in parallel
    block_files <- .jobs_map_parallel_chr(chunks, function(chunk) {
        # Job block
        block <- .block(chunk)
        # Block file name
        block_file <- .file_block_name(
            pattern = .file_pattern(out_file), block = block,
            output_dir = output_dir
        )
        # Resume processing in case of failure
        if (.raster_is_valid(block_file)) {
            return(block_file)
        }
        # Read and preprocess values
        values <- .tile_read_block(
            tile = tile, band = .tile_bands(tile), block = block
        )
        # Apply the probability function to values
        values <- smooth_fn(values = values, block = block)
        # Prepare probability to be saved
        band_conf <- .conf_derived_band(
            derived_class = "probs_cube", band = band
        )
        offset <- .offset(band_conf)
        if (.has(offset) && offset != 0) {
            values <- values - offset
        }
        scale <- .scale(band_conf)
        if (.has(scale) && scale != 1) {
            values <- values / scale
        }
        # Job crop block
        crop_block <- .block(.chunks_no_overlap(chunk))
        # Prepare and save results as raster
        .raster_write_block(
            files = block_file, block = block, bbox = .bbox(chunk),
            values = values, data_type = .data_type(band_conf),
            missing_value = .miss_value(band_conf),
            crop_block = crop_block
        )
        # Free memory
        gc()
        # Return block file
        block_file
    }, progress = progress)
    # Merge blocks into a new probs_cube tile
    probs_tile <- .tile_probs_merge_blocks(
        file = out_file, band = band, labels = .tile_labels(tile),
        base_tile = tile, block_files = block_files,
        multicores = .jobs_multicores(), update_bbox = FALSE
    )
    # Return probs tile
    probs_tile
}

#---- smooth functions ----

.smooth_fn_bayes <- function(window_size,
                             smoothness,
                             covar,
                             nlabels) {
    # Check window size
    .check_window_size(window_size, min = 7)
    # Check covar
    .check_lgl_type(covar)
    # Prepare smoothness parameter
    if (!is.matrix(smoothness)) {
        smoothness <- diag(smoothness, nrow = nlabels, ncol = nlabels)
    }
    # Check smoothness
    .check_smoothness_mat(smoothness, nlabels)
    # Create a window
    window <- matrix(1, nrow = window_size, ncol = window_size)

    # Define smooth function
    smooth_fn <- function(values, block) {
        # Check values length
        input_pixels <- nrow(values)
        # Compute logit
        values <- log(values / (rowSums(values) - values))
        # Process Bayesian
        values <- bayes_smoother(
            m = values,
            m_nrow = .nrows(block),
            m_ncol = .ncols(block),
            w = window,
            sigma = smoothness,
            covar_sigma0 = covar
        )
        # Compute inverse logit
        values <- exp(values) / (exp(values) + 1)
        # Are the results consistent with the data input?
        .check_processed_values(values, input_pixels)
        # Return values
        values
    }
    # Return a closure
    smooth_fn
}
.smooth_fn_bilat <- function(window_size, sigma, tau) {
    # Check window size
    .check_window_size(window_size)
    # Check variance
    .check_num_parameter(sigma, exclusive_min = 0)
    # Check tau
    .check_num_parameter(tau, exclusive_min = 0)
    # Create a Gaussian window
    center <- ceiling(window_size / 2)
    seq <- seq_len(window_size)
    seq_h <- rep(seq, each = window_size)
    seq_v <- rep(seq, window_size)
    x <- stats::dnorm(
        x = sqrt((seq_h - center)^2 + (seq_v - center)^2), sd = sigma
    ) / stats::dnorm(0)
    # Normalize and convert to matrix
    window <- matrix(x / sum(x), nrow = window_size, byrow = TRUE)

    # Define smooth function
    smooth_fn <- function(values, block) {
        # Check values length
        input_pixels <- nrow(values)
        # Process bilateral smoother and return
        values <- bilateral_smoother(
            m = values, m_nrow = .nrows(block), m_ncol = .ncols(block),
            w = window, tau = tau
        )
        # Are the results consistent with the data input?
        .check_processed_values(values, input_pixels)
        # Return values
        values
    }
    # Return a closure
    smooth_fn
}

#' @rdname sits_smooth
#' @export
sits_smooth_variance <- function(cube,
                                 window_size = 5,
                                 multicores = 2,
                                 memsize = 4,
                                 output_dir = getwd(),
                                 version = "v1") {

    # Check if cube has probability data
    .check_is_probs_cube(cube)
    # Check memsize
    .check_memsize(memsize)
    # Check multicores
    .check_multicores(multicores)
    # Check memory and multicores
    # Get block size
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(cube)))
    # Overlapping pixels
    overlap <- ceiling(window_size / 2) - 1
    # Check minimum memory needed to process one block
    job_memsize <- .jobs_memsize(
        job_size = .block_size(block = block, overlap = overlap),
        # npaths = input(nlayers) + output(nlayers)
        npaths = length(.tile_labels(cube)) * 2,
        nbytes = 8,
        proc_bloat = .conf("processing_bloat")
    )
    # Update multicores parameter
    multicores <- .jobs_max_multicores(
        job_memsize = job_memsize, memsize = memsize, multicores = multicores
    )

    # Prepare parallel processing
    .sits_parallel_start(workers = multicores, log = FALSE)
    on.exit(.sits_parallel_stop(), add = TRUE)

    # Smooth parameters checked in smooth function creation
    # Create smooth function
    smooth_fn <- .smooth_fn_variance(window_size = window_size)
    # Overlapping pixels
    overlap <- ceiling(window_size / 2) - 1
    # Smoothing
    # Process each tile sequentially
    variance_cube <- .cube_foreach_tile(cube, function(tile) {
        # Smooth the data
        variance_tile <- .smooth_tile(
            tile = tile,
            band = "variance",
            overlap = overlap,
            smooth_fn = smooth_fn,
            output_dir = output_dir,
            version = version
        )
        return(variance_tile)
    })
    return(variance_cube)
}
.smooth_fn_variance <- function(window_size) {
    # Check window size
    .check_window_size(window_size)
    # Define smooth function
    smooth_fn <- function(values, block) {
        # Check values length
        input_pixels <- nrow(values)
        # Compute logit
        values <- log(values / (rowSums(values) - values))
        # Process Bayesian
        for (i in seq_len(ncol(values))) {
            values[,i] <- C_kernel_var(
                x = values,
                ncols = .ncols(block),
                nrows = .nrows(block),
                band = i - 1,
                window_size = window_size)
        }
        # Are the results consistent with the data input?
        .check_processed_values(values, input_pixels)
        # Return values
        values
    }
    # Return a closure
    smooth_fn
}
