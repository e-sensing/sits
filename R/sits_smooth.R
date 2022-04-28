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
#'
#' @export
#'
sits_smooth <- function(cube, type = "bayes", ...) {

    # set caller to show in errors
    .check_set_caller("sits_smooth")

    .check_require_packages("parallel")

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
            msg = paste(
                "smoothness must be square matrix of",
                "the same length as the number of labels"
            )
        )
    } else {
        .check_num(
            x = smoothness,
            min = 1,
            len_max = 1,
            allow_zero = FALSE,
            msg = "smoothness must be greater than 1"
        )
        smoothness <- diag(smoothness, nrow = n_labels, ncol = n_labels)
    }

    # precondition 4 - multicores
    .check_num(
        x = multicores,
        len_max = 1,
        min = 1,
        allow_zero = FALSE,
        msg = "multicores must be at least 1"
    )

    # precondition 5 - memory
    .check_num(
        x = memsize,
        len_max = 1,
        min = 1,
        allow_zero = FALSE,
        msg = "memsize must be positive"
    )

    # precondition 6 - output dir
    .check_file(
        x = output_dir,
        msg = "invalid output dir"
    )

    # precondition 7 - version
    .check_chr(
        x = version,
        len_min = 1,
        msg = "invalid version"
    )

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
        data <- bayes_smoother(
            m = logit,
            m_nrow = .raster_nrows(chunk),
            m_ncol = .raster_ncols(chunk),
            w = window,
            sigma = smoothness,
            covar_sigma0 = covar
        )

        # calculate the Bayesian probability for the pixel
        data <- exp(data) * mult_factor / (exp(data) + 1)

        # create cube smooth
        res <- .raster_rast(
            r_obj = chunk,
            nlayers = .raster_nlayers(chunk)
        )

        # copy values
        res <- .raster_set_values(
            r_obj = res,
            values = data
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
            band_name  = "bayes",
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
        if (file.exists(out_file))
            return(NULL)

        # overlapping pixels
        overlapping_y_size <- ceiling(window_size / 2) - 1

        # get cube size
        size <- .cube_size(tile)

        # for now, only vertical blocks are allowed, i.e. 'x_blocks' is 1
        blocks <- .smth_compute_blocks(
            xsize = size[["ncols"]],
            ysize = size[["nrows"]],
            block_y_size = block_size[["block_y_size"]],
            overlapping_y_size = overlapping_y_size)

        # open probability file
        in_file <- .file_info_path(tile)

        # process blocks in parallel
        block_files_lst <- .sits_parallel_map(blocks, function(block) {

            # open brick
            b <- .raster_open_rast(in_file)

            # crop adding overlaps
            chunk <- .raster_crop(r_obj = b, block = block)

            # process it
            raster_out <- .do_bayes(chunk = chunk)

            # create extent
            blk_no_overlap <- list(first_row = block$crop_first_row,
                                   nrows = block$crop_nrows,
                                   first_col = block$crop_first_col,
                                   ncols = block$crop_ncols)

            # crop removing overlaps
            raster_out <- .raster_crop(raster_out, block = blk_no_overlap)
            block_file <- .smth_filename(tile = tile_new,
                                         output_dir = output_dir,
                                         block = block)
            # save chunk
            .raster_write_rast(
                r_obj = raster_out,
                file = block_file,
                format = "GTiff",
                data_type = .raster_data_type(
                    .config_get("probs_cube_data_type")
                ),
                gdal_options = .config_gtiff_default_options(),
                overwrite = TRUE
            )

            return(block_file)
        })

        block_files <- unlist(block_files_lst)

        return(invisible(block_files))
    })


    # process each brick layer (each time step) individually
    result_cube <- .sits_parallel_map(seq_along(blocks_tile_lst), function(i) {

        # get tile from cube
        tile <- cube[i,]

        # create metadata for raster cube
        tile_new <- .cube_derived_create(
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
        out_file <- .file_info_path(tile_new)

        # if file exists skip it (resume feature)
        if (file.exists(out_file))
            return(tile_new)

        tmp_blocks <- blocks_tile_lst[[i]]

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

        return(tile_new)
    })

    # bind rows
    result_cube <- dplyr::bind_rows(result_cube)

    class(result_cube) <- class(cube)

    return(result_cube)
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
    .check_num(
        x = sigma,
        len_max = 1,
        min = 1,
        allow_zero = FALSE,
        msg = "sigma must be positive"
    )

    # precondition 4 - multicores
    .check_num(
        x = multicores,
        len_max = 1,
        min = 1,
        allow_zero = FALSE,
        msg = "multicores must be at least 1"
    )

    # precondition 5 - memsize
    .check_num(
        x = memsize,
        len_max = 1,
        min = 1,
        allow_zero = FALSE,
        msg = "memsize must be positive"
    )

    # precondition 6 - output dir
    .check_file(
        x = output_dir,
        msg = "invalid output dir"
    )

    # precondition 7 - version
    .check_chr(
        x = version,
        len_min = 1,
        msg = "invalid version"
    )

    # calculate gauss kernel
    gauss_kernel <- function(window_size, sigma) {

        stopifnot(window_size %% 2 != 0)

        w_center <- ceiling(window_size / 2)
        w_seq <- seq_len(window_size)
        x <- stats::dnorm(
            (abs(rep(w_seq, each = window_size) - w_center) ^ 2 +
                 abs(rep(w_seq, window_size) - w_center) ^ 2) ^ (1 / 2),
            sd = sigma) / stats::dnorm(0)
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
        if (file.exists(out_file))
            return(NULL)

        # overlapping pixels
        overlapping_y_size <- ceiling(window_size / 2) - 1

        # get cube size
        size <- .cube_size(tile)

        # for now, only vertical blocks are allowed, i.e. 'x_blocks' is 1
        blocks <- .smth_compute_blocks(
            xsize = size[["ncols"]],
            ysize = size[["nrows"]],
            block_y_size = block_size[["block_y_size"]],
            overlapping_y_size = overlapping_y_size)

        # open probability file
        in_file <- .file_info_path(tile)

        # process blocks in parallel
        block_files_lst <- .sits_parallel_map(blocks, function(block) {

            # open brick
            b <- .raster_open_rast(in_file)

            # crop adding overlaps
            chunk <- .raster_crop(r_obj = b, block = block)

            # process it
            raster_out <- .do_bilateral(chunk = chunk)

            # create extent
            blk_no_overlap <- list(first_row = block$crop_first_row,
                                   nrows = block$crop_nrows,
                                   first_col = block$crop_first_col,
                                   ncols = block$crop_ncols)

            # crop removing overlaps
            raster_out <- .raster_crop(raster_out, block = blk_no_overlap)
            block_file <- .smth_filename(tile = tile_new,
                                         output_dir = output_dir,
                                         block = block)

            # save chunk
            .raster_write_rast(
                r_obj = raster_out,
                file = block_file,
                format = "GTiff",
                data_type = .raster_data_type(
                    .config_get("probs_cube_data_type")
                ),
                gdal_options = .config_gtiff_default_options(),
                overwrite = TRUE
            )

            return(block_file)
        })

        block_files <- unlist(block_files_lst)

        return(invisible(block_files))
    })


    # process each brick layer (each time step) individually
    result_cube <- .sits_parallel_map(seq_along(blocks_tile_lst), function(i) {

        # get tile from cube
        tile <- cube[i,]

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
        if (file.exists(out_file))
            return(tile_new)

        tmp_blocks <- blocks_tile_lst[[i]]

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

        return(tile_new)
    })

    # bind rows
    result_cube <- dplyr::bind_rows(result_cube)

    class(result_cube) <- class(cube)

    return(result_cube)
}
