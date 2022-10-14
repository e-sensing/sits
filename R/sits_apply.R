#' @title Apply a function on a set of time series
#'
#' @name sits_apply
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description
#' Apply a named expression to a sits cube or a sits tibble
#' to be evaluated and generate new bands (indices). In the case of sits
#' cubes, it materializes a new band in \code{output_dir} using
#' \code{gdalcubes}.
#'
#' @param data          Valid sits tibble or cube
#' @param window_size   An odd number representing the size of the
#'                      sliding window of sits kernel functions
#'                      used in expressions (for a list of supported
#'                      kernel functions, please see details).
#' @param memsize       Memory available for classification (in GB).
#' @param multicores    Number of cores to be used for classification.
#' @param output_dir    Directory where files will be saved.
#' @param progress      Show progress bar?
#' @param ...           Named expressions to be evaluated (see details).
#'
#' @details
#' \code{sits_apply()} allow any valid R expression to compute new bands.
#' Use R syntax to pass an expression to this function.
#' Besides arithmetic operators, you can use virtually any R function
#' that can be applied to elements of a matrix (functions that are
#' unaware of matrix sizes, e.g. \code{sqrt()}, \code{sin()},
#' \code{log()}).
#'
#' Also, \code{sits_apply()} accepts a predefined set of kernel functions
#' (see below) that can be applied to pixels considering its
#' neighborhood. \code{sits_apply()} considers a neighborhood of a
#' pixel as a set of pixels equidistant to it (including itself)
#' according the Chebyshev distance. This neighborhood form a
#' square window (also known as kernel) around the central pixel
#' (Moore neighborhood). Users can set the \code{window_size}
#' parameter to adjust the size of the kernel window.
#' The image is conceptually mirrored at the edges so that neighborhood
#' including a pixel outside the image is equivalent to take the
#' 'mirrored' pixel inside the edge.
#'
#' \code{sits_apply()} applies a function to the kernel and its result
#' is assigned to a corresponding central pixel on a new matrix.
#' The kernel slides throughout the input image and this process
#' generates an entire new matrix, which is returned as a new band
#' to the cube. The kernel functions ignores any \code{NA} values
#' inside the kernel window. Central pixel is \code{NA} just only
#' all pixels in the window are \code{NA}.
#'
#' @section Summarizing kernel functions:
#' \itemize{
#' \item{\code{w_median()}: returns the median of the neighborhood's values.}
#' \item{\code{w_sum()}: returns the sum of the neighborhood's values.}
#' \item{\code{w_mean()}: returns the mean of the neighborhood's values.}
#' \item{\code{w_sd()}: returns the standard deviation of the neighborhood's
#'   values.}
#' \item{\code{w_var()}: returns the variance of the neighborhood's values.}
#' \item{\code{w_min()}: returns the minimum of the neighborhood's values.}
#' \item{\code{w_max()}: returns the maximum of the neighborhood's values.}
#' }
#'
#'
#' @return A sits tibble or a sits cube with new bands, produced
#'         according to the requested expression.
#'
#' @examples
#' if (sits_run_examples()) {
#' # Get a time series
#' # Apply a normalization function
#'
#' point2 <-
#'     sits_select(point_mt_6bands, "NDVI") %>%
#'     sits_apply(NDVI_norm = (NDVI - min(NDVI)) / (max(NDVI) - min(NDVI)))
#'
#' # Example of generation texture band with variance
#' # Create a data cube from local files
#' data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#' cube <- sits_cube(
#'     source = "BDC",
#'     collection = "MOD13Q1-6",
#'     data_dir = data_dir,
#'     delim = "_",
#'     parse_info = c("X1", "tile", "band", "date")
#' )
#'
#' # Generate a texture images with variance in NDVI images
#' cube_texture <- sits_apply(
#'     data = cube,
#'     NDVITEXTURE = w_var(NDVI),
#'     window_size = 5,
#'     output_dir = tempdir()
#' )
#' }
#' @rdname sits_apply
#' @export
sits_apply <- function(data, ...) {
    UseMethod("sits_apply", data)
}

#' @rdname sits_apply
#' @export
sits_apply.sits <- function(data, ...) {
    .check_set_caller("sits_apply.sits")

    .apply(data, col = "time_series", fn = dplyr::mutate, ...)
}

#' @rdname sits_apply
#' @export
sits_apply.raster_cube <- function(data, ..., window_size = 3, memsize = 1,
                                   multicores = 2, output_dir = getwd(),
                                   progress = TRUE) {

    # Check cube
    .check_is_sits_cube(data)
    .check_is_regular(data)
    # Check window size
    .check_window_size(window_size)
    # Check memsize
    .check_memsize(memsize)
    # Check multicores
    .check_multicores(multicores)
    # Check output_dir
    .check_output_dir(output_dir)

    # Get output band expression
    expr <- .apply_capture_expression(...)
    out_band <- names(expr)
    # Get all input bands in cube data
    in_bands <- .apply_input_bands(data, expr = expr)

    # Check memory and multicores
    # Get block size
    block <- .raster_file_blocksize(.raster_open_rast(.fi_path(.fi(data))))
    # Overlapping pixels
    overlap <- ceiling(window_size / 2) - 1
    # Check minimum memory needed to process one block
    # npaths = input(bands) + output(band)
    job_memsize <- .jobs_memsize(
        job_size = .block_size(block = block, overlap = overlap),
        npaths = length(in_bands) + 1,
        nbytes = 8, proc_bloat = .conf("processing_bloat")
    )
    # Update multicores parameter
    multicores <- .jobs_max_multicores(
        job_memsize = job_memsize, memsize = memsize, multicores = multicores
    )

    # Prepare parallelization
    .sits_parallel_start(workers = multicores, log = FALSE)
    on.exit(.sits_parallel_stop(), add = TRUE)

    # Create features as jobs
    features_cube <- .cube_split_features(data)

    # Process each feature in parallel
    features_band <- .jobs_map_parallel_dfr(features_cube, function(feature) {
        # Process the data
        output_feature <- .apply_feature(
            feature = feature,
            expr = expr,
            window_size = window_size,
            out_band = out_band,
            in_bands = in_bands,
            overlap = overlap,
            output_dir = output_dir
        )
        return(output_feature)
    }, progress = progress)
    # Join output features as a cube and return it
    .cube_merge_tiles(dplyr::bind_rows(list(features_cube, features_band)))
}
#' @title Apply a function to one band of a time series
#' @name .apply
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param  data      Tibble.
#' @param  col       Column where function should be applied
#' @param  fn        Function to be applied.
#' @return           Tibble where function has been applied.
#'
.apply <- function(data, col, fn, ...) {

    # pre-condition
    .check_chr_within(col,
                      within = names(data),
                      msg = "invalid column name"
    )
    # select data do unpack
    x <- data[col]
    # prepare to unpack
    x[["#.."]] <- seq_len(nrow(data))
    # unpack
    x <- tidyr::unnest(x, cols = dplyr::all_of(col))
    x <- dplyr::group_by(x, .data[["#.."]])
    # apply user function
    x <- fn(x, ...)
    # pack
    x <- dplyr::ungroup(x)
    x <- tidyr::nest(x, `..unnest_col` = -dplyr::any_of("#.."))
    # remove garbage
    x[["#.."]] <- NULL
    names(x) <- col
    # prepare result
    data[[col]] <- x[[col]]
    return(data)
}
.apply_feature <- function(feature, window_size, expr, out_band, in_bands,
                           overlap, output_dir) {
    # Output file
    out_file <- .file_eo_name(
        tile = feature, band = out_band,
        date = .tile_start_date(feature), output_dir = output_dir
    )
    # Resume feature
    if (.raster_is_valid(out_file)) {
        # # Callback final tile classification
        # .callback(process = "Apply", event = "recovery",
        #           context = environment())
        message("Recovery: band ",
                paste0("'", out_band, "'", collapse = ", "),
                " already exists.")
        message("(If you want to produce a new image, please ",
                "change 'output_dir' or 'version' parameters)")
        # Create tile based on template
        feature <- .tile_eo_from_files(
            files = out_file, fid = .fi_fid(.fi(feature)),
            bands = out_band, date = .tile_start_date(feature),
            base_tile = feature, update_bbox = FALSE
        )
        return(feature)
    }
    # Remove remaining incomplete fractions files
    unlink(out_file)
    # Create chunks as jobs
    chunks <- .tile_chunks_create(tile = feature, overlap = overlap)
    # Process jobs sequentially
    block_files <- .jobs_map_sequential(chunks, function(chunk) {
        # Get job block
        block <- .block(chunk)
        # Block file name for each fraction
        block_files <- .file_block_name(
            pattern = .file_pattern(out_file), block = block,
            output_dir = output_dir
        )
        # Resume processing in case of failure
        if (.raster_is_valid(block_files)) {
            return(block_files)
        }
        # Read bands data
        values <- .apply_data_read(
            tile = feature, block = block, in_bands = in_bands
        )
        # Evaluate expression here
        # Band and kernel evaluation
        values <- eval(
            expr = expr[[out_band]],
            envir = values,
            enclos = .kern_functions(
                window_size = window_size,
                img_nrow = block[["nrows"]],
                img_ncol = block[["ncols"]]
            )
        )
        # Prepare fractions to be saved
        band_conf <- .tile_band_conf(tile = feature, band = out_band)
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
            files = block_files, block = block, bbox = .bbox(chunk),
            values = values, data_type = .data_type(band_conf),
            missing_value = .miss_value(band_conf),
            crop_block = crop_block
        )
        # Free memory
        gc()
        # Returned block files for each fraction
        block_files
    })
    # Merge blocks into a new class_cube tile
    band_tile <- .tile_eo_merge_blocks(
        files = out_file, bands = out_band, base_tile = feature,
        block_files = block_files, multicores = 1, update_bbox = FALSE
    )
    # Return a feature tile
    band_tile
}

.apply_data_read <- function(tile, block, in_bands) {
    # for cubes that have a time limit to expire - mpc cubes only
    tile <- .cube_token_generator(tile)
    # Read and preprocess values from cloud
    # Get cloud values (NULL if not exists)
    cloud_mask <- .tile_cloud_read_block(tile = tile, block = block)
    # Read and preprocess values from each band
    values <- purrr::map_dfc(in_bands, function(band) {
        # Get band values
        values <- .tile_read_block(tile = tile, band = band, block = block)
        # Remove cloud masked pixels
        if (.has(cloud_mask)) {
            values[cloud_mask] <- NA
        }
        # Return values
        as.data.frame(values)
    })
    # Set columns name
    colnames(values) <- in_bands
    # Return values
    values
}

#' @title Apply an expression across all bands
#'
#' @name .apply_across
#' @keywords internal
#' @noRd
#'
#' @param data  Tile name.
#'
#' @return      A sits tibble with all processed bands.
#'
.apply_across <- function(data, fn, ...) {

    # Pre-conditions
    .check_samples(data)

    result <-
        .apply(data, col = "time_series", fn = function(x, ...) {
            dplyr::mutate(x, dplyr::across(
                dplyr::matches(sits_bands(data)),
                fn, ...
            ))
        }, ...)

    return(result)
}

#' @title Captures a band expression
#'
#' @name .apply_capture_expression
#' @keywords internal
#' @noRd
#'
#' @param tile_name  Tile name.
#'
#' @return           Named list with one expression
#'
.apply_capture_expression <- function(...) {
    # Capture dots as a list of quoted expressions
    list_expr <- lapply(substitute(list(...), env = environment()),
                        unlist,
                        recursive = FALSE)[-1]

    # Check bands names from expression
    .check_expression(list_expr)

    # Get out band
    out_band <- toupper(gsub("_", "-", names(list_expr)))
    names(list_expr) <- out_band

    return(list_expr)
}

#' @title Finds out all existing bands in an expression
#'
#' @name .apply_input_bands
#' @keywords internal
#' @noRd
#'
#' @param tile       Data cube tile.
#' @param expr       Band expression.
#'
#' @return           List of combination among tiles, bands, and dates
#'                   that are missing from the cube.
#'
.apply_input_bands <- function(cube, expr) {

    # Get all required bands in expression
    expr_bands <- toupper(.apply_get_all_names(expr[[1]]))

    # Get all input bands in cube data
    bands <- .cube_bands(cube)

    # Select bands that are in input expression
    bands <- bands[bands %in% expr_bands]

    # Found bands
    found_bands <- expr_bands %in% bands

    # Post-condition
    .check_that(
        x = all(found_bands),
        local_msg = "use 'sits_bands()' to check available bands",
        msg = paste("band(s)", paste0("'", expr_bands[!found_bands],
                                      "'", collapse = ", "), "not found")
    )

    return(bands)
}

#' @title Returns all names in an expression
#'
#' @name .apply_get_all_names
#' @keywords internal
#' @noRd
#' @param expr       Expression.
#'
#' @return           Character vector with all names in expression.
#'
.apply_get_all_names <- function(expr) {
    if (is.call(expr)) {
        unique(unlist(lapply(as.list(expr)[-1], .apply_get_all_names)))
    } else if (is.name(expr)) {
        paste0(expr)
    } else {
        character()
    }
}

.kern_functions <- function(window_size, img_nrow, img_ncol) {

    # Pre-conditions
    .check_window_size(window_size, max = min(img_nrow, img_ncol) - 1)

    result_env <- list2env(list(
        w_median = function(m) {
            C_kernel_median(
                x = as.matrix(m), ncols = img_ncol, nrows = img_nrow,
                band = 0, window_size = window_size
            )
        },
        w_sum = function(m) {
            C_kernel_sum(
                x = as.matrix(m), ncols = img_ncol, nrows = img_nrow,
                band = 0, window_size = window_size
            )
        },
        w_mean = function(m) {
            C_kernel_mean(
                x = as.matrix(m), ncols = img_ncol, nrows = img_nrow,
                band = 0, window_size = window_size
            )
        },
        w_sd = function(m) {
            C_kernel_sd(
                x = as.matrix(m), ncols = img_ncol, nrows = img_nrow,
                band = 0, window_size = window_size
            )
        },
        w_var = function(m) {
            C_kernel_var(
                x = as.matrix(m), ncols = img_ncol, nrows = img_nrow,
                band = 0, window_size = window_size
            )
        },
        w_min = function(m) {
            C_kernel_min(
                x = as.matrix(m), ncols = img_ncol, nrows = img_nrow,
                band = 0, window_size = window_size
            )
        },
        w_max = function(m) {
            C_kernel_max(
                x = as.matrix(m), ncols = img_ncol, nrows = img_nrow,
                band = 0, window_size = window_size
            )
        }
    ), parent = parent.env(environment()), hash = TRUE)

    return(result_env)
}
