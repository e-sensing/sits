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
#' @param window_size   An even number representing the size of the
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
#' # Get a time series
#' # Apply a normalization function
#'
#' point2 <-
#'     sits_select(point_mt_6bands, "NDVI") %>%
#'     sits_apply(NDVI_norm = (NDVI - min(NDVI)) / (max(NDVI) - min(NDVI)))
#' @rdname sits_apply
#' @export
sits_apply <- function(data, ...) {
    UseMethod("sits_apply", data)
}

#' @rdname sits_apply
#' @export
sits_apply.sits <- function(data, ...) {
    .check_set_caller("sits_apply.sits")

    .sits_fast_apply(data, col = "time_series", fn = dplyr::mutate, ...)
}

#' @rdname sits_apply
#' @export
sits_apply.raster_cube <- function(data, ...,
                                   window_size = 3,
                                   memsize = 1,
                                   multicores = 2,
                                   output_dir = getwd(),
                                   progress = TRUE) {

    .check_set_caller("sits_apply.raster_cube")
    progress <- .check_documentation(progress)

    # precondition - test if cube is regular
    .check_that(
        x = .cube_is_regular(data),
        local_msg = "Please use sits_regularize()",
        msg = "sits can only create new bands in regular cubes"
    )

    # Check output_dir
    output_dir <- path.expand(output_dir)
    .check_file(output_dir,
        msg = "invalid output directory"
    )

    # Get output band expression
    expr <- .apply_capture_expression(...)
    out_band <- names(expr)

    # Prepare parallelization
    .sits_parallel_start(workers = multicores, log = FALSE)
    on.exit(.sits_parallel_stop(), add = TRUE)

    # Find the tiles that have not been processed yet
    jobs <- .apply_find_missing_band(
        cube = data,
        band = out_band
    )

    # Already processed?
    if (length(jobs) == 0) return(data)

    while (length(jobs) > 0) {
        # for cubes that have a time limit to expire - mpc cubes only
        data <- .cube_token_generator(data)

        # Process bands and tiles in parallel
        .sits_parallel_map(jobs, function(job) {

            # Get parameters from each job
            tile_name <- job[[1]]
            fid <- job[[3]]

            # we consider token is expired when the remaining time is
            # less than 5 minutes
            if (.cube_is_token_expired(data)) {
                return(NULL)
            }

            tile <- .cube_filter(cube = data, tile = tile_name, fid = fid)

            # Get all input bands in cube data
            in_bands <- .apply_input_bands(tile, expr = expr)

            # Output file name
            out_file_path <- .apply_out_file_name(
                tile_name = .cube_tiles(tile),
                band = out_band,
                date = sits_timeline(tile)[[1]],
                output_dir = output_dir
            )

            # if file exists skip it (resume feature)
            if (file.exists(out_file_path)) {
                return(out_file_path)
            }

            # Compute the size of blocks to be computed
            # (it should be the whole cube)
            block_size <- .apply_estimate_block_size(
                cube = data,
                multicores = multicores,
                memsize = memsize
            )

            # Overlapping pixels
            overlapping_y_size <- ceiling(window_size / 2) - 1

            # For now, only vertical blocks are allowed, i.e. 'x_blocks' is 1
            blocks <- .apply_compute_blocks(
                xsize = .file_info_ncols(tile),
                ysize = .file_info_nrows(tile),
                block_y_size = block_size[["block_y_size"]],
                overlapping_y_size = overlapping_y_size
            )

            # Save each output block and return paths
            blocks_path <- purrr::map(blocks, function(block) {
                # Define the file name of the raster file to be written
                filename_block <- paste0(
                    tools::file_path_sans_ext(out_file_path),
                    "_block_", block[["first_row"]], "_",
                    block[["nrows"]], ".tif"
                )
                # if file exists skip it (resume feature)
                if (file.exists(filename_block)) return(filename_block)

                # Load bands data
                in_values <- purrr::map(in_bands, function(band) {

                    # Transform file_info columns as bands and values as paths
                    in_files <- .file_info(tile) %>%
                        dplyr::select(dplyr::all_of(c("band", "path"))) %>%
                        tidyr::pivot_wider(
                            names_from = "band",
                            values_from = "path"
                        )

                    # Get the missing values, minimum values and scale factors
                    missing_value <-
                        .cube_band_missing_value(tile, band = band)
                    minimum_value <-
                        .cube_band_minimum_value(tile, band = band)
                    maximum_value <-
                        .cube_band_maximum_value(tile, band = band)

                    # Scale the data set
                    scale_factor <-
                        .cube_band_scale_factor(tile, band = band)
                    offset_value <-
                        .cube_band_offset_value(tile, band = band)

                    # Read the values
                    values <- tryCatch(
                        {
                            .raster_read_stack(in_files[[band]], block = block)
                        },
                        error = function(e) {
                            return(NULL)
                        }
                    )

                    if (is.null(values)) {
                        return(NULL)
                    }

                    # Correct NA, minimum, maximum, and missing values
                    values[values == missing_value] <- NA
                    values[values < minimum_value] <- NA
                    values[values > maximum_value] <- NA

                    # compute scale and offset
                    values <- scale_factor * values + offset_value

                    return(values)
                })

                if (length(Filter(is.null, in_values)) > 0) {
                    return(NULL)
                }

                # Set band names
                names(in_values) <- in_bands

                # Evaluate expression here
                # Band and kernel evaluation
                out_values <- eval(
                    expr = expr[[out_band]],
                    envir = in_values,
                    enclos = .kern_functions(
                        window_size = window_size,
                        img_nrow = block[["nrows"]],
                        img_ncol = block[["ncols"]]
                    )
                )

                # Apply scale and offset
                out_values <- out_values /
                    .config_get("raster_cube_scale_factor") -
                    .config_get("raster_cube_offset_value")

                # Compute block spatial parameters
                params <- .cube_params_block(cube = tile, block = block)

                # New raster
                raster_out <- .raster_new_rast(
                    nrows = params[["nrows"]],
                    ncols = params[["ncols"]],
                    xmin = params[["xmin"]],
                    xmax = params[["xmax"]],
                    ymin = params[["ymin"]],
                    ymax = params[["ymax"]],
                    nlayers = 1,
                    crs = params[["crs"]]
                )

                # Set values
                raster_out <- .raster_set_values(raster_out, out_values)

                # get default missing value
                missing_value <- .config_get("raster_cube_missing_value")

                # Create extent
                blk_no_overlap <- list(
                    first_row = block$crop_first_row,
                    nrows = block$crop_nrows,
                    first_col = block$crop_first_col,
                    ncols = block$crop_ncols
                )

                # Save chunk
                # Crop removing overlaps
                .raster_crop(
                    r_obj = raster_out,
                    file = filename_block,
                    format = "GTiff",
                    data_type = .raster_data_type(
                        .config_get("raster_cube_data_type")
                    ),
                    gdal_options = .config_gtiff_default_options(),
                    overwrite = TRUE,
                    block = blk_no_overlap
                )

                # Clean memory
                gc()

                return(filename_block)
            })

            if (length(Filter(is.null, blocks_path)) > 0) {
                return(NULL)
            }

            # Merge result
            blocks_path <- unlist(blocks_path)

            # Join predictions
            if (!is.null(blocks_path)) {
                .raster_merge(
                    in_files = blocks_path,
                    out_file = out_file_path,
                    format = "GTiff",
                    gdal_datatype = .raster_gdal_datatype(
                        .config_get("raster_cube_data_type")
                    ),
                    gdal_options = .config_gtiff_default_options(),
                    overwrite = TRUE,
                    progress = progress
                )
            }
            return(out_file_path)
        }, progress = progress, n_retries = 0)

        # Find the tiles that have not been processed yet
        # Create local cube from files in output directory
        local_cube <- sits_cube(
            source = .cube_source(data),
            collection = .cube_collection(data),
            data_dir = output_dir,
            parse_info = c("x1", "tile", "band", "date"),
            multicores = multicores,
            progress = FALSE
        )

        # Find the tiles that have not been processed yet
        jobs <- .apply_find_missing_band(
            cube = local_cube,
            band = out_band
        )
    }

    return(local_cube)
}

#' @title Apply an expression across all bands
#'
#' @name .apply_across
#' @keywords internal
#'
#' @param data  Tile name.
#'
#' @return      A sits tibble with all processed bands.
#'
.apply_across <- function(data, fn, ...) {

    # Pre-conditions
    .check_that(
        x = inherits(data, "sits"),
        local_msg = "(data should be a sits tibble)",
        msg = "invalid samples parameter"
    )

    result <-
        .sits_fast_apply(data, col = "time_series", fn = function(x, ...) {
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
    .check_lst(list_expr,
               min_len = 1, max_len = 1,
               msg = "invalid expression value"
    )

    # Get out band
    out_band <- toupper(gsub("_", "-", names(list_expr)))
    names(list_expr) <- out_band

    return(list_expr)
}

#' @title Returns a new file name
#'
#' @name .apply_out_file_name
#' @keywords internal
#'
#' @param tile_name  Tile name.
#' @param band       Band name.
#' @param date       Observation date.
#' @param output_dir Base directory.
#'
#' @return           File path.
#'
.apply_out_file_name <- function(tile_name, band, date, output_dir) {
    # Prepare file name
    file_prefix <- paste("cube", tile_name, band, date, sep = "_")
    file_name <- paste(file_prefix, "tif", sep = ".")
    file_path <- paste(output_dir, file_name, sep = "/")
    return(file_path)
}

#' @title Finds the missing bands in a cube
#'
#' @name .apply_find_missing_band
#' @keywords internal
#'
#' @param cube   Data cube.
#' @param band   Band name.
#'
#' @return       List of combination among tiles, bands, and dates
#'               that are missing from the cube.
#'
.apply_find_missing_band <- function(cube, band) {

    # Pre-condition
    .check_length(band, len_min = 1, len_max = 1)

    tile_band_fid <- unlist(slider::slide(cube, function(tile) {
        tl <- sits_timeline(tile)
        fi <- .file_info(tile)
        fi_band <- fi[fi[["band"]] == band, ]
        missing_dates <- tl[!tl %in% fi_band[["date"]]]
        fi <- fi[fi[["date"]] %in% missing_dates, ]
        if (nrow(fi) == 0) {
            return(NULL)
        }
        purrr::cross3(.cube_tiles(tile), band, unique(fi[["fid"]]))
    }), recursive = FALSE)

    return(tile_band_fid)
}


# function to compute blocks grid
.apply_compute_blocks <- function(xsize,
                                  ysize,
                                  block_y_size,
                                  overlapping_y_size) {
    r1 <- seq(1, ysize - 1, by = block_y_size)
    r2 <- c(r1[-1] - 1, ysize)
    nr1 <- r2 - r1 + 1
    ovr_r1 <- c(1, c(r1[-1] - overlapping_y_size))
    ovr_r2 <- c(r2[-length(r2)] + overlapping_y_size, ysize)
    ovr_nr1 <- ovr_r2 - ovr_r1 + 1

    # define each block as a list element
    blocks <- mapply(
        list,
        first_row      = ovr_r1,
        nrows          = ovr_nr1,
        first_col      = 1,
        ncols          = xsize,
        crop_first_row = r1 - ovr_r1 + 1,
        crop_nrows     = nr1,
        crop_first_col = 1,
        crop_ncols     = xsize,
        SIMPLIFY       = FALSE
    )

    return(blocks)
}
#' @title Estimate the number of blocks
#' @name .apply_estimate_block_size
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
.apply_estimate_block_size <- function(cube, multicores, memsize) {

    # precondition 1 - check if cube has probability data
    .check_that(
        x = inherits(cube, "raster_cube"),
        msg = "input is not a raster cube"
    )

    size <- .cube_size(cube[1,])
    n_bands <- length(.cube_bands(cube))
    n_times <- length(.file_info_timeline(cube[1,]))
    bloat_mem <- .config_processing_bloat()
    n_bytes <- 8

    # total memory needed to do all work in GB
    image_size <- size[["ncols"]] * size[["nrows"]]
    needed_memory <- image_size * 1E-09 * n_bands * n_times *
        bloat_mem * n_bytes

    # minimum block size
    min_block_x_size <- size["ncols"] # for now, only vertical blocking
    min_block_y_size <- 1

    # compute factors
    memory_factor <- needed_memory / memsize

    blocking_factor <- image_size / (min_block_x_size * min_block_y_size)

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
        block_y_size = min(
            floor(blocking_factor / memory_factor / multicores),
            size[["nrows"]]
        )
    )

    return(blocks)
}

#' @title Finds out all existing bands in an expression
#'
#' @name .apply_input_bands
#' @keywords internal
#'
#' @param tile       Data cube tile.
#' @param expr       Band expression.
#'
#' @return           List of combination among tiles, bands, and dates
#'                   that are missing from the cube.
#'
.apply_input_bands <- function(tile, expr) {

    # Pre-condition
    .check_num(
        x = nrow(tile),
        min = 1,
        max = 1,
        msg = "invalid tile parameter"
    )

    # Get all required bands in expression
    expr_bands <- toupper(.apply_get_all_names(expr[[1]]))

    # Get all input bands in cube data
    bands <- .cube_bands(tile)

    # Select bands that are in input expression
    bands <- bands[bands %in% expr_bands]

    # Found bands
    found_bands <- expr_bands %in% bands

    # Post-condition
    .check_that(
        x = all(found_bands),
        local_msg = "use sits_bands() to check available bands",
        msg = paste("band(s)", paste0("'", expr_bands[!found_bands],
                                      "'", collapse = ", "), "not found")
    )

    return(bands)
}


#' @title Returns all kernel functions in an expression
#'
#' @name .apply_input_kernels
#' @keywords internal
#'
#' @param expr       Expression.
#'
#' @return           List with all used kernel functions.
#'
.apply_get_kernel_window_size <- function(expr) {

}

#' @title Returns all names in an expression
#'
#' @name .apply_get_all_names
#' @keywords internal
#'
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
    .check_num(
        x = window_size,
        min = 3,
        max = min(img_nrow, img_ncol) - 1,
        is_integer = TRUE,
        msg = "invalid 'window_size' parameter"
    )

    result_env <- list2env(list(
        w_median = function(m) {
            kernel_fun(
                data = m,
                band = 0,
                img_nrow = img_nrow,
                img_ncol = img_ncol,
                window_size = window_size,
                fun = 0
            )
        },
        w_sum = function(m) {
            kernel_fun(
                data = m,
                band = 0,
                img_nrow = img_nrow,
                img_ncol = img_ncol,
                window_size = window_size,
                fun = 1
            )
        },
        w_mean = function(m) {
            kernel_fun(
                data = m,
                band = 0,
                img_nrow = img_nrow,
                img_ncol = img_ncol,
                window_size = window_size,
                fun = 2
            )
        },
        w_sd = function(m) {
            kernel_fun(
                data = m,
                band = 0,
                img_nrow = img_nrow,
                img_ncol = img_ncol,
                window_size = window_size,
                fun = 3
            )
        },
        w_var = function(m) {
            kernel_fun(
                data = m,
                band = 0,
                img_nrow = img_nrow,
                img_ncol = img_ncol,
                window_size = window_size,
                fun = 4
            )
        },
        w_min = function(m) {
            kernel_fun(
                data = m,
                band = 0,
                img_nrow = img_nrow,
                img_ncol = img_ncol,
                window_size = window_size,
                fun = 5
            )
        },
        w_max = function(m) {
            kernel_fun(
                data = m,
                band = 0,
                img_nrow = img_nrow,
                img_ncol = img_ncol,
                window_size = window_size,
                fun = 6
            )
        }
    ), parent = parent.env(environment()), hash = TRUE)

    return(result_env)
}
