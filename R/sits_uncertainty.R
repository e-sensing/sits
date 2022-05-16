#' @title Estimate classification uncertainty based on probs cube
#'
#' @name  sits_uncertainty
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#'
#' @param  cube              Probability data cube.
#' @param  type              Method to measure uncertainty. See details.
#' @param  ...               Parameters for specific functions.
#' @param  window_size       Size of neighborhood to calculate entropy.
#' @param  window_fn         Function to be applied in entropy calculation.
#' @param  multicores        Number of cores to run the function.
#' @param  memsize           Maximum overall memory (in GB) to run the
#'                           function.
#' @param  output_dir        Output directory for image files.
#' @param  version           Version of resulting image.
#'                           (in the case of multiple tests)
#' @return An uncertainty data cube
#'
#' @description Calculate the uncertainty cube based on the probabilities
#' produced by the classifier. Takes a probability cube as input.
#' The uncertainty measure is relevant in the context of active leaning,
#' and helps to increase the quantity and quality of training samples by
#' providing information about the confidence of the model.
#' The supported types of uncertainty are 'entropy', 'least', 'margin' and
#' 'ratio'. 'entropy' is the difference between all predictions expressed as
#' entropy, 'least' is the difference between 100% and most confident
#' prediction, 'margin' is the difference between the two most confident
#' predictions, and 'ratio' is the ratio between the two most confident
#' predictions.
#'
#' @references Monarch, Robert Munro. Human-in-the-Loop Machine Learning:
#' Active learning and annotation for human-centered AI. Simon and Schuster,
#' 2021.
#'
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # select a set of samples
#'     samples_ndvi <- sits_select(samples_modis_4bands, bands = c("NDVI"))
#'     # create a random forest model
#'     rfor_model <- sits_train(samples_ndvi, sits_rfor())
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6",
#'         data_dir = data_dir,
#'         delim = "_",
#'         parse_info = c("X1", "X2", "tile", "band", "date")
#'     )
#'     # classify a data cube
#'     probs_cube <- sits_classify(data = cube, ml_model = rfor_model)
#'     # calculate uncertainty
#'     uncert_cube <- sits_uncertainty(probs_cube)
#'     # plot the resulting uncertainty cube
#'     plot(uncert_cube)
#' }
#' @export
sits_uncertainty <- function(cube, type = "least", ...,
                             multicores = 2,
                             memsize = 8,
                             output_dir = ".",
                             version = "v1") {
    # set caller to show in errors
    .check_set_caller("sits_uncertainty")

    .check_require_packages("parallel")

    # check if cube has probability data
    .check_that(
        x = inherits(cube, "probs_cube"),
        msg = "input is not probability cube"
    )
    # define the class of the smoothing
    class(type) <- c(type, class(type))
    UseMethod("sits_uncertainty", type)
}

#' @rdname sits_uncertainty
#'
#' @export
#'
sits_uncertainty.entropy <- function(cube, type = "entropy", ...,
                                     window_size = 5,
                                     window_fn = "median",
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
    if (!purrr::is_null(window_size)) {
        .check_that(
            x = window_size %% 2 != 0,
            msg = "window_size must be an odd number"
        )
    }

    # precondition 3 - test window function
    .check_chr_within(
        x = window_fn,
        within = .config_names("uncertainty_window_functions"),
        msg = "Invalid 'window_fn' parameter"
    )
    # resolve window_fn parameter
    window_fn <- .config_get(key = c(
        "uncertainty_window_functions",
        window_fn
    ))
    config_fun <- strsplit(window_fn, "::")[[1]]
    window_fn <- get(config_fun[[2]], envir = asNamespace(config_fun[[1]]))

    # find out how many labels exist
    n_labels <- length(sits_labels(cube[1, ]))

    # precondition 4 - multicores
    .check_num(
        x = multicores,
        min = 1,
        len_min = 1,
        len_max = 1,
        is_integer = TRUE,
        msg = "invalid 'multicores' parameter"
    )

    # precondition 5 - memory
    .check_num(
        x = memsize,
        exclusive_min = 0,
        len_min = 1,
        len_max = 1,
        msg = "invalid 'memsize' parameter"
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
    window <- NULL
    if (!purrr::is_null(window_size) && window_size > 1) {
        window <- matrix(1, nrow = window_size, ncol = window_size)
    }

    # entropy uncertainty index to be executed by workers cluster
    .do_entropy <- function(chunk) {
        data <- .raster_get_values(r_obj = chunk)
        # process entropy
        unc <- entropy_probs(data, n_labels)
        # create cube
        res <- .raster_rast(
            r_obj = chunk,
            nlayers = 1
        )
        # copy values
        res <- .raster_set_values(
            r_obj = res,
            values = unc
        )
        # process window
        if (!is.null(window)) {
            res <- terra::focal(
                res,
                w = window,
                fun = window_fn,
                na.rm = TRUE
            )
        }
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
            cube_class = "uncertainty_cube",
            band_name  = "entropy",
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
            return(NULL)
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
            chunk <- .raster_crop(r_obj = b, block = block)

            # process it
            raster_out <- .do_entropy(chunk = chunk)

            # create extent
            blk_no_overlap <- list(
                first_row = block$crop_first_row,
                nrows = block$crop_nrows,
                first_col = block$crop_first_col,
                ncols = block$crop_ncols
            )

            # crop removing overlaps
            raster_out <- .raster_crop(raster_out, block = blk_no_overlap)

            # export to temp file
            block_file <- .smth_filename(
                tile = tile_new,
                output_dir = output_dir,
                block = block
            )

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
        tile <- cube[i, ]

        # create metadata for raster cube
        tile_new <- .cube_derived_create(
            cube       = tile,
            cube_class = "uncertainty_cube",
            band_name  = "entropy",
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

        tmp_blocks <- blocks_tile_lst[[i]]

        # apply function to blocks
        on.exit(unlink(tmp_blocks))

        # merge to save final result
        suppressWarnings(
            .raster_merge(
                in_files = tmp_blocks,
                out_file = out_file,
                format = "GTiff",
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

    class(result_cube) <- c("uncertainty_cube", class(cube))

    return(result_cube)
}



#' @rdname sits_uncertainty
#'
#' @export
#'
sits_uncertainty.least <- function(cube, type = "least", ...,
                                   window_size = 5,
                                   window_fn = "median",
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
    if (!purrr::is_null(window_size)) {
        .check_that(
            x = window_size %% 2 != 0,
            msg = "window_size must be an odd number"
        )
    }

    # precondition 3 - test window function
    .check_chr_within(
        x = window_fn,
        within = .config_names("uncertainty_window_functions"),
        msg = "Invalid 'window_fn' parameter"
    )
    # resolve window_fn parameter
    window_fn <- .config_get(key = c(
        "uncertainty_window_functions",
        window_fn
    ))
    config_fun <- strsplit(window_fn, "::")[[1]]
    window_fn <- get(config_fun[[2]], envir = asNamespace(config_fun[[1]]))

    # find out how many labels exist
    n_labels <- length(sits_labels(cube[1, ]))

    # precondition 4 - multicores
    .check_num(
        x = multicores,
        min = 1,
        len_min = 1,
        len_max = 1,
        is_integer = TRUE,
        msg = "invalid 'multicores' parameter"
    )

    # precondition 5 - memory
    .check_num(
        x = memsize,
        exclusive_min = 0,
        len_min = 1,
        len_max = 1,
        msg = "invalid 'memsize' parameter"
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
    window <- NULL
    if (!purrr::is_null(window_size) && window_size > 1) {
        window <- matrix(1, nrow = window_size, ncol = window_size)
    }

    # least confidence  uncertainty index to be executed by workers cluster
    .do_least <- function(chunk) {
        data <- .raster_get_values(r_obj = chunk)
        # process least confidence
        unc <- least_probs(data, n_labels)
        # create cube
        res <- .raster_rast(
            r_obj = chunk,
            nlayers = 1
        )
        # copy values
        res <- .raster_set_values(
            r_obj = res,
            values = unc
        )
        # process window
        if (!is.null(window)) {
            res <- terra::focal(
                res,
                w = window,
                fun = window_fn,
                na.rm = TRUE
            )
        }
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
            cube_class = "uncertainty_cube",
            band_name  = "least",
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
            return(NULL)
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
            chunk <- .raster_crop(r_obj = b, block = block)

            # process it
            raster_out <- .do_least(chunk = chunk)

            # create extent
            blk_no_overlap <- list(
                first_row = block$crop_first_row,
                nrows = block$crop_nrows,
                first_col = block$crop_first_col,
                ncols = block$crop_ncols
            )

            # crop removing overlaps
            raster_out <- .raster_crop(raster_out, block = blk_no_overlap)

            # export to temp file
            block_file <- .smth_filename(
                tile = tile_new,
                output_dir = output_dir,
                block = block
            )

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
        tile <- cube[i, ]

        # create metadata for raster cube
        tile_new <- .cube_derived_create(
            cube       = tile,
            cube_class = "uncertainty_cube",
            band_name  = "least",
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

        tmp_blocks <- blocks_tile_lst[[i]]

        # apply function to blocks
        on.exit(unlink(tmp_blocks))

        # merge to save final result
        suppressWarnings(
            .raster_merge(
                in_files = tmp_blocks,
                out_file = out_file,
                format = "GTiff",
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

    class(result_cube) <- c("uncertainty_cube", class(cube))

    return(result_cube)
}



#' @rdname sits_uncertainty
#'
#' @export
#'
sits_uncertainty.margin <- function(cube, type = "margin", ...,
                                    window_size = 5,
                                    window_fn = "median",
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
    if (!purrr::is_null(window_size)) {
        .check_that(
            x = window_size %% 2 != 0,
            msg = "window_size must be an odd number"
        )
    }

    # precondition 3 - test window function
    .check_chr_within(
        x = window_fn,
        within = .config_names("uncertainty_window_functions"),
        msg = "Invalid 'window_fn' parameter"
    )
    # resolve window_fn parameter
    window_fn <- .config_get(key = c(
        "uncertainty_window_functions",
        window_fn
    ))
    config_fun <- strsplit(window_fn, "::")[[1]]
    window_fn <- get(config_fun[[2]], envir = asNamespace(config_fun[[1]]))

    # find out how many labels exist
    n_labels <- length(sits_labels(cube[1, ]))

    # precondition 4 - multicores
    .check_num(
        x = multicores,
        min = 1,
        len_min = 1,
        len_max = 1,
        is_integer = TRUE,
        msg = "invalid 'multicores' parameter"
    )

    # precondition 5 - memory
    .check_num(
        x = memsize,
        exclusive_min = 0,
        len_min = 1,
        len_max = 1,
        msg = "invalid 'memsize' parameter"
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
    window <- NULL
    if (!purrr::is_null(window_size) && window_size > 1) {
        window <- matrix(1, nrow = window_size, ncol = window_size)
    }

    # margin of confidence uncertainty index to be executed by workers cluster
    .do_margin <- function(chunk) {
        data <- .raster_get_values(r_obj = chunk)
        # process margin of confidence
        unc <- margin_probs(data, n_labels)
        # create cube
        res <- .raster_rast(
            r_obj = chunk,
            nlayers = 1
        )
        # copy values
        res <- .raster_set_values(
            r_obj = res,
            values = unc
        )
        # process window
        if (!is.null(window)) {
            res <- terra::focal(
                res,
                w = window,
                fun = window_fn,
                na.rm = TRUE
            )
        }
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
            cube_class = "uncertainty_cube",
            band_name  = "margin",
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
            return(NULL)
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
            chunk <- .raster_crop(r_obj = b, block = block)

            # process it
            raster_out <- .do_margin(chunk = chunk)

            # create extent
            blk_no_overlap <- list(
                first_row = block$crop_first_row,
                nrows = block$crop_nrows,
                first_col = block$crop_first_col,
                ncols = block$crop_ncols
            )

            # crop removing overlaps
            raster_out <- .raster_crop(raster_out, block = blk_no_overlap)

            # export to temp file
            block_file <- .smth_filename(
                tile = tile_new,
                output_dir = output_dir,
                block = block
            )

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
        tile <- cube[i, ]

        # create metadata for raster cube
        tile_new <- .cube_derived_create(
            cube       = tile,
            cube_class = "uncertainty_cube",
            band_name  = "margin",
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

        tmp_blocks <- blocks_tile_lst[[i]]

        # apply function to blocks
        on.exit(unlink(tmp_blocks))

        # merge to save final result
        suppressWarnings(
            .raster_merge(
                in_files = tmp_blocks,
                out_file = out_file,
                format = "GTiff",
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

    class(result_cube) <- c("uncertainty_cube", class(cube))

    return(result_cube)
}



#' @rdname sits_uncertainty
#'
#' @export
#'
sits_uncertainty.ratio <- function(cube, type = "ratio", ...,
                                    window_size = 5,
                                    window_fn = "median",
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
    if (!purrr::is_null(window_size)) {
        .check_that(
            x = window_size %% 2 != 0,
            msg = "window_size must be an odd number"
        )
    }

    # precondition 3 - test window function
    .check_chr_within(
        x = window_fn,
        within = .config_names("uncertainty_window_functions"),
        msg = "Invalid 'window_fn' parameter"
    )
    # resolve window_fn parameter
    window_fn <- .config_get(key = c(
        "uncertainty_window_functions",
        window_fn
    ))
    config_fun <- strsplit(window_fn, "::")[[1]]
    window_fn <- get(config_fun[[2]], envir = asNamespace(config_fun[[1]]))

    # find out how many labels exist
    n_labels <- length(sits_labels(cube[1, ]))

    # precondition 4 - multicores
    .check_num(
        x = multicores,
        min = 1,
        len_min = 1,
        len_max = 1,
        is_integer = TRUE,
        msg = "invalid 'multicores' parameter"
    )

    # precondition 5 - memory
    .check_num(
        x = memsize,
        exclusive_min = 0,
        len_min = 1,
        len_max = 1,
        msg = "invalid 'memsize' parameter"
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
    window <- NULL
    if (!purrr::is_null(window_size) && window_size > 1) {
        window <- matrix(1, nrow = window_size, ncol = window_size)
    }

    # ratio of confidence uncertainty index to be executed by workers cluster
    .do_ratio <- function(chunk) {
        data <- .raster_get_values(r_obj = chunk)
        # process ratio of confidence
        unc <- ratio_probs(data, n_labels)
        # create cube
        res <- .raster_rast(
            r_obj = chunk,
            nlayers = 1
        )
        # copy values
        res <- .raster_set_values(
            r_obj = res,
            values = unc
        )
        # process window
        if (!is.null(window)) {
            res <- terra::focal(
                res,
                w = window,
                fun = window_fn,
                na.rm = TRUE
            )
        }
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
            cube_class = "uncertainty_cube",
            band_name  = "ratio",
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
            return(NULL)
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
            chunk <- .raster_crop(r_obj = b, block = block)

            # process it
            raster_out <- .do_ratio(chunk = chunk)

            # create extent
            blk_no_overlap <- list(
                first_row = block$crop_first_row,
                nrows = block$crop_nrows,
                first_col = block$crop_first_col,
                ncols = block$crop_ncols
            )

            # crop removing overlaps
            raster_out <- .raster_crop(raster_out, block = blk_no_overlap)

            # export to temp file
            block_file <- .smth_filename(
                tile = tile_new,
                output_dir = output_dir,
                block = block
            )

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
        tile <- cube[i, ]

        # create metadata for raster cube
        tile_new <- .cube_derived_create(
            cube       = tile,
            cube_class = "uncertainty_cube",
            band_name  = "ratio",
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

        tmp_blocks <- blocks_tile_lst[[i]]

        # apply function to blocks
        on.exit(unlink(tmp_blocks))

        # merge to save final result
        suppressWarnings(
            .raster_merge(
                in_files = tmp_blocks,
                out_file = out_file,
                format = "GTiff",
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

    class(result_cube) <- c("uncertainty_cube", class(cube))

    return(result_cube)
}
