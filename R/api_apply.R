#' @title Apply a function to one band of a time series
#' @name .apply
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param  data      Tibble.
#' @param  col       Column where function should be applied
#' @param  fn        Function to be applied.
#' @return           Tibble where function has been applied.
.apply <- function(data, col, fn, ...) {
    .check_set_caller(".apply")
    # pre-condition
    .check_chr_within(
        col,
        within = names(data)
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
    x <- tidyr::nest(x, `..unnest_col` = -"#..")
    # remove garbage
    x[["#.."]] <- NULL
    names(x) <- col
    # prepare result
    data[[col]] <- x[[col]]
    return(data)
}
#' @title Apply an expression to block of a set of input bands
#' @name .apply_feature
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param  feature         Subset of a data cube containing the input bands
#'                         used in the expression
#' @param  block           Individual block that will be processed
#' @param  window_size     Size of the neighbourhood (if required)
#' @param  expr            Expression to be applied
#' @param  out_band        Output band
#' @param  in_bands        Input bands
#' @param  overlap         Overlap between tiles (if required)
#' @param  normalized      Produce normalized band?
#' @param  output_dir      Directory where image will be save
#'
#' @return                 A feature compose by a combination of tile and band.
.apply_feature <- function(feature, block, window_size, expr,
                           out_band, in_bands, overlap,
                           normalized, output_dir) {
    # Output file
    out_file <- .file_eo_name(
        tile = feature, band = out_band,
        date = .tile_start_date(feature), output_dir = output_dir
    )
    # Resume feature
    if (.raster_is_valid(out_file, output_dir = output_dir)) {
        # recovery message
        .check_recovery(out_file)

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
    chunks <- .tile_chunks_create(
        tile = feature, overlap = overlap, block = block
    )
    # Get band configuration
    band_conf <- .tile_band_conf(tile = feature, band = out_band)
    if (.has_not(band_conf)) {
        band_conf <- .conf("default_values", "FLT4S")
        if (normalized)
            band_conf <- .conf("default_values", "INT2S")
    }
    # Process jobs sequentially
    block_files <- .jobs_map_sequential(chunks, function(chunk) {
        # Get job block
        block <- .block(chunk)
        # Block file name for each fraction
        block_files <- .file_block_name(
            pattern = .file_pattern(out_file),
            block = block,
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
    # Merge blocks into a new eo_cube tile
    band_tile <- .tile_eo_merge_blocks(
        files = out_file,
        bands = out_band,
        band_conf = band_conf,
        base_tile = feature,
        block_files = block_files,
        multicores = 1,
        update_bbox = FALSE
    )
    # Return a feature tile
    band_tile
}
#' @title Read data for the apply operation
#' @name .apply_data_read
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param  tile            Subset of a data cube containing the input bands
#' @param  block           Individual block that will be processed
#' @param  in_bands        Input bands
#'
#' @return Values read from the block
.apply_data_read <- function(tile, block, in_bands) {
    # for cubes that have a time limit to expire - mpc cubes only
    tile <- .cube_token_generator(tile)
    # Read and preprocess values from cloud
    # Get cloud values (NULL if not exists)
    cloud_mask <- .tile_cloud_read_block(tile = tile, block = block)
    # Read and preprocess values from each band
    values <- .map_dfc(in_bands, function(band) {
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
#' @name .apply_across
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#'
#' @param data  Tile name.
#' @param fn    Function to be applied
#' @param ...   Further parameters for the function
#' @return      A sits tibble with all processed bands.
#'
.apply_across <- function(data, fn, ...) {
    # Pre-conditions
    data <- .check_samples(data)

    result <-
        .apply(data, col = "time_series", fn = function(x, ...) {
            dplyr::mutate(x, dplyr::across(
                dplyr::matches(.samples_bands(data)),
                fn, ...
            ))
        }, ...)

    return(result)
}
#' @title Captures a band expression
#' @name .apply_capture_expression
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#'
#' @param ...  Expression to be applied
#' @return     Named list with one expression
#'
.apply_capture_expression <- function(...) {
    # Capture dots as a list of quoted expressions
    list_expr <- lapply(substitute(list(...), env = environment()),
                        unlist,
                        recursive = FALSE
    )[-1]

    # Check bands names from expression
    .check_expression(list_expr)

    # Get out band
    out_band <- toupper(gsub("_", "-", names(list_expr), fixed = TRUE))
    names(list_expr) <- out_band

    return(list_expr)
}
#' @title Finds out all existing bands in an expression
#' @name .apply_input_bands
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#'
#' @param cube       Data cube.
#' @param bands      Input bands in a cube or samples.
#' @param expr       Band combination expression.
#' @return           List of input bands required to run the expression
#'
.apply_input_bands <- function(cube, bands, expr) {
    # set caller to show in errors
    .check_set_caller(".apply_input_bands")

    # Get all required bands in expression
    expr_bands <- toupper(.apply_get_all_names(expr[[1]]))

    # Select bands that are in input expression
    bands <- bands[bands %in% expr_bands]

    # Post-condition
    .check_that(all(expr_bands %in% bands))

    return(bands)
}
#' @title Returns all names in an expression
#' @name .apply_get_all_names
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
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
#' @title Kernel function for window operations in spatial neighbourhoods
#' @name .kern_functions
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @noRd
#' @param windows size of local window
#' @param img_nrow  image size in rows
#' @param img_ncol  image size in cols
#' @return operations on local kernels
#'
.kern_functions <- function(window_size, img_nrow, img_ncol) {
    result_env <- list2env(list(
        w_median = function(m) {
            C_kernel_median(
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
        },
        w_var = function(m) {
            C_kernel_var(
                x = as.matrix(m), ncols = img_ncol, nrows = img_nrow,
                band = 0, window_size = window_size
            )
        },
        w_modal = function(m) {
            C_kernel_modal(
                x = as.matrix(m), ncols = img_ncol, nrows = img_nrow,
                band = 0, window_size = window_size
            )
        }
    ), parent = parent.env(environment()), hash = TRUE)

    return(result_env)
}
