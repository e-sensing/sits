#' @title Reduce tile into one image
#' @name .reduce_tile
#'
#' @param  tile       Single tile of a data cube.
#' @param  block      Optimized block to be read into memory.
#' @param  expr       An expression to be evaluated.
#' @param  out_band   Output band
#' @param  in_bands   Input bands
#' @param  impute_fn  Imputation function
#' @param  output_dir Output directory.
#' @param  progress   how progress bar?
#'
#' @noRd
#' @return A reduced temporal tile
.reduce_tile <- function(tile,
                         block,
                         expr,
                         out_band,
                         in_bands,
                         impute_fn,
                         output_dir,
                         progress) {

    # Output file
    out_file <- .file_eo_name(
        tile = tile, band = out_band,
        date = .tile_start_date(tile),
        output_dir = output_dir
    )
    # Resume feature
    if (.raster_is_valid(out_file, output_dir = output_dir)) {
        # recovery message
        .check_recovery(out_file)

        # Create tile based on template
        tile <- .tile_eo_from_files(
            files = out_file, fid = .fi_fid(.fi(tile)),
            bands = out_band, date = .tile_start_date(tile),
            base_tile = tile, update_bbox = FALSE
        )
        return(tile)
    }
    # Remove remaining incomplete fractions files
    unlink(out_file)
    # Create chunks as jobs
    chunks <- .tile_chunks_create(
        tile = tile, overlap = 0, block = block
    )
    # Add cloud band in input bands
    if (.tile_contains_cloud(tile)) {
        in_bands <- c(in_bands, .band_cloud())
    }
    tile <- .tile_filter_bands(tile, in_bands)
    # Get band configuration
    band_conf <- .conf_eo_band(
        source = .tile_source(tile),
        collection = .tile_collection(tile),
        band = out_band
    )
    # In case the user has not defined a config for the output band
    if (.has_not(band_conf)) {
        fn_name <- .as_chr(as.list(expr[[out_band]])[[1]])
        band_conf <- .conf("default_values", .reduce_datatypes(fn_name))
    }
    # Process jobs in parallel
    block_files <- .jobs_map_parallel_chr(chunks, function(chunk) {
        # Get job block
        block <- .block(chunk)
        # Block file name
        block_file <- .file_block_name(
            pattern = .file_pattern(out_file),
            block = block,
            output_dir = output_dir
        )
        # Resume processing in case of failure
        if (.raster_is_valid(block_file)) {
            return(block_file)
        }
        # Read and preprocess values
        values <- .classify_data_read(
            tile = tile,
            block = block,
            bands = in_bands,
            ml_model = NULL,
            impute_fn = impute_fn,
            filter_fn = NULL
        )
        # Convert to named list
        values <- list(values)
        names(values) <- in_bands
        # Evaluate expression here
        # Band and kernel evaluation
        values <- eval(
            expr = expr[[out_band]],
            envir = values,
            enclos = .reduce_fns()
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
            files = block_file, block = block, bbox = .bbox(chunk),
            values = values, data_type = .data_type(band_conf),
            missing_value = .miss_value(band_conf),
            crop_block = crop_block
        )
        # Free memory
        gc()
        # Returned block files for each fraction
        block_file

    }, progress = progress)
    # Merge blocks into a new eo_cube tile
    band_tile <- .tile_eo_merge_blocks(
        files = out_file,
        bands = out_band,
        band_conf = band_conf,
        base_tile = tile,
        block_files = block_files,
        multicores = 1,
        update_bbox = FALSE
    )
    # Return a reduced tile
    band_tile
}

#' @title Reduce samples
#' @name .reduce_samples
#'
#' @param  data      A sits tibble
#' @param  expr      An expression to be evaluated.
#' @param  in_band   Input bands
#' @param  out_band  Output band
#'
#' @noRd
#' @return A reduced temporal tile
.reduce_samples <- function(data, expr, in_band, out_band) {
    col <- "time_series"
    min_date <- min(.samples_timeline(data))
    # Pre-condition
    .check_chr_within(
        col,
        within = names(data),
        msg = "invalid column name"
    )
    # Select data do unpack
    x <- data[col]
    # Prepare to unpack
    x[["#.."]] <- as.factor(seq_len(nrow(data)))
    # Unpack
    x <- tidyr::unnest(x, cols = dplyr::all_of(col))
    # Apply the temporal operation
    x <- by(x, x[, "#.."], function(y) {
        y <- list(t(y[[in_band]]))
        names(y) <- in_band
        eval(
            expr = expr[[out_band]],
            envir = y,
            enclos = .reduce_fns()
        )
    })
    # Unlist results
    x <- unlist(x)
    # Create a data frame
    x <- tibble::tibble(x)
    colnames(x) <- out_band
    x[["#.."]] <- as.factor(seq_len(nrow(x)))
    # Add index column
    x <- tibble::add_column(x, Index = min_date, .before = out_band)
    # pack
    x <- tidyr::nest(x, `..unnest_col` = -"#..")
    # remove garbage
    x[["#.."]] <- NULL
    names(x) <- col
    # prepare result
    data[[col]] <- x[[col]]
    return(data)
}

#' @title Temporal functions for reduce operations
#' @name .reduce_fns
#' @noRd
#' @return operations on reduce function
.reduce_fns <- function() {
    result_env <- list2env(list(
        t_max = function(m) {
            C_temp_max(mtx = as.matrix(m))
        },
        t_min = function(m) {
            C_temp_min(mtx = as.matrix(m))
        },
        t_mean = function(m) {
            C_temp_mean(mtx = as.matrix(m))
        },
        t_median = function(m) {
            C_temp_median(mtx = as.matrix(m))
        },
        t_sum = function(m) {
            C_temp_sum(mtx = as.matrix(m))
        },
        t_std = function(m) {
            C_temp_std(mtx = as.matrix(m))
        },
        t_skewness = function(m) {
            C_temp_skew(mtx = as.matrix(m))
        },
        t_kurtosis = function(m) {
            C_temp_kurt(mtx = as.matrix(m))
        },
        t_amplitude = function(m) {
            C_temp_amplitude(mtx = as.matrix(m))
        },
        t_fslope = function(m) {
            C_temp_fslope(mtx = as.matrix(m))
        },
        t_mse = function(m) {
            C_temp_mse(mtx = as.matrix(m))
        },
        t_fqr = function(m) {
            C_temp_fqr(mtx = as.matrix(m))
        },
        t_tqr = function(m) {
            C_temp_tqr(mtx = as.matrix(m))
        },
        t_iqr = function(m) {
            C_temp_iqr(mtx = as.matrix(m))
        }
    ), parent = parent.env(environment()), hash = TRUE)

    return(result_env)
}

#' @title Output datatypes for a defined reduce function
#' @name .reduce_datatypes
#'
#' @param fn_name a character with a reduce function name.
#'
#' @noRd
#' @return An output datatype
.reduce_datatypes <- function(fn_name) {
    switch(
        fn_name,
        t_sum = , t_std = , t_skewness = , t_kurtosis = , t_mse = "FLT4S",
        "INT2S"
    )
}
