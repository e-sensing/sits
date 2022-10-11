#' @title Reclassify a classified cube
#'
#' @name sits_reclassify
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description
#' Apply a set of named expressions to reclassify a classified image.
#' The expressions should use character values to refer to labels in
#' logical expressions.
#'
#' @param cube       Classified image cube to be reclassified.
#' @param mask       Classified image cube with additional information
#'                   to be used in expressions.
#' @param rules      Named expressions to be evaluated (see details).
#' @param memsize    Memory available for classification (in GB).
#' @param multicores Number of cores to be used for classification.
#' @param output_dir Directory where files will be saved.
#' @param version    Version of resulting image (in the case of multiple runs).
#'
#' @details
#' \code{sits_reclassify()} allow any valid R expression to compute
#' reclassification. User should refer to \code{cube} and \code{mask}
#' to construct logical expressions.
#' Users can use can use any R expression that evaluates to logical.
#' \code{TRUE} values will be relabeled to expression name.
#' Updates are done in asynchronous manner, that is, all expressions
#' are evaluated using original classified values. Expressions are
#' evaluated sequentially and resulting values are assigned to
#' output cube. Last expressions has precedence over first ones.
#'
#' @return A classified image cube.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # Open mask map
#'     data_dir <- system.file("extdata/raster/prodes", package = "sits")
#'     prodes2021 <- sits_cube(
#'         source = "USGS",
#'         collection = "LANDSAT-C2L2-SR",
#'         data_dir = data_dir,
#'         parse_info = c("X1", "X2", "tile", "start_date", "end_date",
#'                        "band", "version"),
#'         bands = "class",
#'         labels = c("Forest", "Water", "NonForest",
#'                    "NonForest2", "NoClass", "d2007", "d2008",
#'                    "d2009", "d2010", "d2011", "d2012",
#'                    "d2013", "d2014", "d2015", "d2016",
#'                    "d2017", "d2018", "r2010", "r2011",
#'                    "r2012", "r2013", "r2014", "r2015",
#'                    "r2016", "r2017", "r2018", "d2019",
#'                    "r2019", "d2020", "NoClass", "r2020",
#'                    "Clouds2021", "d2021", "r2021")
#'     )
#'
#'     # Open classification map
#'     data_dir <- system.file("extdata/raster/classif", package = "sits")
#'     ro_class <- sits_cube(
#'         source = "MPC",
#'         collection = "SENTINEL-2-L2A",
#'         data_dir = data_dir,
#'         parse_info = c("X1", "X2", "tile", "start_date", "end_date",
#'                        "band", "version"),
#'         bands = "class",
#'         labels = c("ClearCut_Fire", "ClearCut_BareSoil",
#'                    "ClearCut_Veg", "Forest")
#'     )
#'
#'     # Reclassify cube
#'     ro_mask <- sits_reclassify(
#'         cube = ro_class,
#'         mask = prodes2021,
#'         rules = list(
#'             "Old_Deforestation" = mask %in% c(
#'                 "d2007", "d2008", "d2009",
#'                 "d2010", "d2011", "d2012",
#'                 "d2013", "d2014", "d2015",
#'                 "d2016", "d2017", "d2018",
#'                 "r2010", "r2011", "r2012",
#'                 "r2013", "r2014", "r2015",
#'                 "r2016", "r2017", "r2018",
#'                 "d2019", "r2019", "d2020",
#'                 "r2020", "r2021"
#'             ),
#'             "Water_Mask" = mask == "Water",
#'             "NonForest_Mask" = mask %in% c("NonForest", "NonForest2")
#'         ),
#'         memsize = 1,
#'         multicores = 1,
#'         output_dir = getwd()
#'     )
#'
#'     plot(ro_mask, palette = "Geyser")
#' }
#' @rdname sits_reclassify
#' @export
sits_reclassify <- function(cube, mask, rules, memsize = 1, multicores = 2,
                            output_dir = getwd(), version = "v1") {

    # Pre-conditions - Check parameters
    .check_cube_is_class_cube(cube)
    .check_cube_is_class_cube(mask)
    .check_memsize(memsize)
    .check_multicores(multicores)
    # Expand output_dir path
    output_dir <- path.expand(output_dir)
    .check_output_dir(output_dir)
    .check_version(version)

    # Check memory and multicores
    # Get block size
    block <- .raster_file_blocksize(.raster_open_rast(.fi_path(.fi(cube))))
    # Check minimum memory needed to process one block
    # npaths = input(1) + output(1)
    job_memsize <- .jobs_memsize(
        job_size = .block_size(block = block, overlap = 0),
        npaths = 2,
        nbytes = 8, proc_bloat = .conf("processing_bloat")
    )
    # Update multicores parameter
    multicores <- .jobs_max_multicores(
        job_memsize = job_memsize, memsize = memsize, multicores = multicores
    )

    # Prepare parallelization
    .sits_parallel_start(workers = multicores, log = FALSE)
    on.exit(.sits_parallel_stop(), add = TRUE)

    UseMethod("sits_reclassify", cube)
}

#' @rdname sits_reclassify
#' @export
sits_reclassify.class_cube <- function(cube, mask, rules, memsize = 4,
                                       multicores = 2, output_dir = getwd(),
                                       version = "v1") {
    # Capture expression
    rules <- as.list(substitute(rules, environment()))[-1]

    # Reclassify parameters checked in reclassify function
    # Create reclassification function
    reclassify_fn <- .reclassify_fn_expr(
        rules = rules, labels_cube = .tile_labels(cube),
        labels_mask = .tile_labels(mask)
    )
    # Filter mask - bands
    mask <- .cube_filter_bands(cube = mask, bands = "class")
    # Process each tile sequentially
    class_cube <- .cube_foreach_tile(cube, function(tile, mask) {
        # Filter mask - temporal
        mask <- .try({
            .cube_filter_temporal(
                cube = mask, start_date = .tile_start_date(tile),
                end_date = .tile_end_date(tile)
            )
        },
        .msg_error = "mask's interval does not intersect cube"
        )
        # Filter mask - spatial
        mask <- .try({
            .cube_filter_spatial(cube = mask, roi = .bbox(tile))
        },
        .msg_error = "mask's roi does not intersect cube"
        )
        # Get output labels
        labels <- unique(c(.tile_labels(cube), names(rules)))
        # Classify the data
        class_tile <- .reclassify_tile(
            tile = tile, mask = mask, band = "class", labels = labels,
            reclassify_fn = reclassify_fn, output_dir = output_dir,
            version = version
        )
        return(class_tile)
    }, mask = mask)
    return(class_cube)
}

#---- internal functions ----

.reclassify_tile <- function(tile, mask, band, labels, reclassify_fn,
                             output_dir, version) {
    # Output files
    out_file <- .file_derived_name(
        tile = tile, band = band, version = version, output_dir = output_dir
    )
    # Resume feature
    if (file.exists(out_file)) {
        # # Callback final tile classification
        # .callback(process = "tile_classification", event = "recovery",
        #           context = environment())
        message("Recovery: tile '", tile[["tile"]], "' already exists.")
        message("(If you want to produce a new image, please ",
                "change 'output_dir' or 'version' parameters)")
        class_tile <- .tile_class_from_file(
            file = out_file, band = band, base_tile = tile
        )
        return(class_tile)
    }
    # Create chunks as jobs
    chunks <- .tile_chunks_create(tile = tile, overlap = 0)
    # start parallel process
    block_files <- .jobs_map_parallel_chr(chunks, function(chunk) {
        # Get job block
        block <- .block(chunk)
        # Output file name
        block_file <- .file_block_name(
            pattern = .file_pattern(out_file), block = block,
            output_dir = output_dir
        )
        # Output mask file name
        mask_block_file <- .file_block_name(
            pattern = .file_pattern(out_file, suffix = "_mask"),
            block = block, output_dir = output_dir
        )
        # If there is any mask file delete it
        unlink(mask_block_file)
        # Resume processing in case of failure
        if (.raster_is_valid(block_file)) {
            return(block_file)
        }
        # Project mask block to template block
        # Get band conf missing value
        band_conf <- .conf_derived_band(
            derived_class = "class_cube", band = band
        )
        # Create template block for mask
        .gdal_template_block(
            block = block, bbox = .bbox(chunk), file = mask_block_file,
            nlayers = 1, miss_value = .miss_value(band_conf),
            data_type = .data_type(band_conf)
        )
        # Copy values from mask cube into mask template
        .gdal_merge_into(
            file = mask_block_file,
            base_files = .fi_paths(.fi(mask)), multicores = 1
        )
        # Build a new tile for mask based on template
        mask_tile <- .tile_class_from_file(
            file = mask_block_file, band = "class", .tile(mask)
        )
        # Read and preprocess values
        values <- .tile_read_block(
            tile = tile, band = .tile_bands(tile), block = block
        )
        # Read and preprocess values of mask block
        mask_values <- .tile_read_block(
            tile = mask_tile, band = .tile_bands(mask_tile), block = NULL
        )
        # Evaluate expressions
        values <- reclassify_fn(values = values, mask_values = mask_values)
        offset <- .offset(band_conf)
        if (!is.null(offset) && offset != 0) {
            values <- values - offset
        }
        scale <- .scale(band_conf)
        if (!is.null(scale) && scale != 1) {
            values <- values / scale
        }
        # Prepare and save results as raster
        .raster_write_block(
            files = block_file, block = block, bbox = .bbox(chunk),
            values = values, data_type = .data_type(band_conf),
            missing_value = .miss_value(band_conf),
            crop_block = NULL
        )
        # Delete unneeded mask block file
        unlink(mask_block_file)
        # Free memory
        gc()
        # Returned value
        block_file
    })
    # Merge blocks into a new class_cube tile
    class_tile <- .tile_class_merge_blocks(
        file = out_file, band = band, labels = labels, base_tile = tile,
        block_files = block_files, multicores = .jobs_multicores()
    )
    # Return class tile
    class_tile
}

.reclassify_fn_expr <- function(rules, labels_cube, labels_mask) {
    # Check if rules are named
    if (!all(.has_name(rules))) {
        stop("rules should be named")
    }
    # Get output labels
    labels <- unique(c(labels_cube, names(rules)))
    # Define reclassify function
    reclassify_fn <- function(values, mask_values) {
        # Check compatibility
        if (!all(dim(values) == dim(mask_values))) {
            stop("cube and mask values have different sizes")
        }
        # Used to check values (below)
        input_pixels <- nrow(values)
        # New evaluation environment
        env <- list2env(list(
            # Read values and convert to character
            cube = labels_cube[values], mask = labels_mask[mask_values]
        ))
        # Get values as character
        values <- env[["cube"]]
        # Evaluate each expression
        for (label in names(rules)) {
            # Get expression
            expr <- rules[[label]]
            # Evaluate
            result <- eval(expr, envir = env)
            # Update values
            if (!is.logical(result)) {
                stop("expression should evaluate to logical values")
            }
            values[result] <- label
        }
        # Get values as numeric
        values <- matrix(data = match(values, labels), nrow = input_pixels)
        # Mask NA values
        values[is.na(env[["mask"]])] <- NA
        # Are the results consistent with the data input?
        .check_processed_values(values, input_pixels)
        # Return values
        values
    }
    # Return closure
    reclassify_fn
}
