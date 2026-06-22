#' @title Reclassify a classified cube
#' @name sits_reclassify
#'
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description
#' Reclassify a classification cube using a set of named expressions.
#'
#' For cubes of class \code{"class_cube"}, expressions relabel pixels based on
#' logical conditions that may combine information from the classified cube and
#' an optional mask cube.
#'
#' For \code{"probs_cube"} and \code{"probs_vector_cube"}, expressions are used
#' to group input labels into new labels by aggregating probabilities (summing
#' probabilities of the selected input labels).
#'
#' @param cube        Image cube to be reclassified (class \code{"class_cube"},
#'                    \code{"probs_cube"}, or \code{"probs_vector_cube"}).
#' @param ...         Other parameters for specific methods.
#' @param mask        Image cube with additional information to be used in
#'                    expressions (class \code{"class_cube"}). Used only for
#'                    \code{"class_cube"} reclassification.
#' @param rules       Expressions to be evaluated (named list).
#'                    For \code{"class_cube"}, expressions must evaluate to
#'                    logical and may refer to \code{cube} and \code{mask}.
#'                    For \code{"probs_cube"} and \code{"probs_vector_cube"},
#'                    each named rule selects one or more input labels (for
#'                    example using \code{cube \%in\% c(...)}). The
#'                    probabilities of the selected labels are summed to
#'                    produce the new label given by the rule name.
#' @param exclude_mask_na Should cube pixels be set to \code{NA} when \code{NA}
#'                    values are found in mask pixels? (logical, default TRUE).
#'                    Used only for \code{"class_cube"}.
#' @param memsize     Memory available for processing in GB
#'                    (integer, min = 1, max = 16384).
#' @param multicores  Number of cores to be used for processing
#'                    (integer, min = 1, max = 2048).
#' @param output_dir  Directory where files will be saved
#'                    (character vector of length 1 with valid location).
#' @param version     Version of resulting image (character).
#' @param progress    Show progress bar? (logical).
#'
#' @note
#' For \code{"class_cube"}, reclassification changes the class assigned to
#' each pixel based on user-defined rules. Users should refer to \code{cube}
#' and \code{mask} to construct logical expressions. Expressions are evaluated
#' sequentially on the original classified values; later rules override
#' earlier ones.
#'
#' For \code{"probs_cube"} and \code{"probs_vector_cube"}, reclassification is
#' intended to group classes by combining probabilities. Each named rule
#' defines a new output label. For each pixel, the probabilities of the
#' selected input labels are summed and assigned to the corresponding output
#' label. Rules are evaluated on the original probability layers.
#'
#' @return
#' An object of the same type as \code{cube}:
#' \code{"class_cube"} for label cubes, or \code{"probs_cube"} and
#' \code{"probs_vector_cube"} for probability cubes and probability vector
#' cubes, respectively.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # Example for probs_cube: group labels by summing probabilities
#'
#'     # Train a model
#'     rf_model <- sits_train(samples_modis_ndvi, ml_method = sits_rfor)
#'
#'     # Open a cube
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'
#'     # Classify cube
#'     probs_cube <- sits_classify(
#'         data = cube,
#'         ml_model = rf_model,
#'         output_dir = tempdir(),
#'         version = "classify"
#'     )
#'
#'     # Reclassify probs_cube
#'     probs_nat_veg <- sits_reclassify(
#'         cube = probs_cube,
#'         rules = list(
#'             "Cerrado" = cube %in% c("Cerrado", "Forest")
#'         ),
#'         output_dir = tempdir()
#'     )
#'     plot(probs_nat_veg)
#'
#'     # Example for label cube: replacement of labels
#'
#'     # Open mask map
#'     data_dir <- system.file("extdata/raster/prodes", package = "sits")
#'     prodes2021 <- sits_cube(
#'         source = "USGS",
#'         collection = "LANDSAT-C2L2-SR",
#'         data_dir = data_dir,
#'         parse_info = c(
#'             "X1", "X2", "tile", "start_date", "end_date",
#'             "band", "version"
#'         ),
#'         bands = "class",
#'         version = "v20220606",
#'         labels = c(
#'             "1" = "Forest", "2" = "Water", "3" = "NonForest",
#'             "4" = "NonForest2", "6" = "d2007", "7" = "d2008",
#'             "8" = "d2009", "9" = "d2010", "10" = "d2011",
#'             "11" = "d2012", "12" = "d2013", "13" = "d2014",
#'             "14" = "d2015", "15" = "d2016", "16" = "d2017",
#'             "17" = "d2018", "18" = "r2010", "19" = "r2011",
#'             "20" = "r2012", "21" = "r2013", "22" = "r2014",
#'             "23" = "r2015", "24" = "r2016", "25" = "r2017",
#'             "26" = "r2018", "27" = "d2019", "28" = "r2019",
#'             "29" = "d2020", "31" = "r2020", "32" = "Clouds2021",
#'             "33" = "d2021", "34" = "r2021"
#'         ),
#'         progress = FALSE
#'     )
#'     #' Open classification map
#'     data_dir <- system.file("extdata/raster/classif", package = "sits")
#'     ro_class <- sits_cube(
#'         source = "MPC",
#'         collection = "SENTINEL-2-L2A",
#'         data_dir = data_dir,
#'         parse_info = c(
#'             "X1", "X2", "tile", "start_date", "end_date",
#'             "band", "version"
#'         ),
#'         bands = "class",
#'         labels = c(
#'             "1" = "ClearCut_Fire", "2" = "ClearCut_Soil",
#'             "3" = "ClearCut_Veg", "4" = "Forest"
#'         ),
#'         progress = FALSE
#'     )
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
#'         memsize = 4,
#'         multicores = 2,
#'         output_dir = tempdir(),
#'         version = "ex_reclassify"
#'     )
#' }
#'
#' @export
sits_reclassify <- function(cube, ...) {
    .check_set_caller("sits_reclassify")
    UseMethod("sits_reclassify", cube)
}

#' @rdname sits_reclassify
#' @export
sits_reclassify.class_cube <- function(cube, ...,
                                       mask = NULL,
                                       rules,
                                       exclude_mask_na = TRUE,
                                       memsize = 4L,
                                       multicores = 2L,
                                       output_dir,
                                       version = "v1",
                                       progress = TRUE) {
    # Preconditions
    .check_raster_cube_files(cube)
    # # check mask
    if (is.null(mask)) mask <- cube
    .check_that(inherits(mask, "class_cube"))
    .check_raster_cube_files(mask)
    # check other params
    .check_int_parameter(memsize, min = 1L, max = 16384L)
    .check_int_parameter(multicores, min = 1L, max = 2048L)
    .check_output_dir(output_dir)
    # Check version and progress
    version <- .message_version(version)
    progress <- .message_progress(progress)

    # The following functions define optimal parameters for parallel processing
    #
    # Get block size
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(cube)))
    # Check minimum memory needed to process one block
    job_block_memsize <- .jobs_block_memsize(
        block_size = .block_size(block = block, overlap = 0L),
        npaths = 3L,
        nbytes = 4L,
        proc_bloat = .conf("processing_bloat")
    )
    # Update multicores parameter
    multicores <- .jobs_max_multicores(
        job_block_memsize = job_block_memsize,
        memsize = memsize,
        multicores = multicores
    )
    # Update block parameter based on the size of memory and number of cores
    block <- .jobs_optimal_block(
        job_block_memsize = job_block_memsize,
        block = block,
        image_size = .tile_size(.tile(cube)),
        memsize = memsize,
        multicores = multicores
    )
    # Prepare parallel processing
    if (.parallel_start(workers = multicores)) {
        on.exit(.parallel_stop(), add = TRUE)
    }
    # Capture expression
    rules <- as.list(substitute(rules, environment()))[-1L]
    # Reclassify parameters checked in reclassify function
    # Create reclassification function
    reclassify_fn <- .recl_labels_expr_fn(
        rules = rules,
        labels_cube = .cube_labels(cube),
        labels_mask = .cube_labels(mask),
        exclude_mask_na = exclude_mask_na
    )
    # Filter mask - bands
    mask <- .cube_filter_bands(cube = mask, bands = "class")
    # Process each tile sequentially
    class_cube <- .cube_foreach_tile(cube, function(tile, mask) {
        # Filter mask - spatial
        mask <- .try(
            {
                .cube_filter_spatial(cube = mask, roi = .tile_bbox(tile))
            },
            .msg_error = .conf("messages", "sits_reclassify_mask_intersect")
        )
        # Get new labels from cube and pre-defined rules from user
        cube_labels <- .recl_labels_new(cube, rules)
        # Classify the data
        .recl_labels_tile(
            tile = tile,
            mask = mask,
            band = "class",
            labels = cube_labels,
            reclassify_fn = reclassify_fn,
            block = block,
            multicores = multicores,
            memsize = memsize,
            output_dir = output_dir,
            version = version,
            progress = progress
        )
    }, mask = mask)
    class(class_cube) <- c("class_cube", class(class_cube))
    return(class_cube)
}

#' @rdname sits_reclassify
#' @export
sits_reclassify.probs_cube <- function(cube, ...,
                                       rules,
                                       memsize = 4L,
                                       multicores = 2L,
                                       output_dir,
                                       version = "v1",
                                       progress = TRUE) {
    # Preconditions
    .check_raster_cube_files(cube)
    # Capture expression
    rules <- as.list(substitute(rules, environment()))[-1L]
    .check_reclassify_probs_rules(cube, rules)
    # check other params
    .check_int_parameter(memsize, min = 1L, max = 16384L)
    .check_int_parameter(multicores, min = 1L, max = 2048L)
    .check_output_dir(output_dir)
    # Check version and progress
    version <- .message_version(version)
    progress <- .message_progress(progress)
    # Get new labels from cube and pre-defined rules from user
    cube_labels <- .recl_probs_new(
        rules = rules,
        labels_cube = .cube_labels(cube)
    )
    # The following functions define optimal parameters for parallel processing
    #
    # Get block size
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(cube)))
    # Check minimum memory needed to process one block
    job_block_memsize <- .jobs_block_memsize(
        block_size = .block_size(block = block, overlap = 0L),
        npaths = length(.cube_labels(cube)) + length(cube_labels) + 1L,
        nbytes = 4L,
        proc_bloat = .conf("processing_bloat")
    )
    # Update multicores parameter
    multicores <- .jobs_max_multicores(
        job_block_memsize = job_block_memsize,
        memsize = memsize,
        multicores = multicores
    )
    # Update block parameter based on the size of memory and number of cores
    block <- .jobs_optimal_block(
        job_block_memsize = job_block_memsize,
        block = block,
        image_size = .tile_size(.tile(cube)),
        memsize = memsize,
        multicores = multicores
    )
    # Prepare parallel processing
    if (.parallel_start(workers = multicores)) {
        on.exit(.parallel_stop(), add = TRUE)
    }
    # Reclassify parameters checked in reclassify function
    # Create reclassification function
    reclassify_fn <- .recl_probs_expr_fn(
        rules = rules,
        labels_cube = .cube_labels(cube)
    )
    # Process each tile sequentially
    probs_cube <- .cube_foreach_tile(cube, function(tile) {
        # Classify the data
        .recl_probs_tile(
            tile = tile,
            band = "probs",
            labels = cube_labels,
            reclassify_fn = reclassify_fn,
            block = block,
            multicores = multicores,
            memsize = memsize,
            output_dir = output_dir,
            version = version,
            progress = progress
        )
    })
    class(probs_cube) <- c("probs_cube", class(probs_cube))
    return(probs_cube)
}

#' @rdname sits_reclassify
#' @export
sits_reclassify.default <- function(cube, ...) {
    stop(.conf("messages", "sits_reclassify"))
}
