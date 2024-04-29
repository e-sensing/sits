#' @title Reclassify a classified cube
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
#' @param cube        Image cube to be reclassified (class = "class_cube")
#' @param mask        Image cube with additional information
#'                    to be used in expressions (class = "class_cube").
#' @param rules       Expressions to be evaluated (named list).
#' @param memsize     Memory available for classification in GB
#'                    (integer, min = 1, max = 16384).
#' @param multicores  Number of cores to be used for classification
#'                    (integer, min = 1, max = 2048).
#' @param output_dir  Directory where files will be saved
#'                    (character vector of length 1 with valid location).
#' @param version    Version of resulting image (character).
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
#' @return An object of class "class_cube" (reclassified cube).
#'
#' @examples
#' if (sits_run_examples()) {
#' # Open mask map
#' data_dir <- system.file("extdata/raster/prodes", package = "sits")
#' prodes2021 <- sits_cube(
#'     source = "USGS",
#'     collection = "LANDSAT-C2L2-SR",
#'     data_dir = data_dir,
#'     parse_info = c(
#'         "X1", "X2", "tile", "start_date", "end_date",
#'         "band", "version"
#'     ),
#'     bands = "class",
#'     version = "v20220606",
#'     labels = c("1" = "Forest", "2" = "Water", "3" = "NonForest",
#'                "4" = "NonForest2", "6" = "d2007", "7" = "d2008",
#'                "8" = "d2009", "9" = "d2010", "10" = "d2011",
#'                "11" = "d2012", "12" = "d2013", "13" = "d2014",
#'                "14" = "d2015", "15" = "d2016", "16" = "d2017",
#'                "17" = "d2018", "18" = "r2010", "19" = "r2011",
#'                "20" = "r2012", "21" = "r2013", "22" = "r2014",
#'                "23" = "r2015", "24" = "r2016", "25" = "r2017",
#'                "26" = "r2018", "27" = "d2019", "28" = "r2019",
#'                "29" = "d2020", "31" = "r2020", "32" = "Clouds2021",
#'                "33" = "d2021", "34" = "r2021"),
#'     progress = FALSE
#' )
#' #' Open classification map
#' data_dir <- system.file("extdata/raster/classif", package = "sits")
#' ro_class <- sits_cube(
#'     source = "MPC",
#'     collection = "SENTINEL-2-L2A",
#'     data_dir = data_dir,
#'     parse_info = c(
#'         "X1", "X2", "tile", "start_date", "end_date",
#'         "band", "version"
#'     ),
#'     bands = "class",
#'     labels = c(
#'         "1" = "ClearCut_Fire", "2" = "ClearCut_Soil",
#'         "3" = "ClearCut_Veg", "4" = "Forest"
#'     ),
#'     progress = FALSE
#' )
#' # Reclassify cube
#' ro_mask <- sits_reclassify(
#'     cube = ro_class,
#'     mask = prodes2021,
#'     rules = list(
#'         "Old_Deforestation" = mask %in% c(
#'             "d2007", "d2008", "d2009",
#'             "d2010", "d2011", "d2012",
#'             "d2013", "d2014", "d2015",
#'             "d2016", "d2017", "d2018",
#'             "r2010", "r2011", "r2012",
#'             "r2013", "r2014", "r2015",
#'             "r2016", "r2017", "r2018",
#'             "d2019", "r2019", "d2020",
#'             "r2020", "r2021"
#'         ),
#'         "Water_Mask" = mask == "Water",
#'         "NonForest_Mask" = mask %in% c("NonForest", "NonForest2")
#'     ),
#'     memsize = 4,
#'     multicores = 2,
#'     output_dir = tempdir(),
#'     version = "ex_reclassify"
#' )
#' }
#'
#' @export
sits_reclassify <- function(cube,
                            mask,
                            rules,
                            memsize = 4L,
                            multicores = 2L,
                            output_dir,
                            version = "v1") {
    .check_set_caller("sits_reclassify")
    # Pre-conditions - Check parameters
    .check_na_null_parameter(cube)
    .check_na_null_parameter(mask)
    # check cube
    .check_is_class_cube(cube)
    .check_raster_cube_files(cube)
    # # check mask
    .check_that(inherits(mask, "class_cube"))
    .check_raster_cube_files(mask)
    UseMethod("sits_reclassify", cube)
}

#' @rdname sits_reclassify
#' @export
sits_reclassify.class_cube <- function(cube,
                                       mask,
                                       rules,
                                       memsize = 4L,
                                       multicores = 2L,
                                       output_dir,
                                       version = "v1") {
    # Preconditions
    # check other params
    .check_int_parameter(memsize, min = 1, max = 16384)
    .check_int_parameter(multicores, min = 1, max = 2048)
    .check_output_dir(output_dir)
    version <- .check_version(version)

    # Get block size
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(cube)))
    # Check minimum memory needed to process one block
    job_memsize <- .jobs_memsize(
        job_size = .block_size(block = block, overlap = 0),
        npaths = 2,
        nbytes = 8, proc_bloat = .conf("processing_bloat_cpu")
    )
    # Update multicores parameter
    multicores <- .jobs_max_multicores(
        job_memsize = job_memsize,
        memsize = memsize,
        multicores = multicores
    )
    # Prepare parallelization
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop(), add = TRUE)
    # Capture expression
    rules <- as.list(substitute(rules, environment()))[-1]
    # Reclassify parameters checked in reclassify function
    # Create reclassification function
    reclassify_fn <- .reclassify_fn_expr(
        rules = rules,
        labels_cube = unlist(.cube_labels(cube, dissolve = FALSE)),
        labels_mask = unlist(.cube_labels(mask, dissolve = FALSE))
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
        labels <- .reclassify_new_labels(cube, rules)
        # Classify the data
        class_tile <- .reclassify_tile(
            tile = tile,
            mask = mask,
            band = "class",
            labels = labels,
            reclassify_fn = reclassify_fn,
            output_dir = output_dir,
            version = version
        )
        return(class_tile)
    }, mask = mask)
    class(class_cube) <- c("class_cube", class(class_cube))
    return(class_cube)
}
#' @rdname sits_reclassify
#' @export
sits_reclassify.default <- function(cube, mask, rules, memsize,
                                    multicores, output_dir, version = "v1") {
    stop(.conf("messages", "sits_reclassify"))
}
