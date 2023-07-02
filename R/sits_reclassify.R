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
#'         parse_info = c(
#'             "X1", "X2", "tile", "start_date", "end_date",
#'             "band", "version"
#'         ),
#'         bands = "class",
#'         labels = c(
#'             "1" = "Forest", "2" = "Water", "3" = "NonForest",
#'             "4" = "NonForest2", "6" = "d2007", "7" = "d2008",
#'             "8" = "d2009", "9" = "d2010", "10" = "d2011", "11" = "d2012",
#'             "12" = "d2013", "13" = "d2014", "14" = "d2015", "15" = "d2016",
#'             "16" = "d2017", "17" = "d2018", "18" = "r2010", "19" = "r2011",
#'             "20" = "r2012", "21" = "r2013", "22" = "r2014", "23" = "r2015",
#'             "24" = "r2016", "25" = "r2017", "26" = "r2018", "27" = "d2019",
#'             "28" = "r2019", "29" = "d2020", "31" = "r2020",
#'             "32" = "Clouds2021", "33" = "d2021", "34" = "r2021"
#'         ),
#'         version = "v20220606"
#'     )
#'
#'     # Open classification map
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
#'             "1" = "ClearCut_Burn", "2" = "ClearCut_Soil",
#'             "3" = "ClearCut_Veg", "4" = "Forest"
#'         )
#'     )
#'
#'     # Reclassify cube
#'     ro_mask <- sits_reclassify(
#'         cube = ro_class,
#'         mask = prodes2021,
#'         rules = list(
#'             "Deforestation_Mask" = mask %in% c(
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
#'             "Water" = mask == "Water",
#'             "NonForest" = mask %in% c("NonForest", "NonForest2")
#'         ),
#'         memsize = 4,
#'         multicores = 2,
#'         output_dir = tempdir(),
#'         version = "v2"
#'     )
#'
#'     plot(ro_mask)
#' }
#' @rdname sits_reclassify
#' @export
sits_reclassify <- function(cube,
                            mask,
                            rules,
                            memsize = 4,
                            multicores = 2,
                            output_dir,
                            version = "v1") {
    # Pre-conditions - Check parameters
    .check_cube_is_class_cube(cube)
    .check_cube_is_class_cube(mask)
    .check_memsize(memsize)
    .check_multicores(multicores)
    .check_output_dir(output_dir)
    .check_version(version)
    # Get block size
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(cube)))
    # Check minimum memory needed to process one block
    job_memsize <- .jobs_memsize(
        job_size = .block_size(block = block, overlap = 0),
        npaths = 2,
        nbytes = 8, proc_bloat = .conf("processing_bloat")
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

    UseMethod("sits_reclassify", cube)
}

#' @rdname sits_reclassify
#' @export
sits_reclassify.class_cube <- function(cube,
                                       mask,
                                       rules,
                                       memsize = 4,
                                       multicores = 2,
                                       output_dir,
                                       version = "v1") {
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
            .msg_error = "mask roi does not intersect cube"
        )
        # Get output labels
        labels <- unique(c(.cube_labels(cube), names(rules)))
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
    return(class_cube)
}
