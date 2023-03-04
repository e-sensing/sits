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
#'         labels = c("ClearCut_Burn", "ClearCut_Soil",
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
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(cube)))
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
        rules = rules,
        labels_cube = .cube_labels(cube),
        labels_mask = .cube_labels(mask)
    )
    # Filter mask - bands
    mask <- .cube_filter_bands(cube = mask, bands = "class")
    # Process each tile sequentially
    class_cube <- .cube_foreach_tile(cube, function(tile, mask) {
        # Filter mask - spatial
        mask <- .try({
            .cube_filter_spatial(cube = mask, roi = .tile_bbox(tile))
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
