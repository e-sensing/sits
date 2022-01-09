#' @title Estimate classification uncertainty based on probs cube
#'
#' @name  sits_uncertainty
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#'
#' @param  cube              Probability data cube
#' @param  type              Method to measure uncertainty
#' @param  ...               Parameters for specific functions
#' @param  multicores        Number of cores to run the function
#' @param  memsize           Maximum overall memory (in GB) to run the
#'                           function.
#' @param  output_dir        Output directory for image files
#' @param  version           Version of resulting image
#'                           (in the case of multiple tests)
#' @return An uncertainty data cube
#'
#' @export
sits_uncertainty <- function(cube, type = "entropy", ...,
                             multicores = 2,
                             memsize = 8,
                             output_dir = ".",
                             version = "v1") {
    # set caller to show in errors
    .check_set_caller("sits_uncertainty")

    if (!requireNamespace("parallel", quietly = TRUE)) {
        stop("Please install package parallel.", call. = FALSE)
    }

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
                                     multicores = 2,
                                     memsize = 8,
                                     output_dir = ".",
                                     version = "v1"){
    # precondition 1 - check if cube has probability data
    .check_that(
        x = inherits(cube, "probs_cube"),
        msg = "input is not probability cube"
    )

    # precondition 2 - multicores
    .check_num(x = multicores,
               len_max = 1,
               min = 1,
               allow_zero = FALSE,
               msg = "multicores must be at least 1")

    # precondition 3 - memory
    .check_num(x = memsize,
               len_max = 1,
               min = 1,
               allow_zero = FALSE,
               msg = "memsize must be positive")

    # precondition 4 - output dir
    .check_file(x = output_dir,
                msg = "invalid output dir")

    # precondition 5 - version
    .check_chr(x = version,
               len_min = 1,
               msg = "invalid version")

    # find out how many labels exist
    n_labels <- length(sits_labels(cube[1, ]))

    # Bayesian smoother to be executed by workers cluster
    .do_entropy <- function(chunk) {

        data <- .raster_get_values(r_obj = chunk)

        # process entropy
        unc <- entropy_probs(data, n_labels)

        # create cube
        res <- .raster_rast(r_obj = chunk,
                            nlayers = 1)

        # copy values
        res <- .raster_set_values(r_obj = res,
                                  values = unc)

        return(res)
    }
    # process each tile individually
    cube_uncert <- slider::slide_dfr(cube, function(tile) {

        # create metadata for raster cube
        tile_uncert <- .cube_derived_create(
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


        .sits_smooth_map_layer(
            cube = tile,
            cube_out = tile_uncert,
            overlapping_y_size = 0,
            func = .do_entropy,
            multicores = multicores,
            memsize = memsize,
            gdal_datatype = .raster_gdal_datatype(.config_get("probs_cube_data_type")),
            gdal_options = .config_gtiff_default_options()
        )

        return(tile_uncert)
    })

    class(cube_uncert) <- c("uncertainty_cube", "raster_cube", "sits_cube",
                            "tbl_df", "tbl", "data.frame")

    return(cube_uncert)
}
