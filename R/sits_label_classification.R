#' @title Post-process a classified data raster probs to obtain a labelled image
#'
#' @name  sits_label_classification
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Takes a set of classified raster layers with probabilities,
#'              and label them based on the maximum probability for each pixel.
#'
#' @param  cube              Classified image data cube.
#' @param  multicores        Number of process to label the classification in
#'                           snow subprocess.
#' @param  memsize           Maximum overall memory (in GB) to label the
#'                           classification.
#' @param  output_dir        Output directory where to out the file
#' @param  version           Version of resulting image
#'                           (in the case of multiple tests)
#' @return A data cube
#' @examples
#' \dontrun{
#' # Retrieve the samples for Mato Grosso
#' # select band "ndvi"
#' samples_ndvi <- sits_select(samples_modis_4bands, bands = "NDVI")
#'
#' # select a random forest model
#' rfor_model <- sits_train(samples_ndvi, sits_rfor(num_trees = 500))
#'
#' # create a data cube based on the information about the files
#' data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#' cube <- sits_cube(
#'     source = "BDC",
#'     collection = "MOD13Q1-6",
#'     data_dir = data_dir,
#'     delim = "_",
#'     parse_info = c("X1", "X2", "tile", "band", "date")
#' )
#'
#' # classify the raster image
#' probs_cube <- sits_classify(cube,
#'     ml_model = rfor_model,
#'     output_dir = tempdir(),
#'     memsize = 4, multicores = 2
#' )
#'
#' # label the classification
#' label_cube <- sits_label_classification(probs_cube, output_dir = tempdir())
#' }
#'
#' @export
sits_label_classification <- function(cube,
                                      multicores = 1,
                                      memsize = 1,
                                      output_dir = ".",
                                      version = "v1") {

    # set caller to show in errors
    .check_set_caller("sits_label_classification")

    # precondition - check if cube has probability data
    .check_that(
        x = inherits(cube, "probs_cube"),
        msg = "input is not probability cube"
    )

    # mapping function to be executed by workers cluster
    .do_map <- function(chunk) {

        # read raster
        data <- .raster_get_values(r_obj = chunk)

        # get layer of max probability
        data <- apply(data, 1, which.max)

        # create cube labels
        res <- .raster_rast(r_obj = chunk, nlayers = 1)

        # copy values
        res <- .raster_set_values(r_obj = res, values = data)

        return(res)
    }

    # process each brick layer (each tile) individually
    label_cube <- slider::slide_dfr(cube, function(row) {

        # create metadata for labeled raster cube
        row_label <- .cube_probs_label(
            cube       = row,
            output_dir = output_dir,
            ext        = "class",
            version    = version
        )

        .sits_smooth_map_layer(
            cube = row,
            cube_out = row_label,
            overlapping_y_size = 0,
            func = .do_map,
            multicores = multicores,
            memsize = memsize,
            gdal_datatype = .raster_gdal_datatype(.config_get("class_cube_data_type")),
            gdal_options = .config_gtiff_default_options()
        )

        return(row_label)
    })

    class(label_cube) <- unique(c("classified_image", class(label_cube)))

    return(label_cube)
}
