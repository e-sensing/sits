#' @title Post-process a classified data raster probs to obtain a labelled image
#'
#' @name  sits_label_classification
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Takes a set of classified raster layers with probabilities,
#'              and label them based on the maximum probability for each pixel.
#'
#' @param  cube              Classified image data cube.
#' @param  smoothing         (deprecated)
#' @param  output_dir        Output directory where to out the file
#' @param  version           Version of resulting image
#'                           (in the case of multiple tests)
#' @return A data cube
#' @examples
#' \dontrun{
#' # Retrieve the samples for Mato Grosso
#' # select band "ndvi"
#'
#' samples_ndvi <- sits_select(samples_mt_4bands, bands = "NDVI")
#'
#' # select a random forest model
#' rfor_model <- sits_train(samples_ndvi, sits_rfor(num_trees = 500))
#'
#' # Classify a raster file with 23 instances for one year
#' files <- c(system.file("extdata/raster/mod13q1/sinop-crop-ndvi.tif",
#'     package = "sits"
#' ))
#'
#' # create a data cube based on the information about the files
#' sinop <- sits_cube(
#'     type = "BRICK", satellite = "TERRA",
#'     sensor = "MODIS", name = "Sinop-crop",
#'     timeline = timeline_modis_392,
#'     output_dir = tempdir(),
#'     bands = c("NDVI"), files = files
#' )
#'
#' # classify the raster image
#' sinop_probs <- sits_classify(sinop,
#'     ml_model = rfor_model,
#'     output_dir = tempdir(),
#'     memsize = 4, multicores = 2
#' )
#'
#' # label the classification and smooth the result with a bayesian filter
#' sinop_label <- sits_label_classification(sinop_probs, output_dir = tempdir())
#' }
#'
#' @export
sits_label_classification <- function(cube,
                                      smoothing = NULL,
                                      output_dir = "./",
                                      version = "v1") {

    # Backwards compatibility
    if (!purrr::is_null(smoothing)) {
        message("to do bayesian smoothing, please use sits_smooth")
        message("please revise your script")
        stop()
    }
    # precondition - check if cube has probability data
    assertthat::assert_that("probs_cube" %in% class(cube),
        msg = "sits_label_classification: input is not probability cube"
    )
    # find out how many labels exist
    n_labels <- length(.sits_cube_labels(cube[1, ]))

    # create metadata for labeled raster cube
    cube_labels <- .sits_label_cube(
        cube_probs = cube,
        output_dir = output_dir,
        version = version
    )

    # define the extent to be read
    extent <- vector(mode = "integer", length = 4)
    names(extent) <- c("row", "nrows", "col", "ncols")

    # traverse all tiles
    slider::slide2(cube, cube_labels, function(cube_row, cube_labels_row) {

        # allocate matrix of probabilities
        cube_size <- cube_row$nrows * cube_row$ncols
        lab_values <- matrix(NA, nrow = cube_size, ncol = 1)

        # retrieve the files to be read and written
        in_files <- .sits_cube_files(cube_row)
        out_files <- .sits_cube_files(cube_labels_row)

        # traverse all years
        purrr::map2(in_files, out_files, function(in_file, out_file) {

            # read values from file
            t_obj <- terra::rast(in_file)
            data_values <- terra::values(t_obj)

            # select the best class by choosing the maximum value
            lab_values[] <- apply(data_values, 1, which.max)

            # write values into a file
            cube_labels <- .sits_raster_api_write(
                params = .sits_raster_api_params_cube(cube_row),
                num_layers = 1,
                values = lab_values,
                filename = out_file,
                datatype = "INT1U"
            )
        })
    })
    return(cube_labels)
}
#' @title Post-process a classified data raster with a majority filter
#'
#' @name  sits_label_majority
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Takes a set of classified raster layers with labels,
#'              and executes a majority filter for post-processing.
#'
#' @param  cube              Classified image data cube.
#' @param  window_size       Size of the window to apply majority filter
#' @param  output_dir        Output directory where to out the file
#' @param  version           Version of resulting image
#'                           (in the case of multiple tests)
#' @return A data cube with metadata information on post-processed data
#' @examples
#' \dontrun{
#' # Retrieve the samples for Mato Grosso
#' # select band "ndvi"
#'
#' samples_ndvi <- sits_select(samples_mt_4bands, bands = "NDVI")
#'
#' # select a random forest model
#' rfor_model <- sits_train(samples_ndvi, sits_rfor(num_trees = 500))
#'
#' # Classify a raster file with 23 instances for one year
#' files <- c(system.file("extdata/raster/mod13q1/sinop-crop-ndvi.tif",
#'     package = "sits"
#' ))
#'
#' # create a data cube based on the information about the files
#' sinop <- sits_cube(
#'     type = "BRICK", satellite = "TERRA",
#'     sensor = "MODIS", name = "Sinop-crop",
#'     timeline = timeline_modis_392,
#'     output_dir = tempdir(),
#'     bands = c("NDVI"), files = files
#' )
#'
#' # classify the raster image
#' sinop_probs <- sits_classify(sinop,
#'     ml_model = rfor_model,
#'     output_dir = tempdir(),
#'     memsize = 4, multicores = 2
#' )
#'
#' # label the classification and smooth the result with a bayesian filter
#' sinop_label <- sits_label_classification(sinop_probs, output_dir = tempdir())
#'
#' # smooth the result with a majority filter
#' sinop_label_maj <- sits_label_majority(sinop_label, output_dir = tempdir())
#'
#' }
#' @export
sits_label_majority <- function(cube,
                                window_size = 3,
                                output_dir = "./",
                                version = "v1") {

    # precondition 1 - check if cube has classification info
    assertthat::assert_that("classified_image" %in% class(cube),
            msg = "sits_label_majority: input is not classified cube"
    )

    # precondition 2 - test window size
    assertthat::assert_that(window_size >= 3,
            msg = "sits_label_majority: window size must be >= 3"
    )

    cube_maj <- .sits_cube_clone(cube = cube,
                                 name = paste0(cube$name,"_maj"),
                                 ext = "_maj",
                                 output_dir = output_dir,
                                 version = version)

    # retrieve the files to be read and written
    in_files <- .sits_cube_files(cube)
    out_files <- .sits_cube_files(cube_maj)

    purrr::map2(in_files, out_files,
        function(in_file, out_file) {
            # read the input classified image
            layer <- terra::rast(in_file)
            # calculate the majority values
            layer <- terra::focal(
                x = layer,
                w = window_size,
                na.rm = TRUE,
                fun = terra::modal
            )
        # write the result
            suppressWarnings(terra::writeRaster(
                layer,
                filename = out_file,
                wopt = list(
                    filetype = "GTiff",
                    datatype = "INT1U",
                    gdal = c("COMPRESS=LZW")
                ),
                overwrite = TRUE
            ))
        # was the file written correctly?
        assertthat::assert_that(file.info(out_file)$size > 0,
            msg = "sits_label_majority : unable to save raster object"
        )

    })
    class(cube_maj) <- class(cube)
    return(cube_maj)
}

#' @title Create a set of RasterLayer objects
#'        to store data cube classification results (labelled classes)
#' @name .sits_label_cube
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a tibble containing metadata about a data cube wuth
#' classification probabilites and and creates a
#' set of RasterLayers to store the classification result. Each RasterLayer is
#' to one time step. The time steps are specified in a list of dates.
#'
#' @param  cube_probs        Metadata about the input data cube (probability).
#' @param  output_dir        Output directory where to put the files
#' @param  version           Name of the version of the result
#' @return                   Metadata about the output RasterLayer objects.
.sits_label_cube <- function(cube_probs, output_dir, version) {

    labels_lst <- slider::slide(cube_probs, function(cube_probs_row) {

        # labels come from the input cube
        labels <- .sits_cube_labels(cube_probs_row)

        # how many objects are to be created?
        n_objs <- length(.sits_cube_files(cube_probs_row))

        # set scale factors, missing values, minimum and maximum values
        scale_factors <- rep(1, n_objs)
        missing_values <- rep(-9999, n_objs)
        minimum_values <- rep(1, n_objs)
        maximum_values <- rep(length(labels), n_objs)

        # name of the cube
        name <- paste0(cube_probs_row$name, "_class")

        # loop through the list of dates and create list of raster layers
        times_probs <- seq_len(n_objs) %>%
            purrr::map(function(i){
                # define the timeline for the raster data sets
                timeline <- cube_probs_row$timeline[[1]][[i]]
                start_date <- timeline[1]
                return(start_date)
            })
        bands <- seq_len(n_objs) %>%
            purrr::map(function(i){
                timeline <- cube_probs_row$timeline[[1]][[i]]
                band <- .sits_cube_class_band_name(
                    name = cube_probs_row$name,
                    type = "class",
                    start_date = timeline[1],
                    end_date = timeline[length(timeline)]
                )
                return(band)
            })
        # define the filename for the classified image
        files <- seq_len(n_objs) %>%
            purrr::map(function(i){
                timeline <- cube_probs_row$timeline[[1]][[i]]
                file <- .sits_raster_api_filename(
                    output_dir = output_dir,
                    version = version,
                    name = cube_probs_row$name,
                    type = "class",
                    start_date = timeline[1],
                    end_date = timeline[length(timeline)]
                )
                return(file)
            })

        # get the file information
        file_info <- .sits_raster_api_file_info(bands, times_probs, files)

        # create a new RasterLayer for a defined period and generate metadata
        cube_labels <- .sits_cube_create(
            type = "CLASSIFIED",
            satellite = cube_probs_row$satellite,
            sensor = cube_probs_row$sensor,
            name = name,
            bands = bands,
            labels = labels,
            scale_factors = scale_factors,
            missing_values = missing_values,
            minimum_values = minimum_values,
            maximum_values = maximum_values,
            timelines = cube_probs_row$timeline[[1]],
            nrows = cube_probs_row$nrows,
            ncols = cube_probs_row$ncols,
            xmin = cube_probs_row$xmin,
            xmax = cube_probs_row$xmax,
            ymin = cube_probs_row$ymin,
            ymax = cube_probs_row$ymax,
            xres = cube_probs_row$xres,
            yres = cube_probs_row$yres,
            crs = cube_probs_row$crs,
            file_info = file_info
        )

        return(cube_labels)
    })

    cube_labels <- dplyr::bind_rows(labels_lst)

    class(cube_labels) <- c("classified_image", "raster_cube",
                            class(cube_labels))
    return(cube_labels)
}
