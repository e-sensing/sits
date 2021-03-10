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
#' samples_ndvi <- sits_select(samples_mt_4bands, bands = "NDVI")
#'
#' # select a random forest model
#' rfor_model <- sits_train(samples_ndvi, sits_rfor(num_trees = 500))
#'
#' # create a data cube based on the information about the files
#' data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#' cube <- sits_cube(
#'     type = "STACK",
#'     name = "sinop_2014",
#'     satellite = "TERRA",
#'     sensor = "MODIS",
#'     data_dir = data_dir,
#'     delim = "_",
#'     parse_info = c("X1", "X2", "band", "date")
#' )
#'
#' # classify the raster image
#' probs_cube <- sits_classify(cube,
#'     ml_model = rfor_model,
#'     output_dir = tempdir(),
#'     memsize = 4, multicores = 2
#' )
#'
#' # label the classification and smooth the result with a bayesian filter
#' label_cube <- sits_label_classification(probs_cube, output_dir = tempdir())
#' }
#'
#' @export
sits_label_classification <- function(cube,
                                      smoothing = NULL,
                                      multicores = 1,
                                      memsize = 1,
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

    # create metadata for labeled raster cube
    label_cube <- .sits_label_cube(
        probs_cube = cube,
        output_dir = output_dir,
        version = version
    )

    # mapping function to be executed by workers cluster
    .do_map <- function(chunk) {

        # create cube smooth
        res <- raster::brick(chunk, nl = 1)
        res[] <- apply(unname(raster::values(chunk)), 1, which.max)
        return(res)
    }

    # process each brick layer (each tile) individually
    .sits_map_layer_cluster(cube = cube,
                            cube_out = label_cube,
                            overlapping_y_size = 0,
                            func = .do_map,
                            multicores = multicores,
                            memsize = memsize,
                            datatype = "INT1U",
                            options = c("COMPRESS=LZW",
                                        "BIGTIFF=YES"))

    return(label_cube)
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
#' cube <- sits_cube(
#'     type = "STACK",
#'     name = "sinop-2014",
#'     satellite = "TERRA",
#'     sensor = "MODIS",
#'     data_dir = data_dir,
#'     delim = "_",
#'     parse_info = c("X1", "X2", "band", "date")
#' )
#'
#' # classify the raster image
#' probs_cube <- sits_classify(cube,
#'     ml_model = rfor_model,
#'     output_dir = tempdir(),
#'     memsize = 4, multicores = 2
#' )
#'
#' # label the classification and smooth the result with a bayesian filter
#' label_cube <- sits_label_classification(probs_cube, output_dir = tempdir())
#'
#' # smooth the result with a majority filter
#' label_maj_cube <- sits_label_majority(label_cube, output_dir = tempdir())
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
    in_files <- cube$file_info[[1]]$path
    out_files <- cube_maj$file_info[[1]]$path

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
#' data cube to store the classification result.
#'
#' @param  probs_cube        Metadata about the input data cube (probability).
#' @param  output_dir        Output directory where to put the files
#' @param  version           Name of the version of the result
#' @return                   Metadata about the output data cube.
.sits_label_cube <- function(probs_cube, output_dir, version) {

    labels_lst <- slider::slide(probs_cube, function(probs_row) {

        # labels come from the input cube
        labels <- .sits_cube_labels(probs_row)
        # get timeline from input cube
        timeline <- sits_timeline(probs_row)

        # name of the cube
        name <- paste0(probs_row$name, "_class")

        # start and end dates
        start_date <- as.Date(probs_row$file_info[[1]]$start_date)
        end_date <- as.Date(probs_row$file_info[[1]]$end_date)
        # band
        band <- paste0(probs_row$name, "_class")

        # file name for the classified image
        # use the file name for the probs_row (includes dates)
        # replace "probs" by "class"
        # remove the extension
        # split the file names
        # get a vector of strings
        # remove the version information
        # include the version information from the function
        file_name <- probs_row$file_info[[1]]$path %>%
            basename() %>%
            gsub("probs", "class", .) %>%
            tools::file_path_sans_ext() %>%
            strsplit(split = "_") %>%
            unlist() %>%
            .[1:length(.) - 1] %>%
            paste0(collapse = "_") %>%
            paste0(output_dir,"/", ., "_", version, ".tif")

        # get the file information
        file_info <- tibble::tibble(
            band = band,
            start_date = start_date,
            end_date = end_date,
            path = file_name
        )

        # create a new RasterLayer for a defined period and generate metadata
        label_row <- .sits_cube_create(
            type = "CLASSIFIED",
            satellite = probs_row$satellite,
            sensor    = probs_row$sensor,
            name   = name,
            bands  = band,
            labels = labels,
            nrows = probs_row$nrows,
            ncols = probs_row$ncols,
            xmin  = probs_row$xmin,
            xmax  = probs_row$xmax,
            ymin  = probs_row$ymin,
            ymax  = probs_row$ymax,
            xres  = probs_row$xres,
            yres  = probs_row$yres,
            crs   = probs_row$crs,
            file_info = file_info
        )
        return(label_row)
    })

    label_cube <- dplyr::bind_rows(labels_lst)

    class(label_cube) <- c("classified_image", "raster_cube", class(label_cube))
    return(label_cube)
}
