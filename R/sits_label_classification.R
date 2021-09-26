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
#'     source = "LOCAL",
#'     name = "sinop_2014",
#'     origin = "BDC",
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
                                      output_dir = tempdir(),
                                      version = "v1") {

    # set caller to show in errors
    .check_set_caller("sits_label_classification")

    # precondition - check if cube has probability data
    .check_that(
        x = inherits(cube, "probs_cube"),
        msg = "input is not probability cube"
    )

    # create metadata for labeled raster cube
    label_cube <- .sits_label_cube(
        probs_cube = cube,
        output_dir = output_dir,
        version = version
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
    .sits_smooth_map_layer(
        cube = cube,
        cube_out = label_cube,
        overlapping_y_size = 0,
        func = .do_map,
        multicores = multicores,
        memsize = memsize,
        gdal_datatype = .raster_gdal_datatype("INT1U"),
        gdal_options = .config_gtiff_default_options()
    )

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
#' samples_ndvi <- sits_select(samples_modis_4bands, bands = "NDVI")
#'
#' # select a random forest model
#' rfor_model <- sits_train(samples_ndvi, sits_rfor(num_trees = 500))
#'
#' # Classify a raster file with 23 instances for one year
#' data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#' # create a data cube based on the information about the files
#' cube <- sits_cube(
#'     source = "LOCAL",
#'     name = "sinop-2014",
#'     origin = "BDC",
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

    # set caller to show in errors
    .check_set_caller("sits_label_majority")

    # precondition 1 - check if cube has classification info
    .check_that(
        x = inherits(cube, "classified_image"),
        msg = "input is not classified cube"
    )

    # precondition 2 - test window size
    .check_num(
        x = window_size,
        min = 3,
        msg = "window size must be >= 3"
    )

    cube_maj <- .sits_cube_clone(cube = cube,
                                 name = paste0(cube$name, "_maj"),
                                 ext = "_maj",
                                 output_dir = output_dir,
                                 version = version)

    # retrieve the files to be read and written
    in_files <- cube$file_info[[1]]$path
    out_files <- cube_maj$file_info[[1]]$path

    purrr::map2(in_files, out_files, function(in_file, out_file) {

        # read the input classified image
        r_obj <- .raster_open_rast(in_file)

        # calculate the majority values
        r_obj <- .raster_focal(
            r_obj = r_obj,
            window_size = window_size,
            fn = "modal"
        )

        # write the result
        .raster_write_rast(
            r_obj = r_obj,
            file = out_file,
            format = "GTiff",
            data_type = .raster_data_type("INT1U"),
            gdal_options = .config_gtiff_default_options(),
            overwrite = TRUE
        )

        # was the file written correctly?
        .check_file(
            x = out_file,
            msg = "unable to save raster object"
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
        labels <- sits_labels(probs_row)

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
            .[seq_along(.) - 1] %>%
            paste0(collapse = "_") %>%
            paste0(output_dir, "/", ., "_", version, ".tif")

        # get the file information
        file_info <- tibble::tibble(
            band = band,
            start_date = start_date,
            end_date = end_date,
            path = file_name
        )

        # create a new RasterLayer for a defined period and generate metadata
        label_row <- .sits_cube_create(
            name    = name,
            source = "CLASSIFIED",
            satellite = probs_row$satellite,
            sensor    = probs_row$sensor,
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
