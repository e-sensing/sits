#' @title Create sits cubes from cubes in flat files in a local
#' @name sits_cube.local_cube
#' @description
#' Creates data cubes based on files on local directory. Assumes users
#' have downloaded the data from a known cloud collection or the data
#' has been created by \code{sits}.
#'
#'
#' @param source       Data source: one of \code{"AWS"}, \code{"BDC"},
#'                     \code{"CDSE"}, \code{"DEAFRICA"}, \code{"DEAUSTRALIA"},
#'                     \code{"HLS"}, \code{"PLANETSCOPE"}, \code{"MPC"},
#'                     \code{"SDC"} or \code{"USGS"}. This is the source
#'                     from which the data has been downloaded.
#' @param collection   Image collection in data source.
#'                     To find out the supported collections,
#'                     use \code{\link{sits_list_collections}()}).
#' @param ...          Other parameters to be passed for specific types.
#' @param tiles        Tiles from the collection to be included in
#'                     the cube (see details below).
#' @param bands        Spectral bands and indices to be included
#'                     in the cube (optional).
#' @param start_date,end_date Initial and final dates to include
#'                     images from the collection in the cube (optional).
#'                     (Date in YYYY-MM-DD format).
#' @param data_dir     Local directory where images are stored.
#' @param parse_info   Parsing information for local files.
#' @param delim        Delimiter for parsing local files (default = "_")
#' @param multicores   Number of workers for parallel processing
#'                     (integer, min = 1, max = 2048).
#' @param progress     Logical: show a progress bar?
#' @return A \code{tibble} describing the contents of a data cube.

#' @note
#' To create a cube from local files, please inform:
#' \itemize{
#'  \item \code{source}: The data provider from which the data was
#'  downloaded (e.g, "BDC", "MPC");
#'  \item \code{collection}: The collection from which the data comes from.
#'  (e.g., \code{"SENTINEL-2-L2A"} for the Sentinel-2 MPC collection level 2A);
#'  \item \code{data_dir}: The local directory where the image files are stored.
#'  \item \code{parse_info}: Defines how to extract metadata from file names
#'  by specifying the order and meaning of each part, separated by the
#'  \code{"delim"} character. Default value is
#'  \code{c("X1", "X2", "tile", "band", "date")}.
#'  \item \code{delim}: The delimiter character used to separate components in
#'  the file names. Default is \code{"_"}.
#' }
#' Please ensure that local files meet the following requirements:
#'
#' \itemize{
#'  \item All image files must have the same spatial resolution and projection;
#'  \item Each file should represent a single image band for a single date;
#'  \item File names must include information about the \code{tile},
#'  \code{date}, and \code{band} in their names.
#'  \item{The \code{parse_info} parameter tells \code{sits} how to extract
#'          metadata from file names.}
#'  \item{By default the \code{parse_info} parameter is
#'          \code{c(satellite, sensor, tile, band, date)}.}
#' }
#' Example of supported file names are:
#' \itemize{
#'  \item \code{"CBERS-4_WFI_022024_B13_2021-05-15.tif"};
#'  \item \code{"SENTINEL-1_GRD_30TXL_VV_2023-03-10.tif"};
#'  \item \code{"LANDSAT-8_OLI_198030_B04_2020-09-12.tif"}.
#' }
#'
#' When you load a local data cube specifying the \code{source}
#' (e.g., AWS, MPC) and \code{collection}, \code{sits} assumes that the data
#' properties (e.g., scale factor, minimum, and maximum values) match those
#' defined for the selected provider. If you are working with
#' custom data from an unsupported source or data that does not follow the
#' standard definitions of providers in sits, refer to the Technical Annex of
#' the \code{sits} online book for guidance on handling such cases
#' (e-sensing.github.io/sitsbook/technical-annex.html).
#'
#' @examples
#' if (sits_run_examples()) {
#'     # --- Create a cube based on a local MODIS data
#'     # MODIS local files have names such as
#'     # "TERRA_MODIS_012010_NDVI_2013-09-14.jp2"
#'     # see the parse info parameter as an example on how to
#'     # decode local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     modis_cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir,
#'         parse_info = c("satellite", "sensor", "tile", "band", "date")
#'     )
#' }
#' @export
sits_cube.local_cube <- function(source,
                                 collection, ...,
                                 bands = NULL,
                                 tiles = NULL,
                                 start_date = NULL,
                                 end_date = NULL,
                                 data_dir,
                                 parse_info = c("X1", "X2", "tile", "band", "date"),
                                 delim = "_",
                                 multicores = 2L,
                                 progress = TRUE) {
    # set caller for error messages
    .check_set_caller("sits_cube_local_cube")
    # precondition - data directory must be provided
    .check_file(data_dir)
    # expanding the shortened paths since gdal functions do not work with them
    data_dir <- path.expand(data_dir)
    # deal with wrong parameter "band" in dots
    dots <- list(...)
    if ("band" %in% names(dots) && missing(bands)) {
        message("please, use 'bands' instead of 'band' as parameter")
        bands <- as.character(dots[["band"]])
    }
    .check_source(source = source)
    .check_source_collection(source = source, collection = collection)
    # show progress bar?
    progress <- .message_progress(progress)
    # builds a sits data cube
    cube <- .local_raster_cube(
        source = source,
        collection = collection,
        data_dir = data_dir,
        parse_info = parse_info,
        delim = delim,
        tiles = tiles,
        bands = bands,
        start_date = start_date,
        end_date = end_date,
        multicores = multicores,
        progress = progress, ...
    )
    # fix tile system name
    cube <- .cube_revert_tile_name(cube)
    return(cube)
}
#' @title Create a vector cube from local files
#' @name  sits_cube.vector_cube
#' @description
#' Creates a data cube from local files which include a vector file
#' produced by a segmentation algorithm.
#'
#' @param source       Data source: one of \code{"AWS"}, \code{"BDC"},
#'                     \code{"CDSE"}, \code{"DEAFRICA"}, \code{"DEAUSTRALIA"},
#'                     \code{"HLS"}, \code{"PLANETSCOPE"}, \code{"MPC"},
#'                     \code{"SDC"} or \code{"USGS"}. This is the source
#'                     from which the data has been downloaded.
#' @param collection   Image collection in data source.
#'                     To find out the supported collections,
#'                     use \code{\link{sits_list_collections}()}).
#' @param ...          Other parameters to be passed for specific types.
#' @param raster_cube   Raster cube to be merged with vector data
#' @param vector_dir   Local directory where vector files are stored
#' @param vector_band  Band for vector cube ("segments", "probs", "class")
#' @param parse_info   Parsing information for local image files
#' @param delim        Delimiter for parsing local files
#'                     (default = "_")
#' @param version      Version of the classified and/or labelled files.
#' @param multicores   Number of workers for parallel processing
#'                     (integer, min = 1, max = 2048).
#' @param progress     Logical: show a progress bar?
#' @return A \code{tibble} describing the contents of a data cube.
#'
#' @note
#' This function creates vector cubes from local files produced by
#' \code{\link[sits]{sits_segment}}, \code{\link[sits]{sits_classify}}
#' or \code{\link[sits]{sits_label_classification}} when the output
#' is a vector cube. In this case,
#' \code{parse_info} is specified differently as \code{c("X1", "X2", "tile",
#' "start_date", "end_date", "band")}.
#' The parameter \code{vector_dir} is the directory where the vector file is
#' stored.
#' Parameter \code{vector_band} is band name of the type of vector cube:
#' \itemize{
#' \item{\code{"segments"}, for vector cubes produced by
#'    \code{\link{sits_segment}}.}
#'  \item{\code{"probs"}, for probability cubes produced by
#'    \code{\link{sits_classify.vector_cube}}.}
#' \item{\code{"entropy"} when using
#'    \code{\link{sits_uncertainty.probs_vector_cube}}.}
#' \item{\code{"class"} for cubes produced by
#'    \code{\link{sits_label_classification}}.}
#' }
#'
#' @examples
#' if (sits_run_examples()) {
#'     # --- Create a cube based on a local MODIS data
#'     # MODIS local files have names such as
#'     # "TERRA_MODIS_012010_NDVI_2013-09-14.jp2"
#'     # see the parse info parameter as an example on how to
#'     # decode local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     modis_cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir,
#'         parse_info = c("satellite", "sensor", "tile", "band", "date")
#'     )
#'     # segment the vector cube
#'     segs_cube <- sits_segment(
#'         cube = modis_cube,
#'         seg_fn = sits_slic(
#'             step = 10,
#'             compactness = 1,
#'             dist_fun = "euclidean",
#'             avg_fun = "median",
#'             iter = 30,
#'             minarea = 10
#'         ),
#'         output_dir = tempdir()
#'     )
#'     plot(segs_cube)
#'
#'     # recover the local segmented cube
#'     local_segs_cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         raster_cube = modis_cube,
#'         vector_dir = tempdir(),
#'         vector_band = "segments"
#'     )
#'     # plot the recover model and compare
#'     plot(local_segs_cube)
#'
#'     # classify the segments
#'     # create a random forest model
#'     rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
#'     probs_vector_cube <- sits_classify(
#'         data = segs_cube,
#'         ml_model = rfor_model,
#'         output_dir = tempdir(),
#'         n_sam_pol = 10
#'     )
#'     plot(probs_vector_cube)
#'
#'     # recover vector cube
#'     local_probs_vector_cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         raster_cube = modis_cube,
#'         vector_dir = tempdir(),
#'         vector_band = "probs"
#'     )
#'     plot(local_probs_vector_cube)
#'
#'     # label the segments
#'     class_vector_cube <- sits_label_classification(
#'         cube = probs_vector_cube,
#'         output_dir = tempdir(),
#'     )
#'     plot(class_vector_cube)
#'
#'     # recover vector cube
#'     local_class_vector_cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         raster_cube = modis_cube,
#'         vector_dir = tempdir(),
#'         vector_band = "class"
#'     )
#'     plot(local_class_vector_cube)
#' }
#'
#' @export
sits_cube.vector_cube <- function(source,
                                  collection, ...,
                                  raster_cube,
                                  vector_dir,
                                  vector_band,
                                  parse_info = c(
                                      "X1", "X2", "tile", "start_date",
                                      "end_date", "band", "version"
                                  ),
                                  version = "v1",
                                  delim = "_",
                                  multicores = 2L,
                                  progress = TRUE) {
    # set caller to show in errors
    .check_set_caller("sits_cube_vector_cube")
    # show progress bar?
    progress <- .message_progress(progress)
    # obtain vector items
    vector_items <- .local_vector_items(
        source = source,
        collection = collection,
        vector_dir = vector_dir,
        vector_band = vector_band,
        parse_info = parse_info,
        version = version,
        delim = delim,
        multicores,
        progress, ...
    )
    cube <- .local_cube_include_vector_info(raster_cube, vector_items)

    class(cube) <- .cube_s3class(cube)
    if (vector_band == "segments") {
        class(cube) <- c("segs_cube", "vector_cube", class(cube))
    } else if (vector_band == "probs" || vector_band == "probs-vector") {
        class(cube) <- c(
            "probs_vector_cube",
            "derived_vector_cube",
            "segs_cube",
            "vector_cube",
            class(cube)
        )
    } else if (vector_band == "class" || vector_band == "class-vector") {
        class(cube) <- c(
            "class_vector_cube",
            "derived_vector_cube",
            "segs_cube",
            "vector_cube",
            class(cube)
        )
    }
    return(cube)
}
#' @title Create a results cube from local files
#' @name  sits_cube.results_cube
#' @description
#' Creates a data cube from local files produced by sits operations
#' that produces results (such as probs_cubs and class_cubes)
#'
#' @param source       Data source: one of \code{"AWS"}, \code{"BDC"},
#'                     \code{"CDSE"}, \code{"DEAFRICA"}, \code{"DEAUSTRALIA"},
#'                     \code{"HLS"}, \code{"PLANETSCOPE"}, \code{"MPC"},
#'                     \code{"SDC"} or \code{"USGS"}. This is the source
#'                     from which the original data has been downloaded.
#' @param collection   Image collection in data source from which
#'                     the original data has been downloaded.
#'                     To find out the supported collections,
#'                     use \code{\link{sits_list_collections}()}).
#' @param ...          Other parameters to be passed for specific types.
#' @param data_dir     Local directory where images are stored
#' @param tiles        Tiles from the collection to be included in
#'                     the cube.
#' @param bands        Results bands to be retrieved
#'                     ("probs", "bayes", "variance", "class", "uncertainty")
#' @param labels       Named vector with labels associated to the classes
#' @param parse_info   Parsing information for local files
#'                     (see notes below).
#' @param version      Version of the classified and/or labelled files.
#' @param delim        Delimiter for parsing local results cubes
#'                     (default = "_")
#' @param multicores   Number of workers for parallel processing
#'                     (integer, min = 1, max = 2048).
#' @param progress     Logical: show a progress bar?
#' @return A \code{tibble} describing the contents of a data cube.
#'
#' @note
#' This function creates result cubes from local files produced by
#' classification or post-classification algorithms. In this case, the
#' \code{parse_info} is specified differently, and additional parameters
#' are required.
#' The parameter \code{bands} should be a single character vector with
#' the name associated to the type of result:
#' \itemize{
#' \item{\code{"probs"}, for probability cubes produced by
#'   \code{\link[sits]{sits_classify}}.}
#' \item{\code{"bayes"}, for smoothed cubes produced by
#'    \code{\link[sits]{sits_smooth}}.}
#' \item{\code{"entropy"} when using \code{\link[sits]{sits_uncertainty}} to
#'   measure entropy in pixel classification.}
#' \item{\code{"margin"} when using \code{\link[sits]{sits_uncertainty}} to
#'  measure  probability margin in pixel classification.}
#' \item{\code{"least"} when using \code{\link[sits]{sits_uncertainty}} to
#'  measure difference between 100\% and
#'  most probable class in pixel classification.}
#' \item{\code{"class"} for cubes produced by
#'          \code{\link[sits]{sits_label_classification}}.}
#' }
#' For cubes of type \code{"probs"}, \code{"bayes"}, \code{"class"}, the
#'     \code{labels} parameter should be named vector associated to the
#'     classification results. For \code{"class"} cubes, its names should be
#'     integers associated to the values of the raster files that represent
#'     the classified cube.
#'
#' Parameter \code{parse_info} should contain parsing information
#'   to deduce the values of \code{tile}, \code{start_date},
#'   \code{end_date} and \code{band} from the file name.
#'   Default is c("X1", "X2", "tile", "start_date", "end_date", "band").
#'   Cubes processed by \code{sits} adhere to this format.
#'
#'
#' @examples
#' if (sits_run_examples()) {
#'     # create a random forest model
#'     rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'     # classify a data cube
#'     probs_cube <- sits_classify(
#'         data = cube, ml_model = rfor_model, output_dir = tempdir()
#'     )
#'     # plot the probability cube
#'     plot(probs_cube)
#'
#'     # obtain and name the labels of the local probs cube
#'     labels <- sits_labels(rfor_model)
#'     names(labels) <- seq_along(labels)
#'
#'     # recover the local probability cube
#'     probs_local_cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = tempdir(),
#'         bands = "probs",
#'         labels = labels
#'     )
#'     # compare the two plots (they should be the same)
#'     plot(probs_local_cube)
#'
#'     # smooth the probability cube using Bayesian statistics
#'     bayes_cube <- sits_smooth(probs_cube, output_dir = tempdir())
#'     # plot the smoothed cube
#'     plot(bayes_cube)
#'
#'     # recover the local smoothed cube
#'     smooth_local_cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = tempdir(),
#'         bands = "bayes",
#'         labels = labels
#'     )
#'     # compare the two plots (they should be the same)
#'     plot(smooth_local_cube)
#'
#'     # label the probability cube
#'     label_cube <- sits_label_classification(
#'         bayes_cube,
#'         output_dir = tempdir()
#'     )
#'     # plot the labelled cube
#'     plot(label_cube)
#'
#'     # recover the local classified cube
#'     class_local_cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = tempdir(),
#'         bands = "class",
#'         labels = labels
#'     )
#'     # compare the two plots (they should be the same)
#'     plot(class_local_cube)
#'
#'     # obtain an uncertainty cube with entropy
#'     entropy_cube <- sits_uncertainty(
#'         cube = bayes_cube,
#'         type = "entropy",
#'         output_dir = tempdir()
#'     )
#'     # plot entropy values
#'     plot(entropy_cube)
#'
#'     # recover an uncertainty cube with entropy
#'     entropy_local_cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = tempdir(),
#'         bands = "entropy"
#'     )
#'     . # plot recovered entropy values
#'     plot(entropy_local_cube)
#'
#'     # obtain an uncertainty cube with margin
#'     margin_cube <- sits_uncertainty(
#'         cube = bayes_cube,
#'         type = "margin",
#'         output_dir = tempdir()
#'     )
#'     # plot entropy values
#'     plot(margin_cube)
#'
#'     # recover an uncertainty cube with entropy
#'     margin_local_cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = tempdir(),
#'         bands = "margin"
#'     )
#'     . # plot recovered entropy values
#'     plot(margin_local_cube)
#' }
#' @export
sits_cube.results_cube <- function(source,
                                   collection, ...,
                                   data_dir,
                                   tiles = NULL,
                                   bands,
                                   labels = NULL,
                                   parse_info = c(
                                       "X1", "X2", "tile", "start_date",
                                       "end_date", "band", "version"
                                   ),
                                   version = "v1",
                                   delim = "_",
                                   multicores = 2L,
                                   progress = TRUE) {
    # set caller to show in errors
    .check_set_caller("sits_cube_results_cube")

    # check if cube is results cube
    .check_chr_contains(bands,
        contains = .conf("sits_results_bands"),
        discriminator = "one_of",
        msg = .conf("messages", "sits_cube_results_cube")
    )

    # check if labels exist and are named
    if (any(bands %in% c("probs", "bayes", "class"))) {
        .check_labels_named(labels)
    }
    # show progress bar?
    progress <- .message_progress(progress)
    # builds a sits data cube
    .local_results_cube(
        source = source,
        collection = collection,
        data_dir = data_dir,
        tiles = tiles,
        bands = bands,
        labels = labels,
        parse_info = parse_info,
        version = version,
        delim = delim,
        multicores = multicores,
        progress = progress, ...
    )
}
