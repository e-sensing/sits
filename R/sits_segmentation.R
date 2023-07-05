#' @title Segment an image
#' @name sits_segment
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @description
#' Apply a spatial segmentation on a data cube based on a user defined
#' segmentation function. The user defines the tiles of
#' the cube to be segmented and informs the bands and the date to
#' be used. The function applies the segmentation algorithm
#' "seg_fn" to each tile.
#'
#' Segmentation uses the following steps:
#' \itemize{
#'  \item{use \code{\link[sits]{sits_segment}} to obtain the \code{sf}
#'        polygons that define the boundary of the segments.}
#'  \item{use \code{\link[sits]{sits_get_data}} to obtain one time series
#'        associated to each segment.}
#'  \item{use \code{\link[sits]{sits_classify}} to classify the
#'       time series associated to the segments.}
#'  \item{use \code{\link[sits]{sits_join_segments}} to update the class
#'       for each segment, based on the time series classification.}
#'  \item{use \code{\link[sits]{plot}} or \code{\link[sits]{sits_view}}
#'        to display the results.}
#'  \item{The result of \code{\link[sits]{sits_join_segments}} is a
#'        list of \code{sf} objects with a "class" attribute.
#'        Use the functions available in \code{sf} for further analysis.}
#'  }
#'
#' @param cube          Regular data cube
#' @param tiles         Tiles to be segmented
#' @param bands         Bands to include in the segmentation
#' @param dates         Dates to consider to be segmented
#' @param seg_fn        Function to apply the segmentation
#' @param ...           Other params to be passed to segmentation function
#'
#' @return              A list of "sf" objects, indexed by tile.
#'                      Each "sf" object contains the polygons that define
#'                      the segments.
#' @examples
#' if (sits_run_examples()) {
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     # create a data cube
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6",
#'         data_dir = data_dir
#'     )
#'     # segment the image
#'     segments <- sits_segment(
#'         cube = cube,
#'         tile = "012010",
#'         bands = "NDVI",
#'         dates = sits_timeline(cube)[1],
#'         seg_fn = sits_supercells(step = 20)
#'     )
#'     # create a classification model
#'     rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
#'     # get the average value per segment
#'     samples_seg <- sits_get_data(
#'         cube = cube,
#'         samples = segments,
#'         n_sam_pol = 10
#'     )
#'     # classify the segments
#'     seg_class <- sits_classify(
#'         data = samples_seg,
#'         ml_model = rfor_model
#'     )
#'     # add a column to the segments by class
#'     sf_seg <- sits_join_segments(
#'         data = seg_class,
#'         segments = segments
#'     )
#' }
#' @export
sits_segment <- function(cube,
                         tiles = NULL,
                         bands = NULL,
                         dates = NULL,
                         seg_fn, ...) {
    # is the cube regular?
    .check_is_regular(cube)
    # does tile belong to the cube?
    tiles <- .default(tiles, .cube_tiles(cube))
    .check_chr_within(
        x = tiles,
        within = .cube_tiles(cube),
        msg = "tiles not available in the cube"
    )
    # Are bands OK?
    bands <- .default(bands, .cube_bands(cube))
    .check_chr_within(bands, .cube_bands(cube),
        msg = "bands not available in the cube"
    )
    # Is date OK?
    dates <- .default(dates, .cube_timeline(cube)[[1]][[1]])
    .check_that(all(as.Date(dates) %in% .cube_timeline(cube)[[1]]),
        msg = "dates not available in the cube"
    )
    # get start and end date
    start_date <- as.Date(dates[[1]])
    end_date <- as.Date(dates[[length(dates)]])
    .check_that(
        start_date <= end_date,
        msg = "start_date should be earlier than end_date"
    )
    # segment each tile
    segments <- purrr::map(tiles, function(tile) {
        tile_seg <- .cube_filter_tiles(cube, tile) |>
            .cube_filter_bands(bands) |>
            .cube_filter_interval(
                start_date = start_date,
                end_date = end_date
            )
        seg_fn(tile_seg, ...)
    })
    names(segments) <- tiles
    class(segments) <- c("segments", class(segments))
    return(segments)
}
#'
#' @title Segment an image using supercells
#' @name sits_supercells
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @description
#' Apply a segmentation on a data cube based on the "supercells" package.
#' This is an adaptation and extension to remote sensing data of the
#' SLIC superpixels algorithm proposed by Achanta et al. (2012).
#' See references for more details.
#'
#' @param tile          Tile, bands, date to be segmented
#' @param step          Distance (in number of cells) between initial
#'                      supercells' centers.
#' @param compactness   A compactness value. Larger values cause clusters to
#'                      be more compact/even (square).
#' @param dist_fun      Distance function. Currently implemented: "euclidean",
#'                      "jsd", "dtw", and any distance function from the
#'                      \code{philentropy} package.
#'                      See \code{philentropy::getDistMethods()}.
#'                      Default: "dtw"
#' @param avg_fun       Averaging function to calculate the values
#'                      of the supercells' centers.
#'                      Accepts any fitting R function
#'                      (e.g., base::mean() or stats::median())
#'                      or one of internally implemented "mean" and "median".
#'                      Default: "median"
#' @param iter          Number of iterations to create the output.
#' @param minarea       Specifies the minimal size of a supercell (in cells).
#' @param multicores    Number of cores for parallel processing
#'
#' @return              Set of segments for a single tile
#'
#' @references
#'         Achanta, Radhakrishna, Appu Shaji, Kevin Smith, Aurelien Lucchi,
#'         Pascal Fua, and Sabine Süsstrunk. 2012. “SLIC Superpixels Compared
#'         to State-of-the-Art Superpixel Methods.” IEEE Transactions on
#'         Pattern Analysis and Machine Intelligence 34 (11): 2274–82.
#'
#'         Nowosad, Jakub, and Tomasz F. Stepinski. 2022. “Extended SLIC
#'         Superpixels Algorithm for Applications to Non-Imagery Geospatial
#'         Rasters.” International Journal of Applied Earth Observation
#'         and Geoinformation 112 (August): 102935.
#'
#' @examples
#' if (sits_run_examples()) {
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     # create a data cube
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6",
#'         data_dir = data_dir
#'     )
#'
#'     # segment the image
#'     segments <- sits_segment(
#'         cube = cube,
#'         tile = "012010",
#'         bands = "NDVI",
#'         date = sits_timeline(cube)[1],
#'         seg_fn = sits_supercells(step = 10)
#'     )
#'     # create a classification model
#'     rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
#'     # get the average value per segment
#'     samples_seg <- sits_get_data(
#'         cube = cube,
#'         samples = segments
#'     )
#'     # classify the segments
#'     seg_class <- sits_classify(
#'         data = samples_seg,
#'         ml_model = rfor_model
#'     )
#'     # add a column to the segments by class
#'     sf_seg <- sits_join_segments(
#'         data = seg_class,
#'         segments = segments
#'     )
#' }
#' @export
sits_supercells <- function(tile = NULL,
                            step = 50,
                            compactness = 1,
                            dist_fun = "dtw",
                            avg_fun = "median",
                            iter = 10,
                            minarea = 30,
                            multicores = 1) {
    seg_fun <- function(tile) {
        # step is OK?
        .check_int_parameter(step, min = 1, max = 500)
        # compactness is OK?
        .check_int_parameter(compactness, min = 1, max = 50)
        # iter is OK?
        .check_int_parameter(iter, min = 10, max = 100)
        # minarea is OK?
        .check_int_parameter(minarea, min = 10, max = 100)
        # multicores
        .check_int_parameter(multicores, min = 1, max = 1000)
        # set multicores to 1
        multicores <- 1
        # obtain the image files to perform the segmentation
        files <- .tile_paths(tile)
        # obtain the SpatRaster (terra) object
        rast <- terra::rast(files)
        # segment the terra object
        cells_sf <- supercells::supercells(
            x = rast,
            compactness = compactness,
            step = step,
            dist_fun = dist_fun,
            avg_fun = avg_fun,
            iter = iter,
            minarea = minarea,
            chunks = FALSE,
            future = FALSE
        )
        class(cells_sf) <- c("supercells", class(cells_sf))
        return(cells_sf)
    }
    # If samples is informed, train a model and return a predict function
    # Otherwise give back a train function to train model further
    result <- .factory_function(tile, seg_fun)
    return(result)
}
#' @title Return segments from a classified set of time series
#' @name sits_join_segments
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @description The \code{\link{sits_segment}} function produces
#' a list of "sf" segments. These segments are used to obtain a set
#' of time series (one per segment) using
#' \code{\link{sits_get_data}}. The time series can then be classified
#' using \code{\link{sits_classify}}. The next step is to add the result
#' of time series classification to the "sf" segments file. This action
#' is performed by this function.
#'
#' @param data     A sits tibble with predicted values
#' @param segments A list of "sf" segments with polygon geometry
#'                 organized by tile.
#' @return         An list of sf objects of polygon geometry
#'                 with an additional class attribute
#'                 organized by tile
#' @examples
#' if (sits_run_examples()) {
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     # create a data cube
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6",
#'         data_dir = data_dir
#'     )
#'
#'     # segment the image
#'     segments <- sits_segment(
#'         cube = cube,
#'         tile = "012010",
#'         bands = "NDVI",
#'         date = sits_timeline(cube)[1],
#'         seg_fn = sits_slic(step = 10)
#'     )
#'     # create a classification model
#'     rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
#'     # get the average value per segment
#'     samples_seg <- sits_get_data(
#'         cube = cube,
#'         samples = segments
#'     )
#'     # classify the segments
#'     seg_class <- sits_classify(
#'         data = samples_seg,
#'         ml_model = rfor_model
#'     )
#'     # add a column to the segments by class
#'     sf_seg <- sits_join_segments(
#'         data = seg_class,
#'         segments = segments
#'     )
#' }
#' @export
sits_join_segments <- function(data, segments) {
    # pre-conditions
    .check_that(
        x = inherits(data, "predicted"),
        msg = "input data should be a set of classified time series"
    )
    .check_that(
        x = inherits(segments, "segments"),
        msg = "invalid segments input"
    )
    # select polygon_id and class for the time series tibble
    data_id <- data |>
        tidyr::unnest(cols = "predicted") |>
        dplyr::select(dplyr::all_of(c("polygon_id", "class")))
    # join the data_id tibble with the segments (sf objects)
    segments_tile <- purrr::map(segments, function(seg) {
        dplyr::left_join(seg, data_id, by = c("supercells" = "polygon_id"))
    })
    # keep the names
    names(segments_tile) <- names(segments)
    class(segments_tile) <- class(segments)
    return(segments_tile)
}
