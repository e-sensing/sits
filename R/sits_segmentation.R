#' @title Segment an image
#' @name sits_segment
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @description
#' Apply a spatial-temporal segmentation on a data cube based on a user defined
#' segmentation function. The function applies the segmentation algorithm
#' "seg_fn" to each tile.
#'
#' Segmentation uses the following steps:
#' \enumerate{
#'  \item Create a regular data cube with \code{\link[sits]{sits_cube}} and
#'        \code{\link[sits]{sits_regularize}};
#'  \item Run \code{\link[sits]{sits_segment}} to obtain a vector data cube
#'        with polygons that define the boundary of the segments;
#'  \item Classify the time series associated to the segments
#'        with \code{\link[sits]{sits_classify}}, to get obtain
#'        a vector probability cube;
#'  \item Use \code{\link[sits]{sits_label_classification}} to label the
#'      vector probability cube;
#'  \item Display the results with \code{\link[sits]{plot}} or
#'        \code{\link[sits]{sits_view}}.
#'}
#'
#'
#' @param  cube       Regular data cube
#' @param  seg_fn     Function to apply the segmentation
#' @param  roi        Region of interest (see below)
#' @param  start_date Start date for the segmentation
#' @param  end_date   End date for the segmentation.
#' @param  memsize    Memory available for classification (in GB).
#' @param  multicores Number of cores to be used for classification.
#' @param  output_dir Directory for output file.
#' @param  version    Version of the output (for multiple
#' segmentations).
#' @param  progress   Show progress bar?
#'
#' @return          A tibble of class 'segs_cube' representing the
#' segmentation.
#'
#' @note
#'    The "roi" parameter defines a region of interest. It can be
#'    an sf_object, a shapefile, or a bounding box vector with
#'    named XY values ("xmin", "xmax", "ymin", "ymax") or
#'    named lat/long values ("lon_min", "lat_min", "lon_max", "lat_max")
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
#'     # segment the vector cube
#'     segments <- sits_segment(
#'         cube = cube,
#'         output_dir = tempdir()
#'     )
#'     # create a classification model
#'     rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
#'     # classify the segments
#'     seg_probs <- sits_classify(
#'         data = segments,
#'         ml_model = rfor_model,
#'         output_dir = tempdir()
#'     )
#'     # label the probability segments
#'     seg_label <- sits_label_classification(
#'         cube = seg_probs,
#'         output_dir = tempdir()
#'     )
#' }
#' @export
sits_segment <- function(cube,
                         seg_fn = sits_slic(),
                         roi = NULL,
                         start_date = NULL,
                         end_date = NULL,
                         memsize = 8,
                         multicores = 2,
                         output_dir,
                         version = "v1",
                         progress = TRUE) {
    # Preconditions
    .check_is_raster_cube(cube)
    .check_is_regular(cube)
    .check_memsize(memsize, min = 1, max = 16384)
    .check_output_dir(output_dir)
    version <- .check_version(version)
    .check_progress(progress)
    .check_function(seg_fn)

    # Spatial filter
    if (.has(roi)) {
        roi <- .roi_as_sf(roi)
        cube <- .cube_filter_spatial(cube = cube, roi = roi)
    }
    # Temporal filter
    if (.has(start_date) || .has(end_date)) {
        cube <- .cube_filter_interval(
            cube = cube, start_date = start_date, end_date = end_date
        )
    }
    start_date <- .default(start_date, .cube_start_date(cube))
    end_date <- .default(end_date, .cube_end_date(cube))

    # Check memory and multicores
    # Get block size
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(cube)))
    # Check minimum memory needed to process one block
    job_memsize <- .jobs_memsize(
        job_size = .block_size(block = block, overlap = 0),
        npaths = length(.tile_paths(cube)),
        nbytes = 8,
        proc_bloat = .conf("processing_bloat_seg")
    )
    # Update multicores parameter
    multicores <- .jobs_max_multicores(
        job_memsize = job_memsize, memsize = memsize, multicores = multicores
    )
    # Update block parameter
    block <- .jobs_optimal_block(
        job_memsize = job_memsize, block = block,
        image_size = .tile_size(.tile(cube)), memsize = memsize,
        multicores = multicores
    )
    # Terra requires at least two pixels to recognize an extent as valid
    # polygon and not a line or point
    block <- .block_regulate_size(block)
    # Prepare parallel processing
    .parallel_start(workers = multicores, output_dir = output_dir)
    on.exit(.parallel_stop(), add = TRUE)
    # Segmentation
    # Process each tile sequentially
    segs_cube <- .cube_foreach_tile(cube, function(tile) {
        # Segment the data
        segs_tile <- .segments_tile(
            tile = tile,
            seg_fn = seg_fn,
            band = "segments",
            block = block,
            roi = roi,
            output_dir = output_dir,
            version = version,
            progress = progress
        )
        return(segs_tile)
    })
    return(segs_cube)
}

#' @title Segment an image using SLIC
#' @name sits_slic
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @description
#' Apply a segmentation on a data cube based on the \code{supercells} package.
#' This is an adaptation and extension to remote sensing data of the
#' SLIC superpixels algorithm proposed by Achanta et al. (2012).
#' See references for more details.
#'
#' @param data          A matrix with time series.
#' @param step          Distance (in number of cells) between initial
#'                      supercells' centers.
#' @param compactness   A compactness value. Larger values cause clusters to
#'                      be more compact/even (square).
#' @param dist_fun      Distance function. Currently implemented:
#'                      \code{euclidean, jsd, dtw},
#'                      and any distance function from the
#'                      \code{philentropy} package.
#'                      See \code{philentropy::getDistMethods()}.
#' @param avg_fun       Averaging function to calculate the values
#'                      of the supercells' centers.
#'                      Accepts any fitting R function
#'                      (e.g., base::mean() or stats::median())
#'                      or one of internally implemented "mean" and "median".
#'                      Default: "median"
#' @param iter          Number of iterations to create the output.
#' @param minarea       Specifies the minimal size of a supercell (in cells).
#' @param verbose       Show the progress bar?
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
#'     # segment the vector cube
#'     segments <- sits_segment(
#'         cube = cube,
#'         output_dir = tempdir(),
#'         version = "slic-demo"
#'     )
#'     # create a classification model
#'     rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
#'     # classify the segments
#'     seg_probs <- sits_classify(
#'         data = segments,
#'         ml_model = rfor_model,
#'         output_dir = tempdir(),
#'         version = "slic-demo"
#'     )
#'     # label the probability segments
#'     seg_label <- sits_label_classification(
#'         cube = seg_probs,
#'         output_dir = tempdir(),
#'         version = "slic-demo"
#'     )
#' }
#' @export
sits_slic <- function(data = NULL,
                      step = 5,
                      compactness = 1,
                      dist_fun = "euclidean",
                      avg_fun = "median",
                      iter = 30,
                      minarea = 10,
                      verbose = FALSE) {
    # step is OK?
    .check_int_parameter(step, min = 1, max = 500)
    # compactness is OK?
    .check_num_parameter(compactness, min = 0.1, max = 50)
    # iter is OK?
    .check_int_parameter(iter, min = 10, max = 100)
    # minarea is OK?
    .check_int_parameter(minarea, min = 10, max = 100)
    function(data, block, bbox) {
        # Create a template rast
        v_temp <- .raster_new_rast(
            nrows = block[["nrows"]], ncols = block[["ncols"]],
            xmin = bbox[["xmin"]], xmax = bbox[["xmax"]],
            ymin = bbox[["ymin"]], ymax = bbox[["ymax"]],
            nlayers = 1, crs = bbox[["crs"]]
        )
        # Get raster dimensions
        mat <- as.integer(
            c(.raster_nrows(v_temp), .raster_ncols(v_temp))
        )
        # Get caller function and call it
        fn <- get("run_slic",
                  envir = asNamespace("supercells"),
                  inherits = FALSE
        )
        slic <- fn(
            mat = mat, vals = data, step = step, compactness = compactness,
            clean = TRUE, centers = TRUE, dist_name = dist_fun,
            dist_fun = function() "", avg_fun_fun = function() "",
            avg_fun_name = avg_fun, iter = iter, minarea = minarea,
            input_centers = matrix(c(0L, 0L), ncol = 2),
            verbose = as.integer(verbose)
        )
        # Set values and NA value in template raster
        v_obj <- .raster_set_values(v_temp, slic[[1]])
        v_obj <- .raster_set_na(v_obj, -1)
        # Polygonize raster and convert to sf object
        v_obj <- .raster_polygonize(v_obj, dissolve = TRUE)
        # TODO: use vector API
        v_obj <- sf::st_as_sf(v_obj)
        if (nrow(v_obj) == 0) {
            return(v_obj)
        }
        # Get valid centers
        valid_centers <- slic[[2]][,1] != 0 | slic[[2]][,2] != 0
        # Bind valid centers with segments table
        v_obj <- cbind(v_obj, stats::na.omit(slic[[2]][valid_centers, ]))
        # Rename columns
        names(v_obj) <- c("supercells", "x", "y", "geometry")
        # Get the extent of template raster
        v_ext <- .raster_bbox(v_temp)
        # Calculate pixel position by rows and cols
        xres <- (v_obj[["x"]] * .raster_xres(v_temp)) + (.raster_xres(v_temp)/2)
        yres <- (v_obj[["y"]] * .raster_yres(v_temp)) - (.raster_yres(v_temp)/2)
        v_obj[["x"]] <- as.vector(v_ext)[[1]] + xres
        v_obj[["y"]] <- as.vector(v_ext)[[4]] - yres
        # Get only polygons segments
        v_obj <- suppressWarnings(sf::st_collection_extract(v_obj, "POLYGON"))
        # Return the segment object
        return(v_obj)
    }
}
