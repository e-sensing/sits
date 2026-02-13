#' @title Segment an image
#' @name sits_segment
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description
#' Apply a spatial-temporal segmentation on a data cube based on a user defined
#' segmentation function. The function applies the segmentation algorithm
#' "seg_fn" to each tile. The output is a vector data cube, which is a data cube
#' with an additional vector file in "geopackage" format.
#'
#' @param  cube       Regular data cube
#' @param  seg_fn     Function to apply the segmentation
#' @param  roi        Region of interest (see below)
#' @param  impute_fn  Imputation function to remove NA values.
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
#' Segmentation requires the following steps:
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
#' }
#'    The "roi" parameter defines a region of interest. It can be
#'    an sf_object, a shapefile, or a bounding box vector with
#'    named XY values ("xmin", "xmax", "ymin", "ymax") or
#'    named lat/long values ("lon_min", "lat_min", "lon_max", "lat_max").
#'
#'    As of version 1.5.4, two segmentation functions are available. The
#'    preferred option is \code{\link[sits]{sits_snic}}, which implements
#'    the Simple Non-Iterative Clustering (SNIC) algorithm to generate
#'    compact and homogeneous superpixels directly from uniformly distributed
#'    seeds. SNIC avoids the iterative refinement step used in SLIC and is
#'    generally faster and more memory-efficient, making it suitable for
#'    large multispectral or multitemporal data cubes.
#'
#'    The previous function \code{\link[sits]{sits_slic}}, based on the
#'    Simple Linear Iterative Clustering (SLIC) algorithm as adapted by
#'    Nowosad and Stepinski for multispectral and multitemporal imagery,
#'    remains available but is now deprecated and will be removed in a future
#'    release. SLIC clusters pixels using spectral similarity and
#'    spatial–temporal proximity to produce nearly uniform superpixels,
#'    but its iterative nature makes it less efficient for large-scale
#'    Earth observation workflows.
#'
#'    The result of \code{sits_segment} is a data cube tibble with an additional
#'    vector file in the \code{geopackage} format. The location of the vector
#'    file is included in the data cube tibble in a new column, called
#'    \code{vector_info}.
#'
#' @references
#'         Achanta, Radhakrishna, and Sabine Susstrunk. 2017.
#'         “Superpixels and Polygons Using Simple Non-Iterative Clustering.”
#'         Proceedings of the IEEE Conference on Computer Vision and Pattern
#'         Recognition, 4651–60.
#'
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
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'     # segment the vector cube
#'     segments <- sits_segment(
#'         cube = cube,
#'         seg_fn = sits_snic(
#'             grid_seeding = "diamond",
#'             spacing = 15,
#'             compactness = 0.5,
#'             padding = 2
#'         ),
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
                         seg_fn = sits_snic(),
                         roi = NULL,
                         impute_fn = impute_linear(),
                         start_date = NULL,
                         end_date = NULL,
                         memsize = 4L,
                         multicores = 2L,
                         output_dir,
                         version = "v1",
                         progress = TRUE) {
    # check required package
    .check_require_packages("vctrs")
    # set caller for error msg
    .check_set_caller("sits_segment")
    # Preconditions
    .check_is_raster_cube(cube)
    .check_cube_is_regular(cube)
    .check_int_parameter(memsize, min = 1L, max = 16384L)
    .check_output_dir(output_dir)
    # Check version and progress
    version <- .message_version(version)
    progress <- .message_progress(progress)
    .check_function(seg_fn)

    # Spatial filter
    if (.has(roi)) {
        roi <- .roi_as_sf(roi)
        cube <- .cube_filter_spatial(cube = cube, roi = roi)
    }
    # Get values for start date and end date
    # if they are NULL, use the cube values
    start_date <- .default(start_date, .cube_start_date(cube))
    end_date <- .default(end_date, .cube_end_date(cube))
    # Temporal filter
    cube <- .cube_filter_interval(
        cube = cube, start_date = start_date, end_date = end_date
    )

    # The following functions define optimal parameters for parallel processing
    #
    # Get block size
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(cube)))
    # Check minimum memory needed to process one block
    job_block_memsize <- .jobs_block_memsize(
        block_size = .block_size(block = block, overlap = 0L),
        npaths = length(.tile_paths(cube)),
        nbytes = 8L,
        proc_bloat = .conf("processing_bloat_seg")
    )
    # Update multicores parameter
    multicores <- .jobs_max_multicores(
        job_block_memsize = job_block_memsize,
        memsize = memsize,
        multicores = multicores
    )
    # Update block parameter
    block <- .jobs_optimal_block(
        job_block_memsize = job_block_memsize,
        block = block,
        image_size = .tile_size(.tile(cube)),
        memsize = memsize,
        multicores = multicores
    )
    # Prepare parallel processing
    if (.parallel_start(workers = multicores)) {
        on.exit(.parallel_stop(), add = TRUE)
    }
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
            impute_fn = impute_fn,
            output_dir = output_dir,
            version = version,
            progress = progress
        )
        segs_tile
    })
    segs_cube
}

#' @title Segment an image using SLIC
#' @name sits_slic
#'
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description
#' Apply a segmentation on a data cube using either the \code{supercells} or
#' \code{snic} packages, depending on the chosen algorithm. As of version
#' 1.5.4, two segmentation methods are supported. The recommended option is
#' SNIC, implemented via the \code{snic} package, which applies a
#' non-iterative clustering strategy to generate compact, homogeneous
#' superpixels from uniformly distributed seeds (Achanta and Susstrunk, 2017).
#' The alternative method uses the SLIC algorithm implemented in the
#' \code{supercells} package, adapted for remote sensing data following
#' Achanta et al. (2012). This SLIC variant is deprecated and will be
#' removed in a future release. See references for more details.
#'
#' @param data          A matrix with time series.
#' @param step          Distance (in number of cells) between initial
#'                      supercells' centers
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
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'     # segment the vector cube
#'     segments <- sits_segment(
#'         cube = cube,
#'         seg_fn = sits_snic(
#'             grid_seeding = "rectangular",
#'             spacing = 10,
#'             compactness = 0.3,
#'             padding = 0
#'         ),
#'         output_dir = tempdir(),
#'         version = "snic-demo"
#'     )
#'     # create a classification model
#'     rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
#'     # classify the segments
#'     seg_probs <- sits_classify(
#'         data = segments,
#'         ml_model = rfor_model,
#'         output_dir = tempdir(),
#'         version = "snic-demo"
#'     )
#'     # label the probability segments
#'     seg_label <- sits_label_classification(
#'         cube = seg_probs,
#'         output_dir = tempdir(),
#'         version = "snic-demo"
#'     )
#'     plot(seg_label)
#' }
#' @export
sits_slic <- function(data = NULL,
                      step = 30L,
                      compactness = 1.0,
                      dist_fun = "euclidean",
                      avg_fun = "median",
                      iter = 30L,
                      minarea = 10L,
                      verbose = FALSE) {
    # notify users about the deprecation
    warning(.conf("messages", "sits_slic_deprec"))
    # set caller for error msg
    .check_set_caller("sits_slic")
    # step is OK?
    .check_int_parameter(step, min = 1L, max = 500L)
    # compactness is OK?
    .check_num_parameter(compactness, min = 0.1, max = 50.0)
    # iter is OK?
    .check_int_parameter(iter, min = 10L, max = 100L)
    # minarea is OK?
    .check_int_parameter(minarea, min = 1L, max = 50L)
    # documentation mode? verbose is FALSE
    verbose <- .message_verbose(verbose)

    function(data, block, bbox) {
        # Create a template rast
        v_temp <- .raster_new_rast(
            nrows = block[["nrows"]], ncols = block[["ncols"]],
            xmin = bbox[["xmin"]], xmax = bbox[["xmax"]],
            ymin = bbox[["ymin"]], ymax = bbox[["ymax"]],
            nlayers = 1L, crs = bbox[["crs"]]
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
            input_centers = matrix(c(0L, 0L), ncol = 2L),
            verbose = as.integer(verbose)
        )
        # Set values and NA value in template raster
        v_obj <- .raster_set_values(v_temp, slic[[1L]])
        v_obj <- .raster_set_na(v_obj, -1L)
        # Extract polygons raster and convert to sf object
        v_obj <- .raster_extract_polygons(v_obj, dissolve = TRUE)
        v_obj <- sf::st_as_sf(v_obj)
        if (nrow(v_obj) == 0L) {
            return(v_obj)
        }
        # Get valid centers
        valid_centers <- slic[[2L]][, 1L] != 0L & slic[[2L]][, 2L] != 0L
        # Bind valid centers with segments table
        v_obj <- cbind(
            v_obj, matrix(stats::na.omit(slic[[2L]][valid_centers, ]), ncol = 2L)
        )
        # Rename columns
        names(v_obj) <- c("supercells", "x", "y", "geometry")
        # Get the extent of template raster
        v_ext <- .raster_bbox(v_temp)
        # Calculate pixel position by rows and cols
        x_pos <- v_obj[["x"]] * .raster_xres(v_temp) + .raster_xres(v_temp) / 2L
        y_pos <- v_obj[["y"]] * .raster_yres(v_temp) - .raster_yres(v_temp) / 2L
        v_obj[["x"]] <- as.vector(v_ext)[[1L]] + x_pos
        v_obj[["y"]] <- as.vector(v_ext)[[4L]] - y_pos
        # Get only polygons segments
        v_obj <- suppressWarnings(sf::st_collection_extract(v_obj, "POLYGON"))
        # Return the segment object
        v_obj
    }
}

#' @title Segment an image using SNIC
#' @name sits_snic
#'
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @description
#' Apply a segmentation on a data cube based on the \code{snic} package.
#' This is an adaptation and extension to remote sensing data of the
#' SNIC superpixels algorithm proposed by Achanta and Süsstrunk (2017).
#' See reference for more details.
#'
#' @param data          A matrix with time series.
#' @param grid_seeding  Method for grid seeding (one of
#'                      "rectangular", "diamond", "hexagonal",
#'                      "random").
#' @param spacing       Distance (in number of cells) between initial
#'                      supercells' centers
#' @param compactness   A compactness value. Larger values cause clusters to
#'                      be more compact/even (square).
#' @param padding       Distance (in pixels) from the image borders within
#'                      which no seeds are placed.
#'
#' @references
#' "Superpixels and Polygons Using Simple Non-Iterative Clustering",
#' R. Achanta and S. Süsstrunk, CVPR 2017.
#'
#' @examples
#' if (sits_run_examples()) {
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     # create a data cube
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'     # segment the vector cube
#'     segments <- sits_segment(
#'         cube = cube,
#'         seg_fn = sits_snic(
#'             grid_seeding = "rectangular",
#'             spacing = 10,
#'             compactness = 0.5,
#'             padding = 5
#'         ),
#'         output_dir = tempdir(),
#'         version = "snic-demo"
#'     )
#'     # create a classification model
#'     rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
#'     # classify the segments
#'     seg_probs <- sits_classify(
#'         data = segments,
#'         ml_model = rfor_model,
#'         output_dir = tempdir(),
#'         version = "snic-demo"
#'     )
#'     # label the probability segments
#'     seg_label <- sits_label_classification(
#'         cube = seg_probs,
#'         output_dir = tempdir(),
#'         version = "snic-demo"
#'     )
#'     plot(seg_label)
#' }
#' @export
sits_snic <- function(data = NULL,
                      grid_seeding = "rectangular",
                      spacing = 10,
                      compactness = 0.5,
                      padding = floor(spacing / 2)) {
    # require snic package
    .check_require_packages("snic")
    # set caller for error msg
    .check_set_caller("sits_snic")
    # spacing is OK?
    .check_int_parameter(spacing, min = 1L, max = 500L)
    # compactness is OK?
    .check_num_parameter(compactness, min = 0L, max = 1L)
    # padding is OK?
    .check_int_parameter(padding, min = 0L, max = 500L)
    # grid seeding
    .check_snic_grid(grid_seeding)

    # calls SNIC for a matrix
    function(data, block, bbox) {
        # Create a template rast
        v_temp <- .raster_new_rast(
            nrows = block[["nrows"]], ncols = block[["ncols"]],
            xmin = bbox[["xmin"]], xmax = bbox[["xmax"]],
            ymin = bbox[["ymin"]], ymax = bbox[["ymax"]],
            nlayers = 1L, crs = bbox[["crs"]]
        )
        # set dimensions for image
        img_height <- block[["nrows"]]
        img_width <- block[["ncols"]]
        img_bands <- ncol(data)
        # Adjust data
        dim(data) <- c(img_width, img_height, img_bands)
        data <- aperm(data, c(2, 1, 3))
        # generate seeds for classification
        seeds <- snic::snic_grid(data,
            type = grid_seeding,
            img = data,
            spacing = spacing,
            padding = padding
        )
        v_obj <- tryCatch(
            {
                # use SNIC to produce a one-band segmented raster image
                seg_img <- snic::snic(
                    x = data,
                    seeds = seeds,
                    compactness = compactness
                )
                # permute dimensions of one-band raster image
                seg_img <- snic::snic_get_seg(seg_img)
                seg_img <- aperm(seg_img, c(2, 1, 3))
                dim(seg_img) <- c(img_width * img_height, 1)

                # extract segments for one-band raster image
                # Set values and NA value in template raster
                v_obj <- .raster_set_values(v_temp, seg_img)
                v_obj <- .raster_set_na(v_obj, -1L)
                # Extract polygons raster and convert to sf object
                v_obj <- .raster_extract_polygons(v_obj, dissolve = TRUE)
                v_obj <- sf::st_as_sf(v_obj)
                v_obj
            },
            error = function(e) {
                snic_err_msg <- "All pixels contain NA values"
                if (!grepl(snic_err_msg, conditionMessage(e))) {
                    stop(e)
                }
                # Generate empty simple features
                sf::st_sf(
                    supercells = integer(),
                    x = double(),
                    y = double(),
                    geometry = sf::st_cast(sf::st_sfc(crs = bbox[["crs"]]), "POLYGON")
                )
            }
        )
        if (nrow(v_obj) == 0L) {
            return(v_obj)
        }
        # Get valid centroids
        centroids <- suppressWarnings(sf::st_centroid(v_obj))
        # Extract centroid matrix from centroids
        centroids_xy <- sf::st_coordinates(centroids)
        # Bind valid centers with segments table
        v_obj <- cbind(v_obj, centroids_xy)
        # Rename columns
        names(v_obj) <- c("supercells", "x", "y", "geometry")
        # Get only polygons segments
        v_obj <- suppressWarnings(sf::st_collection_extract(v_obj, "POLYGON"))
        # Return the segment object
        v_obj
    }
}
