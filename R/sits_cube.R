#' @title Create data cubes from image collections
#' @name sits_cube
#'
#' @references \code{rstac} package (https://github.com/brazil-data-cube/rstac)
#'
#' @description Creates a data cube based on spatial and temporal restrictions
#' in collections available in cloud services or local repositories.
#' The following cloud providers are supported, based on the STAC protocol:
#' \itemize{
#'   \item{\code{"AWS"}: }{Amazon Web Services (AWS),
#'   see https://registry.opendata.aws/ }
#'   \item{\code{"BDC"}: }{Brazil Data Cube (BDC),
#'   see http://brazildatacube.org/}
#'   \item{\code{"DEAFRICA"}: }{Digital Earth Africa,
#'   see https://www.digitalearthafrica.org/}
#'   \item{\code{"MPC"}: }{Microsoft Planetary Computer,
#'   see https://planetarycomputer.microsoft.com/}
#'   \item{\code{"USGS"}:}{USGS LANDSAT collection,
#'   see https://registry.opendata.aws/usgs-landsat/}
#'  }
#'
#' Data cubes can also be created using local files (see details).
#'
#'
#' @param source       Data source (one of \code{"AWS"}, \code{"BDC"},
#'                     \code{"DEAFRICA"}, \code{"MPC"}, \code{"SDC"},
#'                     \code{"USGS"} - character vector of length 1).
#' @param collection   Image collection in data source
#'                     (character vector of length 1).
#'                     To find out the supported collections,
#'                     use \code{\link{sits_list_collections}()}).
#' @param ...          Other parameters to be passed for specific types.
#' @param platform     Optional parameter specifying the platform in case
#'                     of collections that include more than one satellite
#'                     (character vector of length 1).
#' @param tiles        Tiles from the collection to be included in
#'                     the cube (see details below)
#'                     (character vector of length 1).
#' @param  roi         Region of interest (either an sf object, shapefile,
#'                     or a numeric vector with named XY values
#'                     ("xmin", "xmax", "ymin", "ymax") or
#'                     named lat/long values
#'                     ("lon_min", "lat_min", "lon_max", "lat_max").
#' @param bands        Spectral bands and indices to be included
#'                     in the cube (optional - character vector).
#'                     Use \code{\link{sits_list_collections}()} to find out
#'                     the bands available for each collection.
#' @param start_date,end_date Initial and final dates to include
#'                     images from the collection in the cube (optional).
#'                     (Date in YYYY-MM-DD format).
#' @param data_dir     Local directory where images are stored
#'                     (for local cubes - character vector of length 1).
#' @param parse_info   Parsing information for local files
#'                     (for local cubes - character vector).
#' @param version      Version of the classified and/or labelled files.
#'                     (for local cubes - character vector of length 1).
#' @param delim        Delimiter for parsing local files
#'                     (single character)
#' @param labels       Labels associated to the classes
#'                     (Named character vector for cubes of
#'                     classes "probs_cube" or "class_cube").
#' @param multicores   Number of workers for parallel processing
#'                     (integer, min = 1, max = 2048).
#' @param progress     Logical: show a progress bar?
#' @return A \code{tibble} describing the contents of a data cube.
#'
#' @details
#'
#' To create cubes from cloud providers, users need to inform:
#' \itemize{
#' \item{\code{source}: }{One of \code{"AWS"}, \code{"BDC"}, \code{"DEAFRICA"},
#' \code{"MPC"}, \code{"USGS"}, \code{"SDC"} and \code{"HLS"}}.
#' \item{\code{collection}: }{Use \code{sits_list_collections()} to see which
#'   collections are supported.}
#' \item{\code{tiles}: }{A set of tiles defined according to the collection
#'   tiling grid.}
#' \item{\code{roi}: }{Region of interest in WGS84 coordinates.}
#' }
#' Either \code{tiles} or  \code{roi} must be informed.
#' The parameters \code{bands}, \code{start_date}, and
#' \code{end_date} are optional for cubes created from cloud providers.
#'
#' The \code{roi} parameter allows a selection of an area of interest,
#' either using a named \code{vector} (\code{"lon_min"}, \code{"lat_min"},
#' \code{"lon_max"}, \code{"lat_max"}) in WGS84, a \code{sfc} or \code{sf}
#' object from sf package in WGS84 projection.
#' GeoJSON geometries (RFC 7946) and shapefiles should be converted to
#' \code{sf} objects before being used to define a region of interest.
#' This parameter does not crop a region; it only selects images that
#' intersect the \code{roi}.
#'
#' To create a cube from local files, users need to inform:
#' \itemize{
#' \item{\code{source}:} {Provider from where the data has been
#'  downloaded (e.g, "BDC", "AWS").}
#' \item{\code{collection}:}{Collection where the data has been extracted from.}
#' \item{\code{data_dir}: }{Local directory where images are stored.}
#' \item{\code{parse_info}: }{Parsing information for files (see below).}
#' \item{\code{delim}: }{Delimiter character for parsing files (see below).}
#' }
#'
#' To create a cube from local files, all images should have
#' the same spatial resolution and projection and each file should contain
#' a single image band for a single date.
#' Files can belong to different tiles of a spatial reference system and
#' file names need to include tile, date, and band information.
#' For example: \code{"CBERS-4_022024_B13_2018-02-02.tif"}
#' and \code{"cube_20LKP_B02_2018-07-18.jp2"} are accepted names.
#' The user has to provide parsing information to allow \code{sits}
#' to extract values of tile, band, and date. In the examples above,
#' the parsing info is \code{c("X1", "tile", "band", "date")}
#' and the delimiter is \code{"_"}.
#'
#' It is also possible to create result cubes; these are local cubes that have
#' been produced by classification or post-classification algorithms. In
#' this case, there are more parameters that are required (see below) and the
#' parameter \code{parse_info} is specified differently:
#' \itemize{
#' \item{\code{band}: }{The band name is associated to the type of result. Use
#'   \code{"probs"}, for probability cubes produced by \code{sits_classify()};
#'   \code{"bayes"}, for smoothed cubes produced by \code{sits_smooth()};
#'   \code{"entropy"} when using \code{sits_uncertainty()}, or \code{"class"}
#'   for cubes produced by \code{sits_label_classification()}.}
#' \item{\code{labels}: }{Labels associated to the classification results.}
#' \item{\code{parse_info}: }{File name parsing information has to allow
#'   \code{sits} to deduce the values of "tile", "start_date", "end_date" from
#'   the file name. Default is
#'   \code{c("X1", "X2", "tile", "start_date", "end_date", "band")}.
#'   Note that, unlike non-classified image files, cubes with results have both
#'   "start_date" and "end_date".}
#' }
#'
#' @note In MPC, sits can access are two open data collections:
#' \code{"SENTINEL-2-L2A"} for Sentinel-2/2A images, and
#' \code{"LANDSAT-C2-L2"} for the Landsat-4/5/7/8/9 collection.
#' (requester-pays) and \code{"SENTINEL-S2-L2A-COGS"} (open data).
#'
#' @note Sentinel-2/2A level 2A files in MPC are organized by sensor
#' resolution. The bands in 10m resolution are \code{"B02"}, \code{"B03"},
#' \code{"B04"}, and \code{"B08"}. The  20m bands are \code{"B05"},
#' \code{"B06"}, \code{"B07"}, \code{"B8A"}, \code{"B11"}, and \code{"B12"}.
#' Bands \code{"B01"} and \code{"B09"} are available at 60m resolution.
#' The \code{"CLOUD"} band is also available.
#'
#' @note All Landsat-4/5/7/8/9 images in MPC have bands with 30 meter
#' resolution. To account for differences between the different sensors,
#' Landsat bands in this collection have been renamed \code{"BLUE"},
#' \code{"GREEN"}, \code{"RED"}, \code{"NIR08"}, \code{"SWIR16"}
#' and \code{"SWIR22"}. The \code{"CLOUD"} band is also available.
#'
#' @note In AWS, there are two types of collections: open data and
#' requester-pays. Currently, \code{sits} supports collection
#' \code{"SENTINEL-2-L2A"} (open data) and LANDSAT-C2-L2 (requester-pays).
#' There is no need to provide AWS credentials to access open data
#' collections. For requester-pays data, users need to provide their
#' access codes as environment variables, as follows:
#' \code{
#' Sys.setenv(
#'     AWS_ACCESS_KEY_ID     = <your_access_key>,
#'     AWS_SECRET_ACCESS_KEY = <your_secret_access_key>
#' )}
#'
#' @note Sentinel-2/2A level 2A files in AWS are organized by sensor
#' resolution. The AWS bands in 10m resolution are \code{"B02"}, \code{"B03"},
#' \code{"B04"}, and \code{"B08"}. The  20m bands are \code{"B05"},
#' \code{"B06"}, \code{"B07"}, \code{"B8A"}, \code{"B11"}, and \code{"B12"}.
#' Bands \code{"B01"} and \code{"B09"} are available at 60m resolution.
#'
#' @note For DEAFRICA, sits currently works with collections \code{"S2_L2A"}
#' for Sentinel-2 level 2A and \code{"LS8_SR"} for Landsat-8 ARD collection.
#' (open data). These collections are located in Africa
#' (Capetown) for faster access to African users. No payment for access
#' is required.
#'
#' @note For USGS, sits currently works with collection
#' \code{"LANDSAT-C2L2-SR"}, which corresponds to Landsat
#' Collection 2 Level-2 surface reflectance data, covering
#' Landsat-8 dataset. This collection is requester-pays and
#' requires payment for accessing.
#'
#' @note All BDC collections are regularized.
#' BDC users need to provide their credentials using environment
#' variables. To create your credentials, please see
#' <brazil-data-cube.github.io/applications/dc_explorer/token-module.html>.
#' Accessing data in the BDC is free.
#' After obtaining the BDC access key, please include it as
#' an environment variable, as follows:
#' \code{
#' Sys.setenv(
#'     BDC_ACCESS_KEY = <your_bdc_access_key>
#' )}
#'
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # --- Access to the Brazil Data Cube
#'     # create a raster cube file based on the information in the BDC
#'     cbers_tile <- sits_cube(
#'         source = "BDC",
#'         collection = "CBERS-WFI-16D",
#'         bands = c("NDVI", "EVI"),
#'         tiles = "007004",
#'         start_date = "2018-09-01",
#'         end_date = "2019-08-28"
#'     )
#'     # --- Access to Digital Earth Africa
#'     # create a raster cube file based on the information about the files
#'     # DEAFRICA does not support definition of tiles
#'     cube_dea <- sits_cube(
#'         source = "DEAFRICA",
#'         collection = "S2_L2A",
#'         bands = c("B04", "B08"),
#'         roi = c(
#'             "lat_min" = 17.379,
#'             "lon_min" = 1.1573,
#'             "lat_max" = 17.410,
#'             "lon_max" = 1.1910
#'         ),
#'         start_date = "2019-01-01",
#'         end_date = "2019-10-28"
#'     )
#'     # --- Access to AWS open data Sentinel 2/2A level 2 collection
#'     s2_cube <- sits_cube(
#'         source = "AWS",
#'         collection = "SENTINEL-S2-L2A-COGS",
#'         tiles = c("20LKP", "20LLP"),
#'         bands = c("B04", "B08", "B11"),
#'         start_date = "2018-07-18",
#'         end_date = "2019-07-23"
#'     )
#'
#'     # -- Creating Sentinel cube from MPC"
#'     s2_cube <- sits_cube(
#'         source = "MPC",
#'         collection = "SENTINEL-2-L2A",
#'         tiles = "20LKP",
#'         bands = c("B05", "CLOUD"),
#'         start_date = "2018-07-18",
#'         end_date = "2018-08-23"
#'     )
#'
#'     # -- Creating Landsat cube from MPC
#'     roi <- c("lon_min" = -50.410, "lon_max" = -50.379,
#'              "lat_min" = -10.1910 , "lat_max" = -10.1573)
#'     mpc_cube <- sits_cube(
#'         source = "MPC",
#'         collection = "LANDSAT-C2-L2",
#'         bands = c("BLUE", "RED", "CLOUD"),
#'         roi = roi,
#'         start_date = "2005-01-01",
#'         end_date = "2006-10-28"
#'     )
#'
#'     # --- Create a cube based on a local MODIS data
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     modis_cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6",
#'         data_dir = data_dir
#'     )
#' }
#' @export
sits_cube <- function(source, collection, ...) {
    # set caller to show in errors
    .check_set_caller("sits_cube")
    # capture elipsis
    dots <- list(...)
    # if "data_dir" parameters is provided, assumes local cube
    if ("data_dir" %in% names(dots)) {
        source <- .source_new(source = source, is_local = TRUE)
    } else {
        source <- .source_new(source = source, collection = collection)
    }
    # Dispatch
    UseMethod("sits_cube", source)
}
#' @rdname sits_cube
#'
#' @export
sits_cube.stac_cube <- function(source,
                                collection, ...,
                                bands = NULL,
                                tiles = NULL,
                                roi = NULL,
                                start_date = NULL,
                                end_date = NULL,
                                platform = NULL,
                                progress = TRUE) {

    # Check for ROI and tiles
    .check_roi_tiles(roi, tiles)
    # Ensures that there are no duplicate tiles
    if (.has(tiles)) {
        tiles <- unique(tiles)
    }
    # Converts provided roi to sf
    if (.has(roi)) {
        roi <- .roi_as_sf(roi)
    }
    # AWS requires datetime format
    start_date <- .source_adjust_date(source, start_date)
    end_date   <- .source_adjust_date(source, end_date)
    # Configure access if necessary
    .source_configure_access(source, collection)
    # source is upper case
    source <- toupper(source)
    # collection is upper case
    collection <- toupper(collection)
    # pre-condition - check if source and collection exist
    .source_collection_check(
        source = source,
        collection = collection
    )
    # Does the collection need a token for access?
    .source_collection_token_check(
        source = source,
        collection = collection
    )
    # Does the collection need environmental variables for access?
    .source_collection_access_vars_set(
        source = source,
        collection = collection
    )
    if (is.null(bands)) {
        bands <- .source_bands(
            source = source,
            collection = collection
        )
    }
    # Pre-condition - checks if the bands are supported by the collection
    .conf_check_bands(
        source = source,
        collection = collection,
        bands = bands
    )
    # dry run to verify if service is running
    .source_collection_access_test(
        source = source,
        collection = collection,
        bands = bands,
        ...,
        start_date = start_date,
        end_date = end_date
    )
    # builds a sits data cube
    .source_cube(
        source = source,
        collection = collection,
        bands = bands,
        tiles = tiles,
        roi = roi,
        start_date = start_date,
        end_date = end_date,
        platform = platform,
        progress = progress, ...
    )
}
#' @rdname sits_cube
#'
#' @export
sits_cube.local_cube <- function(source,
                                 collection, ...,
                                 data_dir,
                                 tiles = NULL,
                                 bands = NULL,
                                 start_date = NULL,
                                 end_date = NULL,
                                 labels = NULL,
                                 parse_info = NULL,
                                 version = "v1",
                                 delim = "_",
                                 multicores = 2L,
                                 progress = TRUE) {
    # precondition - data directory must be provided
    .check_file(x = data_dir, msg = "'data_dir' parameter must be provided.")
    # expanding the shortened paths since gdal functions do not work with them
    data_dir <- path.expand(data_dir)
    # deal with wrong parameter "band" in dots
    dots <- list(...)
    if ("band" %in% names(dots) && missing(bands)) {
        message("please, use 'bands' instead of 'band' as parameter")
        bands <- as.character(dots[["band"]])
    }
    # precondition - check source and collection for eo_cubes only
    # is this a cube with results?
    if (!purrr::is_null(bands) &&
        all(bands %in% .conf("sits_results_bands"))) {
        results_cube <- TRUE
    } else {
        results_cube <- FALSE
    }
    if (!results_cube) {
        .source_check(source = source)
        .source_collection_check(source = source, collection = collection)
    }
    # builds a sits data cube
    cube <- .local_cube(
        source = source,
        collection = collection,
        data_dir = data_dir,
        parse_info = parse_info,
        version = version,
        delim = delim,
        tiles = tiles,
        bands = bands,
        labels = labels,
        start_date = start_date,
        end_date = end_date,
        multicores = multicores,
        progress = progress, ...
    )
    return(cube)
}
#' @export
sits_cube.default <- function(source, collection, ...) {
    stop("sits_cube: source not found.")
}
