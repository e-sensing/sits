#' @title Create data cubes from image collections
#' @name sits_cube
#'
#' @description Creates a data cube based on spatial and temporal restrictions
#' in collections available in cloud services or local repositories.
#' The following cloud providers are supported, based on the STAC protocol:
#' Amazon Web Services (AWS), Brazil Data Cube (BDC),
#' Copernicus Data Space Ecosystem (CDSE), Digital Earth Africa (DEAFRICA),
#' Digital Earth Australia (DEAUSTRALIA), Microsoft Planetary Computer (MPC),
#' Nasa Harmonized Landsat/Sentinel (HLS), Swiss Data Cube (SDC), TERRASCOPE or
#' USGS Landsat (USGS). Data cubes can also be created using local files.
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
#' @param roi          Region of interest (either an sf object, shapefile,
#'                     \code{SpatExtent}, or a numeric vector with named XY
#'                     values ("xmin", "xmax", "ymin", "ymax") or
#'                     named lat/long values
#'                     ("lon_min", "lat_min", "lon_max", "lat_max").
#' @param crs          The Coordinate Reference System (CRS) of the roi. It
#'                     must be specified when roi is named XY values
#'                     ("xmin", "xmax", "ymin", "ymax") or \code{SpatExtent}
#' @param bands        Spectral bands and indices to be included
#'                     in the cube (optional - character vector).
#'                     Use \code{\link{sits_list_collections}()} to find out
#'                     the bands available for each collection.
#' @param orbit        Orbit name ("ascending", "descending") for SAR cubes.
#' @param vector_band  Band for vector cube ("segments", "probs", "class")
#' @param start_date,end_date Initial and final dates to include
#'                     images from the collection in the cube (optional).
#'                     (Date in YYYY-MM-DD format).
#' @param data_dir     Local directory where images are stored
#'                     (for local cubes - character vector of length 1).
#' @param vector_dir    Local director where vector files are stored
#'                     (for local vector cubes - character vector of length 1).
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
#' @note{
#' To create cubes from cloud providers, users need to inform:
#' \enumerate{
#'  \item \code{source}: One of "AWS", "BDC", "CDSE", "DEAFRICA", "DEAUSTRALIA",
#'  "HLS", "MPC", "SDC", "TERRASCOPE", or "USGS";
#'  \item \code{collection}: Collection available in the cloud provider.
#'         Use \code{\link{sits_list_collections}()} to see which
#'         collections are supported;
#'  \item \code{tiles}: A set of tiles defined according to the collection
#'         tiling grid;
#'  \item \code{roi}: Region of interest. Either
#'        a shapefile, a named \code{vector} (\code{"lon_min"},
#'        \code{"lat_min"}, \code{"lon_max"}, \code{"lat_max"}) in WGS84, a
#'        \code{sfc} or \code{sf} object from sf package in WGS84 projection.
#'        A named \code{vector} (\code{"xmin"}, \code{"xmax"},
#'        \code{"ymin"}, \code{"ymax"}) or a \code{SpatExtent} can also
#'        be used, requiring only the specification of the \code{crs} parameter.
#' }
#'
#' The parameter \code{bands}, \code{start_date}, and \code{end_date} are
#' optional for cubes created from cloud providers.
#'
#' Either \code{tiles} or \code{roi} must be informed. The \code{roi} parameter
#' is used to select images. This parameter does not crop a region; it only
#' selects images that intersect it.
#'
#' If you want to use GeoJSON geometries (RFC 7946) as value \code{roi}, you
#' can convert it to sf object and then use it.
#'
#' \code{sits} can access data from multiple providers, including
#' \code{Amazon Web Services} (AWS), \code{Microsoft Planetary Computer} (MPC),
#' \code{Brazil Data Cube} (BDC), \code{Copernicus Data Space Ecosystem} (CDSE),
#' \code{Digital Earth Africa}, \code{Digital Earth Australia},
#' \code{NASA EarthData}, \code{Terrascope} and more.
#'
#' In each provider, \code{sits} can access multiple collections. For example,
#' in MPC \code{sits} can access multiple open data collections, including
#' \code{"SENTINEL-2-L2A"} for Sentinel-2/2A images, and
#' \code{"LANDSAT-C2-L2"} for the Landsat-4/5/7/8/9 collection.
#'
#' In AWS, there are two types of collections: open data and
#' requester-pays. Currently, \code{sits} supports collections
#' \code{"SENTINEL-2-L2A"}, \code{"SENTINEL-S2-L2A-COGS"} (open data) and
#' \code{"LANDSAT-C2-L2"} (requester-pays). There is no need to provide AWS
#' credentials to access open data collections. For requester-pays data, you
#' need to provide your AWS access codes as environment variables, as follows:
#' \code{
#' Sys.setenv(
#'     AWS_ACCESS_KEY_ID     = <your_access_key>,
#'     AWS_SECRET_ACCESS_KEY = <your_secret_access_key>
#' )}
#'
#' In BDC, there are many collections, including \code{"LANDSAT-OLI-16D"}
#' (Landsat-8 OLI, 30 m resolution, 16-day intervals), \code{"SENTINEL-2-16D"}
#' (Sentinel-2A and 2B MSI images at 10 m resolution, 16-day intervals),
#' \code{"CBERS-WFI-16D"} (CBERS 4 WFI, 64 m resolution, 16-day intervals), and
#' others. All BDC collections are regularized.
#'
#' To explore providers and collections \code{sits} supports, use the
#' \code{\link{sits_list_collections}()} function.
#'
#' If you want to learn more details about each provider and collection
#' available in \code{sits}, please read the online sits book
#' (e-sensing.github.io/sitsbook). The chapter
#' \code{Earth Observation data cubes} provides a detailed description of all
#' collections you can use with \code{sits}
#' (e-sensing.github.io/sitsbook/earth-observation-data-cubes.html).
#'
#' To create a cube from local files, you need to inform:
#' \enumerate{
#'  \item \code{source}: The data provider from which the data was
#'  downloaded (e.g, "BDC", "MPC");
#'
#'  \item \code{collection}: The collection from which the data comes from.
#'  (e.g., \code{"SENTINEL-2-L2A"} for the Sentinel-2 MPC collection level 2A);
#'
#'  \item \code{data_dir}: The local directory where the image files are stored.
#'
#'  \item \code{parse_info}: Defines how to extract metadata from file names
#'  by specifying the order and meaning of each part, separated by the
#'  \code{"delim"} character. Default value is
#'  \code{c("X1", "X2", "tile", "band", "date")}.
#'
#'  \item \code{delim}: The delimiter character used to separate components in
#'  the file names. Default is \code{"_"}.
#' }
#'
#' Note that if you are working with local data cubes created by \code{sits},
#' you do not need to specify \code{parse_info} and \code{delim}. These elements
#' are automatically identified. This is particularly useful when you have
#' downloaded or created data cubes using \code{sits}.
#'
#' For example, if you downloaded a data cube from the Microsoft Planetary
#' Computer (MPC) using the function \code{\link{sits_cube_copy}()}, you do
#' not need to provide \code{parse_info} and \code{delim}.
#'
#' If you are using a data cube from a source supported by \code{sits}
#' (e.g., AWS, MPC) but downloaded / managed with an external tool, you will
#' need to specify the \code{parse_info} and \code{delim} parameters manually.
#' For this case, you first need to ensure that the local files meet some
#' critical requirements:
#'
#' \itemize{
#'  \item All image files must have the same spatial resolution and projection;
#'
#'  \item Each file should represent a single image band for a single date;
#'
#'  \item File names must include information about the \code{"tile"},
#'  \code{"date"}, and \code{"band"} in the file.
#' }
#'
#' For example, if you are creating a Sentinel-2 data cube on your local
#' machine, and the files have the same spatial resolution and projection, with
#' each file containing a single band and date, an acceptable file name could be:
#' \itemize{
#'  \item \code{"SENTINEL-2_MSI_20LKP_B02_2018-07-18.jp2"}
#' }
#'
#' This file name works because it encodes the three key pieces of information
#' used by \code{sits}:
#' \itemize{
#'  \item Tile: "20LKP";
#'
#'  \item Band: "B02";
#'
#'  \item Date: "2018-07-18"
#' }
#'
#' Other example of supported file names are:
#' \itemize{
#'  \item \code{"CBERS-4_WFI_022024_B13_2021-05-15.tif"};
#'
#'  \item \code{"SENTINEL-1_GRD_30TXL_VV_2023-03-10.tif"};
#'
#'  \item \code{"LANDSAT-8_OLI_198030_B04_2020-09-12.tif"}.
#' }
#'
#' The \code{parse_info} parameter tells \code{sits} how to extract essential
#' metadata from file names. It defines the sequence of components in the
#' file name, assigning each part a label such as \code{"tile"}, \code{"band"},
#' and \code{"date"}. For parts of the file name that are irrelevant to
#' \code{sits}, you can use dummy labels like \code{"X1"}, \code{"X2"}, and so
#' on.
#'
#' For example, consider the file name:
#' \itemize{
#'  \item \code{"SENTINEL-2_MSI_20LKP_B02_2018-07-18.jp2"}
#' }
#'
#' With \code{parse_info = c("X1", "X2", "tile", "band", "date")} and
#' \code{delim = "_"}, the extracted metadata would be:
#'
#' \itemize{
#'  \item X1: "SENTINEL-2" (ignored)
#'  \item X2: "MSI" (ignored)
#'  \item tile: "20LKP" (used)
#'  \item band: "B02" (used)
#'  \item date: "2018-07-18" (used)
#' }
#'
#' The \code{delim} parameter specifies the character that separates components
#' in the file name. The default delimiter is \code{"_"}.
#'
#' Note that when you load a local data cube specifying the \code{source}
#' (e.g., AWS, MPC) and \code{collection}, \code{sits} assumes that the data
#' properties (e.g., scale factor, minimum, and maximum values) match those
#' defined for the selected provider. However, if you are working with
#' custom data from an unsupported source or data that does not follow the
#' standard definitions of providers in sits, refer to the Technical Annex of
#' the \code{sits} online book for guidance on handling such cases
#' (e-sensing.github.io/sitsbook/technical-annex.html).
#'
#' It is also possible to create result cubes from local files produced by
#' classification or post-classification algorithms. In this case, the
#' \code{parse_info} is specified differently, and other additional parameters
#' are required:
#'
#' \itemize{
#'
#' \item \code{band}: Band name associated to the type of result. Use
#'   \code{"probs"}, for probability cubes produced by
#'   \code{\link{sits_classify}()};
#'   \code{"bayes"}, for smoothed cubes produced by \code{\link{sits_smooth}()};
#'   \code{"segments"}, for vector cubes produced by
#'   \code{\link{sits_segment}()};
#'   \code{"entropy"} when using \code{\link{sits_uncertainty}()}, and
#'   \code{"class"} for cubes produced by
#'   \code{\link{sits_label_classification}()};
#'
#' \item \code{labels}: Labels associated to the classification results;
#'
#' \item \code{parse_info}: File name parsing information
#'   to deduce the values of \code{"tile"}, \code{"start_date"},
#'   \code{"end_date"} from the file name. Unlike non-classified image files,
#'   cubes with results have both \code{"start_date"} and \code{"end_date"}.
#'   Default is c("X1", "X2", "tile", "start_date", "end_date", "band").
#' }
#'
#' }
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
#'     cube_deafrica <- sits_cube(
#'         source = "DEAFRICA",
#'         collection = "SENTINEL-2-L2A",
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
#'     # --- Access to Digital Earth Australia
#'     cube_deaustralia <- sits_cube(
#'         source = "DEAUSTRALIA",
#'         collection = "GA_LS8CLS9C_GM_CYEAR_3",
#'         bands = c("RED", "GREEN", "BLUE"),
#'         roi = c(
#'             lon_min = 137.15991,
#'             lon_max = 138.18467,
#'             lat_min = -33.85777,
#'             lat_max = -32.56690
#'         ),
#'         start_date = "2018-01-01",
#'         end_date = "2018-12-31"
#'     )
#'     # --- Access to CDSE open data Sentinel 2/2A level 2 collection
#'     # --- remember to set the appropriate environmental variables
#'     # It is recommended that `multicores` be used to accelerate the process.
#'     s2_cube <- sits_cube(
#'         source = "CDSE",
#'         collection = "SENTINEL-2-L2A",
#'         tiles = c("20LKP"),
#'         bands = c("B04", "B08", "B11"),
#'         start_date = "2018-07-18",
#'         end_date = "2019-01-23"
#'     )
#'
#'     ## --- Sentinel-1 SAR from CDSE
#'     # --- remember to set the appropriate environmental variables
#'     roi_sar <- c("lon_min" = 33.546, "lon_max" = 34.999,
#'                  "lat_min" = 1.427, "lat_max" = 3.726)
#'     s1_cube_open <- sits_cube(
#'        source = "CDSE",
#'        collection = "SENTINEL-1-RTC",
#'        bands = c("VV", "VH"),
#'        orbit = "descending",
#'        roi = roi_sar,
#'        start_date = "2020-01-01",
#'        end_date = "2020-06-10"
#'     )
#'
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
#'     # --- Creating Sentinel cube from MPC
#'     s2_cube <- sits_cube(
#'         source = "MPC",
#'         collection = "SENTINEL-2-L2A",
#'         tiles = "20LKP",
#'         bands = c("B05", "CLOUD"),
#'         start_date = "2018-07-18",
#'         end_date = "2018-08-23"
#'     )
#'
#'     # --- Creating Landsat cube from MPC
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
#'     ## Sentinel-1 SAR from MPC
#'     roi_sar <- c("lon_min" = -50.410, "lon_max" = -50.379,
#'                  "lat_min" = -10.1910, "lat_max" = -10.1573)
#'
#'     s1_cube_open <- sits_cube(
#'        source = "MPC",
#'        collection = "SENTINEL-1-GRD",
#'        bands = c("VV", "VH"),
#'        orbit = "descending",
#'        roi = roi_sar,
#'        start_date = "2020-06-01",
#'        end_date = "2020-09-28"
#'     )
#'     # --- Access to World Cover data (2021) via Terrascope
#'     cube_terrascope <- sits_cube(
#'         source = "TERRASCOPE",
#'         collection = "WORLD-COVER-2021",
#'         roi = c(
#'             lon_min = -62.7,
#'             lon_max = -62.5,
#'             lat_min = -8.83,
#'             lat_max = -8.70
#'         )
#'     )
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
#'
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
sits_cube.sar_cube <- function(source,
                               collection, ...,
                               orbit = "ascending",
                               bands = NULL,
                               tiles = NULL,
                               roi = NULL,
                               crs = NULL,
                               start_date = NULL,
                               end_date = NULL,
                               platform = NULL,
                               multicores = 2,
                               progress = TRUE) {

    sits_cube.stac_cube(
        source = source,
        collection = collection,
        bands = bands,
        tiles = tiles,
        roi = roi,
        crs = crs,
        start_date = start_date,
        end_date = end_date,
        platform = platform,
        multicores = multicores,
        progress = progress,
        orbit = orbit,
        ...
    )
}
#' @rdname sits_cube
#'
#' @export
sits_cube.stac_cube <- function(source,
                                collection, ...,
                                bands = NULL,
                                tiles = NULL,
                                roi = NULL,
                                crs = NULL,
                                start_date = NULL,
                                end_date = NULL,
                                platform = NULL,
                                multicores = 2,
                                progress = TRUE) {

    # Check for ROI and tiles
    .check_roi_tiles(roi, tiles)
    # Ensures that there are no duplicate tiles
    if (.has(tiles)) {
        tiles <- unique(tiles)
    }
    # Converts provided roi to sf
    if (.has(roi)) {
        roi <- .roi_as_sf(roi, default_crs = crs)
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
    cube <- .source_cube(
        source = source,
        collection = collection,
        bands = bands,
        tiles = tiles,
        roi = roi,
        start_date = start_date,
        end_date = end_date,
        platform = platform,
        multicores = multicores,
        progress = progress, ...
    )
    # adjust crs of the cube before return
    .cube_adjust_crs(cube)
}
#' @rdname sits_cube
#'
#' @export
sits_cube.local_cube <- function(source,
                                 collection, ...,
                                 data_dir,
                                 vector_dir = NULL,
                                 tiles = NULL,
                                 bands = NULL,
                                 vector_band = NULL,
                                 start_date = NULL,
                                 end_date = NULL,
                                 labels = NULL,
                                 parse_info = NULL,
                                 version = "v1",
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
    # precondition - check source and collection for eo_cubes only
    # is this a cube with results?
    if (.has(bands) && all(bands %in% .conf("sits_results_bands")))
        results_cube <- TRUE
    else
        results_cube <- FALSE
    if (.has(vector_dir)) {
        if (.has(bands)) {
            .check_that(
                !(all(bands %in% .conf("sits_results_bands"))),
                msg = .conf("messages", "sits_cube_local_cube_vector_band")
            )
        }
        .check_chr_parameter(vector_band,
                             msg = .conf("messages", "sits_cube_local_cube_vector_band")
        )
        .check_that(
            vector_band %in% c("segments", "class", "probs"),
            msg = .conf("messages", "sits_cube_local_cube_vector_band")
        )
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
        vector_dir = vector_dir,
        parse_info = parse_info,
        version = version,
        delim = delim,
        tiles = tiles,
        bands = bands,
        vector_band = vector_band,
        labels = labels,
        start_date = start_date,
        end_date = end_date,
        multicores = multicores,
        progress = progress, ...
    )
    # fix tile system name
    cube <- .cube_revert_tile_name(cube)
    return(cube)
}
#' @export
sits_cube.default <- function(source, collection, ...) {
    stop(.conf("messages", "sits_cube_default"))
}
#' @title Convert MGRS tile information to ROI in WGS84
#' @name sits_mgrs_to_roi
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@gmail.com}
#'
#' @description
#' Takes a list of MGRS tiles and produces a ROI covering them
#'
#' @param  tiles                Character vector with names of MGRS tiles
#' @return roi                  Valid ROI to use in other SITS functions
#'
#' @export
sits_mgrs_to_roi <- function(tiles) {
    warning(paste("'sits_mgrs_to_roi()' is deprecated.",
                   "Please, use 'sits_tiles_to_roi()'."))
    sits_tiles_to_roi(tiles = tiles, grid_system = "MGRS")
}

#' @title Convert MGRS tile information to ROI in WGS84
#' @name sits_tiles_to_roi
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@gmail.com}
#'
#' @description
#' Takes a list of MGRS tiles and produces a ROI covering them
#'
#' @param  tiles                Character vector with names of MGRS tiles
#' @param  grid_system          ...
#' @return roi                  Valid ROI to use in other SITS functions
#'
#' @export
sits_tiles_to_roi <- function(tiles, grid_system = "MGRS") {
    # retrieve the ROI
    roi <- .grid_filter_tiles(grid_system = grid_system, roi = NULL, tiles = tiles)
    sf::st_bbox(roi)
}
