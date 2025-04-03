#' @title Create data cubes from image collections
#' @name sits_cube
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @description Creates a data cube based on spatial and temporal restrictions
#' in collections available in cloud services or local repositories.
#' Available options are:
#' \itemize{
#' \item{To create data cubes from cloud providers which support the STAC protocol,
#' use \code{\link[sits]{sits_cube.stac_cube}}.}
#' \item{To create raster data cubes from local image files,
#' use \code{\link[sits]{sits_cube.local_cube}}.}
#' \item{To create vector data cubes from local image and vector files,
#' use \code{\link[sits]{sits_cube.vector_cube}}.}
#' \item{To create raster data cubes from local image files
#' which have been classified or labelled,
#' use \code{\link[sits]{sits_cube.results_cube}}.}
#' }
#'
#'
#' @param source       Data source: one of \code{"AWS"}, \code{"BDC"},
#'                     \code{"CDSE"}, \code{"DEAFRICA"}, \code{"DEAUSTRALIA"},
#'                     \code{"HLS"}, \code{"PLANETSCOPE"}, \code{"MPC"},
#'                     \code{"SDC"} or \code{"USGS"}.
#' @param collection   Image collection in data source.
#'                     To find out the supported collections,
#'                     use \code{\link{sits_list_collections}()}).
#' @param ...          Other parameters to be passed for specific types.
#'
#' @return A \code{tibble} describing the contents of a data cube.
#'
#' @note
#' The main \code{sits} classification workflow has the following steps:
#' \enumerate{
#'      \item{\code{\link[sits]{sits_cube}}: selects a ARD image collection from
#'          a cloud provider.}
#'      \item{\code{\link[sits]{sits_cube_copy}}: copies the ARD image collection
#'          from a cloud provider to a local directory for faster processing.}
#'      \item{\code{\link[sits]{sits_regularize}}: create a regular data cube
#'          from an ARD image collection.}
#'      \item{\code{\link[sits]{sits_apply}}: create new indices by combining
#'          bands of a  regular data cube (optional).}
#'      \item{\code{\link[sits]{sits_get_data}}: extract time series
#'          from a regular data cube based on user-provided labelled samples.}
#'      \item{\code{\link[sits]{sits_train}}: train a machine learning
#'          model based on image time series.}
#'      \item{\code{\link[sits]{sits_classify}}: classify a data cube
#'          using a machine learning model and obtain a probability cube.}
#'      \item{\code{\link[sits]{sits_smooth}}: post-process a probability cube
#'          using a spatial smoother to remove outliers and
#'          increase spatial consistency.}
#'      \item{\code{\link[sits]{sits_label_classification}}: produce a
#'          classified map by selecting the label with the highest probability
#'          from a smoothed cube.}
#' }
#'
#' The following cloud providers are supported, based on the STAC protocol:
#' Amazon Web Services (AWS), Brazil Data Cube (BDC),
#' Copernicus Data Space Ecosystem (CDSE), Digital Earth Africa (DEAFRICA),
#' Digital Earth Australia (DEAUSTRALIA), Microsoft Planetary Computer (MPC),
#' Nasa Harmonized Landsat/Sentinel (HLS), Swiss Data Cube (SDC), TERRASCOPE and
#' USGS Landsat (USGS). Data cubes can also be created using local files.
#'
#' In \code{sits}, a data cube is represented as a tibble with metadata
#' describing a set of image files obtained from cloud providers.
#' It contains information about each individual file.
#'
#' A data cube in \code{sits} is:
#' \itemize{
#' \item{A set of images organized in tiles of a grid system (e.g., MGRS).}
#' \item{Each tile contains single-band images in a
#'  unique zone of the coordinate system (e.g, tile 20LMR in MGRS grid)
#'  covering the period between \code{start_date} and \code{end_date}.}
#' \item{Each image of a tile is associated to a unique temporal interval.
#' All intervals share the same spectral bands.}
#' \item{Different tiles may cover different zones of the same grid system.}
#' }
#' A regular data cube is a data cube where:
#' \itemize{
#' \item{All tiles share the same set of regular temporal intervals.}
#' \item{All tiles share the same spectral bands and indices.}
#' \item{All images have the same spatial resolution.}
#' \item{Each location in a tile is associated a set of multi-band time series.}
#' \item{For each tile, interval and band, the cube is associated to a 2D image.}
#' }
#
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
sits_cube <- function(source, collection, ...) {
    # set caller to show in errors
    .check_set_caller("sits_cube")
    # capture elipsis
    dots <- list(...)
    # if "data_dir" parameters is provided, assumes local cube
    if ("data_dir" %in% names(dots)) {
        source <- .source_new(source = source, is_local = TRUE)
        if ("bands" %in% names(dots)) {
            bands <- dots["bands"]
            if (bands %in% .conf("sits_results_bands")) {
                source <- .source_new(source = source,
                                      is_local = TRUE, is_result = TRUE)
                return(source)

            }
        } else if ("vector_dir" %in% names(dots)) {
            if ("vector_band" %in% names(dots)) {
                vector_band <- dots["vector_band"]
                if (vector_band %in% .conf("sits_results_bands")) {
                    source <- .source_new(source = source, is_vector = TRUE,
                                          is_local = TRUE)
                }
            }
        }
    } else if ("raster_cube" %in% names(dots)) {
        source <- .source_new(source = source, is_local = TRUE,
                              is_vector = TRUE)
    } else {
        source <- .source_new(source = source, collection = collection)
    }
    # Dispatch
    UseMethod("sits_cube", source)
}
#' @title Create data cubes from image collections acessible by STAC
#' @name sits_cube.stac_cube
#'
#' @description Creates a data cube based on spatial and temporal restrictions
#' in collections accesible by the STAC protocol
#'
#' @param source       Data source: one of \code{"AWS"}, \code{"BDC"},
#'                     \code{"CDSE"}, \code{"DEAFRICA"}, \code{"DEAUSTRALIA"},
#'                     \code{"HLS"}, \code{"PLANETSCOPE"}, \code{"MPC"},
#'                     \code{"SDC"} or \code{"USGS"}.
#' @param collection   Image collection in data source.
#'                     To find out the supported collections,
#'                     use \code{\link{sits_list_collections}()}).
#' @param ...          Other parameters to be passed for specific types.
#' @param platform     Optional parameter specifying the platform in case
#'                     of "LANDSAT" collection. Options: \code{Landsat-5,
#'                     Landsat-7, Landsat-8, Landsat-9}.
#' @param tiles        Tiles from the collection to be included in
#'                     the cube (see details below).
#' @param roi          Region of interest (see below).
#' @param crs          The Coordinate Reference System (CRS) of the roi.
#'                     (see details below).
#' @param bands        Spectral bands and indices to be included
#'                     in the cube (optional).
#'                     Use \code{\link{sits_list_collections}()} to find out
#'                     the bands available for each collection.
#' @param start_date,end_date Initial and final dates to include
#'                     images from the collection in the cube (optional).
#'                     (Date in YYYY-MM-DD format).
#' @param orbit        Orbit name ("ascending", "descending") for SAR cubes.
#' @param multicores   Number of workers for parallel processing
#'                     (integer, min = 1, max = 2048).
#' @param progress     Logical: show a progress bar?
#' @return A \code{tibble} describing the contents of a data cube.
#'
#' @note
#'
#' Data cubes are identified on cloud providers using \code{sits_cube}.
#' The result of \code{sits_cube} is a description of the location
#' of the requested data in the cloud provider. No download is done.
#'
#' To create data cube objects from cloud providers, users need to inform:
#' \itemize{
#'  \item{\code{source}: Name of the cloud provider.
#'   One of "AWS", "BDC", "CDSE", "DEAFRICA", "DEAUSTRALIA",
#'  "HLS", "PLANETSCOPE", "MPC", "SDC", "TERRASCOPE", or "USGS";}
#'  \item{\code{collection}: Name of an image collection available
#'         in the cloud provider (e.g, "SENTINEL-1-RTC" in MPC).
#'         Use \code{\link{sits_list_collections}()} to see which
#'         collections are supported;}
#'  \item{ \code{tiles}: A set of tiles defined according to the collection
#'         tiling grid (e.g, c("20LMR", "20LMP") in MGRS);}
#'  \item{\code{roi}: Region of interest (see below)}
#' }
#'
#' The parameters \code{bands}, \code{start_date}, and \code{end_date} are
#'  optional for cubes created from cloud providers.
#'
#' Either \code{tiles} or \code{roi} must be informed. The \code{tiles}
#' should specify a set of valid tiles for the ARD collection.
#' For example, Landsat data has tiles in \code{WRS2} tiling system
#' and Sentinel-2 data uses the \code{MGRS} tiling system.
#' The \code{roi} parameter is used to select all types of images.
#' This parameter does not crop a region; it only
#' selects images that intersect it.
#'
#' To define a \code{roi} use one of:
#'  \itemize{
#'        \item{A path to a shapefile with polygons;}
#'        \item{A \code{sfc} or \code{sf} object from \code{sf} package;}
#'        \item{A \code{SpatExtent} object from \code{terra} package;}
#'        \item{A named \code{vector} (\code{"lon_min"},
#'             \code{"lat_min"}, \code{"lon_max"}, \code{"lat_max"}) in WGS84;}
#'        \item{A named \code{vector} (\code{"xmin"}, \code{"xmax"},
#'              \code{"ymin"}, \code{"ymax"}) with XY coordinates.}
#'  }
#' Defining a region of interest using \code{SpatExtent} or XY values not in
#' WGS84 requires the \code{crs} parameter to be specified.
#'
#' To get more details about each provider and collection
#'  available in \code{sits}, please read the online sits book
#' (e-sensing.github.io/sitsbook). The chapter
#' \code{Earth Observation data cubes} provides a detailed description of all
#' collections you can use with \code{sits}
#' (e-sensing.github.io/sitsbook/earth-observation-data-cubes.html).
#'
#'
#' @examples
#' if (sits_run_examples()) {
#' # --- Creating Sentinel cube from MPC
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
#'     # --- Obtain a AWS_ACCESS_KEY_ID and AWS_ACCESS_SECRET_KEY_ID
#'     # --- from CDSE
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
#'      )
#'
#'
#'    # -- Access to World Cover data (2021) via Terrascope
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
#' }
#' @export
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
                                orbit = "descending",
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
    roi <- .grid_filter_tiles(grid_system = grid_system,
                              roi = NULL,
                              tiles = tiles)
    sf::st_bbox(roi)
}
