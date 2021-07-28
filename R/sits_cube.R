#' @title Defines a data cube
#' @name sits_cube
#'
#' @references `rstac` package (https://github.com/brazil-data-cube/rstac)
#'
#' @description Creates a data cube based on spatial and temporal restrictions
#' on a collection available in repositories such as AWS, Brazil Data Cube
#' (BDC), and Digital Earth Africa (DEA), using information provided by STAC
#' end points. Users can also create data cubes from local files.
#'
#' A data cube does not contain actual data; it points to the files where the
#' required data is archived. Other functions (e.g. `sits_classify`) use
#' that information to retrieve and process data.
#'
#' Currently, users can create data cube from the following sources:
#' \itemize{
#'  \item{"BDC": }{Brazil Data Cube (BDC),
#'   see also https://brazil-data-cube.github.io/applications/stac.html}
#'   \item{"WTSS": }{Web Time Series Service from BDC,
#'    see also https://brazil-data-cube.github.io/applications/wtss.html}
#'  \item{"DEAFRICA": }{Digital Earth Africa,
#'   see also https://www.digitalearthafrica.org/}
#'  \item{"AWS": }{Amazon Web Services (AWS),
#'   see also https://earth-search.aws.element84.com/v0/ }
#'  \item{"USGS": }{United States Geological Survey (USGS),
#'   see also https://landsatlook.usgs.gov/sat-api/stac/}
#'  \item{"LOCAL": }{Defines a cube from on a set of local files.}
#'  \item{"PROBS": }{Defines a cube to from a set of classified image files}.
#'  \item{"SATVEG": }{Defines a cube to use the SATVEG web service,
#'   see also https://www.satveg.cnptia.embrapa.br/satveg/login.html}
#'  }
#'
#' For big data sources such as AWS, BDC and DEA, users need to provide:
#' \itemize{
#' \item{collection: }{Collections are the highest level of aggregation on
#' bug data repositories. Each repository has its own set of collections,
#' described by STAC. To use STAC for quering repositories, please use the
#' package `rstac`.}
#' \item{spatial extent: }{The spatial extent of the data cube can be defined
#' in two ways: (a) a region of interest(`roi`) in WGS 84 coordinates;
#' (b) a set of tiles defined according the collection tiling system.}
#' \item{temporal extent: }{The start and end date of the cube}
#' }
#'
#'@note For AWS, sits currently only works with collection "s2_l2a".
#' Users need to provide AWS credentials using environment variables.
#' # Sys.setenv(
#' # "AWS_ACCESS_KEY_ID"     = <your_access_key>,
#' # "AWS_SECRET_ACCESS_KEY" = <your_secret_access_key>,
#' # "AWS_DEFAULT_REGION"    = <your AWS region>,
#' # "AWS_S3_ENDPOINT" = "s3.amazonaws.com",
#' # "AWS_REQUEST_PAYER"     = "requester"
#' # )
#'
#' @note Sentinel-2/2A level 2A files in AWS are organized by sensor
#' resolution. The AWS bands in 10m resolution are "B02", "B03", "B04", and
#' "B08". The  20m bands are "B02", "B03", "B04", "B05", "B06", "BO7", B08",
#' "B8A", "B11", and "B12". All 12 bands are available at 60m resolution.
#' For creating data cubes from Sentinel-2/2A, users also have to specify
#' the `s2_resolution` parameter.
#'
#' @note For DEA, sits currently only works with collections 'ga_s2_gm' and
#' 's2_l2a'. DEA users also need to provide their AWS credentials.
#'
#' @note For USGS, use the 'landsat-c2l2-sr' collection to consult the Landsat-8
#' satellite images. In order to do so, you need to provide the AWS keys as
#' mentioned above.
#'
#'@note BDC users need to provide their credentials using environmental
#' variables.To create your credencials, please see
#'  "https://brazildatacube.dpi.inpe.br/portal/explore"
#' # Sys.setenv(
#' # "BDC_ACCESS_KEY" = <your_bdc_access_key>
#' # )
#'
#'@note To create a cube from local files, all image files should have
#' the same spatial resolution and projection. Files can belong to different
#' tiles of a spatial reference system.
#' Each file should contain a single image band for a single date.
#' File names must include date and band information, since times and bands
#' are deduced from filenames. For example: "CBERS-4_022024_B13_2018-02-02.tif"
#' and "cube_20LKP_B02_2018-07-18.jp2" are accepted names.
#' The user has to provide parsing information to allow `sits`
#' to extract the tile, the band and the date. In the examples above,
#' the parsing info is c("X1", "X2", "tile", "band", "date") and the delimiter is "_".
#'
#'
#' @note The SATVEG service is run by Embrapa Agricultural
#'  Informatics Centre provides access to time series from the MODIS sensor.
#'  There are three collections: "terra" (from the TERRA satellite),
#'  "aqua" (from the AQUA satellite) and "comb" (combination of
#'  both satellites).
#'
#'
#' @param source            Data source (one of "SATVEG", "LOCAL",
#'                          "BDC", "AWS", "USGS", "DEAFRICA", "PROBS").
#' @param ...               Other parameters to be passed for specific types
#' @param name              Name of the output data cube.
#' @param url               URL for the STAC endpoint of the data source
#' @param collection        Collection to be searched in the data source
#' @param bands             Bands to be included
#' @param tiles             Tiles from the collection to be included in the
#'                          data cube
#' @param bbox              Area of interest (see details below)
#' @param start_date        Initial date for the cube (optional).
#' @param end_date          Final date for the cube  (optional)
#' @param s2_resolution     Resolution of S2 images ("10m", "20m" or "60m")
#'                          used to build cubes (only for AWS cubes)
#' @param satellite         Satellite that produced the images.
#'                          (only for creating data cubes from local files)
#' @param sensor            Sensor that produced the images.
#' @param data_dir          directory where local data is located
#'                          (only for creating data cubes from local files)
#' @param delim             delimiter for parsing files without STAC information
#'                          (only for creating data cubes from local files)
#' @param parse_info        parsing information for files without STAC
#'                          information
#'                          (only for creating data cubes from local files)
#' @param probs_files       File names (used for creating a cube from
#'                          probabilities)
#' @param probs_labels      Labels associated to a probabilities cube
#'
#' @details  The "bbox" parameter allows a selection of an area of interest.
#' Either using a named \code{vector} ("xmin", "ymin", "xmax", "ymax") with
#' values in WGS 84, a \code{sfc} or \code{sf} object from sf package, or a
#' GeoJSON geometry (RFC 7946). Note that this parameter does not crop a
#' region, but only selects the images that intersect with it.
#'
#' @return The description of a data cube
#'
#' @examples
#' \dontrun{-=-
#'
#' # --- Access to the Brazil Data Cube
#' # Provide your BDC credentials as environment variables
#' Sys.setenv(
#'     "BDC_ACCESS_KEY" = <your_bdc_access_key>
#' )
#'
#' # create a raster cube file based on the information in the BDC
#' cbers_tile <- sits_cube(
#'     source = "BDC",
#'     collection = "CB4_64_16D_STK-1",
#'     name = "cbers_022024",
#'     bands = c("NDVI", "EVI"),
#'     tiles = "022024",
#'     start_date = "2018-09-01",
#'     end_date = "2019-08-28"
#' )
#'
#' # --- Create a WTSS cube from BDC cubes
#' # Provide your BDC credentials as environment variables
#' Sys.setenv(
#'     "BDC_ACCESS_KEY" = <your_bdc_access_key>
#' )
#'
#' cube_wtss <- sits::sits_cube(source = "WTSS",
#'                              collection = "MOD13Q1-6")
#'
#' # --- Access to Digital Earth Africa
#' # Provide your AWS credentials as environment variables
#' Sys.setenv(
#'     "AWS_ACCESS_KEY_ID" = <your_aws_access_key>,
#'     "AWS_SECRET_ACCESS_KEY" = <your_aws_secret_access_key>
#' )
#'
#' # create a raster cube file based on the information about the files
#' cube_dea <- sits_cube(source = "DEAFRICA",
#'                       name = "deafrica_cube",
#'                       collection = "s2_l2a",
#'                       bands = c("B04", "B08"),
#'                       bbox = c("xmin" = 17.379,
#'                               "ymin" = 1.1573,
#'                               "xmax" = 17.410,
#'                                "ymax" = 1.1910),
#'                       start_date = "2019-01-01",
#'                       end_date = "2019-10-28")
#'
#' # --- Access to Sentinel 2/2A level 2 data in AWS
#' # Provide your AWS credentials as environment variables
#' Sys.setenv(
#'     "AWS_ACCESS_KEY_ID" = <your_aws_access_key>,
#'     "AWS_SECRET_ACCESS_KEY" = <your_aws_secret_access_key>
#' )
#'
#' s2_cube <- sits_cube(source = "AWS",
#'                       name = "T20LKP_2018_2019",
#'                       collection = "sentinel-s2-l2a",
#'                       tiles = c("20LKP","20LLP"),
#'                       start_date = as.Date("2018-07-18"),
#'                       end_date = as.Date("2018-07-23"),
#'                       s2_resolution = 20
#' )
#'
#' # --- Access to Landsat-8 level 2 data in AWS
#' # Provide your AWS credentials as environment variables
#' Sys.setenv(
#'     "AWS_ACCESS_KEY_ID" = <your_aws_access_key>,
#'     "AWS_SECRET_ACCESS_KEY" = <your_aws_secret_access_key>
#' )
#'
#'
#'l8_cube <- sits_cube(source = "USGS",
#'                      name = "L8_CUBE_140048_140045_18_2019",
#'                      collection = "landsat-c2l2-sr",
#'                      tiles = c("140048", "140045"),
#'                      start_date = as.Date("2019-01-01"),
#'                      end_date = as.Date("2019-12-31"),
#' )
#'
#' # --- Create a cube based on a stack of CBERS data
#' data_dir <- system.file("extdata/raster/cbers", package = "sits")
#'
#' cbers_cube <- sits_cube(
#'     source = "LOCAL",
#'     name = "022024",
#'     satellite = "CBERS-4",
#'     sensor = "AWFI",
#'     data_dir = data_dir,
#'     delim = "_",
#'     parse_info = c("X1", "X2", "tile", "band", "date")
#' )
#'
#' # Create a raster cube based on files with probability information
#' # inform the files that make up a raster probs brick with 23 time instances
#' probs_file <- c(system.file(
#'     "extdata/raster/probs/sinop-2014_probs_2013_9_2014_8_v1.tif",
#'     package = "sits"
#' ))
#'
#' # inform the labels
#' labels <- c("Cerrado", "Fallow_Cotton", "Forest", "Pasture", "Soy_Corn",
#' "Soy_Cotton", "Soy_Fallow", "Soy_Millet", "Soy_Sunflower")
#'
#'
#' # create a raster cube file based on the information about the files
#' probs_cube <- sits_cube(
#'     source = "PROBS",
#'     name = "Sinop-crop-probs",
#'     satellite = "TERRA",
#'     sensor  = "MODIS",
#'     start_date = as.Date("2013-09-14"),
#'     end_date = as.Date("2014-08-29"),
#'     probs_labels = labels,
#'     probs_files = probs_file
#' )
#' }
#'
#' @export
#'
sits_cube <- function(source, ...) {

    s <- .source_new(source = source)

    # Dispatch
    UseMethod("sits_cube", s)
}

#' @rdname sits_cube
#'
#' @export
#'
sits_cube.wtss_cube <- function(source = "WTSS", ...,
                                name = "wtss_cube",
                                url = NULL,
                                collection) {


    # precondition - is the collection name valid?
    assertthat::assert_that(
        !purrr::is_null(collection),
        msg = "sits_cube: WTSS collection must be provided."
    )

    # Pre-condition - try to find the access key as an environment variable
    bdc_access_key <- Sys.getenv("BDC_ACCESS_KEY")
    assertthat::assert_that(
        nchar(bdc_access_key) != 0,
        msg = "sits_cube: BDC_ACCESS_KEY needs to be provided"
    )

    .source_access_test(source, collection, ...)

    .source_cube(source = source, ...,
                 collection = collection,
                 name = name,
                 bands = bands)
}

#' @rdname sits_cube
#'
#' @export
sits_cube.bdc_cube <- function(source = "BDC", ...,
                               name = "bdc_cube",
                               collection,
                               bands = NULL,
                               tiles = NULL,
                               bbox = NULL,
                               start_date = NULL,
                               end_date = NULL) {

    # TODO: check all user parameters and environment

    # precondition
    assertthat::assert_that(
        length(collection) == 1,
        msg = "sits_cube.bdc_cube: only one BDC collection should be specified."
    )

    # precondition
    assertthat::assert_that(
        collection %in% .config_collections(source = source),
        msg = sprintf(paste("sits_cube.bdc_cube: collection '%s' not found in",
                            "BDC source.\nPlease, check sits config with",
                            "?sits_config command."), collection)
    )

    # try to find the access key as an environment variable
    bdc_access_key <- Sys.getenv("BDC_ACCESS_KEY")
    assertthat::assert_that(
        nchar(bdc_access_key) != 0,
        msg = "sits_cube.bdc_cube: BDC_ACCESS_KEY needs to be provided"
    )

    if (is.null(bands))
        bands <- .config_bands(source = source,
                               collection = collection)

    assertthat::assert_that(
        all(bands %in% c(.config_bands(source = source, collection = collection),
                         .config_bands_band_name(source = source,
                                                 collection = collection))),
        msg = "sits_cube.bdc_cube: invalid bands.\nPlease the provided bands."
    )

    # check if source can be access
    .source_access_test(source = source,
                        collection = collection, ...,
                        bands = bands)

    .source_cube(source = source,
                 collection = collection,
                 name = name,
                 bands = bands,
                 tiles = tiles,
                 bbox = bbox,
                 start_date = start_date,
                 end_date = end_date, ...)
}

#' @rdname sits_cube
#'
#' @export
sits_cube.deafrica_cube <- function(source = "DEAFRICA", ...,
                                    name = "deafrica_cube",
                                    url = NULL,
                                    collection = "s2_l2a",
                                    bands = NULL,
                                    tiles = NULL,
                                    bbox = NULL,
                                    start_date = NULL,
                                    end_date = NULL) {

    # precondition
    assertthat::assert_that(
        length(collection) == 1,
        msg = paste("sits_cube.deafrica_cube: only one BDC collection should",
                    "be specified.")
    )

    # precondition
    assertthat::assert_that(
        collection %in% .config_collections(source = source),
        msg = sprintf(paste("sits_cube.deafrica_cube: collection '%s' not",
                            "found in DEAfrica source.\nPlease, check sits",
                            "config with ?sits_config command."), collection)
    )

    # precondition - is AWS access available?
    .check_aws_environment(source, collection)

    if (is.null(bands))
        bands <- .config_bands(source = source,
                               collection = collection)

    assertthat::assert_that(
        all(bands %in% c(.config_bands(source = source, collection = collection),
                         .config_bands_band_name(source = source,
                                                 collection = collection))),
        msg = paste("sits_cube.deafrica_cube: invalid bands.\nPlease the",
                    "provided bands.")
    )

    # check if source can be access
    .source_access_test(source = source,
                        collection = collection, ...,
                        bands = bands)

    .source_cube(source = source,
                 collection = collection,
                 name = name,
                 bands = bands,
                 tiles = tiles,
                 bbox = bbox,
                 start_date = start_date,
                 end_date = end_date, ...)
}

#' @rdname sits_cube
#'
#' @export
sits_cube.aws_cube <- function(source = "AWS", ...,
                               name = "aws_cube",
                               url = NULL,
                               collection = "sentinel-s2-l2a",
                               tiles = NULL,
                               bands = NULL,
                               bbox = NULL,
                               s2_resolution = 20,
                               start_date = NULL,
                               end_date = NULL) {

    # precondition - is the provided resolution is valid?
    assertthat::assert_that(
        s2_resolution %in% c(10, 20, 60),
        msg = "sits_cube: s2_resolution should be one of c(10, 20, 60)"
    )

    # precondition
    assertthat::assert_that(
        length(collection) == 1,
        msg = paste("sits_cube.aws_cube: only one aws collection should",
                    "be specified.")
    )

    # precondition
    assertthat::assert_that(
        collection %in% .config_collections(source = source),
        msg = sprintf(paste("sits_cube.aws_cube: collection '%s' not",
                            "found in aws source.\nPlease, check sits",
                            "config with ?sits_config command."), collection)
    )

    # precondition - is AWS access available?
    .check_aws_environment(source, collection)

    if (is.null(bands))
        bands <- .aws_bands(source = source,
                            collection = collection,
                            s2_resolution = s2_resolution)

    assertthat::assert_that(
        all(bands %in% c(.aws_bands(source = source,
                                    collection = collection,
                                    s2_resolution = s2_resolution),
                         .aws_bands_band_name(source = source,
                                              collection = collection,
                                              s2_resolution = s2_resolution))),
        msg = paste("sits_cube.aws_cube: invalid bands.\nPlease the provided",
                    "bands.")
    )


    # check if source can be access
    .source_access_test(source = source,
                        collection = collection, ...,
                        bands = bands,
                        s2_resolution = s2_resolution)

    .source_cube(source = source,
                 collection = collection,
                 name = name,
                 bands = bands,
                 tiles = tiles,
                 bbox = bbox,
                 start_date = start_date,
                 end_date = end_date, ...,
                 s2_resolution = s2_resolution)
}

#' @rdname sits_cube
#'
#' @export
sits_cube.usgs_cube <- function(source = "USGS", ...,
                                name = "usgs_cube",
                                url = NULL,
                                collection = "landsat-c2l2-sr",
                                tiles = NULL,
                                bands = NULL,
                                bbox = NULL,
                                start_date = NULL,
                                end_date = NULL) {

    # precondition
    assertthat::assert_that(
        length(collection) == 1,
        msg = paste("sits_cube.usgs_cube: only one USGS collection should",
                    "be specified.")
    )

    # precondition
    assertthat::assert_that(
        collection %in% .config_collections(source = source),
        msg = sprintf(paste("sits_cube.usgs_cube: collection '%s' not",
                            "found in USGS source.\nPlease, check sits",
                            "config with ?sits_config command."), collection)
    )

    # precondition - is AWS access available?
    .check_aws_environment(source, collection)

    if (is.null(bands))
        bands <- .config_bands(source = source,
                               collection = collection)

    assertthat::assert_that(
        all(bands %in% c(.config_bands(source = source, collection = collection),
                         .config_bands_band_name(source = source,
                                                 collection = collection))),
        msg = paste("sits_cube.usgs_cube: invalid bands.\nPlease the",
                    "provided bands.")
    )

    # check if source can be access
    .source_access_test(source = source,
                        collection = collection, ...,
                        bands = bands)

    .source_cube(source = source,
                 collection = collection,
                 name = name,
                 bands = bands,
                 tiles = tiles,
                 bbox = bbox,
                 start_date = start_date,
                 end_date = end_date, ...)
}

#' @rdname sits_cube
#'
#' @export
sits_cube.local_cube <- function(source = "LOCAL", ...,
                                 name   = "local_cube",
                                 satellite,
                                 sensor,
                                 bands = NULL,
                                 start_date = NULL,
                                 end_date = NULL,
                                 data_dir,
                                 parse_info = c("X1", "X2", "tile", "band", "date"),
                                 delim = "_") {


    # precondition - data directory must be provided
    assertthat::assert_that(
        !purrr::is_null(data_dir),
        msg = "sits_cube: data_dir must be to be provided"
    )

    collection <- paste0(satellite, "/", sensor)

    # precondition - check satellite and sensor
    .source_access_test(source = source, collection = collection)

    # precondition - check parse info
    assertthat::assert_that(
        length(parse_info) >= 2,
        msg = "sits_cube: invalid parsing information"
    )

    # precondition - does the parse info have band and date?
    assertthat::assert_that(
        all(c("tile", "band", "date") %in% parse_info),
        msg = paste("sits_cube.local_cube: parse_info must include tile, date,",
                    "and band.")
    )

    # bands in upper case
    if (!purrr::is_null(bands))
        bands <- toupper(bands)

    # create local cube
    .source_cube(source = source, ...,
                 collection = collection,
                 name = name,
                 data_dir = data_dir,
                 parse_info = parse_info,
                 delim = delim,
                 bands = bands,
                 start_date = start_date,
                 end_date = end_date)
}

#' @rdname sits_cube
#'
#' @export
sits_cube.probs_cube <- function(source = "PROBS", ...,
                                 name = "probs_cube",
                                 satellite,
                                 sensor,
                                 start_date,
                                 end_date,
                                 probs_labels,
                                 probs_files) {


    .source_cube(source = source, ...,
                 name = name,
                 satellite = satellite,
                 sensor = sensor,
                 start_date = start_date,
                 end_date = end_date,
                 probs_labels = probs_labels,
                 probs_files = probs_files)
}

#' @rdname sits_cube
#'
#' @export
sits_cube.satveg_cube <- function(source = "SATVEG", ...,
                                  collection = "terra") {


    # precondition
    assertthat::assert_that(
        collection %in% c("terra", "aqua", "comb"),
        msg = "sits_cube.satveg_cube: invalid SATVEG collection."
    )

    # precondition - is service online?
    .source_access_test(source = source, collection = collection)

    # creating satveg cube
    .source_cube(source = source, collection = collection, name = "SATVEG")
}

#' @export
sits_cube.default <- function(source, ...) {
    stop("sits_cube: source not found.")
}

#' @title Creates the contents of a data cube
#'
#' @name sits_cube_copy
#'
#' @description Copies the metadata and data of a cube to a different
#' directory. This function can be use to transfer data on the cloud
#' to a local machine. The region of interest (roi) should be either
#' an "sf" object, a box in XY coordinates ("xmin", "xmax", "ymin", "ymax") or
#' a box in lat-long coordinates ("lon_min", "lon_max", "lat_min", "lat_max").
#'
#' @param  cube      Input data cube
#' @param  name      Output cube name
#' @param  dest_dir  Destination directory
#' @param  bands     Bands to include in output (optional)
#' @param  roi       Region of interest (either "sf", "xy", or "latlong")
#'
#' @return           Output data cube
#'
#' @examples
#' \donttest{
#' data_dir <- system.file("extdata/raster/cbers", package = "sits")
#'
#' cbers_022024 <- sits_cube(
#'     source = "LOCAL",
#'     name = "cbers_022024",
#'     satellite = "CBERS-4",
#'     band = "NDVI",
#'     sensor = "AWFI",
#'     data_dir = data_dir,
#'     parse_info = c("X1", "X2", "tile", "band", "date")
#' )
#'
#' cbers_022024_copy <- sits_cube_copy(cbers_022024,
#'     name = "cb_022024_cp",
#'     dest_dir = tempdir()
#' )
#' }
#'
#' @export
sits_cube_copy <- function(cube,
                           name,
                           dest_dir,
                           bands = sits_bands(cube),
                           roi = NULL) {

    # does the output directory exist?
    assertthat::is.dir(dest_dir)
    if (purrr::is_null(roi))
        bbox <- sits_bbox(cube)
    else
        # get the bounding box
        bbox <- .sits_roi_bbox(roi, cube)

    # Get the subimage
    si <- .sits_sub_image_from_bbox(bbox, cube)

    # test subwindow
    srcwin <- vector("double", length = 4)
    names(srcwin) <- c("xoff", "yoff", "xsize", "ysize")
    srcwin["xoff"] <- si["first_col"] - 1
    srcwin["yoff"] <- si["first_row"] - 1
    srcwin["xsize"] <- si["ncols"]
    srcwin["ysize"] <- si["nrows"]

    assertthat::assert_that(
        (srcwin["xoff"] + srcwin["xsize"]) <= cube$ncols,
        msg = "sits_cube_copy: srcwin x values bigger than cube size"
    )
    assertthat::assert_that(
        (srcwin["yoff"] + srcwin["ysize"]) <= cube$nrows,
        msg = "sits_cube_copy: srcwin y values bigger than cube size"
    )

    # the label cube may contain several classified images
    cube_rows <- slider::slide(cube, function(row) {
        # get information on the file
        file_info <- row$file_info[[1]]

        # are the selected bands in the cube?
        assertthat::assert_that(
            all(bands %in% sits_bands(row)),
            msg = "sits_cube_copy: input bands not available in the cube"
        )

        # get all the bands which are requested
        file_info_out <- dplyr::filter(file_info, band %in% bands)
        # get the file extension
        file_ext <- tools::file_ext(file_info_out$path[1])

        if (file_ext == "jp2") {
            gdal_of <- "JP2OpenJPEG"
        } else {
            gdal_of <- "GTiff"
        }

        # save files with date information
        paths <- slider::slide(file_info_out, function(file_row) {
            dest_file <- paste0(
                dest_dir, "/",
                row$satellite, "_",
                row$sensor, "_",
                file_row$band, "_",
                file_row$date, ".",
                file_ext
            )

            gdalUtilities::gdal_translate(
                src_dataset = file_row$path,
                dst_dataset = dest_file,
                of = gdal_of,
                srcwin = srcwin
            )
            return(dest_file)
        })
        # update file info
        new_paths <- unlist(paths)

        # update cube
        row$nrows <- srcwin["ysize"]
        row$ncols <- srcwin["xsize"]
        row$xmin <- row$xmin + srcwin["xoff"] * row$xres
        row$ymin <- row$ymin + srcwin["yoff"] * row$yres
        row$xmax <- row$xmin + (srcwin["xsize"] - 1) * row$xres
        row$ymax <- row$ymin + (srcwin["ysize"] - 1) * row$yres
        file_info_out$path <- new_paths
        row$file_info[[1]] <- file_info_out
        row$name <- name
        row$bands[[1]] <- bands
        return(row)
    })
    cube <- do.call(rbind, cube_rows)
    return(cube)
}


#' @title Auxiliary functions for sits cubes
#' @keywords internal
#'
#' @name cube_functions
#'
#' @description Auxiliary functions for extracting information from sits cubes.
#'
#' @param cube  Data cube from where data is to be retrieved.
#' @param tile  Tiles from the collection to be included in the data cube
#' @param bands Bands to be included.
#' @param ....  Additional parameters to be included.
#'
#' @return a \code{vector} for get attributes functions and NULL or error for
#' check parameters functions.

#' @rdname cube_functions
.cube_satellite <- function(cube) {

    cube[["satellite"]][[1]]
}

#' @rdname cube_functions
.cube_sensor <- function(cube) {

    cube[["sensor"]][[1]]
}

#' @rdname cube_functions
.cube_collection <- function(cube) {

    cube[["collection"]][[1]]
}

#' @rdname cube_functions
.cube_name <- function(cube) {

    cube[["name"]][[1]]
}

#' @rdname cube_functions
.cube_labels <- function(cube) {

    cube[["labels"]][[1]]
}

#' @rdname cube_functions
.cube_bands <- function(cube) {

    cube[["bands"]][[1]]
}

#' @rdname cube_functions
.cube_source <- function(cube) {

    cube[["source"]][[1]]
}

#' @rdname cube_functions
.cube_timeline <- function(cube) {

    sort(unique(cube[["file_info"]][["date"]]))
}

#' @rdname cube_functions
.cube_tiles <- function(cube) {

    cube[["tile"]]
}

#' @rdname cube_functions
.cube_tile_check <- function(cube, tile) {

    assertthat::assert_that(
        is.numeric(tile) || is.character(tile),
        msg = ".cube_tile_check: tile must be numeric or character"
    )

    if (is.numeric(tile)) {
        assertthat::assert_that(
            tile > 0 && tile < nrow(cube),
            msg = ".cube_tile_check: invalid tile"
        )
    }

    if (is.character(tile)) {
        assertthat::assert_that(
            tile %in% .cube_tiles(cube = cube),
            msg = ".cube_tile_check: invalid tile"
        )
    }

    return(invisible(NULL))
}

#' @rdname cube_functions
.cube_tile_get_fields <- function(cube, tile, fields) {

    tile <- tile[[1]]

    if (is.numeric(tile))
        return(unlist(cube[tile, fields]))

    return(unlist(cube[which(.cube_tiles(cube = cube) %in% tile),
                       fields]))
}

#' @rdname cube_functions
.cube_tile_crs <- function(cube, ...,
                           tile = 1) {

    .cube_tile_get_fields(cube = cube,  tile = tile, fields = "crs")
}

#' @rdname cube_functions
.cube_tile_bbox <- function(cube, ...,
                            tile = 1) {

    .cube_tile_get_fields(cube = cube, tile = tile,
                         fields = c("xmin", "ymin", "xmax", "ymax"))
}

#' @rdname cube_functions
.cube_tile_resolution <- function(cube, ...,
                                  tile = 1) {

    .cube_tile_get_fields(cube = cube, tile = tile,
                         fields = c("xres", "yres"))
}

#' @rdname cube_functions
.cube_tile_size <- function(cube, ...,
                            tile = 1) {

    .cube_tile_get_fields(cube = cube, tile = tile,
                         fields = c("nrows", "ncols"))
}

#' @rdname cube_functions
.cube_bands_missing_value <- function(cube, ...,
                                      bands = NULL) {

    if (is.null(bands))
        bands <- .cube_bands(cube = cube)

    .config_bands_reap(source = .cube_source(cube),
                       collection = .cube_collection(cube),
                       key = "missing_value", bands = bands,
                       add_cloud = FALSE)
}

#' @rdname cube_functions
.cube_bands_missing_value <- function(cube, ...,
                                      bands = NULL) {

    if (is.null(bands))
        bands <- .cube_bands(cube = cube)

    .config_bands_reap(source = .cube_source(cube),
                       collection = .cube_collection(cube),
                       key = "missing_value", bands = bands,
                       add_cloud = FALSE)
}

#' @rdname cube_functions
.cube_bands_minimum_value <- function(cube, ...,
                                      bands = NULL) {

    if (is.null(bands))
        bands <- .cube_bands(cube = cube)

    .config_bands_reap(source = .cube_source(cube = cube),
                       collection = .cube_collection(cube = cube),
                       key = "minimum_value", bands = bands,
                       add_cloud = FALSE)
}

#' @rdname cube_functions
.cube_bands_maximum_value <- function(cube, ...,
                                      bands = NULL) {

    if (is.null(bands))
        bands <- .cube_bands(cube = cube)

    .config_bands_reap(source = .cube_source(cube = cube),
                       collection = .cube_collection(cube = cube),
                       key = "maximum_value", bands = bands,
                       add_cloud = FALSE)
}

#' @rdname cube_functions
.cube_bands_scale_factor <- function(cube, ...,
                                     bands = NULL) {

    if (is.null(bands))
        bands <- .cube_bands(cube = cube)

    .config_bands_reap(source = .cube_source(cube = cube),
                       collection = .cube_collection(cube = cube),
                       key = "scale_factor", bands = bands,
                       add_cloud = FALSE)
}

#' @rdname cube_functions
.cube_bands_offset_value <- function(cube, ...,
                                     bands = NULL) {

    if (is.null(bands))
        bands <- .cube_bands(cube = cube)

    .config_bands_reap(source = .cube_source(cube = cube),
                       collection = .cube_collection(cube = cube),
                       key = "offset_value", bands = bands,
                       add_cloud = FALSE)
}

#' @rdname cube_functions
.cube_bands_resampling <- function(cube, ...,
                                   bands = NULL) {

    if (is.null(bands))
        bands <- .cube_bands(cube = cube)

    .config_bands_reap(source = .cube_source(cube = cube),
                       collection = .cube_collection(cube = cube),
                       key = "resampling", bands = bands,
                       add_cloud = TRUE)
}

#' @rdname cube_functions
.cube_bands_resolutions <- function(cube, ...,
                                    bands = NULL) {

    if (is.null(bands))
        bands <- .cube_bands(cube = cube)

    .config_bands_reap(source = .cube_source(cube = cube),
                       collection = .cube_collection(cube = cube),
                       key = "resolutions", bands = bands,
                       add_cloud = TRUE)
}

#' @rdname cube_functions
.cube_bands_band_name <- function(cube, ...,
                                  bands = NULL) {

    if (is.null(bands))
        bands <- .cube_bands(cube = cube)

    .config_bands_reap(source = .cube_source(cube = cube),
                       collection = .cube_collection(cube = cube),
                       key = "band_name", bands = bands,
                       add_cloud = TRUE)
}

#' @rdname cube_functions
.cube_has_cloud <- function(cube) {

    .config_cloud() %in% .cube_bands(cube)
}
