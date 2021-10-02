#' @title Defines a data cube
#' @name sits_cube
#'
#' @references `rstac` package (https://github.com/brazil-data-cube/rstac)
#'
#' @description Creates a data cube based on spatial and temporal restrictions
#' on a collection available in repositories such as AWS, Brazil Data Cube
#' (BDC), and Digital Earth Africa (DEA) using information provided by STAC
#' endpoints. Users can also create data cubes from local files.
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
#'  \item{"LOCAL": }{Defines a cube from on a set of local files.}
#'  \item{"PROBS": }{Defines a cube to from a set of classified image files}.
#'  \item{"SATVEG": }{Defines a cube to use the SATVEG web service,
#'   see also https://www.satveg.cnptia.embrapa.br/satveg/login.html}
#'  }
#'
#' For big data sources such as AWS, BDC, and DEA users need to provide:
#' \itemize{
#' \item{collection: }{Collections are the highest level of aggregation on
#' bug data repositories. Each repository has its own set of collections,
#' described by STAC. To use STAC for querying repositories, please see the
#' package `rstac` for examples.}
#' \item{spatial extent: }{The spatial extent of the data cube can be defined
#' in two ways: (a) a region of interest(`bbox`) in WGS 84 coordinates;
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
#'@note BDC users need to provide their credentials using environmental
#' variables.To create your credentials, please see
#'  "https://brazildatacube.dpi.inpe.br/portal/explore"
#' # Sys.setenv(
#' # "BDC_ACCESS_KEY" = <your_bdc_access_key>
#' # )
#'
#'@note To create a cube from local files, all image files should have
#' the same spatial resolution and projection. Files can belong to different
#' tiles of a spatial reference system.
#' Each file should contain a single image band for a single date.
#' File names must include tile, date and band information.
#' For example: "CBERS-4_022024_B13_2018-02-02.tif"
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
#' @param source            Data source (one of "AWS", "BDC", "DEAFRICA",
#'                          "LOCAL", "PROBS", "SATVEG", "USGS", "WTSS").
#' @param ...               Other parameters to be passed for specific types.
#' @param name              Name of the output data cube.
#' @param url               URL for the STAC endpoint of the data source.
#' @param collection        Collection to be searched in the data source.
#' @param bands             Bands to be included.
#' @param tiles             Tiles from the collection to be included in the
#'                          data cube.
#' @param roi               Region of interest (see details below).
#' @param start_date        Initial date for the cube (optional).
#' @param end_date          Final date for the cube  (optional).
#' @param s2_resolution     Resolution of S2 images ("10m", "20m" or "60m").
#'                          used to build cubes (only for AWS cubes).
#' @param satellite         Satellite that produced the images
#'                          (only for creating data cubes from local files).
#' @param sensor            Sensor that produced the images.
#' @param data_dir          directory where local data is located
#'                          (only for creating data cubes from local files).
#' @param delim             delimiter for parsing files without STAC information
#'                          (only for creating data cubes from local files).
#' @param parse_info        parsing information for files without STAC
#'                          information
#'                          (only for creating data cubes from local files).
#' @param probs_files       File names (used for creating a cube from
#'                          probabilities).
#' @param probs_labels      Labels associated to a probabilities cube.
#'
#' @details The \code{roi} parameter allows a selection of an area of interest.
#' Either using a named \code{vector} ("lon_min", "lat_min", "lon_max", "lat_max") with
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
#' # --- Create a cube based on a local MODIS data
#' data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'
#' modis_cube <- sits_cube(
#'     source = "LOCAL",
#'     name = "modis_sinop",
#'     origin = "BDC",
#'     collection = "MOD13Q1-6",
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

    # set caller to show in errors
    .check_set_caller("sits_cube")

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

    # pre-condition
    .source_collection_check(source = source,
                             collection = collection)

    # Pre-condition - try to find the access key as an environment variable
    .check_env_var(x = "BDC_ACCESS_KEY",
                   msg = "BDC_ACCESS_KEY needs to be provide.")

    # dry run to verify if service is running
    .source_access_test(source, collection, ...)

    # builds a sits data cube
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
                               roi = NULL,
                               start_date = NULL,
                               end_date = NULL) {

    # pre-condition
    .source_collection_check(source = source,
                             collection = collection)

    # Pre-condition - try to find the access key as an environment variable
    .check_env_var(x = "BDC_ACCESS_KEY",
                   msg = "BDC_ACCESS_KEY needs to be provide.")

    if (is.null(bands))
        bands <- .source_bands(source = source,
                               collection = collection)

    # Pre-condition - checks if the bands are supported by the collection
    .check_bands(source = source,
                 collection = collection,
                 bands = bands)


    # dry run to verify if service is running
    .source_access_test(source = source,
                        collection = collection, ...,
                        bands = bands)

    # builds a sits data cube
    .source_cube(source = source,
                 collection = collection,
                 name = name,
                 bands = bands,
                 tiles = tiles,
                 bbox = roi,
                 start_date = start_date,
                 end_date = end_date, ...)
}

#' @rdname sits_cube
#'
#' @export
sits_cube.deafrica_cube <- function(source = "DEAFRICA", ...,
                                    name = "deafrica_cube",
                                    url = NULL,
                                    collection = "S2_L2A",
                                    bands = NULL,
                                    tiles = NULL,
                                    roi = NULL,
                                    start_date = NULL,
                                    end_date = NULL) {

    # collection name is upper case
    collection <- toupper(collection)

    # pre-condition
    .source_collection_check(source = source,
                             collection = collection)

    # precondition - is AWS access available?
    .source_collection_aws_check(source = source,
                                 collection = collection)

    if (is.null(bands))
        bands <- .source_bands(source = source,
                               collection = collection)

    # Pre-condition - checks if the bands are supported by the collection
    .check_bands(source = source,
                 collection = collection,
                 bands = bands)

    # dry run to verify if service is running
    .source_access_test(source = source,
                        collection = collection, ...,
                        bands = bands)

    # builds a sits data cube
    .source_cube(source = source,
                 collection = collection,
                 name = name,
                 bands = bands,
                 tiles = tiles,
                 bbox = roi,
                 start_date = start_date,
                 end_date = end_date, ...)
}

#' @rdname sits_cube
#'
#' @export
sits_cube.aws_cube <- function(source = "AWS", ...,
                               name = "aws_cube",
                               url = NULL,
                               collection = "SENTINEL-S2-L2A-COGS",
                               tiles = NULL,
                               bands = NULL,
                               roi = NULL,
                               s2_resolution = 20,
                               start_date = NULL,
                               end_date = NULL) {


    # collection name is upper case
    collection <- toupper(collection)

    # precondition - the provided resolution is valid?
    .check_num(s2_resolution,
               allow_zero = FALSE,
               len_max = 1,
               msg = "invalid resolution.")

    # precondition - is the provided resolution is valid?
    .check_that(x = s2_resolution %in% c(10, 20, 60),
                msg = "s2_resolution should be one of c(10, 20, 60)")

    # pre-condition
    .source_collection_check(source = source,
                             collection = collection)

    # precondition - is AWS access available?
    .source_collection_aws_check(source, collection)

    if (is.null(bands))
        bands <- .aws_bands(source = source,
                            collection = collection,
                            s2_resolution = s2_resolution)

    # Pre-condition - checks if the bands are supported by the collection
    .check_bands(source = source, collection = collection,
                 bands = bands, s2_resolution = s2_resolution)

    # dry run to verify if service is running
    .source_access_test(source = source,
                        collection = collection, ...,
                        bands = bands,
                        s2_resolution = s2_resolution)

    # builds a sits data cube
    .source_cube(source = source,
                 collection = collection,
                 name = name,
                 bands = bands,
                 tiles = tiles,
                 bbox = roi,
                 start_date = start_date,
                 end_date = end_date, ...,
                 s2_resolution = s2_resolution)
}

#' @rdname sits_cube
#'
#' @export
sits_cube.opendata_cube <- function(source = "OPENDATA", ...,
                                    name = "opendata_cube",
                                    url = NULL,
                                    collection = "SENTINEL-S2-L2A-COGS",
                                    tiles = NULL,
                                    bands = NULL,
                                    roi = NULL,
                                    start_date = NULL,
                                    end_date = NULL) {


    # collection name is upper case
    collection <- toupper(collection)

    # pre-condition
    .source_collection_check(source = source,
                             collection = collection)

    if (is.null(bands))
        bands <- .source_bands(source = source,
                               collection = collection)

    # Pre-condition - checks if the bands are supported by the collection
    .check_bands(source = source,
                 collection = collection,
                 bands = bands)

    # dry run to verify if service is running
    .source_access_test(source = source,
                        collection = collection, ...,
                        bands = bands)

    # builds a sits data cube
    .source_cube(source = source,
                 collection = collection,
                 name = name,
                 bands = bands,
                 tiles = tiles,
                 bbox = roi,
                 start_date = start_date,
                 end_date = end_date, ...)
}

#' @rdname sits_cube
#' @keywords internal
sits_cube.usgs_cube <- function(source = "USGS", ...,
                                name = "usgs_cube",
                                url = NULL,
                                collection = "landsat-c2l2-sr",
                                tiles = NULL,
                                bands = NULL,
                                roi = NULL,
                                start_date = NULL,
                                end_date = NULL) {

    # collection name is upper case
    collection <- toupper(collection)

    # pre-condition
    .source_collection_check(source = source,
                             collection = collection)

    # precondition
    .check_chr(x = tiles,
               allow_empty = FALSE,
               len_min = 1,
               msg = paste("for the USGS cubes you need to provide the tiles",
                           "of the region you want to query.")
    )

    # precondition - is AWS access available?
    .source_collection_aws_check(source, collection)

    if (is.null(bands))
        bands <- .source_bands(source = source,
                               collection = collection)

    # Pre-condition - checks if the bands are supported by the collection
    .check_bands(source = source,
                 collection = collection,
                 bands = bands)

    # dry run to verify if service is running
    .source_access_test(source = source,
                        collection = collection, ...,
                        bands = bands)

    # builds a sits data cube
    .source_cube(source = source,
                 collection = collection,
                 name = name,
                 bands = bands,
                 tiles = tiles,
                 bbox = roi,
                 start_date = start_date,
                 end_date = end_date, ...)
}

#' @rdname sits_cube
#'
#' @export
sits_cube.local_cube <- function(source = "LOCAL", ...,
                                 name   = "local_cube",
                                 origin,
                                 collection,
                                 bands = NULL,
                                 start_date = NULL,
                                 end_date = NULL,
                                 data_dir,
                                 parse_info,
                                 delim = "_") {


    # precondition - data directory must be provided
    .check_file(x = data_dir,
                msg = "data_dir must be to be provided.")

    collection <- paste0(origin, "/", collection)

    # precondition - check satellite and sensor
    .source_access_test(source = source, collection = collection)

    # precondition - check parse info
    .check_chr(x = parse_info,
               allow_empty = FALSE,
               len_min = 2,
               msg = "invalid parsing information.")

    # precondition - does the parse info have band and date?
    .check_chr_contains(
        parse_info,
        contains = c("tile", "band", "date"),
        msg = "parse_info must include tile, date, and band.")

    # bands in upper case
    if (!purrr::is_null(bands))
        bands <- toupper(bands)

    # builds a sits data cube
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
                                 start_date,
                                 end_date,
                                 probs_labels,
                                 probs_files) {

    # builds a sits probs cube
    .source_cube(source = source, ...,
                 name = name,
                 start_date = start_date,
                 end_date = end_date,
                 probs_labels = probs_labels,
                 probs_files = probs_files)
}

#' @rdname sits_cube
#'
#' @export
sits_cube.satveg_cube <- function(source = "SATVEG", ...,
                                  collection = "TERRA") {


    # precondition
    .check_chr_within(x = collection,
                      within = c("TERRA", "AQUA", "COMB"),
                      msg = "invalid SATVEG collection.")

    # precondition - is service online?
    .source_access_test(source = source, collection = collection)

    # creating satveg cube
    .source_cube(source = source, collection = collection, name = "SATVEG")
}

#' @export
sits_cube.default <- function(source, ...) {
    stop("source not found.")
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
#' data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'
#' modis_cube <- sits_cube(
#'     source = "LOCAL",
#'     name = "modis_sinop",
#'     origin = "BDC",
#'     collection = "MOD13Q1-6",
#'     band = "NDVI",
#'     data_dir = data_dir,
#'     parse_info = c("X1", "X2", "tile", "band", "date")
#' )
#'
#' modis_cube_copy <- sits_cube_copy(modis_cube,
#'     name = "modis_sinop_cp",
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

    # set caller to show in errors
    .check_set_caller("sits_cube_copy")

    # precondition - does the output directory exist?
    .check_file(dest_dir)

    if (purrr::is_null(roi))
        bbox <- sits_bbox(cube)
    else
        # get the bounding box
        bbox <- .sits_roi_bbox(roi, cube)

    # Get the subimage
    si <- .sits_raster_sub_image_from_bbox(bbox, cube)

    # test subwindow
    srcwin <- vector("double", length = 4)
    names(srcwin) <- c("xoff", "yoff", "xsize", "ysize")
    srcwin["xoff"] <- si["first_col"] - 1
    srcwin["yoff"] <- si["first_row"] - 1
    srcwin["xsize"] <- si["ncols"]
    srcwin["ysize"] <- si["nrows"]

    .check_that(
        x = (srcwin["xoff"] + srcwin["xsize"]) <= cube$ncols,
        msg = "srcwin x values bigger than cube size"
    )
    .check_that(
        x = (srcwin["yoff"] + srcwin["ysize"]) <= cube$nrows,
        msg = "srcwin y values bigger than cube size"
    )

    # the label cube may contain several classified images
    cube_rows <- slider::slide(cube, function(row) {
        # get information on the file
        file_info <- row$file_info[[1]]

        # are the selected bands in the cube?
        .check_chr_within(
            x = bands,
            within = sits_bands(row),
            msg = "input bands not available in the cube")

        # get all the bands which are requested
        file_info_out <- dplyr::filter(file_info, band %in% bands)
        # remove token (if existing)
        file_no_token <- gsub("^([^?]+)(\\?.*)?$", "\\1", file_info_out$path[[1]])
        # get the file extension
        file_ext <- tools::file_ext(file_no_token)

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
                row$tile,"_",
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
NULL

#' @rdname cube_functions
.cube_check <- function(cube) {

    return(inherits(cube, "sits_cube"))
}

#' @rdname cube_functions
.cube_satellite <- function(cube) {

    res <- unique(cube[["satellite"]])

    # post-condition
    .check_chr(res, allow_empty = FALSE, len_min = 1, len_max = 1,
               msg = "invalid satellite value")

    return(res)
}

#' @rdname cube_functions
.cube_sensor <- function(cube) {

    res <- unique(cube[["sensor"]])

    # post-condition
    .check_chr(res, allow_empty = FALSE, len_min = 1, len_max = 1,
               msg = "invalid sensor value")

    return(res)
}

#' @rdname cube_functions
.cube_collection <- function(cube) {

    res <- unique(cube[["collection"]])

    # post-condition
    .check_chr(res, allow_empty = FALSE, len_min = 1, len_max = 1,
               msg = "invalid collection value")

    return(res)
}

#' @rdname cube_functions
.cube_name <- function(cube) {

    res <- unique(cube[["name"]])

    # post-condition
    .check_chr(res, allow_empty = FALSE, len_min = 1, len_max = 1,
               msg = "invalid cube 'name' value")

    return(res)
}

#' @rdname cube_functions
.cube_labels <- function(cube) {

    res <- unique(cube[["labels"]])

    # post-condition
    .check_lst(res, min_len = 1, max_len = 1,
               is_named = FALSE,
               msg = "invalid cube 'labels' value")

    res <- unlist(res, use.names = FALSE)

    # post-condition
    .check_chr(res, allow_na = TRUE, allow_empty = FALSE,
               len_min = 1, allow_null = TRUE,
               msg = "invalid cube 'labels' value")

    return(res)
}

#' @rdname cube_functions
.cube_bands <- function(cube, ...,
                        add_cloud = TRUE) {

    res <- unique(cube[["bands"]])

    # post-condition
    .check_lst(res, min_len = 1, max_len = 1, is_named = FALSE,
               msg = "inconsistent 'bands' among tiles")

    # simplify
    res <- unlist(res, use.names = FALSE)

    if (!add_cloud)
        res <- res[res != .source_cloud()]

    return(res)
}

#' @rdname cube_functions
.cube_bands_check <- function(cube, bands, ...,
                              add_cloud = TRUE) {

    # all bands are upper case
    .check_chr_within(bands, within = .cube_bands(cube = cube,
                                                  add_cloud = add_cloud),
                      case_sensitive = FALSE,
                      msg = "invalid 'bands' parameter")

    return(invisible(NULL))
}

#' @rdname cube_functions
.cube_source <- function(cube) {

    res <- unique(cube[["source"]])

    # post-condition
    .check_chr(res, allow_empty = FALSE, len_min = 1, len_max = 1,
               msg = "invalid cube 'source' value")

    return(res)
}

#' @rdname cube_functions
.cube_timeline <- function(cube) {

    res <- unique(cube[["file_info"]][["date"]])

    # post-condition
    # check if all tiles have same timeline
    .check_lst(res, min_len = 1, max_len = 1,
               msg = "invalid cube timeline values")

    # simplify
    res <- unlist(res)

    return(res)
}

#' @rdname cube_functions
.cube_tiles <- function(cube) {

    res <- unique(cube[["tile"]])

    # post-condition
    .check_chr(res, allow_empty = FALSE, len_min = nrow(cube),
               len_max = nrow(cube),
               msg = "invalid cube 'tile' values")

    return(res)
}

#' @rdname cube_functions
.cube_tile_check <- function(cube, tile) {

    .check_that(
        is.numeric(tile) || is.character(tile),
        local_msg = "'tile' parameter must be numeric or character"
    )

    .check_length(tile, len_min = 1, len_max = nrow(cube),
                  "invalid 'tile' parameter")

    if (is.numeric(tile)) {
        .check_num(tile, min = 1, max = nrow(cube),
                   len_min = 1, is_integer = TRUE,
                   msg = "invalid 'tile' parameter"
        )
    } else if (is.character(tile)) {
        .check_chr_within(tile,
                          within = .cube_tiles(cube = cube),
                          discriminator = "one_of",
                          msg = "invalid 'tile' parameter"
        )
    }

    return(invisible(tile))
}

#' @rdname cube_functions
.cube_tile_get_fields <- function(cube, tile, fields = NULL) {

    # pre-condition
    .cube_tile_check(cube = cube, tile = tile)

    .check_chr_within(fields,
                      within = names(cube),
                      discriminator = "any_of",
                      msg = "invalid 'fields' parameter")

    if (is.numeric(tile))
        res <- c(cube[tile, fields])
    else
        res <- c(cube[which(.cube_tiles(cube = cube) %in% tile), fields])

    # post-condition
    .check_lst(res, min_len = length(fields), max_len = length(fields),
               msg = "invalid 'fields' parameter")

    return(res)
}

#' @rdname cube_functions
.cube_tile_crs <- function(cube, ...,
                           tile = 1) {

    res <- .cube_tile_get_fields(cube = cube,  tile = tile, fields = "crs")

    # simplify
    res <- unlist(res, use.names = FALSE)

    # post-condition
    .check_chr(res, allow_empty = FALSE, len_min = 1, len_max = 1,
               "invalid tile 'crs' value")

    return(res)
}

#' @rdname cube_functions
.cube_tile_bbox <- function(cube, ...,
                            tile = 1) {

    res <- .cube_tile_get_fields(cube = cube, tile = tile,
                                 fields = c("xmin", "ymin", "xmax", "ymax"))

    # post-condition
    .check_lst(res, min_len = 4, max_len = 4, fn_check = .check_num,
               len_min = 1, len_max = 1,
               msg = "invalid tile 'bbox' value")

    return(res)
}

#' @rdname cube_functions
.cube_tile_resolution <- function(cube, ...,
                                  tile = 1) {

    res <- .cube_tile_get_fields(cube = cube, tile = tile,
                                 fields = c("xres", "yres"))

    # post-condition
    .check_lst(res, min_len = 2, max_len = 2,
               fn_check = .check_num, min = 0, allow_zero = FALSE,
               len_min = 1, len_max = 1,
               msg = "invalid tile 'xres' and 'yres' values")

    return(res)
}

#' @rdname cube_functions
.cube_tile_size <- function(cube, ...,
                            tile = 1) {

    .cube_tile_get_fields(cube = cube, tile = tile,
                          fields = c("nrows", "ncols"))

    # post-condition
    .check_lst(res, min_len = 2, max_len = 2,
               fn_check = .check_num, min = 0, allow_zero = FALSE,
               len_min = 1, len_max = 1,
               msg = "invalid tile 'nrows' and 'ncols' values")

    return(res)
}

#' @rdname cube_functions
.cube_band_missing_value <- function(cube, band) {

    # pre-condition
    .check_chr(band, len_min = 1, len_max = 1,
               msg = "invalid 'band' parameter")

    .check_chr_within(band,
                      within = .cube_bands(cube = cube, add_cloud = FALSE),
                      discriminator = "one_of",
                      case_sensitive = FALSE,
                      msg = "invalid 'band' parameter")

    # bands names are upper case
    band <- toupper(band)

    res <- .config_get(key = c("sources", .cube_source(cube = cube),
                               "collections", .cube_collection(cube = cube),
                               "bands", band, "missing_value"))

    # post-condition
    .check_num(res, len_min = 1, len_max = 1,
               msg = "invalid 'missing_value' value")

    return(res)
}

#' @rdname cube_functions
.cube_band_minimum_value <- function(cube, band) {

    # pre-condition
    .check_chr(band, len_min = 1, len_max = 1,
               msg = "invalid 'band' parameter")

    .check_chr_within(band,
                      within = .cube_bands(cube = cube, add_cloud = FALSE),
                      discriminator = "one_of",
                      case_sensitive = FALSE,
                      msg = "invalid 'band' parameter")

    # bands names are upper case
    band <- toupper(band)

    res <- .config_get(key = c("sources", .cube_source(cube = cube),
                               "collections", .cube_collection(cube = cube),
                               "bands", band, "minimum_value"))

    # post-condition
    .check_num(res, len_min = 1, len_max = 1,
               msg = "invalid 'minimum_value' value")

    return(res)
}

#' @rdname cube_functions
.cube_band_maximum_value <- function(cube, band) {

    # pre-condition
    .check_chr(band, len_min = 1, len_max = 1,
               msg = "invalid 'band' parameter")

    .check_chr_within(band,
                      within = .cube_bands(cube = cube, add_cloud = FALSE),
                      discriminator = "one_of",
                      case_sensitive = FALSE,
                      msg = "invalid 'band' parameter")

    # bands names are upper case
    band <- toupper(band)

    res <- .config_get(key = c("sources", .cube_source(cube = cube),
                               "collections", .cube_collection(cube = cube),
                               "bands", band, "maximum_value"))

    # post-condition
    .check_num(res, len_min = 1, len_max = 1,
               msg = "invalid 'maximum_value' value")

    return(res)
}

#' @rdname cube_functions
.cube_band_scale_factor <- function(cube, band) {

    # pre-condition
    .check_chr(band, len_min = 1, len_max = 1,
               msg = "invalid 'band' parameter")

    .check_chr_within(band,
                      within = .cube_bands(cube = cube, add_cloud = FALSE),
                      discriminator = "one_of",
                      case_sensitive = FALSE,
                      msg = "invalid 'band' parameter")

    # bands names are upper case
    band <- toupper(band)

    res <- .config_get(key = c("sources", .cube_source(cube = cube),
                               "collections", .cube_collection(cube = cube),
                               "bands", band, "scale_factor"))

    # post-condition
    .check_num(res, allow_zero = FALSE, len_min = 1, len_max = 1,
               msg = "invalid 'scale_factor' value")

    return(res)
}

#' @rdname cube_functions
.cube_band_offset_value <- function(cube, band) {

    # pre-condition
    .check_chr(band, len_min = 1, len_max = 1,
               msg = "invalid 'band' parameter")

    .check_chr_within(band,
                      within = .cube_bands(cube = cube, add_cloud = FALSE),
                      discriminator = "one_of",
                      case_sensitive = FALSE,
                      msg = "invalid 'band' parameter")

    # bands names are upper case
    band <- toupper(band)

    res <- .config_get(key = c("sources", .cube_source(cube = cube),
                               "collections", .cube_collection(cube = cube),
                               "bands", band, "offset_value"))

    # post-condition
    .check_num(res, len_min = 1, len_max = 1,
               msg = "invalid 'offset_value' value")

    return(res)
}

#' @rdname cube_functions
.cube_band_resolutions <- function(cube, band) {

    # pre-condition
    .check_chr(band, len_min = 1, len_max = 1,
               msg = "invalid 'band' parameter")

    .check_chr_within(band,
                      within = .cube_bands(cube = cube, add_cloud = TRUE),
                      discriminator = "one_of",
                      case_sensitive = FALSE,
                      msg = "invalid 'band' parameter")

    # bands names are upper case
    band <- toupper(band)

    res <- .config_get(key = c("sources", .cube_source(cube = cube),
                               "collections", .cube_collection(cube = cube),
                               "bands", band, "resolutions"))

    # post-condition
    .check_num(res, min = 0, allow_zero = FALSE, len_min = 1,
               msg = "invalid 'resolutions' value")

    return(res)
}

#' @rdname cube_functions
.cube_has_cloud <- function(cube) {

    .source_cloud() %in% .cube_bands(cube = cube, add_cloud = TRUE)
}

#' @rdname cube_functions
.cube_s3class <- function(cube) {

    unique(c(.source_s3class(source = .cube_source(cube = cube)),
             class(cube)))
}

#' @title meta-type for data
#' @name .config_data_meta_type
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param  data    tibble (time series or cube)
#'
#' @return file path to the appended to data_dir
.config_data_meta_type <- function(data) {

    # set caller to show in errors
    .check_set_caller(".config_data_meta_type")

    if (inherits(data, c("sits", "patterns", "predicted",
                         "sits_model", "sits_cube", "raster_cube",
                         "probs_cube", "wtss_cube", "satveg_cube",
                         "stac_cube", "aws_cube"))) {
        return(data)

    } else if (inherits(data, "tbl_df")) {

        if (all(c("name", "source", "collection", "satellite",
                  "sensor", "tile", "bands", "labels", "nrows",
                  "ncols", "xmin", "xmax", "ymin", "ymax",
                  "xres", "yres", "crs") %in% colnames(data))) {

            class(data) <- .cube_s3class(cube = data)

            return(data)
        } else if (all(c("longitude", "latitude", "start_date",
                         "end_date", "label", "cube",
                         "time_series") %in% colnames(data))) {

            class(data) <- c("sits", class(data))
            return(data)
        }
    }

    .check_that(FALSE,
                local_msg = "Data not recognized as a sits object",
                msg = "Invalid data parameter")
}

#' @title Check cube collection
#' @name .check_collection
#'
#' @description A suite of check to verify collection in cube.
#'
#' @param source        Data source
#' @param collection    Collection to be searched in the data source.
#' @param bands         Bands to be included.
#' @param s2_resolution Resolution of S2 images ("10m", "20m" or "60m").
#'                          used to build cubes (only for AWS cubes).
#'
#' @return An invisible null
.check_bands <- function(source, collection, bands, s2_resolution = NULL) {

    # set caller to show in errors
    .check_set_caller(".check_bands")

    if (!is.null(s2_resolution)) {
        sits_bands <- .aws_bands(source = source,
                                 collection = collection,
                                 s2_resolution = s2_resolution)

        source_bands <- .aws_bands_band_name(source = source,
                                             collection = collection,
                                             s2_resolution = s2_resolution)
    } else {
        sits_bands <- .source_bands(source = source,
                                    collection = collection)
        source_bands <- .source_bands_band_name(source = source,
                                                collection = collection)
    }

    .check_chr_within(x = bands,
                      within = c(sits_bands, source_bands),
                      msg = paste("invalid bands.\nPlease verify",
                                  "the provided bands."))

    # remove bands with equal names, like NDVI, EVI...
    source_bands <- source_bands[!source_bands %in% sits_bands]

    # warning in case user provide source band
    if (any(bands %in% source_bands))
        warning(
            sprintf("Bands %s converted to sits names %s",
                    paste(bands, collapse = ", "),
                    paste(
                        .source_bands_to_sits(source = source,
                                              collection = collection,
                                              bands = bands),
                        collapse = ", ")),
            call. = FALSE)

    return(invisible(NULL))
}
