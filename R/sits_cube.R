#' @title Defines a data cube
#' @name sits_cube
#'
#' @description Defines a cube to retrieve data. This is a generic function.
#' Cubes can be of the following types. See each function description for the
#' required parameters:
#' \itemize{
#'  \item{"SATVEG": }{ see \code{\link{sits_cube.satveg_cube}}}
#'  \item{"BRICK": }{see \code{\link{sits_cube.brick_cube}}}
#'  \item{"STACK": }{see \code{\link{sits_cube.stack_cube}}}
#'  \item{"BDC"}{Brazil Data Cube - see \code{\link{sits_cube.bdc_cube}}}
#'  \item{"S2_L2A_AWS"}{Sentinel-2 data in AWS -
#'                      see \code{\link{sits_cube.s2_l2a_aws_cube}}}
#'  \item{"GDALCUBES"}{gdalcubes compose function -
#'                      see \code{\link{sits_cube.gdalcubes_cube}}}
#'  \item{"PROBS"}{create a cube from a classified image -
#'                      see \code{\link{sits_cube.probs_cube}}}
#'
#' }
#'
#' @param type        Type of cube (one of "WTSS", "BRICK", "STACK",
#'                    "BDC_TILE", "S2_L2A_AWS", "GDALCUBES", "PROBS",
#'                    "CLASSIFIED")
#' @param name              Name of the output data cube.
#' @param ...               Other parameters to be passed for specific types
#' @return  The description of a sits cube
#'
#' @export
sits_cube <- function(type = "RASTER", name, ...) {
    spec_class <- .sits_config_cube_class(type)
    class(type) <- c(spec_class, class(type))
    # Dispatch
    UseMethod("sits_cube", type)
}


#' @title Defines a data cube for the SATVEG service
#' @name sits_cube.satveg_cube
#' @description The SATVEG service is provided by the Embrapa Agricultural
#'  Informatics Centre and provides access to time series from the MODIS sensor.
#'  There are three types of time series: "terra" (from the TERRA satellite),
#'  "aqua" (from the AQUA satellite) and "comb" (combination of both satellites)
#' @param type              Type of cube
#' @param name              Name of the input data ("terra", "aqua", "comb").
#' @param ...               Other parameters to be passed for specific types
#' @return                  A valid data cube
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a data cube based on the SATVEG service
#' cube_satveg <- sits_cube(
#'     type = "SATVEG",
#'     name = "terra"
#' )
#' }
#'
sits_cube.satveg_cube <- function(type = "SATVEG", name = NULL, ...) {
    # Pre-condition - check if SATVEG is working
    satveg_ok <- .sits_satveg_check()
    # if OK, go ahead a create a SATVEG cube
    if (satveg_ok) {
          cube <- .sits_satveg_cube(name = name)
      } else {
        message("SATVEG service not responding")
        return(NULL)
    }

    return(cube)
}
#' @title Backward compatibility
#' @name sits_cube.raster_cube
#' @description Using "RASTER" as a type for a data cube is deprecated in favour
#'              of "STACK" and "BRICK" types.
#' @param type              Type of cube
#' @param ...               Other parameters to be passed for specific types
#' @param name              Name of the input data
#' @return                  A message
#'
#' @export
sits_cube.raster_cube <- function(type = "RASTER",  name = NULL, ...) {
    message("type RASTER is deprecated, please use BRICK or STACK")
}

#' @title Defines a cube from a set of single image files
#' @name sits_cube.stack_cube
#'
#' @description Defines a cube to retrieve data from a set of image files.
#'              All image files should have the same spatial resolution
#'              and same projection. Each file contains a single image band
#'              for a single date; its name contains date and band information.
#'              This is the case of files in repositories such as Sentinel Hub
#'              and AWS. This case is called an "image stack", following the
#'              terminology used by the "raster" R package
#'
#'              Timeline and the bands are deduced from filenames.
#'              Examples of valid image names include
#'              "CB4_64_16D_STK_022024_2018-08-29_2018-09-13_EVI.tif" and
#'              "B02_2018-07-18.jp2". In each case, the user has to provide
#'              appropriate parsing information that allows SITS to extract
#'              the band and the date. In the examples above, the parsing info
#'              would include "_" as a delimiter. In the first, the names of the
#'              resulting columns for parsing are "X1", "X2", "X3", "X4", "X5",
#'              "date", "X7", and "band". In the second, only "band" and "date".
#'
#' @param type              type of cube
#' @param name              name of output data cube
#' @param ...               other parameters
#' @param satellite         satellite
#' @param sensor            sensor
#' @param bands             bands to be used (optional)
#' @param start_date        starting date of the cube (optional)
#' @param end_date          ending date of the cube (optional)
#' @param data_dir          directory where data is located
#' @param delim             character to use as delimiter (default = "_")
#' @param parse_info        parsing information (see above)
#'
#' @return                  data cube
#'
#' @examples
#'
#' # Create a cube based on a stack of CBERS data
#' data_dir <- system.file("extdata/raster/cbers", package = "sits")
#'
#' cbers_cube <- sits_cube(
#'     type = "STACK",
#'     name = "022024",
#'     satellite = "CBERS-4",
#'     sensor = "AWFI",
#'     data_dir = data_dir,
#'     delim = "_",
#'     parse_info = c("X1", "X2", "band", "date")
#' )
#' @export
sits_cube.stack_cube <- function(type = "STACK",
                        name = "stack_cube",
                        ...,
                        satellite,
                        sensor,
                        bands = NULL,
                        start_date = NULL,
                        end_date = NULL,
                        data_dir = NULL,
                        parse_info = sits:::.sits_config_data_parse_info(type),
                        delim = sits:::.sits_config_data_delim(type)) {

    # precondition - check satellite and sensor
    .sits_config_satellite_sensor(satellite, sensor)
    # precondition - data directory must be provided
    assertthat::assert_that(!purrr::is_null(data_dir),
        msg = "data_dir must be to be provided"
    )
    # precondition - check parse info
    assertthat::assert_that(length(parse_info) >= 2,
        msg = "invalid parsing information"
    )
    # precondition - does the parse info have band and date?
    assertthat::assert_that(all(c("band", "date") %in% parse_info),
        msg = "invalid columns for date and band"
    )

    # get the file information
    file_info <- .sits_raster_stack_info(
        satellite = satellite,
        sensor = sensor,
        data_dir = data_dir,
        parse_info = parse_info,
        delim = delim,
        bands = bands,
        start_date = start_date,
        end_date = end_date
    )
    # create a data cube
    cube <- .sits_raster_stack_cube(
        satellite = satellite,
        sensor = sensor,
        name = name,
        file_info = file_info
    )

    class(cube) <- c("stack_cube", "raster_cube", class(cube))
    return(cube)
}
#' @title Defines a cube from a set of image bricks
#' @name sits_cube.brick_cube
#'
#' @description Defines a cube to retrieve data from a set of image files.
#'              All image files should have the same spatial resolution
#'              and same projection. Each input band
#'              has to be organised as a raster brick, and the
#'              number of input files must match the number of bands.
#'              All input files must have the same spatial resolution and
#'              share the same timeline (in order).
#'              The timeline for the cube must be provided.
#'
#' @param type              type of cube
#' @param name              name of output data cube
#' @param ...               other parameters
#' @param satellite         satellite
#' @param sensor            sensor
#' @param timeline          vector with timeline of the files
#' @param bands             vector of bands associated to the files
#' @param files             vector of file names for each band
#'
#' @return                  data cube
#'
#' @examples
#'
#' # Create a raster cube based on bricks
#' # inform the files that make up a raster brick with 392 time instances
#' files <- c(system.file("extdata/raster/mod13q1/sinop-crop-ndvi.tif",
#'     package = "sits"
#' ))
#'
#' # create a raster cube file based on the information about the files
#' raster_cube <- sits_cube(
#'     type = "BRICK",
#'     name = "Sinop-crop",
#'     satellite = "TERRA",
#'     sensor = "MODIS",
#'     timeline = timeline_modis_392,
#'     bands = "NDVI",
#'     files = files
#' )
#' @export
sits_cube.brick_cube <- function(type = "BRICK",
                                 name = "brick_cube",
                                 ...,
                                 satellite,
                                 sensor,
                                 timeline = NULL,
                                 bands = NULL,
                                 files = NULL) {

    # precondition - check if the files are bricks
    .sits_raster_brick_check(
        satellite = satellite,
        sensor = sensor,
        name = name,
        timeline = timeline,
        bands = bands,
        files = files
    )

    cube <- .sits_raster_brick_cube(
        satellite = satellite,
        sensor = sensor,
        name = name,
        timeline = timeline,
        bands = bands,
        files = files
    )

    class(cube) <- c("brick_cube", "raster_cube", class(cube))
    return(cube)
}
#' @title Defines a data cube for a BDC STAC
#' @name sits_cube.bdc_cube
#'
#' @references `rstac` package (https://github.com/brazil-data-cube/rstac)
#'
#' @description Defines a cube to retrieve data from the Brazil Data Cube (BDC)
#'              STAC. The retrieval is based on tiles of a given cube.
#'              For more on BDC, please see http://brazildatacube.dpi.inpe.br/
#'
#' @param type       Type of cube.
#' @param name       Name of the output data cube.
#' @param ...        Other parameters to be passed for specific types.
#' @param url        URL for the BDC catalog (mandatory).
#' @param collection BDC collection to be searched (mandatory).
#' @param tiles      Tile names to be searched (optional).
#' @param bands      Bands names to be filtered (optional).
#' @param roi        Region of interest (optional), expressed either as
#'  an \code{sfc} or \code{sf} object from sf package, a
#'  a GeoJSON following the rules from RFC 7946, or a
#'  bounding box with named XY values ("xmin", "xmax", "ymin", "ymax").
#' @param start_date Initial date for the cube files (optional).
#' @param end_date   Final date for the cube files (optional).
#'
#' @return           A data cube.
#'
#' @examples
#' \dontrun{
#' # this example requires access to an external service, so should not be run
#' # by CRAN
#'
#' # Provide your BDC credentials as enviroment variables
#' # Sys.setenv(
#' # "BDC_ACCESS_KEY" = <your_bdc_access_key>
#' # )
#'
#' # create a raster cube file based on the information about the files
#' cbers_tile <- sits_cube(
#'     type = "BDC",
#'     name = "cbers_022024",
#'     bands = c("NDVI", "EVI"),
#'     tiles = "022024",
#'     collection = "CB4_64_16D_STK-1",
#'     start_date = "2018-09-01",
#'     end_date = "2019-08-28"
#' )
#' }
#' @export
sits_cube.bdc_cube <- function(type = "BDC",
                               name = "bdc_cube",
                               ...,
                               url = NULL,
                               collection = NULL,
                               tiles = NULL,
                               bands = NULL,
                               roi = NULL,
                               start_date = NULL,
                               end_date = NULL) {

    # require package
    if (!requireNamespace("rstac", quietly = TRUE)) {
        stop(paste("Please install package rstac from CRAN:",
                   "install.packages('rstac')"), call. = FALSE
        )
    }
    # precondition - is the url correct?
    if (purrr::is_null(url)) {
          url <- .sits_config_bdc_stac()
      }

    # test if BDC is accessible
    assertthat::assert_that(.sits_config_bdc_stac_access(url),
                            msg = "BDC is not accessible"
    )
    # precondition - is the collection name valid?
    assertthat::assert_that(!purrr::is_null(collection),
          msg = "sits_cube: BDC collection must be provided"
    )

    assertthat::assert_that(!(length(collection) > 1),
        msg = "sits_cube: only one BDC collection should be specified"
    )

    # verify  bdc access credentials
    access_key <- .sits_bdc_access_check()

    # retrieve information from the collection
    collection_info <- .sits_stac_collection(
        url = url,
        collection = collection,
        bands = bands, ...
    )

    # retrieve item information
    items_info <- .sits_stac_items(
        url = url,
        collection = collection,
        tiles = tiles,
        roi = roi,
        start_date = start_date,
        end_date = end_date, ...
    )

    # creating a group of items per tile
    items_group <- .sits_stac_group(items_info,
                                    fields = c("properties", "bdc:tiles")
    )


    tiles <- purrr::map(items_group, function(items) {

        # retrieve the information from STAC
        stack <- .sits_stac_items_info(items, collection_info$bands)

        # add the information for each tile
        cube_t <- .sits_stac_tile_cube(
            url = url,
            name = name,
            collection = collection_info,
            items = items,
            cube = collection,
            file_info = stack
        )

        class(cube_t) <- c("stack_cube", "raster_cube", class(cube_t))
        return(cube_t)
    })
    cube <- dplyr::bind_rows(tiles)

    # include access key information in file
    cube <- .sits_bdc_access_info(cube, access_key)

    return(cube)
}
#' @title Defines a data cube for a Sentinel-2 L2A AWS cube
#' @name sits_cube.s2_l2a_aws_cube
#'
#' @description Defines a cube to retrieve data from the Sentinel-2 L2A data
#'              available in AWS. Users need to be an AWS
#'              user and provide her access key and secret key. These keys may
#'              be passed as environment variables. The AWS bands in
#'              10m resolution are "B02", "B03", "B04", and "B08".
#'              The  20m bands are "B02", "B03", "B04", "B05", "B06", "BO7",
#'              "B08", "B8A", "B11", and "B12".
#'              All 12 bands are available at 60m resolution.
#'
#'    Users need to provide AWS credentials in their environmental variables\cr
#' Sys.setenv(\cr
#'      "AWS_ACCESS_KEY_ID"     = <your_access_key>, \cr
#'      "AWS_SECRET_ACCESS_KEY" = <your_secret_access_key>,\cr
#'      "AWS_DEFAULT_REGION"    = <your AWS region>, \cr
#'      "AWS_ENDPOINT" = "sentinel-s2-l2a.s3.amazonaws.com", \cr
#'      "AWS_REQUEST_PAYER"     = "requester" \cr
#' )
#'
#' @param type              type of cube
#' @param name              output data cube.
#' @param ...               other parameters to be passed for specific types
#' @param bands             vector of bands.
#' @param tiles             vector of tiles
#' @param start_date        starting date of the cube
#' @param end_date          ending date of the cube
#' @param s2_aws_resolution resolution of S2 images ("10m", "20m" or "60m")
#' @return                  data cube
#' @export
#'
#' @examples
#' \dontrun{
#' # this example requires access to an external service, so should not be run
#' # by CRAN
#'
#' # s3://sentinel-cogs/sentinel-s2-l2a-cogs/2017/S2A_35MNR_20171025_0_L2A/
#'
#' # Provide your AWS credentials here
#' # Sys.setenv(
#' # "AWS_ACCESS_KEY_ID"     = <your_access_key>,
#' # "AWS_SECRET_ACCESS_KEY" = <your_secret_access_key>,
#' # "AWS_DEFAULT_REGION"    = <your AWS region>,
#' # "AWS_ENDPOINT" = "sentinel-s2-l2a.s3.amazonaws.com",
#' # "AWS_REQUEST_PAYER"     = "requester"
#' # )
#'
#' s2_cube <- sits_cube(
#'     type = "S2_L2A_AWS",
#'     name = "T20LKP_2018_2019",
#'     satellite = "SENTINEL-2",
#'     sensor = "MSI",
#'     tiles = "20LKP",
#'     s2_aws_resolution = "20m",
#'     start_date = as.Date("2018-07-18"),
#'     end_date = as.Date("2018-07-23")
#' )
#' }
#'
sits_cube.s2_l2a_aws_cube <- function(type = "S2_L2A_AWS",
                                      name = NULL,
                                      ...,
                                      bands = NULL,
                                      tiles = NULL,
                                      start_date = NULL,
                                      end_date = NULL,
                                      s2_aws_resolution = NULL) {
    # precondition - is AWS access available?
    aws_access_ok <- .sits_aws_check_access(type = type)
    if (!aws_access_ok)
          return(NULL)

    tiles_cube <- purrr::map(tiles, function(tile) {
        stack <- .sits_s2_l2a_aws_info_tiles(
            tile = tile,
            bands = bands,
            resolution = s2_aws_resolution,
            start_date = start_date,
            end_date = end_date
        )
        cube_t <- .sits_s2_l2a_aws_tile_cube(
            name = name,
            bands = bands,
            tile = tile,
            file_info = stack
        )

        class(cube_t) <- c("stack_cube", "raster_cube", class(cube_t))
        return(cube_t)
    })
    cube <- dplyr::bind_rows(tiles_cube)
    return(cube)
}
#' @title Create a composed data cube for a Sentinel-2 L2A AWS cube
#' @name sits_cube.gdalcubes_cube
#'
#' @description Creates composed cubes using the gdalcubes package.
#' Cubes can be composed of the following functions: "min", "max", "mean",
#' "median" or "first". To create the composition it is necessary to provide an
#' image period, in which it is used to apply the composition function.
#' For now, only Sentinel-2 L2A AWS cube can be composed.
#'
#' @references APPEL, Marius; PEBESMA, Edzer. On-demand processing of data cubes
#'  from satellite image collections with the gdalcubes library. Data, v. 4,
#'  n. 3, p. 92, 2019. DOI: 10.3390/data4030092
#'
#' @param type        Type of cube.
#' @param name        Name of output data cube.
#' @param ...         Other parameters to be passed for the function
#'  \code{write_tif} of gdalcubes package.
#' @param cube        A Sentinel-2 L2A AWS data cube
#' @param path_images A \code{character} with the path where the
#'  aggregated images will be write.
#' @param path_db     A \code{character} with the path and name where the
#'  database will be create. E.g. "my/path/gdalcubes.db"
#' @param period      A \code{character} with the period of time in which
#'  it is desired to apply in the cube, must be provided based on ISO8601, where
#'  1 number and a unit are provided, for example "P16D" for 16 days. For unit,
#'  use "D", "M" and "Y" for days, month and year, respectively.
#' @param agg_method  A \code{character} with the method that will be applied in
#'  the aggregation, the following are available: "min", "max", "mean",
#'  "median" or "first".
#' @param resampling  A \code{character} with the method that will be
#'  applied in the resampling in mosaic operation. The following are available:
#'  "near", "bilinear", "bicubic" or others supported by gdalwarp
#'  (see https://gdal.org/programs/gdalwarp.html).
#' @param cloud_mask  A \code{logical} corresponds to the use of the cloud band
#'  for aggregation.
#'
#' @return A data cube.
#' @export
#'
#' @examples
#' \dontrun{
#' # this example requires access to an external service, so should not be run
#' # by CRAN
#
#'
#' s2_cube <- sits_cube(
#'     type = "S2_L2A_AWS",
#'     name = "T20LKP_2018_2019",
#'     satellite = "SENTINEL-2",
#'     sensor = "MSI",
#'     tiles = "20LKP",
#'     s2_aws_resolution = "20m",
#'     start_date = as.Date("2018-07-18"),
#'     end_date = as.Date("2018-07-23")
#' )
#'
#' gc_cube <- sits_cube(type        = "GDALCUBES",
#'                      name        = "T20LKP_2018_2019_1M",
#'                      cube        = s2_cube,
#'                      path_db     = "/my/path/cube.db",
#'                      path_images = "/my/path/images/",
#'                      period      = "P1M",
#'                      agg_method  = "median",
#'                      resampling  = "bilinear")
#' }
#'
sits_cube.gdalcubes_cube <- function(type = "GDALCUBES",
                                     name,
                                     ...,
                                     cube,
                                     path_images,
                                     path_db = NULL,
                                     period  = NULL,
                                     agg_method = NULL,
                                     resampling = "bilinear",
                                     cloud_mask = TRUE) {
    # require gdalcubes package
    if (!requireNamespace("gdalcubes", quietly = TRUE)) {
        stop(paste("Please install package gdalcubes from CRAN:",
                   "install.packages('gdalcubes')"), call. = FALSE
        )
    }

    # test if provided object its a sits cube
    assertthat::assert_that("stack_cube" %in% class(cube[1,]),
                            msg = paste("The provided cube is invalid,",
                                        "please provide a 'stack_cube' object.",
                                        "See '?sits_cube' for more information.")
    )

    # in case of null path a temporary directory is generated
    if (is.null(path_db))
        path_db <- file.path(tempdir(), "cube.db")

    # create an image collection
    img_col <- .sits_gc_database(cube, path_db)

    # create a list of cube view object
    cv_list <- .sits_gc_cube(cube, period, agg_method, resampling)

    # create of the aggregate cubes
    gc_cube <- .sits_gc_compose(cube, name, cv_list, img_col, path_db, path_images,
                                cloud_mask)

    return(gc_cube)
}
#' @title Defines a cube from a set of image bricks
#' @name sits_cube.probs_cube
#'
#' @description Defines a cube to retrieve data from a set of image files
#'              that have been classified.
#'              All image files should have the same spatial resolution
#'              and same projection. Each probs image
#'              has to be organised as a raster brick, and the
#'              number of layers must match the number of labels.
#'              All input files must have the same spatial resolution and
#'              share the same timeline (in order).
#'              The timeline for the cube must be provided.
#'
#' @param type              type of cube
#' @param name              name of output data cube
#' @param ...               other parameters
#' @param satellite         satellite
#' @param sensor            sensor
#' @param timeline          vector with timeline of the files
#' @param labels            vector of labels associated to the probs cube
#' @param files             vector of file names for each band
#'
#' @return                  data cube
#'
#' @examples
#'
#' # Create a raster cube based on bricks
#' # inform the files that make up a raster probs brick with 23 time instances
#' probs_file <- c(system.file("extdata/raster/mod13q1/sinop-2014_probs_2013_9_2014_8_v1.tif",
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
#'     type = "PROBS",
#'     name = "Sinop-crop-probs",
#'     satellite = "TERRA",
#'     sensor  = "MODIS",
#'     timeline = timeline_2013_2014,
#'     labels = labels,
#'     files = probs_file
#' )
#' @export
sits_cube.probs_cube <- function(type = "PROBS",
                                 name = "probs_cube",
                                 ...,
                                 satellite,
                                 sensor,
                                 timeline,
                                 labels,
                                 files) {

    # iterate through the input files
    rows <- purrr::map(files, function(f){
        # precondition - check if labels match files
        # read the information from the files using GDAL
        rg_obj <- suppressWarnings(rgdal::GDALinfo(f))
        n_layers <- as.numeric(rg_obj["bands"])
        assertthat::assert_that(n_layers == length(labels),
                                msg = paste0("mismatch btw labels and bands in file ", f))

        # get the file params
        params <- .sits_raster_api_params_file(f)
        # build the file information
        file_info <- tibble::tibble(
            band = "probs",
            date = as.Date(timeline[1]),
            path = f
        )
        # go row by row
        row <- tibble::tibble(
            type = "PROBS",
            satellite = satellite,
            sensor = sensor,
            name = name,
            bands = list("probs"),
            labels = list(labels),
            scale_factors  = list(.sits_config_probs_scale_factor()),
            missing_values = list(.sits_config_probs_missing_value()),
            minimum_values = list(.sits_config_probs_minimum_value()),
            maximum_values = list(.sits_config_probs_maximum_value()),
            timeline = list(list(timeline)),
            nrows = params$nrows,
            ncols = params$ncols,
            xmin  = params$xmin,
            xmax  = params$xmax,
            ymin  = params$ymin,
            ymax  = params$ymax,
            xres  = params$xres,
            yres  = params$yres,
            crs   = params$crs,
            file_info = list(file_info),
        )
        return(row)
    })
    probs_cube <- dplyr::bind_rows(rows)

    class(probs_cube) <- c("probs_cube", "raster_cube", class(probs_cube))
    return(probs_cube)
}
#' @title Default methods for sits_cube
#' @name sits_cube.default
#'
#' @param type              Type of cube
#' @param ...               Other parameters to be passed for specific types
#'
#' @export
sits_cube.default <- function(type = NULL, ...) {
    stop("Error - cube type unknown"
    )
}
#' @title Creates the contents of a data cube
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
#' @return           Output data cube
#'
#' @examples
#' data_dir <- system.file("extdata/raster/cbers", package = "sits")
#'
#' cbers_022024 <- sits_cube(
#'     type = "STACK",
#'     name = "cbers_022024",
#'     satellite = "CBERS-4",
#'     sensor = "AWFI",
#'     resolution = "64m",
#'     data_dir = data_dir,
#'     parse_info = c("X1", "X2", "band", "date")
#' )
#'
#' cbers_022024_copy <- sits_cube_copy(cbers_022024,
#'     name = "cb_022024_cp",
#'     dest_dir = tempdir()
#' )
#' @export
#'
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

    assertthat::assert_that((srcwin["xoff"] + srcwin["xsize"]) <= cube$ncols,
                            msg = "srcwin x values bigger than cube size"
    )
    assertthat::assert_that((srcwin["yoff"] + srcwin["ysize"]) <= cube$nrows,
                            msg = "srcwin y values bigger than cube size"
    )

    # the label cube may contain several classified images
    cube_rows <- slider::slide(cube, function(row) {
        # get information on the file
        file_info <- row$file_info[[1]]

        # are the selected bands in the cube?
        assertthat::assert_that(all(bands %in% sits_bands(row)),
                  msg = "input bands not available in the cube"
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

            gdalUtils::gdal_translate(
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
