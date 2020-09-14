#' @title Defines a data cube
#' @name sits_cube
#'
#' @description Defines a cube to retrieve data. This is a generic function.
#' Cubes can be of the following types. See each function description for the
#' required parameters:
#' \itemize{
#'  \item{"WTSS": }{Web Time Series Service - see \code{\link{sits_cube.wtss_cube}}}
#'  \item{"SATVEG": }{ SATVEG Time Series Service - see \code{\link{sits_cube.satveg_cube}}}
#'  \item{"BRICK": }{Raster Brick files - see \code{\link{sits_cube.brick_cube}}}
#'  \item{"STACK":}{Raster Stack files - see \code{\link{sits_cube.stack_cube}}}
#'  \item{"BDC_TILE"}{A tile from the Brazil Data Cube - see \code{\link{sits_cube.bdc_cube}}}
#'  \item{"S2_L2A_AWS"}{A tile of Sentinel-2 data in AWS - see \code{\link{sits_cube.s2_l2a_aws_cube}}}
#' }
#'
#' @param type              Type of cube (one of "WTSS", "SATVEG", "BRICK", "BDC_TILE",
#'                          "S2_L2A_AWS", "PROBS", "CLASSIFIED")
#' @param ...               Other parameters to be passed for specific types
#'
#' @examples
#' \donttest{
#' # Example 1. Create a data cube based on a WTSS service
#' cube_wtss <- sits_cube(type = "WTSS",
#'           name = "MOD13Q1",
#'           URL = "http://www.esensing.dpi.inpe.br/wtss/")
#'
#' # Example 2. Create a data cube based on the SATVEG service
#' cube_satveg <- sits_cube(type = "SATVEG",
#'                          name = "terra")
#'
#' # Example 3. Create a raster cube based on bricks
#' # inform the files that make up a raster brick with 392 time instances
#' files <- c(system.file("extdata/raster/mod13q1/sinop-crop-ndvi.tif",
#'            package = "sits"))
#'
#' # create a raster cube file based on the information about the files
#' raster.tb <- sits_cube(type = "BRICK",
#'                        name      = "Sinop-crop",
#'                        satellite = "TERRA",
#'                        sensor    = "MODIS",
#'                        timeline  = timeline_modis_392,
#'                        bands     = "NDVI",
#'                        files     = files)
#'
#' }
#' @export
sits_cube <- function(type, ...) {

    class_type <- .sits_config_cube_specific(type)
    class(type) <- c(class_type, class(type))
    # Dispatch
    UseMethod("sits_cube", type)
}
#' @title Defines a data cube for the WTSS service
#' @name sits_cube.wtss_cube
#'
#' @description  Implements an interface to a web time series service (WTSS)
#'               that offers time series of remote sensing data using a simple API.
#'
#' @param type              Type of cube
#' @param ...               Other parameters to be passed for specific types
#' @param URL               URL of the service provider.
#' @param name              Name of the output data cube.
#' @return                  A valid data cube
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Create a data cube based on a WTSS service
#' cube_wtss <- sits_cube(type = "WTSS",
#'           name = "MOD13Q1",
#'           URL = "http://www.esensing.dpi.inpe.br/wtss/")
#' }
sits_cube.wtss_cube <- function(type = "WTSS", ..., name = NULL, URL = NULL) {
    # Pre-condition
    wtss_ok <- .sits_wtss_check(URL = URL, name = name)
    # create a cube
    if (wtss_ok)
        cube.tb <- .sits_wtss_cube(URL = URL, name = name)
    return(cube.tb)
}

#' @title Defines a data cube for the SATVEG service
#' @name sits_cube.satveg_cube
#' @description The SATVEG service is provided by the Embrapa Agricultural Informatics Centre
#'              and provides access to time series from the MODIS sensor. There are three types
#'              of time series: "terra" (from the TERRA satellite), "aqua" (from the AQUA satellite)
#'              and "comb" (combination of both satellites)
#' @param type              Type of cube
#' @param ...               Other parameters to be passed for specific types
#' @param name              Name of the input data ("terra", "aqua", "comb").
#' @return                  A valid data cube
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Create a data cube based on the SATVEG service
#' cube_satveg <- sits_cube(type = "SATVEG",
#'                          name = "terra")
#' }
#'
sits_cube.satveg_cube <- function(type = "SATVEG", ..., name = NULL) {
    # Pre-condition - check if SATVEG is working
    satveg_ok <- .sits_satveg_check()
    # if OK, go ahead a create a SATVEG cube
    if (satveg_ok)
        cube.tb <- .sits_satveg_cube(name = name)
    return(cube.tb)
}
#' @title Defines a data cube for a BRICK
#' @name sits_cube.brick_cube
#'
#' @description Defines a cube to retrieve data from a set of raster bricks.
#'              Each band has to be organised as a raster brick, and the
#'              number of input files must match the number of bands.
#'              All input files must have the same spatial resolution and
#'              share the same timeline (in order).
#'              The timeline for the cube must be provided.
#' @param type              Type of cube
#' @param name              Name of the output data cube.
#' @param ...               Other parameters to be passed for specific types
#' @param satellite         Name of satellite
#' @param sensor            Name of sensor
#' @param timeline          Vector with the timeline of the collection
#' @param bands             Vector of bands.
#' @param files             Vector of file names for each band
#' @param start_date        Starting date of the cube
#' @param end_date          Ending date of the cube
#'
#' @return                  A brick data cube
#' @export
#'
#' @examples
#' \donttest{
#' # Create a raster cube based on bricks
#' # inform the files that make up a raster brick with 392 time instances
#' files <- c(system.file("extdata/raster/mod13q1/sinop-crop-ndvi.tif",
#'            package = "sits"))
#'
#' # create a raster cube file based on the information about the files
#' raster.tb <- sits_cube(type = "BRICK",
#'                        name      = "Sinop-crop",
#'                        satellite = "TERRA",
#'                        sensor    = "MODIS",
#'                        timeline  = timeline_modis_392,
#'                        bands     = "NDVI",
#'                        files     = files)
#' }
sits_cube.brick_cube <- function(type = "BRICK", ...,
                                 name = NULL,
                                 satellite = NULL,
                                 sensor = NULL,
                                 timeline = NULL,
                                 bands = NULL,
                                 files = NULL,
                                 start_date = NULL,
                                 end_date = NULL) {

    # check if need to include "/vsicurl" to be read by GDAL
    files <- .sits_raster_check_webfiles(files)
    # check if the files are bricks
    bricks_ok <- .sits_raster_brick_check(satellite = satellite,
                                           sensor    = sensor,
                                           name      = name,
                                           timeline  = timeline,
                                           bands     = bands,
                                           files     = files)
    if (bricks_ok)
        cube <- .sits_raster_brick_cube(satellite = satellite,
                                        sensor    = sensor,
                                        name      = name,
                                        timeline  = timeline,
                                        bands     = bands,
                                        files     = files)

    class(cube) <- c("brick_cube", class(cube))
    return(cube)
}
#' @title Defines a cube to retrieve data from a set of image files
#' @name sits_cube.stack_cube
#'
#' @description Defines a cube to retrieve data from a set of image files.
#'              All image files should have the same spatial resolution
#'              and same projection. In addition, image file names should
#'              include information on band and date.
#'              The timeline and the bands are deduced from this information.
#'              Examples of valid image names include
#'              "CB4_64_16D_STK_022024_2018-08-29_2018-09-13_EVI.tif" and
#'              "B02_2018-07-18.jp2". In each case, the user has to provide
#'              appropriate parsing information that allows SITS to extract
#'              the band and the date. In the examples above, the parsing info
#'              would include "_" as a delimiter. In the first, the names of the
#'              resulting columns after parsing are "X1", "X2", "X3", "X4", "X5",
#'              "date", "X7", and "band". In the second, they are "band" and "date".
#'
#'
#' @param type              Type of cube
#' @param name              Name of the output data cube.
#' @param ...               Other parameters to be passed for specific types
#' @param satellite         Name of satellite
#' @param sensor            Name of sensor
#' @param resolution        Resolution of sensor
#' @param data_dir          Directory where data is located
#' @param delim             A character to use as delimiter (default = "_")
#' @param parse_info        The parsing information (see above)
#'
#' @return                  A brick data cube
#' @export
#'
#' @examples
#' \donttest{
#' # Create a raster cube based on CBERS data provided by the inSitu package
#' data_dir <- system.file("extdata/CBERS/CB4_64_16D_STK/022024", package = "inSitu")
#'
#' # create a raster cube file based on the information about the files
#' cbers_stack.tb <- sits_cube(type       = "STACK",
#'                             name       = "022024",
#'                             satellite  = "CBERS-4",
#'                             sensor     = "AWFI",
#'                             resolution = "64m",
#'                             data_dir   = data_dir,
#'                             parse_info = c("X1", "X2", "X3", "X4", "X5", "date", "X7", "band"))
#' }
sits_cube.stack_cube <- function(type = "STACK", ...,
                                 name,
                                 satellite,
                                 sensor,
                                 resolution,
                                 data_dir,
                                 parse_info,
                                 delim = NULL) {

    # precondition - check satellite and sensor
    ok <- .sits_raster_satellite_sensor(satellite, sensor)
    # precondition - check parsing info
    assertthat::assert_that(length(parse_info) >= 2,
                msg = "invalid parsing information")

    assertthat::assert_that(all(c("band", "date") %in% parse_info),
                msg = "parsing info need to contain valid columns for date and band")

    if (ok) {
        # get the file information
        file_info.tb <- .sits_raster_stack_info(type = "STACK",
                                                satellite  = satellite,
                                                sensor     = sensor,
                                                data_dir   = data_dir,
                                                parse_info = parse_info,
                                                delim      = delim)
        # create the data cube
        cube   <- .sits_raster_stack_cube(satellite    = satellite,
                                          sensor       = sensor,
                                          name         = name,
                                          file_info    = file_info.tb)

    }
    class(cube) <- c("stack_cube", class(cube))
    return(cube)

}
#' @title Defines a data cube for a BDC TILE
#' @name sits_cube.bdc_cube
#'
#' @description Defines a cube to retrieve data from the Brazil Data Cube (BDC). The retrieval
#'              is based on tiles of a given cube. Allows local or web access. For
#'              local access, the user should be logged in the BDC.
#'              For more on BDC, please see http://brazildatacube.dpi.inpe.br/
#'
#' @param type              Type of cube
#' @param ...               Other parameters to be passed for specific types
#' @param name              Name of the output data cube.
#' @param satellite         Name of satellite
#' @param sensor            Name of sensor
#' @param bands             Vector of bands.
#' @param cube              Name of the input data cube (or image collection)
#' @param tile              Name of the tile
#' @param version           Version of the cube
#' @param data_access       Type of access (local or web)
#' @param start_date        Starting date of the cube
#' @param end_date          Ending date of the cube
#' @param .local            Directory for local access to the input cube (optional)
#' @param .web              Directory for web access to the input cube (optional)
#' @param .cloud_band       Include cloud band? (TRUE/FALSE)
#'
#' @return                  A brick data cube
#' @export
#'
#' @examples
#' \donttest{
#'
#' # create a raster cube file based on the information about the files
#' cbers_bdc_tile <- sits_cube(type       = "BDC_TILE",
#'                             name       = "022024",
#'                             satellite  = "CBERS-4",
#'                             sensor     = "AWFI",
#'                             cube       = "CB4_64_16D_STK",
#'                             tile       = "022024",
#'                             version    = "v001",
#'                             data_access = "web",
#'                             bands       = c("NDVI", "EVI"),
#'                             start_date  = as.Date("2018-08-29"),
#'                             end_date    = as.Date("2019-08-13"))
#' }
#'
sits_cube.bdc_cube <- function(type        = "BDC_TILE", ...,
                               name        = NULL,
                               satellite   = NULL,
                               sensor      = NULL,
                               bands       = NULL,
                               cube        = NULL,
                               tile        = NULL,
                               version     = "v001",
                               data_access = "web",
                               start_date  = NULL,
                               end_date    = NULL,
                               .local      = NULL,
                               .web        = NULL,
                               .cloud_band = FALSE) {

    # Precondition
    bdc_tile_ok <- .sits_bdc_check_tiles(satellite      = satellite,
                                         sensor         = sensor,
                                         bands          = bands,
                                         cube           = cube,
                                         tile           = tile,
                                         version        = version,
                                         data_access    = data_access,
                                         start_date     = start_date,
                                         end_date       = end_date)

    if (!bdc_tile_ok)
        return(NULL)

    stack.tb <- .sits_bdc_info_tiles(satellite   = satellite,
                                     sensor      = sensor,
                                     bands       = bands,
                                     cube        = cube,
                                     tile        = tile,
                                     version     = version,
                                     data_access = data_access,
                                     start_date  = start_date,
                                     end_date    = end_date,
                                     .local      = .local,
                                     .web        = .web,
                                     .cloud_band = .cloud_band)

    cube  <- .sits_bdc_tile_cube(satellite    = satellite,
                                 sensor       = sensor,
                                 name         = name,
                                 bands        = bands,
                                 cube         = cube,
                                 tile         = tile,
                                 file_info    = stack.tb)

    class(cube) <- c("stack_cube", class(cube))

    return(cube)
}
#' @title Defines a data cube for a Sentinel-2 L2A AWS cube
#' @name sits_cube.s2_l2a_aws_cube
#'
#' @description Defines a cube to retrieve data from the Sentinel-2 Level 2A data
#'              available in AWS. To access this data, the user needs to be an AWS
#'              user and provide her access key and secret key. These keys may
#'              be passed as environment variables. The bands available in AWS for
#'              10m resolution are "B02", "B03", "B04", and "B08". The  20m bands
#'              are "B02", "B03", "B04", "B05", "B06", "BO7", "B08", "B8A", "B11", and "B12".
#'              All 12 bands are available at 60m resolution.
#'
#' @param type              Type of cube
#' @param ...               Other parameters to be passed for specific types
#' @param name              Name of the output data cube.
#' @param bands             Vector of bands.
#' @param cube              Name of the input data cube (or image collection)
#' @param tile              Name of the tile
#' @param start_date        Starting date of the cube
#' @param end_date          Ending date of the cube
#' @param s2_aws_resolution Resolution of Sentinel images in AWS ("10m", "20m" or "60m")
#' @param access_key        AWS access key
#' @param secret_key        AWS secret key
#' @param region            AWS region
#' @return                  A brick data cube
#' @export
#'
#' @examples
#' \donttest{
#'
#' # Provide your AWS credentials here
#' # Sys.setenv(
#' # "AWS_ACCESS_KEY_ID"     = <your_access_key>,
#' # "AWS_SECRET_ACCESS_KEY" = <your_secret_access_key>,
#' # "AWS_DEFAULT_REGION"    = <your AWS region>,
#' # "AWS_REQUEST_PAYER"     = "requester"
#' # )
#'
#' s2_cube <- sits_cube(type       = "S2_L2A_AWS",
#'                      name       = "T20LKP_2018_2019",
#'                      satellite  = "SENTINEL-2",
#'                      sensor     = "MSI",
#'                      tile       = "20LKP",
#'                      s2_aws_resolution = "20m",
#'                      start_date = as.Date("2018-07-18"),
#'                      end_date   = as.Date("2018-07-23"))
#' }
#'
sits_cube.s2_l2a_aws_cube <- function(type = "S2_L2A_AWS", ...,
                                      name = NULL,
                                      bands = NULL,
                                      cube = NULL,
                                      tile = NULL,
                                      start_date = NULL,
                                      end_date = NULL,
                                      s2_aws_resolution = NULL,
                                      access_key = NULL,
                                      secret_key = NULL,
                                      region = NULL) {
    aws_access_ok <- .sits_sentinel_aws_check_access(access_key = access_key,
                                                     secret_key = secret_key,
                                                     region     = region)
    if (!aws_access_ok)
        return(NULL)

    stack.tb  <- .sits_sentinel_aws_info_tiles(tile       = tile,
                                               bands      = bands,
                                               resolution = s2_aws_resolution,
                                               start_date = start_date,
                                               end_date   = end_date)


    cube <- .sits_sentinel_aws_tile_cube(name      = name,
                                         bands     = bands,
                                         tile      = tile,
                                         file_info = stack.tb)

    class(cube) <- c("stack_cube", class(cube))
    return(cube)
}

#' @title Default methods for sits_cube
#' @name sits_cube.default
#'
#' @param type              Type of cube
#' @param ...               Other parameters to be passed for specific types
#'
#' @export
sits_cube.default <- function(type = NULL, ...){
    stop("Could not create SITS cube - type unknown")
}
#' @title Creates the contents of a data cube
#' @name sits_cube_copy
#'
#' @description Copies the metadata and data of a cube to a different
#' directory. This function can be use to transfer data on the cloud
#' to a local machine
#'
#' @param  cube      Input data cube
#' @param  name      Output cube name
#' @param  dest_dir  Destination directory
#' @param  bands     Bands to include in output (optional)
#' @return           Output data cube
#' @export
#'
sits_cube_copy <- function (cube, name, dest_dir, bands = NULL){
    # ensure input cube exists
    assertthat::assert_that(.sits_cube_check_validity(cube),
                            msg = "invalid input cube")
    assertthat::assert_that(!purrr::is_null(sits_bands(cube)),
                            msg = "cube has no bands")
    # does the output directory exist?
    assertthat::is.dir(dest_dir)

    # if bands are not stated, use all those in the cube
    if (purrr::is_null(bands))
        bands <- sits_bands(cube)
    else
        assertthat::assert_that(all(bands %in% sits_bands(cube)),
                                msg = "input bands not available in the cube")

    # get information on the file
    file_info <- cube$file_info[[1]]
    file_info_out <- dplyr::filter(file_info, band %in% bands)

    # save files with date information
    paths.lst <- purrr::map2(file_info_out$date,file_info_out$path,
                             function (d,p){
                                 dest_file <- paste0(dest_dir,"/",
                                                     tools::file_path_sans_ext(basename(p)),
                                                     "_",d,".jp2")
                                 gdalUtils::gdal_translate(p, dest_file)
                                 return(dest_file)
                        })
    # update file info
    new_paths <- unlist(paths.lst)
    file_info_out$path <- new_paths

    # update cube
    cube$file_info <- list(file_info_out)
    cube$name      <- name
    return(cube)
}

