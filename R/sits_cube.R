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

    class_type <- .sits_config_cube_metadata(type)
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
    bricks_ok <- .sits_raster_check_bricks(satellite = satellite,
                                           sensor    = sensor,
                                           name      = name,
                                           timeline  = timeline,
                                           bands     = bands,
                                           files     = files)
    if (bricks_ok)
        cube.tb <- .sits_raster_brick_cube(satellite = satellite,
                                           sensor    = sensor,
                                           name      = name,
                                           timeline  = timeline,
                                           bands     = bands,
                                           files     = files)
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
#'                             delim      = "_",
#'                             parse_info = c("X1", "X2", "X3", "X4", "X5", "date", "X7", "band"))
#' }
sits_cube.stack_cube <- function(type = "STACK", ...,
                                 name,
                                 satellite,
                                 sensor,
                                 resolution,
                                 data_dir,
                                 delim   = "_",
                                 parse_info) {

    # precondition - check satellite and sensor
    ok <- .sits_raster_satellite_sensor(satellite, sensor)
    # precondition - check parsing info
    assertthat::assert_that(length(parse_info) >= 2,
                msg = "invalid parsing information")

    assertthat::assert_that(all(c("band", "date") %in% parse_info),
                msg = "parsing info need to contain valid columns for date and band")

    if (ok) {
        # get the file information
        file_info.tb <- .sits_raster_stack_info(resolution = resolution,
                                                data_dir = data_dir,
                                                delim = delim,
                                                parse_info = parse_info)
        # create the data cube
        stack_cube   <- .sits_raster_stack_cube(satellite    = satellite,
                                                sensor       = sensor,
                                                name         = name,
                                                file_info    = file_info.tb)

    }

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
#' # Create a raster cube based on CBERS data provided by the inSitu package
#' data_dir <- system.file("extdata/CBERS/", package = "inSitu")
#'
#' # create a raster cube file based on the information about the files
#' cbers_bdc_tile <- sits_cube(type       = "BDC_TILE",
#'                             name       = "022024",
#'                             satellite  = "CBERS-4",
#'                             sensor     = "AWFI",
#'                             cube       = "CB4_64_16D_STK",
#'                             tile       = "022024",
#'                             data_access = "local",
#'                             start_date  = as.Date("2018-08-29"),
#'                             end_date    = as.Date("2019-08-13"),
#'                             .local      = data_dir)
#' }
#'
sits_cube.bdc_cube <- function(type        = "BDC_TILE", ...,
                               name        = NULL,
                               satellite   = NULL,
                               sensor      = NULL,
                               bands       = NULL,
                               cube        = NULL,
                               tile        = NULL,
                               data_access = "local",
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
                                     data_access = data_access,
                                     start_date  = start_date,
                                     end_date    = end_date,
                                     .local      = .local,
                                     .web        = .web,
                                     .cloud_band = .cloud_band)

    cube.tb  <- .sits_bdc_tile_cube(satellite    = satellite,
                                    sensor       = sensor,
                                    name         = name,
                                    bands        = bands,
                                    cube         = cube,
                                    tile         = tile,
                                    file_info    = stack.tb)

    return(cube.tb)
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


    cube.tb  <- .sits_sentinel_aws_tile_cube(name      = name,
                                             bands     = bands,
                                             tile      = tile,
                                             file_info = stack.tb)

    return(cube.tb)
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

#' @title Creates the description of a data cube
#' @name .sits_cube_create
#'
#' @description Print information and save metadata about a data cube.
#'
#' @param type               Type of cube
#' @param URL                URL of the provider (optional)
#' @param satellite          Name of satellite
#' @param sensor             Name of sensor
#' @param name               Name of the data cube (mandatory)
#' @param cube               Name of the input data cube (optional)
#' @param tile               Name of the input data tile (optional)
#' @param bands              Vector with the names of the bands.
#' @param labels             Vector with labels (only for classified data).
#' @param scale_factors      Vector with scale factor for each band.
#' @param missing_values     Vector with missing values for each band.
#' @param minimum_values     Vector with minimum values for each band.
#' @param maximum_values     Vector with maximum values for each band.
#' @param timelines          List with vectors of valid timelines for each band.
#' @param nrows              Number of rows in the cube.
#' @param ncols              Number of columns in the cube.
#' @param xmin               Spatial extent (xmin).
#' @param ymin               Spatial extent (ymin).
#' @param xmax               Spatial extent (xmax).
#' @param ymax               Spatial extent (ymin).
#' @param xres               Spatial resolution (x dimension).
#' @param yres               Spatial resolution (y dimension).
#' @param crs                CRS for cube (EPSG code or PROJ4 string).
#' @param file_info          Tibble with information about stacks (for stacks)
#'
.sits_cube_create <- function(type,
                              URL = NA,
                              satellite,
                              sensor,
                              name,
                              cube = NA,
                              tile = NA,
                              bands,
                              labels = NA,
                              scale_factors,
                              missing_values,
                              minimum_values,
                              maximum_values,
                              timelines,
                              nrows,
                              ncols,
                              xmin,
                              xmax,
                              ymin,
                              ymax,
                              xres,
                              yres,
                              crs,
                              file_info = NULL) {


    # create a tibble to store the metadata (mandatory parameters)
    cube.tb <- tibble::tibble(type           = type,
                              URL            = URL,
                              satellite      = satellite,
                              sensor         = sensor,
                              name           = name,
                              cube           = cube,
                              tile           = tile,
                              bands          = list(bands),
                              labels         = list(labels),
                              scale_factors  = list(scale_factors),
                              missing_values = list(missing_values),
                              minimum_values = list(minimum_values),
                              maximum_values = list(maximum_values),
                              timeline       = list(timelines),
                              nrows          = nrows,
                              ncols          = ncols,
                              xmin           = xmin,
                              xmax           = xmax,
                              ymin           = ymin,
                              ymax           = ymax,
                              xres           = xres,
                              yres           = yres,
                              crs            = crs)

    if (!purrr::is_null(file_info))
        cube.tb <- tibble::add_column(cube.tb, file_info = list(file_info))

    return(cube.tb)
}



#' @title Create a set of RasterLayer objects to store
#' data cube classification results (only the probs)
#' @name .sits_cube_classified
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Take a tibble containing metadata about a data cube
#' containing time series (each Brick has information for one band) and create a
#' set of RasterLayers to store the classification result.
#' Each RasterLayer corresponds to one time step.
#' The time steps are specified in a list of dates.
#'
#' @param  cube              Tibble with metadata about the input data cube.
#' @param  samples           Samples used for training the classification model.
#' @param  interval          Classification interval.
#' @param  output_dir        Prefix of the output files.
#' @param  version           Version of the output files
#' @return A tibble with metadata about the output RasterLayer objects.
.sits_cube_classified <- function(cube, samples, interval,
                                  output_dir, version) {
    # ensure metadata tibble exists
    assertthat::assert_that(NROW(cube) > 0,
        msg = ".sits_classify_cube: need a valid metadata for cube")

    # get the timeline of of the data cube
    timeline <- lubridate::as_date(sits_timeline(cube))

    # Get the reference start date and end date from the samples
    ref_start_date <- lubridate::as_date(samples[1,]$start_date)
    ref_end_date   <- lubridate::as_date(samples[1,]$end_date)

    # produce the breaks used to generate the output rasters
    subset_dates <- sits_timeline_match(timeline = timeline,
                                        ref_start_date = ref_start_date,
                                        ref_end_date = ref_end_date,
                                        interval = interval)

    # how many objects are to be created?
    n_objs <- length(subset_dates)

    # labels come from samples.tb
    labels <- sits_labels(samples)$label

    # create vectors and lists to store the content of the probabilities
    rasters    <- vector("list", length = n_objs)
    bands      <- vector(length = n_objs)
    files      <- vector(length = n_objs)
    n_layers   <- length(labels)
    timelines  <- vector("list", length = n_objs)

    # set scale factors, missing values, minimum and maximum values for probs
    scale_factors   <- rep(0.001,  n_objs)
    missing_values  <- rep(-9999,  n_objs)
    minimum_values  <- rep(0.0,    n_objs)
    maximum_values  <- rep(1.0,    n_objs)

    # loop through the list of dates and create list of raster layers
    for (i in 1:n_objs) {

        # define the timeline for the raster data sets
        start_date     <- subset_dates[[i]][1]
        end_date       <- subset_dates[[i]][2]
        timelines[[i]] <- timeline[lubridate::as_date(timeline) >= start_date &
                                       lubridate::as_date(timeline) <= end_date]

        # define the filename for the classified image
        files[i] <- .sits_raster_filename(output_dir = output_dir,
                                          version = version,
                                          name = cube$name,
                                          type = "probs",
                                          start_date = start_date,
                                          end_date = end_date)
        bands[i] <- .sits_class_band_name(name = cube$name, type = "probs",
                                          start_date = start_date,
                                          end_date = end_date)
    }
    # get the name of the cube
    name   <-  paste0(cube[1,]$name, "_probs")

    # generate a set of timelines for the file_info
    times_probs <- vector(length = n_objs)
    for (i in 1:n_objs){
        times_probs[i] <- timelines[[i]][1]
    }

    # get the file information
    file_info <- .sits_raster_file_info(cube$xres, bands, times_probs, files)

    # set the metadata for the probability cube
    cube_probs <- .sits_cube_create(type            = "PROBS",
                                    satellite       = cube$satellite,
                                    sensor          = cube$sensor,
                                    name            = name,
                                    bands           = bands,
                                    labels          = labels,
                                    scale_factors   = scale_factors,
                                    missing_values  = missing_values,
                                    minimum_values  = minimum_values,
                                    maximum_values  = maximum_values,
                                    timelines       = timelines,
                                    nrows           = cube$nrows,
                                    ncols           = cube$ncols,
                                    xmin            = cube$xmin,
                                    xmax            = cube$xmax,
                                    ymin            = cube$ymin,
                                    ymax            = cube$ymax,
                                    xres            = cube$xres,
                                    yres            = cube$yres,
                                    crs             = cube$crs,
                                    file_info       = file_info)

    class(cube_probs) <- c("probs_cube", class(cube_probs))
    return(cube_probs)
}
#' @title Create a set of RasterLayer objects
#'        to store data cube classification results (labelled classes)
#' @name .sits_cube_labelled
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a tibble containing metadata about a data cube wuth
#' classification probabilites and and creates a
#' set of RasterLayers to store the classification result. Each RasterLayer is
#' to one time step. The time steps are specified in a list of dates.
#'
#' @param  cube_probs        Metadata about the input data cube (probability).
#' @param  smoothing         (optional) smoothing method to be applied
#'                           ("none", "bayesian", "majority")
#' @param  output_dir        Output directory where to put the files
#' @param  version           Name of the version of the result
#' @return                   Metadata about the output RasterLayer objects.
.sits_cube_labelled <- function(cube_probs, smoothing, output_dir, version) {

    # labels come from the input cube
    labels <- .sits_cube_labels(cube_probs)

    # how many objects are to be created?
    n_objs <- length(.sits_cube_files(cube_probs))

    # lists that store the content of the raster layers (classified values))
    bands     <- vector(length = n_objs)
    files     <- vector(length = n_objs)
    timelines <- vector("list", length = n_objs)

    # set scale factors, missing values, minimum and maximum values
    scale_factors   <- rep(1, n_objs)
    missing_values  <- rep(-9999, n_objs)
    minimum_values  <- rep(1, n_objs)
    maximum_values  <- rep(length(labels), n_objs)

    # get the type of the cube
    type  <- paste0("class_", smoothing)
    # name of the cube
    name  <- paste0(cube_probs[1,]$name, "_", type)

    # loop through the list of dates and create list of raster layers
    for (i in 1:n_objs) {

        # define the timeline for the raster data sets
        timelines[[i]] <- lubridate::as_date(sits_timeline(cube_probs, i))
        start_date     <- timelines[[i]][1]
        end_date       <- timelines[[i]][length(timelines[[i]])]

        # # define the filename for the classified image
        bands[i] <- .sits_class_band_name(name = cube_probs[1,]$name,
                                          type = type,
                                          start_date = start_date,
                                          end_date = end_date)
        files[i] <- .sits_raster_filename(output_dir = output_dir,
                                          version = version,
                                          name = cube_probs[1,]$name,
                                          type = type,
                                          start_date = start_date,
                                          end_date = end_date)
    }

    # generate a set of timelines for the file_info
    times_probs <- vector(length = n_objs)
    for (i in 1:n_objs){
        times_probs[i] <- timelines[[i]][1]
    }

    # get the file information
    file_info <- .sits_raster_file_info(cube_probs$xres, bands, times_probs, files)

    # create a new RasterLayer for a defined period and generate metadata
    cube_labels <- .sits_cube_create(type           = "CLASSIFIED",
                                     satellite      = cube_probs$satellite,
                                     sensor         = cube_probs$sensor,
                                     name           = name,
                                     bands          = bands,
                                     labels         = labels,
                                     scale_factors  = scale_factors,
                                     missing_values = missing_values,
                                     minimum_values = minimum_values,
                                     maximum_values = maximum_values,
                                     timelines      = timelines,
                                     nrows          = cube_probs$nrows,
                                     ncols          = cube_probs$ncols,
                                     xmin           = cube_probs$xmin,
                                     xmax           = cube_probs$xmax,
                                     ymin           = cube_probs$ymin,
                                     ymax           = cube_probs$ymax,
                                     xres           = cube_probs$xres,
                                     yres           = cube_probs$yres,
                                     crs            = cube_probs$crs,
                                     file_info      = file_info)


    class(cube_labels) <- c("classified_image", class(cube_labels))
    return(cube_labels)
}

#' @title Define a name for classified band
#' @name .sits_class_band_name
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Creates a name for a raster layer based on timeline
#'
#' @param name           Original cube name (without temporal information).
#' @param type           Type of output
#' @param start_date     Starting date of the time series classification.
#' @param end_date       End date of the time series classification.
#' @return Name of the classification file for the required interval.
.sits_class_band_name <- function(name, type, start_date, end_date){
    y1 <- lubridate::year(start_date)
    m1 <- lubridate::month(start_date)
    y2 <- lubridate::year(end_date)
    m2 <- lubridate::month(end_date)

    band_name <- paste0(name, "_", type, "_", y1, "_", m1, "_", y2, "_", m2)

    return(band_name)
}

#' @title Find the bands associated to a cube
#' @name .sits_cube_bands
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Given a data cube, retrieves the bands
#'
#' @param cube          Metadata about a data cube
#' @return  Vector of bands available in the data cube
.sits_cube_bands <- function(cube) {
    return(cube$bands[[1]])
}
#' @title Set the bands associated to a cube
#' @name .sits_cube_bands_set
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Given a data cube and a set of bands, sets the bands
#'
#' @param cube          Metadata about a data cube
#' @param bands         Bands to be assigned to the cube
#' @return  Vector of bands available in the data cube
.sits_cube_bands_set <- function(cube, bands) {
    cube$bands[[1]] <- bands
    return(cube)
}

#' @title Check that the cube is valid
#' @name .sits_cube_check_validity
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description     Given a data cube, retrieve the scale factors
#' @param cube      Metadata about a data cube
#' @return          Boolean value
.sits_cube_check_validity <- function(cube){

    # check that the service is valid
    .sits_config_check(cube[1,]$type)

    check <- FALSE

    # check is WTSS service is working
    if (cube$type == "WTSS")
        check <- .sits_wtss_check(cube$URL, cube$name)
    # check is SATVEG service is working
    else if (cube$type == "SATVEG")
        check <- .sits_satveg_check()
    # raster cubes have been checked before
    else
        check <- TRUE

    return(invisible(check))
}

#' @title Return a file associated to a data cube, given an index
#' @name .sits_cube_file
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description     Given a data cube and an index, retrieve the file
#' @param cube      Metadata about a data cube
#' @param index     Index for file to be retrived
#' @return          Name of file
.sits_cube_file <- function(cube, index = 1) {
    assertthat::assert_that(index <= length(cube$file_info[[1]]$path),
                   msg = ".sits_cube_file: index is out of range")
    return(cube$file_info[[1]]$path[index])
}

#' @title Return all file associated to a data cube
#' @name .sits_cube_files
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Given a data cube and an index, retrieve the files
#' @param cube     Metadata about a data cube
#' @return         Vector of files
.sits_cube_files <- function(cube) {
    return(cube$file_info[[1]]$path)
}


#' @title Return all labels associated to a data cube
#' @name .sits_cube_labels
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Given a data cube, retrieve the ;abels
#' @param cube     Metadata about a data cube
#' @return         Vector of labels
.sits_cube_labels <- function(cube){
    return(cube$labels[[1]])
}

#' @title Return the timeline associated to a data cube, given an index
#' @name sits_cube_timeline
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Given a data cube, retrieve the timeline
#' @param cube     Metadata about a data cube
#' @param index    Index of timeline list
#' @return         Vector of times for an index
#' @export
sits_cube_timeline <- function(cube, index = 1){
    assertthat::assert_that(index <= length(cube$timeline[[1]]),
                         msg = ".sits_cube_timeline: index out of range")
    return(cube$timeline[[1]][[index]])
}

#' @title Given a band, return the associated Raster object for the cube
#' @name .sits_cube_robj_band
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description          Given a data cube, retrieve the timeline
#' @param cube           Metadata about a data cube
#' @param band_cube      Name of the band to the retrieved
#' @return               Raster object associated to the indexed file
#'
.sits_cube_robj_band <- function(cube, band_cube){

    bands <- unlist(cube$bands)
    index <- grep(band_cube, bands)

    band.tb <- dplyr::filter(cube$file_info[[1]], band == band_cube)
    # Get the robjs for faster access
    if (cube$type == "BRICK")
        r_obj <- raster::brick(band.tb$path)
    else
        r_obj <- raster::stack(band.tb$path)
    return(r_obj)
}

#' @title Retrieve the missing values for a data cube
#' @name .sits_cube_missing_values
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description     Given a data cube, retrieve the missing values
#' @param cube      Metadata about a data cube
#' @return          Vector of missing values.
.sits_cube_missing_values <- function(cube){
    return(cube$missing_values[[1]])
}

#' @title Retrieve the minimum values for a data cube
#' @name .sits_cube_minimum_values
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description     Given a data cube, retrieve the minimum values
#' @param cube      Metadata about a data cube
#' @return          Vector of minimum values.
.sits_cube_minimum_values <- function(cube){
    return(cube$minimum_values[[1]])
}
#' @title Retrieve the minimum values for a data cube
#' @name .sits_cube_maximum_values
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description     Given a data cube, retrieve the maximum values
#' @param cube      Metadata about a data cube
#' @return          Vector of maximum values.
.sits_cube_maximum_values <- function(cube){
    return(cube$maximum_value[[1]])
}

#' @title Retrieve the scale factors for a data cube
#' @name .sits_cube_scale_factors
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description     Given a data cube, retrieve the scale factors
#' @param cube      Metadata about a data cube
#' @return          Vector of scale factors
.sits_cube_scale_factors <- function(cube){
    return(cube$scale_factors[[1]])
}

#' @title Align the bands of the cube with those of the samples
#' @name .sits_cube_align_bands
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description         Given a data cube, retrieve the scale factors
#' @param cube          Metadata about a data cube
#' @param sample_bands  Bands of the data sample
#' @return              Updated cube
.sits_cube_align_bands <- function(cube, sample_bands){

    # retrieve the cube bands
    cube_bands <- .sits_cube_bands(cube)

    # align the indexes
    m <- match(sample_bands, cube_bands)

    # reorganize the bands and the files in the cube
    # they should be aligned with the bands in the samples
    cube_bands <- cube_bands[m]
    cube$bands <- list(cube_bands)

    # adjust the object list
    if (cube$type == "BRICK") {
        cube$files <- cube$files %>%
                unlist() %>%
                .[m]     %>%
                list()
    }
    cube$r_objs_list <- cube$r_objs_list %>%
        unlist() %>%
        .[m]     %>%
        list()

    # need to include the case of BDC_TILE
    return(cube)
}
#' @title Check that the requested bands exist in the cube
#' @name .sits_cube_bands_check
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param cube          Metadata about a data cube
#' @param bands         Requested bands of the data sample
#' @return              Checked bands (cube bands if bands are NULL)
#'
.sits_cube_bands_check <- function(cube, bands = NULL) {
    # check the bands are available
    cb_bands <- sits_bands(cube)
    if (purrr::is_null(bands))
        bands <- cb_bands
    else {
        bands <- toupper(bands)
        assertthat::assert_that(all(bands %in% cb_bands),
                                msg = "sits_from_wtss: bands are not available in the cube")
    }
    return(bands)
}
