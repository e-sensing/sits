#' @title Defines a data cube
#' @name sits_cube
#'
#' @references `rstac` package (https://github.com/brazil-data-cube/rstac)
#' `gdalcubes` package (see APPEL, Marius; PEBESMA, Edzer. On-demand processing of data cubes
#'  from satellite image collections with the gdalcubes library. Data, v. 4,
#'  n. 3, p. 92, 2019. DOI: 10.3390/data4030092)
#'
#' @description Defines a cube to retrieve data. Uses STAC information to
#' create data cubes from repositories such as AWS, Brazil Data Cube, and
#' Digital Earth Africa. Also enables producing cubes with regular time intervals
#' from irregular time series available in repositories such as AWS and DE Africa
#' using the "gdalcubes" package.
#'
#' Users can also create data cubes from individual files and access time series
#' services such as SATVEG.
#'
#' Cubes can be of the following types:
#' \itemize{
#'  \item{"BDC": }{Defines a cube to retrieve data from the Brazil Data Cube (BDC)
#'              STAC. The retrieval is based on tiles of a given cube.
#'              For more on BDC, please see http://brazildatacube.org/}
#'  \item{"DEAFRICA": }{Defines a cube to retrieve data from Digital Earth Africa.
#'  For more on DEAfrica, please see https://www.digitalearthafrica.org/}
#'  \item{"S2_L2A_AWS": }{Defines a cube to retrieve data from the Sentinel-2 L2A
#'  data available in AWS. Users need to be AWS
#'  users and provide their access key and secret key. These keys should
#'  be passed as environment variables. The AWS bands in
#'  10m resolution are "B02", "B03", "B04", and "B08".
#'  The  20m bands are "B02", "B03", "B04", "B05", "B06", "BO7",
#'  B08", "B8A", "B11", and "B12".
#'  All 12 bands are available at 60m resolution.
#'  }
#'  \item{"GDALCUBES": }{Creates cubes with regular time intervals
#'  using the gdalcubes package. Cubes are composed using "min", "max", "mean",
#' "median" or "first" functions. Users need to provide an
#'  time interval which is used by the composition function.
#'  For now, only Sentinel-2 L2A AWS cube can be composed.
#'  }
#'  \item{"SATVEG": }{ The SATVEG service is run by Embrapa Agricultural
#'  Informatics Centre provides access to time series from the MODIS sensor.
#'  There are three types of time series: "terra" (from the TERRA satellite),
#'  "aqua" (from the AQUA satellite) and "comb" (combination of both satellites)
#'  }
#'  \item{"STACK": }{Defines a cube to retrieve data from a set of image files.
#'              All image files should have the same spatial resolution
#'              and same projection. Each file contains a single image band
#'              for a single date; its name must have date and band information.
#'              Timeline and the bands are deduced from filenames. For
#'              example: "CBERS-4_AWFI_B13_2018-02-02.tif" is a valid name.
#'              The user has to provide parsing information toallows SITS
#'              to extract the band and the date. In the example above,
#'              the parsing info is c("X1", "X2", "band", "date) and the
#'              delimiter is "_".
#'  }
#'  \item{"PROBS": }{Defines a cube to retrieve data from a set of image files
#'              that have been classified.
#'              All image files should have the same spatial resolution
#'              and same projection. Each probs image
#'              has to be organised as a raster brick, and the
#'              number of layers must match the number of labels.
#'              All input files must have the same spatial resolution and
#'              share the same timeline (in order).
#'              The timeline for the cube must be provided.}
#'
#' }
#'
#' @note For now, we only support the collections 'ga_s2_gm' and 's2_l2a' in the
#' Digital Earth Africa repository.
#'
#' @param type             Type of cube (one of "SATVEG", "STACK",
#'                          "BDC", "S2_L2A_AWS", "DEAFRICA", GDALCUBES", "PROBS")
#' @param name              Name of the output data cube.
#' @param ...               Other parameters to be passed for specific types
#' @param url               URL for the STAC endpoint of the repository
#' @param collection        Collection to be searched in the repository
#' @param tiles             Tiles from the repository to be included in the data cube
#' @param satellite         Satellite that produced the images.
#' @param sensor            Sensor that produced the images.
#' @param bands             Bands to be included
#' @param roi               Region of interest. Either as an \code{sfc} or \code{sf}
#'  object from sf package, a GeoJSON geometry (RFC 7946), or a named \code{vector}
#'  ("xmin", "ymin", "xmax", "ymax") with values in WGS 84 . This parameter does
#'  not crop a region, but only selects the images that intersect with it.
#' @param start_date        Initial date for the cube (optional).
#' @param end_date          Final date for the cube  (optional)
#' @param s2_resolution     Resolution of S2 images ("10m", "20m" or "60m") used to build cubes
#' @param uneven_cube       A cube whose spacing of observation times is not constant
#'                          and will be regularized by the "gdalcubes" packges
#' @param path_images       Directory where the regularized images will be
#'                          written by \code{gdalcubes}.
#' @param path_db           Path and name where the \code{gdalcubes}
#'                          database will be create. E.g. "my/path/gdalcubes.db"
#' @param period            ISO8601 time period for regular data cubes
#'                          produced by \code{gdalcubes},
#'                          with number and unit, e.g., "P16D" for 16 days.
#'                          Use "D", "M" and "Y" for days, month and year..
#' @param agg_method        Method that will be applied by \code{gdalcubes}
#'                          for aggregation. Options: "min", "max", "mean",
#'                          "median" and "first".
#' @param resampling        Method to be used by \code{gdalcubes}
#'                          for resampling in mosaic operation.
#'                          Options: "near", "bilinear", "bicubic"
#'                          or others supported by gdalwarp
#'                          (see https://gdal.org/programs/gdalwarp.html).
#' @param cloud_mask        Use cloud band for aggregation by \code{gdalcubes}? (TRUE/FALSE)
#' @param data_dir          directory where local data is located
#'                          (used for creating data cubes from local files)
#' @param delim             delimiter for parsing files without STAC information
#'                          (used for creating data cubes from local files)
#' @param parse_info        parsing information for files without STAC information
#'                          (used for creating data cubes from local files)
#' @param probs_files       File names (used for creating a cube from probabilities)
#' @param probs_labels      Labels associated to a probabilities cube
#'
#' @return            The description of a data cube
#'
#' @examples
#' \dontrun{
#' # Create a data cube based on the SATVEG service
#' cube_satveg <- sits_cube(
#'     type = "SATVEG",
#'     name = "terra"
#' )
#'
#' # --- Access to the Brazil Data Cube
#' # Provide your BDC credentials as environment variables
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
#'
#' # --- Access to Digital Earth Africa
#' # Provide your AWS credentials here
#' # Sys.setenv(
#' # "AWS_ACCESS_KEY_ID"     = <your_access_key>,
#' # "AWS_SECRET_ACCESS_KEY" = <your_secret_access_key>,
#' # "AWS_DEFAULT_REGION"    = <your AWS region>,
#' # "AWS_ENDPOINT" = "sentinel-s2-l2a.s3.amazonaws.com",
#' # "AWS_REQUEST_PAYER"     = "requester"
#' # )
#'
#' # create a raster cube file based on the information about the files
#' cube_dea <- sits::sits_cube(type = "DEAFRICA",
#'                            name = "deafrica_cube",
#'                            collection = "s2_l2a",
#'                            bands = c("B04", "B08"),
#'                            roi = c("xmin" = 17.379,
#'                                   "ymin" = 1.1573,
#'                                   "xmax" = 17.410,
#'                                   "ymax" = 1.1910),
#'                            start_date = "2019-01-01",
#'                            end_date = "2019-10-28")
#'
#' # --- Access to Sentinel 2/2A level 2 data in AWS
#'
#' # Provide your AWS credentials
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
#'     tiles = c("20LKP","20LLP"),
#'     s2_aws_resolution = "20m",
#'     start_date = as.Date("2018-07-18"),
#'     end_date = as.Date("2018-07-23")
#' )
#'
#' # --- Using gdalcubes to regularize images
#'
#' # Build an data cube using AWS images
#' s2_cube <- sits_cube(
#'     type = "S2_L2A_AWS",
#'     name = "T20LKP_2018_2019",
#'     satellite = "SENTINEL-2",
#'     sensor = "MSI",
#'     tiles = "20LKP",
#'     s2_aws_resolution = "20m",
#'     start_date = as.Date("2018-08-12"),
#'     end_date = as.Date("2019-07-28")
#' )
#' # Build a data cube of equal intervals using the "gdalcubes" package
#' gc_cube <- sits_cube(type          = "GDALCUBES",
#'                      name          = "T20LKP_2018_2019_1M",
#'                      uneven_cube   = s2_cube,
#'                      path_db       = "/my/path/cube.db",
#'                      path_images   = "/my/path/images/",
#'                      period        = "P1M",
#'                      agg_method    = "median",
#'                      resampling    = "bilinear")
#' }
#'
#' # --- Create a cube based on a stack of CBERS data
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
#'
#' # Create a raster cube based on files with probability information
#' # inform the files that make up a raster probs brick with 23 time instances
#' probs_file <- c(system.file("extdata/raster/probs/sinop-2014_probs_2013_9_2014_8_v1.tif",
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
#'     start_date = as.Date("2013-09-14"),
#'     end_date = as.Date("2014-08-29"),
#'     probs_labels = labels,
#'     probs_files = probs_file
#' )
#' @export
sits_cube <- function(type, name, ...) {

    spec_class <- .sits_config_cube_class(type)
    class(type) <- c(spec_class, class(type))
    # Dispatch
    UseMethod("sits_cube", type)
}

#' @title Defines a data cube for the SATVEG service
#' @rdname sits_cube
#' @export
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

#' @title Defines a cube from a set of single image files
#' @rdname sits_cube
#'
#' @export
sits_cube.stack_cube <- function(type = "STACK",
                        ...,
                        name = "stack_cube",
                        satellite,
                        sensor,
                        bands = NULL,
                        start_date = NULL,
                        end_date = NULL,
                        data_dir,
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

#' @title Defines a data cube for a BDC STAC
#' @rdname sits_cube
#'
#' @export
sits_cube.bdc_cube <- function(type = "BDC",
                               ...,
                               name = "bdc_cube",
                               url = NULL,
                               collection,
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
        return(cube_t)
    })
    cube <- dplyr::bind_rows(tiles)

    # include access key information in file
    cube <- .sits_bdc_access_info(cube, access_key)

    class(cube) <- c("stack_cube", "raster_cube", class(cube))
    return(cube)
}
#' @title Defines a data cube for Digital Earth Africa STAC
#' @rdname sits_cube
#' @export
sits_cube.deafrica_cube <- function(type = "DEAFRICA",
                                    ...,
                                    name = "deafrica_cube",
                                    url = NULL,
                                    collection,
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
    # DE Africa runs on AWS
    # precondition - is AWS access available?
    aws_access_ok <- .sits_aws_check_access(type = type)
    if (!aws_access_ok)
        return(NULL)

    # precondition - is the url correct?
    if (purrr::is_null(url)) {
        url <- .sits_config_deafrica_stac()
    }

    # test if DEA is accessible
    assertthat::assert_that(RCurl::url.exists(url),
                            msg = "DEAfrica is not accessible"
    )

    # precondition - is the collection name valid?
    assertthat::assert_that(!purrr::is_null(collection),
                            msg = paste("sits_cube: DEAfrica collection must",
                                        "be provided")
    )

    assertthat::assert_that(!(length(collection) > 1),
                            msg = paste("sits_cube: for STAC_DEAFRICA one",
                                        "collection should be specified")
    )

    # retrieve item information
    items_info <- .sits_deafrica_items(
        url = url,
        collection = collection,
        tiles = tiles,
        roi = roi,
        start_date = start_date,
        end_date  = end_date,
        bands = bands,
        ...
    )

    # creating a group of items per tile
    items_group <- .sits_stac_group(items_info,
                                    fields = c("properties", "odc:region_code")
    )

    tiles <- purrr::map(items_group, function(items) {

        # retrieve the information from STAC
        stack <- .sits_stac_items_info(items, items$bands)

        # add the information for each tile
        cube_t <- .sits_deafrica_tile_cube(
            url = url,
            name = name,
            items = items,
            cube = collection,
            file_info = stack
        )
        return(cube_t)
    })
    # join the tiles
    cube <- dplyr::bind_rows(tiles)
    class(cube) <- c("stack_cube", "raster_cube", class(cube))

    return(cube)
}
#' @title Defines a data cube for a Sentinel-2 L2A AWS cube
#' @rdname sits_cube
#' @export
#'
sits_cube.s2_l2a_aws_cube <- function(type = "S2_L2A_AWS",
                                      ...,
                                      name = NULL,
                                      url = NULL,
                                      collection = NULL,
                                      tiles = NULL,
                                      bands = NULL,
                                      s2_resolution = NULL,
                                      roi = NULL,
                                      start_date = NULL,
                                      end_date = NULL) {

  # require package
  if (!requireNamespace("rstac", quietly = TRUE)) {
    stop(paste("Please install package rstac from CRAN:",
               "install.packages('rstac')"), call. = FALSE
    )
  }
  # precondition - is AWS access available?
  aws_access_ok <- .sits_aws_check_access(type = type)
  if (!aws_access_ok)
      return(NULL)

  # precondition - is the url correct?
  if (purrr::is_null(url)) {
      url <- .sits_config_aws_stac()
  }

  # test if AWS STAC is accessible
  assertthat::assert_that(RCurl::url.exists(url),
                          msg = "AWS STAC is not accessible"
  )

  # precondition - is the collection name valid?
  assertthat::assert_that(!purrr::is_null(collection),
            msg = "sits_cube: AWS STAC collection must be provided"
  )
  assertthat::assert_that(!(length(collection) > 1),
            msg = paste("sits_cube: for AWS STAC one only collection should",
                        "be specified")
  )

  # select bands by resolution
  bands <- .sits_aws_check_bands(bands, s2_resolution)

  # retrieve item information
  items_info <- .sits_aws_items(
    url = url,
    collection = collection,
    tiles = tiles,
    roi = roi,
    start_date = start_date,
    end_date  = end_date,
    bands = bands,
    ...
  )

  # creating a group of items per tile
  items_group <- .sits_stac_group(items_info,
                                  fields = c("properties", "tile")
  )

  tiles <- purrr::map(items_group, function(items) {

    # retrieve the information from STAC
    stack <- .sits_stac_items_info(items, items$bands)

    # add the information for each tile
    cube_t <- .sits_aws_tile_cube(
      url = url,
      name = name,
      items = items,
      cube = collection,
      resolution = s2_resolution,
      file_info = stack
    )

    class(cube_t) <- c("stack_cube", "raster_cube", class(cube_t))
    return(cube_t)
  })
  cube <- dplyr::bind_rows(tiles)

  return(cube)
}
#' @title Creates a regularized data cube from an irregular one
#' @rdname sits_cube
#' @export
sits_cube.gdalcubes_cube <- function(type = "GDALCUBES",
                                     ...,
                                     uneven_cube,
                                     name,
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
    assertthat::assert_that("stack_cube" %in% class(uneven_cube),
                            msg = paste("The provided cube is invalid,",
                                        "please provide a 'stack_cube' object.",
                                        "See '?sits_cube' for more information.")
    )

    # in case of null path a temporary directory is generated
    if (is.null(path_db))
        path_db <- file.path(tempdir(), "cube.db")

    # create an image collection
    img_col <- .sits_gc_database(uneven_cube, path_db)

    # create a list of cube view object
    cv_list <- .sits_gc_cube(uneven_cube, period, agg_method, resampling)

    # create of the aggregate cubes
    gc_cube <- .sits_gc_compose(uneven_cube, name, cv_list, img_col,
                                path_db, path_images, cloud_mask)

    class(gc_cube) <- c("stack_cube", "raster_cube", class(gc_cube))

    return(gc_cube)
}
#' @title Defines a cube from a classified image
#' @rdname sits_cube
#' @export
sits_cube.probs_cube <- function(type = "PROBS",
                                 ...,
                                 name = "probs_cube",
                                 satellite,
                                 sensor,
                                 start_date,
                                 end_date,
                                 probs_labels,
                                 probs_files) {


    # iterate through the input files
    tiles <- purrr::map(seq_along(probs_files), function(i) {
        # precondition - check if labels match files
        # read the information from the files using GDAL
        rg_obj <- suppressWarnings(rgdal::GDALinfo(probs_files[[i]]))
        n_layers <- as.numeric(rg_obj["bands"])
        assertthat::assert_that(n_layers == length(probs_labels),
             msg = paste0("mismatch btw labels and bands in file ", probs_files[[i]]))

        # get the file params
        params <- .sits_raster_api_params_file(probs_files[[i]])
        # build the file information
        file_info <- tibble::tibble(
            band = "probs",
            start_date = as.Date(start_date),
            end_date = as.Date(end_date),
            path = probs_files[[i]]
        )
        # go tile by tile
        tile <- tibble::tibble(
            type = "PROBS",
            satellite = satellite,
            sensor = sensor,
            name = name,
            bands = list("probs"),
            labels = list(labels),
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
        return(tile)
    })
    probs_cube <- dplyr::bind_rows(tiles)

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
#' @title Backward compatibility
#' @name sits_cube.raster_cube
#' @description Using "RASTER" as a type for a data cube is deprecated in favour
#'              of "STACK" type.
#' @param type              Type of cube
#' @param ...               Other parameters to be passed for specific types
#' @param name              Name of the input data
#' @return                  A message
#'
#' @export
sits_cube.raster_cube <- function(type = "RASTER",  name = NULL, ...) {
  message("type RASTER is deprecated, please use  STACK")
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
