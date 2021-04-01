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
#' that information to retrive and process data.
#'
#' Currently, users can create data cube from the following sources:
#' \itemize{
#'  \item{"BDC": }{Brazil Data Cube (BDC), see also http://brazildatacube.org/}
#'  \item{"DEAFRICA": }{Digital Earth Africa, see also
#'  https://www.digitalearthafrica.org/}
#'  \item{"AWS": }{Amazon Web Services (AWS)}
#'  \item{"STACK": }{Defines a cube from on a set of local files.}
#'  \item{"PROBS": }{Defines a cube to from a set of classified image files}.
#'  \item{"SATVEG": }{Defines a cube to use the SATVEG web service.}
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
#' # "AWS_ENDPOINT" = "sentinel-s2-l2a.s3.amazonaws.com",
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
#' variables.
#' # Sys.setenv(
#' # "BDC_ACCESS_KEY" = <your_bdc_access_key>
#' # )
#'
#'@note To create a cube from local files, all image files should have
#' the same spatial resolution, spatio-temporal extent and projection.
#' Each file should contain a single image band for a single date.
#' File names must include date and band information, since times and bands
#' are deduced from filenames. For example: "CBERS-4_AWFI_B13_2018-02-02.tif"
#' and "S2A_MSI_L2A_20150302_B03_10m.jp2" are accepted names.
#' The user has to provide parsing information to allow `sits`
#' to extract the band and the date. In the first example above,
#' the parsing info is c("X1", "X2", "band", "date) and the delimiter is "_".
#'
#' @note The SATVEG service is run by Embrapa Agricultural
#'  Informatics Centre provides access to time series from the MODIS sensor.
#'  There are three collections: "terra" (from the TERRA satellite),
#'  "aqua" (from the AQUA satellite) and "comb" (combination of
#'  both satellites).
#'
#'
#' @param source            Data source (one of "SATVEG", "LOCAL",
#'                          "BDC", "AWS", "DEAFRICA", "PROBS").
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
#' @details  The "bbox" parameters allows a selection of an area of interest.
#' Either using a named \code{vector} ("xmin", "ymin", "xmax", "ymax") with
#' values in WGS 84, a \code{sfc} or \code{sf} object from sf package, or a
#' GeoJSON geometry (RFC 7946). Note that this parameter does not crop a
#' region, but only selects the images that intersect with it.
#'
#' @return                  The description of a data cube
#'
#' @examples
#' \dontrun{
#' # Create a data cube based on the SATVEG service
#' cube_satveg <- sits_cube(
#'     source = "SATVEG",
#'     collection = "terra"
#' )
#'
#' # --- Access to the Brazil Data Cube
#' # Provide your BDC credentials as environment variables
#' # Sys.setenv(
#' # "BDC_ACCESS_KEY" = <your_bdc_access_key>
#' # )
#'
#' # create a raster cube file based on the information in the BDC



#'
#' # --- Access to Digital Earth Africa

#'
#' # create a raster cube file based on the information about the files
#' cube_dea <- sits_cube(source = "DEAFRICA",
#'                       name = "deafrica_cube",
#'                       collection = "s2_l2a",
#'                       bands = c("B04", "B08"),
#'                       roi = c("xmin" = 17.379,
#'                               "ymin" = 1.1573,
#'                               "xmax" = 17.410,
#'                                "ymax" = 1.1910),
#'                       start_date = "2019-01-01",
#'                       end_date = "2019-10-28")
#'
#' # --- Access to Sentinel 2/2A level 2 data in AWS
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
#'     parse_info = c("X1", "X2", "band", "date")
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

    spec_class <- .sits_config_cube_class(source)
    class(source) <- c(spec_class, class(source))
    # Dispatch
    UseMethod("sits_cube", source)
}

#' @rdname sits_cube
#'
#' @export
#'
sits_cube.bdc_cube <- function(source = "BDC", ...,
                               name = "bdc_cube",
                               url = NULL,
                               collection,
                               bands = NULL,
                               tiles = NULL,
                               bbox = NULL,
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
    assertthat::assert_that(
        .sits_config_bdc_stac_access(url),
        msg = "sits_cube: BDC is not accessible"
    )
    # precondition - is the collection name valid?
    assertthat::assert_that(
        !purrr::is_null(collection),
        msg = "sits_cube: BDC collection must be provided"
    )

    assertthat::assert_that(
        length(collection) == 1,
        msg = "sits_cube: only one BDC collection should be specified"
    )

    # Try to find the access key as an environment variable
    bdc_access_key <- Sys.getenv("BDC_ACCESS_KEY")
    assertthat::assert_that(
        nchar(bdc_access_key) != 0,
        msg = "sits_cube: BDC_ACCESS_KEY needs to be provided"
    )

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
        roi = bbox,
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
            name = name,
            collection = collection,
            collection_info = collection_info,
            items = items,
            file_info = stack
        )
        return(cube_t)
    })
    cube <- dplyr::bind_rows(tiles)

    # include access key information in file
    cube <- .sits_bdc_access_info(cube, bdc_access_key)

    class(cube) <- c("raster_cube", class(cube))
    return(cube)
}

#' @rdname sits_cube
#'
#' @export
#'
sits_cube.deafrica_cube <- function(source = "DEAFRICA", ...,
                                    name = "deafrica_cube",
                                    url = NULL,
                                    collection = "s2_l2a",
                                    bands = NULL,
                                    tiles = NULL,
                                    bbox = NULL,
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
    aws_access_ok <- .sits_aws_check_access(source)
    if (!aws_access_ok)
        return(NULL)

    # precondition - is the url correct?
    if (purrr::is_null(url)) {
        url <- .sits_config_deafrica_stac()
    }

    # test if DEA is accessible
    assertthat::assert_that(
        RCurl::url.exists(url),
        msg = "DEAfrica is not accessible"
    )

    # precondition - is the collection name valid?
    assertthat::assert_that(
        !purrr::is_null(collection),
        msg = paste("sits_cube: DEAfrica collection must",
                    "be provided")
    )

    assertthat::assert_that(
        !(length(collection) > 1),
        msg = paste("sits_cube: for STAC_DEAFRICA one",
                    "collection should be specified")
    )

    # retrieve item information
    items_info <- .sits_deafrica_items(
        url = url,
        collection = collection,
        tiles = tiles,
        roi = bbox,
        start_date = start_date,
        end_date  = end_date,
        bands = bands,
        ...
    )

    # creating a group of items per tile
    items_group <- .sits_stac_group(
        items_info,
        fields = c("properties", "odc:region_code")
    )

    tiles <- purrr::map(items_group, function(items) {

        # retrieve the information from STAC
        stack <- .sits_stac_items_info(items, items$bands)

        # add the information for each tile
        cube_t <- .sits_deafrica_tile_cube(
            name       = name,
            items      = items,
            collection = collection,
            file_info  = stack
        )
        return(cube_t)
    })

    # join the tiles
    cube <- dplyr::bind_rows(tiles)
    class(cube) <- c("raster_cube", class(cube))

    return(cube)
}

#' @rdname sits_cube
#'
#' @export
#'
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

    # require package
    if (!requireNamespace("rstac", quietly = TRUE)) {
        stop(paste("Please install package rstac from CRAN:",
                   "install.packages('rstac')"), call. = FALSE
        )
    }

    # precondition - is AWS access available?
    aws_access_ok <- .sits_aws_check_access(source)
    if (!aws_access_ok)
        return(NULL)

    # precondition - is the url correct?
    if (purrr::is_null(url)) {
        url <- .sits_config_aws_stac()
    }

    # test if AWS STAC is accessible
    assertthat::assert_that(
        RCurl::url.exists(url),
        msg = "sits_cube: AWS STAC is not accessible"
    )

    # precondition - is the collection name valid?
    assertthat::assert_that(
        collection == "sentinel-s2-l2a",
        msg = "sits_cube: AWS supports only sentinel-s2-l2a collection"
    )

    assertthat::assert_that(
        s2_resolution %in% c(10, 20, 60),
        msg = "sits_cube: s2_resolution should be one of c(10, 20, 60)")

    # select bands by resolution
    bands <- .sits_s2_check_bands(bands, s2_resolution)

    # retrieve item information
    items_info <- .sits_s2_aws_items(
        url = url,
        collection = collection,
        tiles = tiles,
        roi = bbox,
        start_date = start_date,
        end_date  = end_date,
        bands = bands,
        ...
    )

    # creating a group of items per tile
    items_group <- .sits_stac_group(items_info,
                                    fields = c("properties", "tile"))

    tiles <- purrr::map(items_group, function(items) {

        # retrieve the information from STAC
        stack <- .sits_stac_items_info(items, items$bands)

        # add the information for each tile
        cube_t <- .sits_s2_aws_tile_cube(
            name = name,
            items = items,
            collection = collection,
            resolution = s2_resolution,
            file_info = stack
        )

        class(cube_t) <- c("raster_cube", class(cube_t))
        return(cube_t)
    })
    cube <- dplyr::bind_rows(tiles)

    return(cube)
}

#' @rdname sits_cube
#'
#' @export
#'
sits_cube.local_cube <- function(source = "LOCAL", ...,
                                 name   = "local_cube",
                                 satellite,
                                 sensor,
                                 bands = NULL,
                                 start_date = NULL,
                                 end_date = NULL,
                                 data_dir,
                                 parse_info,
                                 delim = "_") {

    # precondition - check satellite and sensor
    .sits_config_satellite_sensor(satellite, sensor)

    # precondition - data directory must be provided
    assertthat::assert_that(
        !purrr::is_null(data_dir),
        msg = "sits_cube: data_dir must be to be provided"
    )

    # precondition - check parse info
    assertthat::assert_that(
        length(parse_info) >= 2,
        msg = "sits_cube: invalid parsing information"
    )

    # precondition - does the parse info have band and date?
    assertthat::assert_that(
        all(c("band", "date") %in% parse_info),
        msg = "sits_cube: invalid columns for date and band"
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

    class(cube) <- c("raster_cube", class(cube))
    return(cube)
}

#' @rdname sits_cube
#'
#' @export
#'
sits_cube.probs_cube <- function(source = "PROBS", ...,
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
        assertthat::assert_that(
            n_layers == length(probs_labels),
            msg = paste("sits_cube: mismatch between labels and bands in file",
                        probs_files[[i]]))

        # get the file params
        params <- .sits_raster_api_params_file(probs_files[[i]])

        # build the file information
        file_info <- tibble::tibble(
            band = "probs",
            start_date = as.Date(start_date),
            end_date = as.Date(end_date),
            res = params$xres,
            path = probs_files[[i]]
        )

        # go tile by tile
        tile <- tibble::tibble(
            source = "PROBS",
            satellite = satellite,
            sensor = sensor,
            name = name,
            bands = list("probs"),
            labels = list(probs_labels),
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

#' @rdname sits_cube
#'
#' @export
#'
sits_cube.satveg_cube <- function(source = "SATVEG", ...,
                                  collection = "terra") {
    # Pre-condition - check if SATVEG is working
    satveg_ok <- .sits_satveg_check()
    # if OK, go ahead a create a SATVEG cube
    if (satveg_ok) {
        cube <- .sits_satveg_cube(collection)
    } else {
        message("SATVEG service not responding")
        return(NULL)
    }

    return(cube)
}

#' @export
#'
sits_cube.default <- function(source = NULL, ...) {
    stop("sits_cube: cube source unknown")
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
#'     parse_info = c("X1", "X2", "band", "date")
#' )
#'
#' cbers_022024_copy <- sits_cube_copy(cbers_022024,
#'     name = "cb_022024_cp",
#'     dest_dir = tempdir()
#' )
#' }
#'
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
