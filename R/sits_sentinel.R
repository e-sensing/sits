
#' @title Get information on S2 level 2A tiles in AWS
#' @name .sits_s2_l2a_aws_info_tiles
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param tile          tile
#' @param bands         bands to be retrieved
#' @param resolution    resolution of S2 cube
#' @param start_date    start_date of the cube
#' @param end_date      end date of the cube
#'
#' @return   tibble with information on s2 tile
#'
#'
.sits_s2_l2a_aws_info_tiles <- function(tile, bands, resolution,
                                          start_date, end_date) {

    # pre-conditions
    # for S2, tile must be given
    assertthat::assert_that(!purrr::is_null(tile),
        msg = "For S2 L2A in AWS, tile must be provided"
    )

    # must have 5 chars and be of the form NNAAA
    # where NN is betwen 10 and 60 and AAA is all caps
    assertthat::assert_that(nchar(tile) == 5,
        msg = "For S2 L2A in AWS, invalid tile"
    )
    num_index <- as.numeric(substr(tile, 1, 2))
    assertthat::assert_that(num_index >= 10 & num_index <= 60,
        msg = "For S2 L2A in AWS, invalid tile"
    )
    assertthat::assert_that(grepl("^[A-Z]+$", substr(tile, 3, 5)),
        msg = "For S2 L2A in AWS, invalid tile"
    )

    # precondition - resolution
    assertthat::assert_that(resolution %in% .sits_config_s2_aws_res(),
        msg = "For S2 in AWS, invalid resolution"
    )
    # precondition - dates
    assertthat::assert_that(lubridate::is.Date(start_date),
        msg = "start date is invalid"
    )
    assertthat::assert_that(lubridate::is.Date(end_date),
        msg = "end date is invalid"
    )

    # precondition - bands
    # find the bands available at the chosen resolution
    bands_s2 <- .sits_config_s2_bands(resolution)
    if (!purrr::is_null(bands)) {
        assertthat::assert_that(all(bands %in% bands_s2),
            msg = "requested bands not available"
        )
    }
    else {
          bands <- bands_s2
      }

    # get the name of the bucket
    bucket <- .sits_config_s2_aws_bucket()


    # include the prefix for searching the S2 bucket
    prefix <- paste0(
        "tiles/", substring(tile, 1, 2), "/",
        substring(tile, 3, 3), "/",
        substring(tile, 4, 5), "/"
    )

    # get the files and the bands for the tile
    bucket_s2a <- aws.s3::get_bucket(
        bucket = bucket,
        prefix = prefix,
        headers = list("x-amz-request-payer" = "requester"),
        max = Inf
    )

    assertthat::assert_that(length(bucket_s2a) > 0,
        msg = "empty s3 bucket"
    )
    # get the files associated to the bucket
    # filter image files (Sentinel-2 uses JPEG200)
    s2_files <- purrr::map(
        seq_len(length(bucket_s2a)),
        function(i) {
            return(bucket_s2a[i]$Contents$Key)
        }
    ) %>%
        unlist() %>%
        .[grepl("jp2", .)]

    # split the image name
    s2_files_lst <- strsplit(s2_files, split = "/")
    # joint the list into a tibble
    s2_files_tb <- suppressWarnings(
        tibble::as_tibble(
            do.call(rbind, s2_files_lst)
            )
        )

    # read the image files into a tibble with added parse info
    colnames(s2_files_tb) <- .sits_config_data_parse_info("S2_L2A_AWS")

    # get the information on the required bands, dates and path
    s2_tb <- s2_files_tb %>%
        # select the relevant parts
        dplyr::select(X5, X6, X7, X9, X10) %>%
        # rename the columns
        dplyr::rename(year = X5, month = X6, day = X7, res = X9, band = X10) %>%
        # create a date column
        dplyr::mutate(date = lubridate::make_date(year, month, day)) %>%
        # select important information on the files
        dplyr::select(res, band, date) %>%
        # include path in the tibble
        dplyr::mutate(path = s2_files) %>%
        # remove the leading "R" on the resolution
        dplyr::mutate(res = substring(res, 2, 4)) %>%
        # select bands in given resolution
        dplyr::filter(res == resolution) %>%
        # remove the ".jp2" in the band name
        dplyr::mutate(band = substring(band, 1, 3)) %>%
        # filter for the bands available in the given resolution
        dplyr::filter(band %in% bands) %>%
        # order by dates
        dplyr::arrange(date) %>%
        # select valid dates
        dplyr::filter(date >= start_date & date <= end_date) %>%
        # include /vsis3 into the path
        dplyr::mutate(path = paste0("/vsis3/", bucket, "/", path)) %>%
        # filter to remove duplicate combinations of file and band
        dplyr::distinct(band, date, .keep_all = TRUE)

    return(s2_tb)
}
#' @title Create a data cube for a Sentinel-2 AWS TILE
#' @name .sits_s2_l2a_aws_tile_cube
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description  Builds a Sentinel-2 AWS cube
#'
#' @param  name                  Name of the data cube.
#' @param  bands                 Vector of bands
#' @param  tile                  Tile
#' @param  file_info             Tibble with information about the files.
#' @return A tibble with metadata information about a raster data set.
#'
.sits_s2_l2a_aws_tile_cube <- function(name,
                                         bands,
                                         tile,
                                         file_info) {

    # set the satellite and the sensor
    satellite <- "SENTINEL-2"
    sensor <- "MSI"

    # set the labels
    labels <- c("NoClass")
    # find the bands
    s2_bands <- unique(file_info$band)
    if (purrr::is_null(bands)) {
          bands <- s2_bands
      } else {
          assertthat::assert_that(all(bands %in% s2_bands),
              msg = "mismatch btw requested bands and bands availabe in S2 AWS"
          )
      }

    # get the first image
    # obtain the parameters
    params <- .sits_raster_api_params_file(file_info$path[1])


    # create a tibble to store the metadata
    cube <- .sits_cube_create(
        type = "S2_L2A_AWS",
        satellite = satellite,
        sensor = sensor,
        name = name,
        cube = "AWS-S2-L2A",
        tile = tile,
        bands = bands,
        labels = labels,
        nrows = params$nrows,
        ncols = params$ncols,
        xmin = params$xmin,
        xmax = params$xmax,
        ymin = params$ymin,
        ymax = params$ymax,
        xres = params$xres,
        yres = params$yres,
        crs = params$crs,
        file_info = file_info
    )

    return(cube)
}
