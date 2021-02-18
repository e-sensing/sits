#' @title Obtain the information about files that make up a stack cube
#' @name .sits_raster_stack_info
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param satellite         name of satellite
#' @param sensor            name of sensor
#' @param data_dir          directory where data is located
#' @param parse_info        parsing information
#' @param delim             delimiter
#' @param bands             bands to be used (optional)
#' @param start_date        starting date of the cube (optional)
#' @param end_date          ending date of the cube (optional)
#'
#' @description All image files should have the same spatial resolution
#'              and same projection. In addition, image file names should
#'              include information on band and date.
#'              The timeline and the bands are deduced from this information.
#'              Examples of valid image names include
#'              "CB4_64_16D_STK_022024_2018-08-29_2018-09-13_EVI.tif" and
#'              "B02_2018-07-18.jp2". In each case, the user has to provide
#'              appropriate parsing information that allows SITS to extract
#'              the band and the date. In the examples above, the parsing info
#'              would include "_" as a delimiter. In the first, the names of the
#'              columns for parsing are "X1, X2, X3, X4, X5, date, X7, band".
#'              In the second, they are "band, date".
#'
#' @export
#'
.sits_raster_stack_info <- function(satellite,
                                    sensor,
                                    data_dir,
                                    parse_info,
                                    delim,
                                    bands,
                                    start_date,
                                    end_date) {

    # list the files in the data directory
    img_files <- list.files(data_dir)

    # how many of those files are images?
    # retrieve the known file extensions
    file_ext <- .sits_config_img_file_ext()
    # filter by extension
    matches <- purrr::map(file_ext, function(ext) {
          img_files[grepl(ext, img_files)]
      })
    # get only valid files
    img_files <- unlist(matches)

    # remove the extension
    img_files_noext <- tools::file_path_sans_ext(img_files)
    # split the file names
    img_files_lst <- strsplit(img_files_noext, split = "_")
    # joint the list into a tibble
    img_files_tb <- suppressWarnings(
      tibble::as_tibble(
        do.call(rbind, img_files_lst)
        )
      )
    # read the image files into a tibble with added parse info
    colnames(img_files_tb) <- parse_info

    # get the information on the required bands, dates and path
    info_tb <- img_files_tb %>%
        # select the relevant parts
        dplyr::select(date, band) %>%
        # check the date format
        .sits_timeline_date_format() %>%
        # include path in the tibble
        dplyr::mutate(path = paste0(data_dir, "/", img_files)) %>%
        # order by dates
        dplyr::arrange(date) %>%
        # filter to remove duplicate combinations of file and band
        dplyr::distinct(band, date, .keep_all = TRUE)

    # extract the band names
    bands_files <- dplyr::pull(dplyr::distinct(info_tb, band))

    # convert the names of the bands to those used by SITS
    bands_sits <- .sits_config_bands_convert(satellite, sensor, bands_files)

    # convert the band names to SITS bands
    info_tb <- dplyr::mutate(info_tb, band = bands_sits[band])

    # filter bands
    if (!purrr::is_null(bands)) {
        # get the bands of the cube
        bands_info <- dplyr::pull(dplyr::distinct(info_tb, band))
        # verify that the requested bands exist
        assertthat::assert_that(all(bands %in% bands_info),
                    msg = "requested bands not available in cube")
        # select the requested bands
        info_tb <- dplyr::filter(info_tb, band %in% bands)
    }
    # filter start and end dates
    if (!purrr::is_null(start_date) & !purrr::is_null(end_date)) {
        info_tb <- dplyr::filter(info_tb,
                                 date >= start_date & date <= end_date)

    }

    return(info_tb)
}
#' @title Create a stack cube from a set of files
#' @name .sits_raster_stack_cube
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param satellite         Name of satellite
#' @param sensor            Name of sensor
#' @param name              Name of the output data cube
#' @param file_info         File information
#'
.sits_raster_stack_cube <- function(satellite, sensor, name, file_info) {

    # get the bands
    bands <- unique(file_info$band)
    # get the timeline
    timeline <- unique(file_info$date)
    # get the parameters from the raster object of one of the layers
    # the assumptions is that all layers are consistent
    params <- .sits_raster_api_params_file(file_info$path[1])

    # create a tibble to store the metadata
    stack_cube <- .sits_cube_create(
        type = "STACK",
        URL = NA,
        satellite = satellite,
        sensor = sensor,
        name = name,
        bands = bands,
        scale_factors = .sits_config_scale_factors(sensor, bands),
        missing_values = .sits_config_missing_values(sensor, bands),
        minimum_values = .sits_config_minimum_values(sensor, bands),
        maximum_values = .sits_config_maximum_values(sensor, bands),
        timelines = list(timeline),
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


    return(stack_cube)
}
