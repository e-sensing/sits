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
#'              include information on tile, band and date.
#'              The timeline and the bands are deduced from this information.
#'              Examples of valid image names include
#'              "CB4_64_16D_STK_022024_2018-08-29_2018-09-13_EVI.tif" and
#'              "cube_20LKP_B02_2018-07-18.jp2". In each case, the user has to provide
#'              appropriate parsing information that allows SITS to extract
#'              the tile, the band and the date. In the examples above, the parsing info
#'              would include "_" as a delimiter. In the first, the names of the
#'              columns for parsing are "X1, X2, X3, X4, tile, date, X7, band".
#'              In the second, they are "tile, band, date".
#'
.sits_raster_stack_info <- function(satellite,
                                    sensor,
                                    data_dir,
                                    parse_info,
                                    delim,
                                    bands,
                                    start_date,
                                    end_date) {

    file_info <- purrr::map(data_dir, function(data_dir_row) {

        # how many of those files are images?
        # retrieve the known file extensions
        file_ext <- .config_local_file_extensions()

        # list the files in the data directory
        img_files <- list.files(
            path = data_dir_row,
            pattern = paste0("\\.(",
                             paste0(.config_local_file_extensions(), collapse = "|"),
                             ")$")
        )

        # remove the extension
        img_files_noext <- tools::file_path_sans_ext(img_files)

        # split the file names
        img_files_lst <- strsplit(img_files_noext, split = delim)

        # bind rows
        img_files_mx <- do.call(rbind, img_files_lst)

        # read the image files into a tibble with added parse info
        colnames(img_files_mx) <- parse_info

        # joint the list into a tibble and convert bands name to upper case
        img_files_tb <- tibble::as_tibble(
            img_files_mx,
            .name_repair = "minimal"
        ) %>% dplyr::mutate(band = toupper(band))

        # get the information on the required bands, dates and path
        file_info <- img_files_tb %>%
            # select the relevant parts
            dplyr::select(tile, date, band) %>%
            # check the date format
            .sits_timeline_date_format() %>%
            # include path in the tibble
            dplyr::mutate(path = paste0(data_dir_row, "/", img_files)) %>%
            # filter to remove duplicate combinations of file and band
            dplyr::distinct(tile, band, date, .keep_all = TRUE) %>%
            # order by dates
            dplyr::arrange(date)

        # extract the band names
        bands_files <- unique(file_info$band)

        # create collection name
        collection <- paste0(satellite, "/", sensor)

        # convert the band names to SITS bands
        file_info <- dplyr::mutate(
            file_info,
            band = .source_bands_to_sits(source = "LOCAL",
                                         collection = collection,
                                         bands = band))

        # filter bands
        if (!purrr::is_null(bands)) {

            # get the bands of the cube
            bands_info <- dplyr::pull(dplyr::distinct(file_info, band))

            # verify that the requested bands exist
            assertthat::assert_that(
                all(bands %in% bands_info),
                msg = paste(".sits_raster_stack_info: requested bands not",
                            "available in cube")
            )

            # select the requested bands
            file_info <- dplyr::filter(file_info, band %in% bands)
        }

        # filter start and end dates
        if (!purrr::is_null(start_date) & !purrr::is_null(end_date))
            file_info <- dplyr::filter(file_info,
                                       date >= start_date & date <= end_date)

        params <- .raster_params_file(file_info$path[1])
        resolution <- params$xres
        file_info <- dplyr::mutate(file_info,
                                   res = resolution, .before = path)

        # post condition
        assertthat::assert_that(
            nrow(file_info) > 0,
            msg = paste(".sits_raster_stack_info: no file was found for the",
                        "requested local cube. Please, verify the 'start_date' and",
                        "'end_date' and check if the provided directory is valid.")
        )

        file_info
    })

    return(dplyr::bind_rows(file_info))
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

    # get the tiles
    tiles <- unique(file_info$tile)

    collection <- paste0(satellite, "/", sensor)

    rows <- purrr::map(tiles, function(t){
        # get the files for a given tile
        row_file_info <- dplyr::filter(file_info, tile == t)

        # get the bands
        bands <- unique(row_file_info$band)
        # get the parameters from the raster object of one of the layers
        # the assumptions is that all layers are consistent
        params <- .raster_params_file(row_file_info$path[1])

        # create a tibble to store the metadata
        row <- .sits_cube_create(
            name = name,
            source = "LOCAL",
            satellite = satellite,
            sensor = sensor,
            collection = collection,
            tile = t,
            bands = bands,
            nrows = params$nrows,
            ncols = params$ncols,
            xmin = params$xmin,
            xmax = params$xmax,
            ymin = params$ymin,
            ymax = params$ymax,
            xres = params$xres,
            yres = params$yres,
            crs = params$crs,
            file_info = row_file_info
        )
    })
    stack_cube <- dplyr::bind_rows(rows)

    return(stack_cube)
}
