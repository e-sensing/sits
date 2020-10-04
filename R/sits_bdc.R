#' @title Check if the BDC tiles are working
#' @name .sits_bdc_check_tiles
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param satellite     satellite
#' @param sensor        sensor
#' @param bands         bands
#' @param cube          input cube
#' @param tile          tile
#' @param version       version of the cube
#' @param data_access   type of access
#' @param start_date    start_date of the cube
#' @param end_date      end date of the cube
.sits_bdc_check_tiles <- function(satellite,
								  sensor,
								  bands,
								  cube,
								  tile,
								  version,
								  data_access,
								  start_date,
								  end_date){

	# check if the satellite and sensor are supported by SITS
	assertthat::assert_that(!purrr::is_null(satellite),
							msg = "sits_cube: for BDC_TILE satelite must be provided")
	assertthat::assert_that(!purrr::is_null(sensor),
							msg = "sits_cube: for BDC_TILE sensor must be provided")
	# Tests is satellite and sensor are known to SITS
	.sits_raster_satellite_sensor(satellite, sensor)

	# test if bands are provided
	if (!purrr::is_null(bands)){
		bands_bdc <- .sits_config_band_names(sensor, "BDC_TILE")
		bands_sits <- .sits_config_band_names(sensor, "SITS")
		assertthat::assert_that(all(bands %in% bands_bdc) | all(bands %in% bands_sits),
								msg = "band names inconsistent - use those of SITS")
	}
	# test if cube and tile are provided
	assertthat::assert_that(!purrr::is_null(cube),
							msg = "sits_cube: for BDC_TILE cube name must be provided")

	assertthat::assert_that(!purrr::is_null(tile),
							msg = "sits_cube: for BDC_TILE, the tile name must be provided")

	assertthat::assert_that(!purrr::is_null(version),
	                        msg = "sits_cube: for BDC_TILE, the version name must be provided")

	# test if data_access variable is correct
	assertthat::assert_that(data_access %in% c("local", "web"),
							msg = "sits_cube: for BDC_TILE data_access must one of (local, web)")

	# test if the dates are valid
	if (!purrr::is_null(start_date)) {
		assertthat::assert_that(lubridate::is.Date(lubridate::ymd(start_date)),
								msg = "sits_cube: start_date is not valid")
		assertthat::assert_that(lubridate::is.Date(lubridate::ymd(end_date)),
								msg = "sits_cube: end_date is not valid")
	}

	return(TRUE)
}

#' @title Get information on BDC tiles
#' @name .sits_bdc_info_tiles
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param satellite     satellite
#' @param sensor        sensor
#' @param bands         bands to be included in the cube
#' @param cube          input cube
#' @param tile          tile
#' @param version       version of the cube
#' @param data_access   type of access
#' @param start_date    start_date of the cube
#' @param end_date      end date of the cube
#' @param .local        local address (if different from default)
#' @param .web          web address (if different from default)
#' @param .cloud_band   include cloud band? (TRUE/FALSE)
.sits_bdc_info_tiles <- function(satellite,
								 sensor,
								 bands,
								 cube,
								 tile,
								 version,
								 data_access,
								 start_date,
								 end_date,
								 .local,
								 .web,
								 .cloud_band = FALSE){

	# obtain the directory for local access
	if (data_access == "local") {
		if (!purrr::is_null(.local))
			dir <- .local
		else
			dir <- .sits_config_bdc_local()
	}
	if (data_access == "web") {
		if (!purrr::is_null(.web))
			dir <-  .web
		else
			dir <- .sits_config_bdc_web()
	}
	# compose the directory with the name of the cube and tile
	data_dir <- paste0(dir,"/",cube,"/", version, "/", tile, "/")

	# compose the data directory based on standard path for cube type

	info.tb <- .sits_bdc_stack_info(satellite   = satellite,
	                                sensor      = sensor,
	                                start_date  = start_date,
	                                end_date    = end_date,
	                                bands       = bands,
	                                data_dir    = data_dir,
	                                data_access = data_access)

	return(info.tb)
}

#' @title Obtain the file information about the BDC data
#' @name .sits_bdc_stack_info
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#'
#' @param  satellite             satellite
#' @param  sensor                sensor
#' @param  start_date            starting date for the cube
#' @param  end_date              end date for the cube
#' @param  bands                 bands
#' @param  data_dir              base directory for the search
#' @param  data_access           type of access
#' @return                       tibble with metadata information
#'

.sits_bdc_stack_info <-  function(satellite,
                                  sensor,
                                  start_date,
                                  end_date,
                                  bands,
                                  data_dir,
                                  data_access) {

    # BDC uses a composite path
    # e.g. "2016-01-01_2016-01-16/CB4_64_16D_STK_v001_022024_2016-01-01_2016-01-16_BAND13.tif"

    # convert the names of the bands to those used by SITS
    bands_sits <- .sits_config_band_names_convert(satellite, sensor, type = "BDC_TILE")

    # find out the bands used by SITS in their original names
    bands_orig <- names(bands_sits)

    # list the image files
    if (data_access == "local")
        img_files_full_path <- paste0(data_dir, list.files(data_dir, recursive = TRUE))
    else
        img_files_full_path <- .sits_bdc_list_files_http(data_dir)

    assertthat::assert_that((img_files_full_path[1] != data_dir),
                            msg = "cannot access BDC files - connection error")

    # filter by BDC extension (to get only valid files)
    bdc_ext <- .sits_config_bdc_extension()
    img_files_full_path <- img_files_full_path[grepl(bdc_ext, img_files_full_path)]

    # include the parse_info
    parse_info  <- .sits_config_data_parse_info(type = "BDC_TILE")

    # get the delim information
    delim <- .sits_config_data_delim(type = "BDC_TILE")

    # get the information on the required bands, dates and path
    info.tb <- img_files_full_path %>%
        # get the basename
        basename() %>%
        # remove the extent
        tools::file_path_sans_ext() %>%
        # read the file path into a tibble
        readr::read_delim(delim = delim, col_names = parse_info) %>%
        # select the relevant parts
        dplyr::select(date, band) %>%
        # include path in the tibble
        dplyr::mutate(path = img_files_full_path) %>%
        # order by dates
        dplyr::arrange(date) %>%
        # filter to remove duplicate combinations of file and band
        dplyr::distinct(band, date, .keep_all = TRUE) %>%
        # use only the bands supported by SITS
        dplyr::filter(band %in% bands_orig) %>%
        # convert the band names to SITS bands
        dplyr::mutate(band = bands_sits[band])

    if (!purrr::is_null(start_date) & !purrr::is_null(end_date))
        # filter by starting date and end date
        info.tb <- dplyr::filter(info.tb, date >= start_date & date <=end_date)

    if (!purrr::is_null(bands)) {
        assertthat::assert_that(all(bands %in% bands_sits),
                                msg = "bands do not match band names in SITS - see config file")
        # select the bands
        info.tb <-  dplyr::filter(info.tb, band %in% bands)
    }

    return(info.tb)
}
#' @title Create a data cube for a BDC TILE
#' @name .sits_bdc_tile_cube
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description  Builds a BDC_TILE cube
#'
#' @param  satellite             Name of satellite
#' @param  sensor                Name of sensor
#' @param  name                  Name of the data cube.
#' @param  bands                 Vector of bands
#' @param  cube                  Input cube
#' @param  tile                  Tile
#' @param  file_info             Tibble with information about the files.
#' @return A tibble with metadata information about a raster data set.
#'
.sits_bdc_tile_cube <- function(satellite,
									   sensor,
									   name,
									   bands,
									   cube,
									   tile,
									   file_info){

	# obtain the timeline
	timeline <- unique(lubridate::as_date(file_info$date))

	# set the labels
	labels <- c("NoClass")

	# get the first image
	# check if the file begins with http =:// or with vsicurl/
	full_path_1 <- .sits_raster_check_webfiles(file_info[1,]$path)
	# obtain the parameters
	params <- .sits_raster_params(terra::rast(full_path_1))

	# if (purrr::is_null(bands))
	#     bands <- unique(dplyr::pull(dplyr::select(file_info$band)))

	# get the bands
	bands <- unique(file_info$band)

	# get scale factors
	scale_factors  <- .sits_config_scale_factors(sensor, bands)
	# get missing values
	missing_values <- .sits_config_missing_values(sensor, bands)
	# get minimum values
	minimum_values <- .sits_config_minimum_values(sensor, bands)
	# get maximum values
	maximum_values <- .sits_config_maximum_values(sensor, bands)

	# create a tibble to store the metadata
	cube <- .sits_cube_create(type           = "BDC_TILE",
	                          satellite      = satellite,
	                          sensor         = sensor,
	                          name           = name,
	                          cube           = cube,
	                          tile           = tile,
	                          bands          = bands,
	                          labels         = labels,
	                          scale_factors  = scale_factors,
	                          missing_values = missing_values,
	                          minimum_values = minimum_values,
	                          maximum_values = maximum_values,
	                          timelines      = list(timeline),
	                          nrows          = params$nrows,
	                          ncols          = params$ncols,
	                          xmin           = params$xmin,
	                          xmax           = params$xmax,
	                          ymin           = params$ymin,
	                          ymax           = params$ymax,
	                          xres           = params$xres,
	                          yres           = params$yres,
	                          crs            = params$crs,
	                          file_info      = file_info)

	return(cube)
}

.sits_bdc_list_files_http <- function(data_dir){

    #list the main directory
    dir_html.lst <- xml2::as_list(xml2::read_html(data_dir))

    # number of possible paths
    n_pre <- length(dir_html.lst[[1]][[2]]$pre)
    # sequence of valid paths
    idx_dir <- seq(from = 3, to = n_pre - 1, by = 2)

    # obtain the list of dates
    dates.lst <- purrr::map(idx_dir, function(s){
        d <- dir_html.lst[[1]][[2]]$pre[[s]][[1]]
        return(d)
    })
    # given the list of dates, get list of images
    img_files.lst <- purrr::map(dates.lst, function(d){
        # point to the search directory
        url_d <- paste0(data_dir, d)
        # get the list of results
        html.lst <- xml2::as_list(xml2::read_html(url_d))
        # number of search paths
        n_html <- length(html.lst[[1]][[2]]$pre)
        # sequence of valid search paths indexes
        idx_fil <- seq(from = 3, to = n_html - 1, by = 2)
        # obtain the list of files and combine with path
        files.lst <- purrr::map(idx_fil, function (f){
            fil <- attributes(html.lst[[1]][[2]]$pre[[f]])
            fil <- paste0(d,fil)
            return(fil)
        })
    })
    # transform the list into a vector of files
    # get the full path
    # include /vsicurl/ in the path
    full_path <- img_files.lst %>%
        unlist() %>%
        unname() %>%
        paste("/vsicurl",data_dir, ., sep="/")

    return(full_path)
}
