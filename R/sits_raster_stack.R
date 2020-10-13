#' @title Obtain the information about files that make up a stack cube
#' @name .sits_raster_stack_info
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param type              type of cube
#' @param satellite         name of satellite
#' @param sensor            name of sensor
#' @param start_date        start_date of the cube
#' @param end_date          end date of the cube
#' @param bands             bands to be retrieved
#' @param data_dir          directory where data is located
#' @param parse_info        parsing information
#' @param delim             delimitator
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
#'              resulting columns after parsing are "X1, X2, X3, X4, X5, date, X7, band".
#'              In the second, they are "band, date".
#'
#' @export
#'
.sits_raster_stack_info <-  function (type,
									  satellite,
									  sensor,
									  start_date = NULL,
									  end_date = NULL,
									  bands    = NULL,
									  data_dir,
									  parse_info = NULL,
									  delim = NULL) {



	# list the image files
	img_files <- list.files(data_dir)

	# include the parse_info
	if (purrr::is_null(parse_info))
		parse_info  <- .sits_config_data_parse_info(type)

	# get the delim information
	if (purrr::is_null(delim))
		delim <- .sits_config_data_delim(type)

	# get the information on the required bands, dates and path
	info.tb <- img_files %>%
		# remove the extent
		tools::file_path_sans_ext() %>%
		# read the file path into a tibble
		readr::read_delim(delim = delim, col_names = parse_info) %>%
		# select the relevant parts
		dplyr::select(date, band) %>%
		# include path in the tibble
		dplyr::mutate(path = paste0(data_dir,"/",img_files)) %>%
		# order by dates
		dplyr::arrange(date) %>%
		# filter to remove duplicate combinations of file and band
		dplyr::distinct(band, date, .keep_all = TRUE)

	# extract the band names
	bands_files <- dplyr::pull(dplyr::distinct(info.tb, band))

	# convert the names of the bands to those used by SITS
	bands_sits <- .sits_config_band_names_convert(satellite, sensor, bands_files)

	# convert the band names to SITS bands
	info.tb <- dplyr::mutate(info.tb, band = bands_sits[band])

	if (!purrr::is_null(start_date) & !purrr::is_null(end_date))
		# filter by starting date and end date
		info.tb <- dplyr::filter(info.tb, date >= start_date & date <=end_date)

	if (!purrr::is_null(bands)) {
		assertthat::assert_that(bands %in% bands_sits,
				msg = "bands do not match band names in SITS - see config file")
		# select the bands
		info.tb <-  dplyr::filter(info.tb, band %in% bands)
	}

	return(info.tb)
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
	# get a raster object of one of the layers
	rast <- terra::rast(file_info$path[1])

	# create a tibble to store the metadata
	stack_cube <- .sits_cube_create(type      = "RASTER",
									URL       = NA,
									satellite = satellite,
									sensor    = sensor,
									name      = name,
									bands     = bands,
									scale_factors  = .sits_config_scale_factors(sensor, bands),
									missing_values = .sits_config_missing_values(sensor, bands),
									minimum_values = .sits_config_minimum_values(sensor, bands),
									maximum_values = .sits_config_maximum_values(sensor, bands),
									timelines      = list(timeline),
									nrows = terra::nrow(rast),
									ncols = terra::ncol(rast),
									xmin  = terra::xmin(rast),
									xmax  = terra::xmax(rast),
									ymin  = terra::ymin(rast),
									ymax  = terra::ymax(rast),
									xres  = terra::xres(rast),
									yres  = terra::yres(rast),
									crs   = as.character(suppressWarnings(terra::crs(rast))),
									file_info = file_info)

	return(stack_cube)
}
