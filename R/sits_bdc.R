#' @title Check if the BDC tiles are working
#' @name .sits_bdc_check_tiles
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param satellite     satellite
#' @param sensor        sensor
#' @param bands         bands
#' @param cube          input cube
#' @param tile          tile
#' @param data_access   type of access
#' @param start_date    start_date of the cube
#' @param end_date      end date of the cube
.sits_bdc_check_tiles <- function(satellite,
								  sensor,
								  bands,
								  cube,
								  tile,
								  data_access,
								  start_date,
								  end_date){

	# check if the satellite and sensor are supported by SITS
	assertthat::assert_that(!purrr::is_null(satellite),
							msg = "sits_cube: for type = TILE satelite must be provided")
	assertthat::assert_that(!purrr::is_null(sensor),
							msg = "sits_cube: for type = TILE sensor must be provided")
	# Tests is satellite and sensor are known to SITS
	.sits_raster_satellite_sensor(satellite, sensor)

	# test if bands are provided
	assertthat::assert_that(!purrr::is_null(bands),
							msg = "sits_cube: for type = TILE bands must be provided")

	# test if cube and tile are provided
	assertthat::assert_that(!purrr::is_null(cube),
							msg = "sits_cube: for type = TILE cube name must be provided")

	assertthat::assert_that(!purrr::is_null(tile),
							msg = "sits_cube: for type = TILE, the tile name must be provided")

	# test if data_access variable is correct
	assertthat::assert_that(data_access %in% c("local", "web"),
							msg = "sits_cube: for type = TILE data_access must one of (local, web)")

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
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param satellite     satellite
#' @param sensor        sensor
#' @param cube          input cube
#' @param tile          tile
#' @param data_access   type of access
#' @param start_date    start_date of the cube
#' @param end_date      end date of the cube
#' @param .local        local address (if different from default)
#' @param .web          web address (if different from default)
.sits_bdc_info_tiles <- function(satellite,
								 sensor,
								 cube,
								 tile,
								 data_access,
								 start_date,
								 end_date,
								 .local,
								 .web){

	# obtain the directory for local access
	if (data_access == "local") {
		if (!purrr::is_null(.local))
			dir <- .local
		else
			dir <- .sits_config_cube_bdc_tile_local()
	}
	if (data_access == "web") {
		if (!purrr::is_null(.web))
			dir <-  .web
		else
			dir <- .sits_config_cube_bdc_tile_web()
	}
	# compose the directory with the name of the cube and tile
	data_dir <- paste0(dir,"/",cube,"/",tile)
	# list the files in the directory
	files_tile <- data_dir %>%
		list.files(recursive = TRUE) %>%
		.[grepl("tif", .)]

	# filter the dates as directories (if they are included in the file path)
	files_no_dir.vec <- files_tile %>%
		readr::read_delim(delim = "/", col_names = FALSE) %>%
		as.vector(dplyr::pull(.[,ncol(.)]))

	# puts the dates and bands into a tibble
	prefix <- paste0(cube,"_",tile,"_")
	stack.tb <- files_no_dir.vec %>%
		stringr::str_remove(prefix) %>%
		tools::file_path_sans_ext() %>%
		readr::read_delim(delim = "_", col_names = c("date", "end_date", "band")) %>%
		dplyr::select(date, band)

	# bands are lowercase, except when start with "B"
	bands <- stack.tb$band
	new_bands.lst <- purrr::map(bands, function(b){
		if (grepl("B", b)) {
			l <- stringr::str_locate(b, "B")
			if (l[1,"start"] == 1 && l[1,"end"] == 1)
				return(b)
			else
				return(tolower(b))
		}
		else
			return(tolower(b))
	})
	stack.tb$band <- unlist(new_bands.lst)
	# include a column with the file path
	stack.tb$path <- paste0(data_dir,"/",files_tile)

	# sanity check - is the raster data cover the period [st_date, en_date]?
	assertthat::assert_that(start_date %in% unique(stack.tb$date),
							msg = "raster data does not include start date")
	assertthat::assert_that(end_date %in% unique(stack.tb$date),
							msg = "raster data does not include end date")

	# order the tile by date
	stack.tb <- dplyr::arrange(stack.tb, date)

	# filter by starting date and end date
	stack.tb <- dplyr::filter(stack.tb, start_date >= date & end_date <= date)


	return(stack.tb)

}
#' @title Create a data cube for a BDC TILE
#' @name .sits_bdc_tile_cube
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
	params <- .sits_raster_params(suppressWarnings(raster::raster(full_path_1)))

	# get scale factors
	scale_factors  <- .sits_config_scale_factors(sensor, bands)
	# get missing values
	missing_values <- .sits_config_missing_values(sensor, bands)
	# get minimum values
	minimum_values <- .sits_config_minimum_values(sensor, bands)
	# get maximum values
	maximum_values <- .sits_config_maximum_values(sensor, bands)


	# create a tibble to store the metadata
	cube.tb <- .sits_cube_create(type           = "BDC_TILE",
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

	class(cube.tb) <- c("stack_cube", class(cube.tb))
	return(cube.tb)
}
