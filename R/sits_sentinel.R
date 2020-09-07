#' @title Check access rigths on AWS
#' @name  .sits_sentinel_aws_check_access
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param access_key     AWS access key
#' @param secret_key     AWS secret key
#' @param region         AWS region
#'
#' @return  TRUE if access is granted
#'
.sits_sentinel_aws_check_access <- function(access_key = NULL,
											secret_key = NULL,
											region = NULL) {

	# require package
	if (!requireNamespace("aws.s3", quietly = TRUE)) {
		stop("Please install package aws.s3", call. = FALSE)
	}
	if (purrr::is_null(access_key)) {
		env_access_key <- Sys.getenv("AWS_ACCESS_KEY_ID")
		assertthat::assert_that(nchar(env_access_key) > 1,
								msg = "AWS access key needs to be provided")
	}
	else
		Sys.setenv("AWS_ACCESS_KEY_ID", access_key)

	if (purrr::is_null(secret_key)) {
		env_secret_key <- Sys.getenv("AWS_SECRET_ACCESS_KEY")
		assertthat::assert_that(nchar(env_secret_key) > 1,
								msg = "AWS secret key needs to be provided")
	}
	else
		Sys.setenv("AWS_SECRET_ACCESS_KEY", secret_key)

	if (purrr::is_null(region)) {
		env_region <- Sys.getenv("AWS_DEFAULT_REGION")
		assertthat::assert_that(nchar(env_region) > 1,
								msg = "AWS region needs to be provided")
	}
	else
		Sys.setenv("AWS_DEFAULT_REGION", region)

	test_file <- .sits_config_sentinel_aws_test_file()

	# are the files bricks?
	tryCatch({
		r <- suppressWarnings(raster::raster(test_file))
	}, error = function(e){
		msg <- paste0("Error in accessing AWS files")
		.sits_log_error(msg)
		message(msg)
	})
	return(TRUE)

}
#' @title Get information on S2 leval 2A tiles in AWS
#' @name .sits_sentinel_aws_info_tiles
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
.sits_sentinel_aws_info_tiles <- function(tile, bands, resolution, start_date, end_date) {

	# pre-conditions
	# for S2, tile must be given
	assertthat::assert_that(!purrr::is_null(tile),
							msg = "For S2 L2A in AWS, tile must be provided")

	# must have 5 chars and be of the form NNAAA
	# where NN is betwen 10 and 60 and AAA is all caps
	assertthat::assert_that(nchar(tile) == 5,
							msg = "For S2 L2A in AWS, invalid tile")
	num_index <- as.numeric(substr(tile,1,2))
	assertthat::assert_that(num_index >= 10 & num_index <= 60,
							msg = "For S2 L2A in AWS, invalid tile")
	assertthat::assert_that(grepl('^[A-Z]+$', substr(tile,3,5)),
							msg = "For S2 L2A in AWS, invalid tile")

	# precondition - resolution
	assertthat::assert_that(resolution %in% .sits_config_sentinel_aws_resolutions(),
							msg = "For S2 in AWS, invalid resolution")
	# precondition - dates
	assertthat::assert_that(lubridate::is.Date(start_date),
							msg = "start date is invalid")
	assertthat::assert_that(lubridate::is.Date(end_date),
							msg = "end date is invalid")

	# precondition - bands
	# find the bands available at the chosen resolution
	bands_s2  <- .sits_config_sentinel_bands(resolution)
	if (!purrr::is_null(bands)){
		assertthat::assert_that(all(bands %in% bands_s2),
								msg = "requested bands not available")
	}
	else
		bands <- bands_s2

	# get the name of the bucket
	bucket <- .sits_config_sentinel_aws_bucket()


	# include the prefix for searching the S2 bucket
	prefix <- paste0("tiles/",substring(tile,1,2),"/",
					 substring(tile,3,3),"/",
					 substring(tile,4,5),"/")

	# get the files and the bands for the tile
	bucket_s2a <- aws.s3::get_bucket(
		bucket = bucket,
		prefix = prefix,
		headers = list('x-amz-request-payer' = "requester"),
		max = Inf)

	# get the files associated to the bucket
	# filter image files (Sentinel-2 uses JPEG200)
	s2_files <- purrr::map(1:length(bucket_s2a),
						   function (i) return (bucket_s2a[i]$Contents$Key)) %>%
		unlist() %>%
		.[grepl("jp2",.)]

	# get the information on the required bands, dates and path
	s2.tb <- s2_files %>%
		# read the file path into a tibble
		readr::read_delim(delim = "/", col_names = FALSE) %>%
		# select the relevant parts
		dplyr::select(X5, X6, X7, X9, X10) %>%
		# rename the columns
		dplyr::rename(year = X5, month = X6, day = X7, res = X9, band = X10) %>%
		# create a date column
		dplyr::mutate(date = lubridate::make_date(year,month,day)) %>%
		# select important information on the files
		dplyr::select(res, band, date) %>%
		# include path in the tibble
		dplyr::mutate(path = s2_files) %>%
		# remove the leading "R" on the resolution
		dplyr::mutate(res = substring(res,2,4)) %>%
		# select bands in given resolution
		dplyr::filter(res == resolution) %>%
		# remove the ".jp2" in the band name
		dplyr::mutate(band = substring(band,1,3)) %>%
		# filter for the bands available in the given resolution
		dplyr::filter(band %in% bands) %>%
		# order by dates
		dplyr::arrange(date) %>%
		# select valid dates
		dplyr::filter(date >= start_date & date <= end_date) %>%
		# include /vsis3 into the path
		dplyr::mutate(path = paste0("/vsis3/",bucket,"/",path)) %>%
		# filter to remove duplicate combinations of file and band
		dplyr::distinct(band, date, .keep_all = TRUE)

	return(s2.tb)
}
#' @title Create a data cube for a Sentinel-2 AWS TILE
#' @name .sits_sentinel_aws_tile_cube
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
.sits_sentinel_aws_tile_cube <- function(name,
								     bands,
								     tile,
								     file_info){

	# set the satellite and the sensor
	satellite <- "SENTINEL-2"
	sensor    <- "MSI"
	# obtain the timeline
	timeline <- unique(lubridate::as_date(file_info$date))

	# set the labels
	labels <- c("NoClass")
	# find the bands
	s2_bands <- unique(file_info$band)
	if (purrr::is_null(bands))
		bands <- s2_bands
	else
		assertthat::assert_that(all(bands %in% s2_bands),
			msg = "mismatch btw requested bands and bands availabe in S2 AWS")


	# get the first image
	# obtain the parameters
	params <- .sits_raster_params(suppressWarnings(raster::raster(file_info[1,]$path)))

	# get scale factors
	scale_factors  <- .sits_config_scale_factors(sensor, bands)
	# get missing values
	missing_values <- .sits_config_missing_values(sensor, bands)
	# get minimum values
	minimum_values <- .sits_config_minimum_values(sensor, bands)
	# get maximum values
	maximum_values <- .sits_config_maximum_values(sensor, bands)


	# create a tibble to store the metadata
	cube.tb <- .sits_cube_create(type           = "S2_L2A_AWS",
								 satellite      = satellite,
								 sensor         = sensor,
								 name           = name,
								 cube           = "AWS-S2-L2A",
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
# For the record, the additional bands in L2A S2 images are
# AOT: Aerosol Optical Thickness map (at 550nm)
# CLD: Raster mask values range from 0 for high confidence clear sky to 100 for high confidence cloudy
# SCL: Scene Classification. The meaning of the values is indicated in the Category Names of the band.
# SNW: Raster mask values range from 0 for high confidence NO snow/ice to 100 for high confidence snow/ice
# WVP: Scene-average Water Vapour map


