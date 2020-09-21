#' @title Create a sits tibble to store the time series information
#' @name .sits_tibble
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function returns an empty sits tibble.
#' Sits tibbles are the main structures of sits package.
#' They contain both the satellite image time series and its metadata.
#' A sits tibble is a tibble with pre-defined columns that
#' has the metadata and data for each time series. The columns are
#' <longitude, latitude, start_date, end_date, label, cube, time_series>.
#' Most functions of sits package get a sits tibble as input
#' (with additional parameters)
#' and return another sits tibble as output.
#' This allows chaining functions over sits tibbles.
#'
#' @return A sits tibble.
#'
.sits_tibble <- function() {
	sits.tb <- tibble::tibble(longitude   = double(),
							  latitude    = double(),
							  start_date  = as.Date(character()),
							  end_date    = as.Date(character()),
							  label       = character(),
							  cube        = character(),
							  time_series = list()
	)
	class(sits.tb) <- c("sits",class(sits.tb))
	return(sits.tb)
}
#' @title Aligns dates of time series to a reference date
#' @name .sits_align_dates
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Converts the time indexes of a set of sits
#' tibble to a single reference year.
#' This function is useful to join many time series from
#' different years to a single year,
#' which is required by methods that combine many time series,
#' such as clustering methods.
#' The reference year is taken from the date of the start of the time series
#' available in the data cube.
#'
#' @param  data          Input sits tibble (useful for chaining functions).
#' @param  ref_dates     Dates to align the time series.
#' @return               The converted sits tibble
#'
.sits_align_dates <- function(data, ref_dates) {
	# backward compatibility
	data <- .sits_tibble_rename(data)
	# verify that tibble is correct
	.sits_test_tibble(data)
	# function to shift a time series in time
	shift_ts <- function(d, k) dplyr::bind_rows(utils::tail(d,k),
												utils::head(d,-k))

	# get the reference date
	start_date <- lubridate::as_date(ref_dates[1])
	# create an output tibble
	data1.tb <- .sits_tibble()

	rows.lst <- purrr::pmap(list(data$longitude,
								 data$latitude,
								 data$label,
								 data$cube,
								 data$time_series),
							function(long, lat, lab, cb, ts) {
								# only rows that match  reference dates are kept
								if (length(ref_dates) == nrow(ts)) {
									# in what direction to shift the time series?
									sense <- lubridate::yday(lubridate::as_date(ts[1,]$Index)) -
										lubridate::yday(lubridate::as_date(start_date))
									# find the date of minimum distance to the reference date
									idx <- which.min(abs((lubridate::as_date(ts$Index)
														  - lubridate::as_date(start_date))/lubridate::ddays(1)))
									# do we shift time up or down?
									if (sense < 0) shift <- -(idx - 1) else shift <- (idx - 1)
									# shift the time series to match dates
									if (idx != 1) ts <- shift_ts(ts, -(idx - 1))
									# convert the time index to a reference year
									first_date <- lubridate::as_date(ts[1,]$Index)
									# change the dates to the reference dates
									ts1 <- dplyr::mutate(ts, Index = ref_dates)

									# save the resulting row in the output tibble
									row <- tibble::tibble(
										longitude   = long,
										latitude    = lat,
										start_date  = lubridate::as_date(ref_dates[1]),
										end_date    = ref_dates[length(ref_dates)],
										label       = lab,
										cube        = cb,
										time_series = list(ts1))
								}
								return(row)
							})
	data1.tb <- dplyr::bind_rows(data1.tb, rows.lst)
	return(data1.tb)
}


#' @title Add new sits bands.
#' @name .sits_mutate_bands
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description Adds new bands and preserves existing in the time series
#'              of a sits tibble using \code{dplyr::mutate} function.
#' @param data       Valid sits tibble.
#' @param ...        Expressions written as `name = value`.
#'                   See \code{dplyr::mutate()} help for more details.
#' @return           A sits tibble with same samples and the selected bands.
#' @examples
#' \donttest{
#' # Retrieve data for time series with label samples in Mato Grosso in Brazil
#' data (samples_mt_6bands)
#' # Generate a new image with the SAVI (Soil-adjusted vegetation index)
#' savi.tb <- sits_mutate_bands(samples_mt_6bands, SAVI = (1.5*(NIR - RED)/(NIR + RED + 0.5)))
#' }
#' @export
.sits_mutate_bands <- function(data, ...){

	# backward compatibility
	data <- .sits_tibble_rename(data)

	# verify if data has values
	.sits_test_tibble(data)
	# bands in SITS are uppercase
	data  <- sits_rename(data, names = toupper(sits_bands(data)))

	# compute mutate for each time_series tibble
	proc_fun <- function(...){
		data$time_series <- data$time_series %>%
			purrr::map(function(ts.tb) {
				ts_computed.tb <- ts.tb %>%
					dplyr::mutate(...)
				return(ts_computed.tb)
			})
	}

	# compute mutate for each time_series tibble
	tryCatch({
		data$time_series <- proc_fun(...)
	}, error = function(e){
		msg <- paste0("Error - Are your band names all uppercase?")
		message(msg)
	})

	return(data)
}

#' @title Checks that the timeline of all time series of a data set are equal
#' @name .sits_prune
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function tests if all time series in a sits tibble
#' have the same number of samples, and returns a time series whose indices
#' match the majority of the samples.
#'
#' @param  data  Either a sits tibble or a raster metadata.
#' @return A pruned sits tibble.
#'
.sits_prune <- function(data) {

	# backward compatibility
	data <- .sits_tibble_rename(data)

	.sits_test_tibble(data)

	# create a vector to store the number of indices per time series
	n_samples <- vector()

	data$time_series %>%
		purrr::map(function(t) {
			n_samples[length(n_samples) + 1] <<- NROW(t)
		})

	# check if all time indices are equal to the median
	if (all(n_samples == stats::median(n_samples))) {
		message("Success!! All samples have the same number of time indices")
		return(data)
	}
	else{
		message("Some samples of time series do not have the same time indices
                as the majority of the data")

		# return the time series that have the same number of samples
		ind2 <- which(n_samples == stats::median(n_samples))

		return(data[ind2, ])
	}
}
#' @title Add new sits bands and drops existing.
#' @name .sits_transmute_bands
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Adds new bands and drops existing in the time series
#'               of a sits tibble using dplyr::transmute function.
#' @param data          A sits tibble.
#' @param ...           Pair expressions in the format `name = value`.
#'                      See \code{\link[dplyr]{mutate}} help for more details.
#' @return A sits tibble with same samples and the new bands.
#'
#' @examples
#' \donttest{
#' # Retrieve data for time series with label samples in Mato Grosso
#' data(samples_mt_6bands)
#' # Generate a new image with the SAVI (Soil-adjusted vegetation index)
#' savi.tb <- sits:::.sits_transmute_bands(samples_mt_6bands,
#'                                 SAVI = (1.5*(NIR - RED)/(NIR + RED + 0.5)))
#' }
#'
.sits_transmute_bands <- function(data, ...){
	# backward compatibility
	data <- .sits_tibble_rename(data)

	# verify if data is valid
	.sits_test_tibble(data)

	# tricky to include "Index" column and expand `...` arguments
	proc_fun <- function(..., Index = Index){
		Index <- quote(Index)
		purrr::map(data$time_series, function(ts.tb) {
			ts_computed.tb <- dplyr::transmute(ts.tb, !!(Index), ...)
		})
	}

	# compute transmute for each time_series tibble
	tryCatch({
		data$time_series <- proc_fun(...)
	}, error = function(e){
		msg <- paste0("Error - Are your band names all uppercase?")
		message(msg)
	})
	return(data)
}

#' @title Create partitions of a data set
#' @name  .sits_create_folds
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Alexandre Ywata, \email{alexandre.ywata@@ipea.gov.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Split a sits tibble into k groups, based on the label.
#'
#' @param data   A sits tibble to be partitioned.
#' @param folds     Number of folds.
.sits_create_folds <- function(data, folds = 5) {
	# verify if data exists
	.sits_test_tibble(data)

	# splits the data into k groups
	data$folds <- caret::createFolds(data$label, k = folds,
									 returnTrain = FALSE, list = FALSE)

	return(data)
}
#' @title Extract a subset of the data based on dates
#' @name .sits_extract
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Returns a vector containing the dates of a sits tibble.
#'
#' @param  row.tb     A sits tibble.
#' @param  start_date Starting date of the time series segment.
#' @param  end_date   End date of the time series segment.
#' @return A tibble in sits format with the chosen subset.
.sits_extract <- function(row.tb, start_date, end_date) {
	# create a tibble to store the results
	subset.tb <- .sits_tibble()

	# filter the time series by start and end dates
	ts <- sits_time_series(row.tb)
	indexes <- dplyr::between(ts$Index, start_date, end_date)

	if (any(indexes)) {
		sub.ts <- ts[indexes, ]
		# store the subset of the time series in a list
		ts.lst <- tibble::lst()
		ts.lst[[1]] <- sub.ts
		# create a new row of the output tibble
		subset.tb <- tibble::add_row(subset.tb,
									 longitude    = row.tb$longitude,
									 latitude     = row.tb$latitude,
									 start_date   = as.Date(start_date),
									 end_date     = as.Date(end_date),
									 label        = row.tb$label,
									 cube         = row.tb$cube,
									 time_series  = ts.lst)
	}
	return(subset.tb)
}

#' @title Check that the requested bands exist in the samples
#' @name .sits_samples_bands_check
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param samples       Time series with the samples
#' @param bands         Requested bands of the data sample
#' @return              Checked bands (cube bands if bands are NULL)
#'
.sits_samples_bands_check <- function(samples, bands = NULL) {
	# check the bands are available
	sp_bands <- sits_bands(samples)
	if (purrr::is_null(bands))
		bands <- toupper(sp_bands)
	else {
		bands <- toupper(bands)
		assertthat::assert_that(all(bands %in% sp_bands),
								msg = "required bands are not available in the samples")
	}
	return(bands)
}

#' @title Tests if a sits tibble is valid
#' @name .sits_test_tibble
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Tests if a sits tibble exists or has data inside.
#'
#' @param data  A sits tibble.
#' @return Returns TRUE if data has data.
.sits_test_tibble <- function(data) {
	assertthat::assert_that(!purrr::is_null(data),
							msg = "input data not provided")
	assertthat::assert_that(NROW(data) > 0,
							msg = "input data is empty")

	names <- c("longitude", "latitude", "start_date", "end_date",
			   "label", "cube", "time_series")

	assertthat::assert_that(all(names %in% colnames(data)),
							msg = "data input is not a valid sits tibble")

	return(TRUE)
}

#' @title Store the results of CSV samples that could not be read
#' @name .sits_tibble_csv
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Create an empty tibble to store the results of classification.
#'
#' @return A tibble to store the result of classifications.
.sits_tibble_csv <- function() {
	result <- tibble::tibble(longitude   = double(),
							 latitude    = double(),
							 start_date  = as.Date(character()),
							 end_date    = as.Date(character()),
							 label       = character()
	)
	return(result)
}


#' @title Rename a tibble to use "cube" instead of "coverage"
#' @name .sits_tibble_rename
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param data   Tibble with
.sits_tibble_rename <- function(data)
{
	# is the input data a valid sits tibble?
	if ("coverage" %in% names(data))
		data <- data %>% dplyr::rename(cube = coverage)

	return(data)
}
