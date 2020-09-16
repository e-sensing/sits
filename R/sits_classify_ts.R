
#' @title Shows the predicted labels for a classified tibble
#' @name sits_show_prediction
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function takes a tibble with a classified time series
#' by a machine learning method and displays the result.
#'
#' @param  class.tb  A SITS tibble that has been classified
#' @return returns a tibble with the columns "from", "to", "class"
#' @export
sits_show_prediction <- function(class.tb) {
	.sits_test_tibble(class.tb)
	assertthat::assert_that(all(names(class.tb$predicted[[1]])
								%in% c("from", "to", "class", "probs")),
							msg = "sits_show_prediction: tibble has not been classified")
	return(dplyr::select(dplyr::bind_rows(class.tb$predicted),
						 c("from", "to", "class")))
}
#' @title Create an empty tibble to store the results of predictions
#' @name .sits_tibble_prediction
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Create a tibble to store the results of predictions.
#' @param  data            A tibble with the input data.
#' @param  class_info.tb   A tibble with the information on classification.
#' @param  pred.mtx        The result of the classification
#'                          (one class per column and one row per interval).
#' @return A tibble storing the predictions.
.sits_tibble_prediction <- function(data, class_info.tb, pred.mtx) {
	# retrieve the list of reference dates
	# this list is a global one and it is created based on the samples
	ref_dates.lst   <- class_info.tb$ref_dates[[1]]

	# retrieve the global timeline
	timeline_global <- class_info.tb$timeline[[1]]

	# size of prediction tibble
	num_samples <- nrow(data[1,]$time_series[[1]])

	# get the labels of the data
	labels <- class_info.tb$labels[[1]]
	n_labels <- length(labels)
	# create a named vector with integers match the class labels
	int_labels <- c(1:n_labels)
	names(int_labels) <- labels

	# compute pred.vec
	pred.vec <-  names(int_labels[max.col(pred.mtx)])

	class_idx <-  1

	predicted.lst <- purrr::pmap(
		list(data$start_date, data$end_date, data$time_series),
		function(row_start_date, row_end_date, row_time_series) {

			# get the timeline of the row
			timeline_row <- lubridate::as_date(row_time_series$Index)

			# the timeline of the row may differe\ from the global timeline
			# this happens when we are processing samples with different dates
			if (timeline_row[1] != timeline_global[1]) {
				# what is the reference start date?
				ref_start_date <- lubridate::as_date(row_start_date)
				# what is the reference end date?
				ref_end_date <- lubridate::as_date(row_end_date)
				# what are the reference dates to do the classification?
				ref_dates.lst <- .sits_timeline_match(timeline = timeline_row,
				                                      ref_start_date = ref_start_date,
													  ref_end_date = ref_end_date,
													  num_samples  = num_samples)
			}

			# store the classification results
			pred_row.lst <- ref_dates.lst %>%
				purrr::map(function(rd){
					pred_row <- tibble::tibble(
						from      = as.Date(rd[1]),
						to        = as.Date(rd[2]),
						class     = pred.vec[class_idx],
						probs     = list(as.data.frame(pred.mtx[class_idx,]))
					)
					class_idx  <<- class_idx + 1
					return(pred_row)
				})
			# transform the list into a tibble
			predicted.tb <- dplyr::bind_rows(pred_row.lst)
			return(predicted.tb)
		})

	data$predicted <- predicted.lst
	class(data) <- append(class(data), "predicted", after = 0)

	return(data)
}
