#' @title Use time series values as distances for training patterns
#' @name .sits_distances
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function allows using a set of labelled time series as
#' input to the machine learning models. The attributes used to train the model
#' are the series themselves. It extracts the time series from a sits tibble
#' and "spreads" them in time to produce a tibble with distances.
#'
#' @param  data       A tibble with time series data and metadata.
#' @return            A data.table where columns have the reference label
#'                    and the time series values as distances.
#'
.sits_distances <- function(data) {
	# backward compatibility
	data <- .sits_tibble_rename(data)

	n_rows_data <- nrow(data)

	# create a list with the time series transposed from columns to rows
	ts.lst <- data$time_series %>%
		purrr::map(function(ts){
			as.data.frame(t(unlist(ts[-1])))
		})
	# bind the lists of time series together
	dist_DT <- data.table::rbindlist(ts.lst, use.names = FALSE)
	# create a data frame with the first two columns for training
	distances_DT <- data.table::data.table("original_row" = 1:n_rows_data,
										   "reference" = data$label)
	# join the two references columns with the data values
	distances_DT <- data.table::as.data.table(cbind(distances_DT, dist_DT))

	return(distances_DT)
}

#' @title Classify a distances tibble using machine learning models
#' @name .sits_distances_classify
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Returns a sits tibble with the results of the ML classifier.
#'
#' @param  distances_DT    data.table with distances.
#' @param  class_info.tb   classification information.
#' @param  ml_model        model trained by \code{\link[sits]{sits_train}}.
#' @param  multicores      number of threads to process the time series.
#' @return A vector with the predicted labels.
.sits_distances_classify <- function(distances_DT, class_info.tb,
									 ml_model, multicores) {

	# keras-based models run in single-core mode
	if ("keras_model" %in% class(ml_model) || "rfor_model" %in% class(ml_model))
		multicores <- 1
	# define the column names
	attr_names <- names(.sits_distances(environment(ml_model)$data[1,]))
	assertthat::assert_that(length(attr_names) > 0,
							msg = "sits_classify_distances: training data not available")
	# create a data table to store the distances
	dist_DT <- data.table::data.table(nrow = 0, ncol = length(attr_names))

	# select the data table indexes for each time index
	select.lst <- .sits_timeline_distance_indexes(class_info.tb,
												  ncol(distances_DT))

	# classify a block of data
	classify_block <- function(block_DT) {
		# create a list to store the data tables to be used for prediction
		row.lst <- purrr::map(select.lst, function(sel_index) {
			rows_DT <- block_DT[, sel_index, with = FALSE]
		})
		# create a set of distances to be classified
		dist_DT <- data.table::rbindlist(row.lst, use.names = FALSE)
		# set the attribute names of the columns
		colnames(dist_DT) <- attr_names

		# classify the subset data
		prediction_DT <- ml_model(dist_DT)

		return(prediction_DT)
	}

	join_blocks <- function(blocks.lst) {
		pred.mtx <-
			blocks.lst %>%
			dplyr::bind_rows()
		return(pred.mtx)
	}
	n_rows_dist <- nrow(distances_DT)
	if (multicores > 1) {
		blocks.lst <- split.data.frame(distances_DT, cut(1:n_rows_dist,
														 multicores, labels = FALSE))
		# apply parallel processing to the split dat
		results.lst <- parallel::mclapply(blocks.lst, classify_block,
										  mc.cores = multicores)
		pred.mtx <- join_blocks(results.lst)
	}
	else
		pred.mtx <- classify_block(distances_DT)

	return(pred.mtx)
}

#' @title Sample a percentage of a time series distance matrix
#' @name .sits_distances_sample
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Takes a sits tibble with different labels and
#'              returns a new tibble. For a given field as a group criterion,
#'              this new table contains a given number or percentage
#'              of the total number of samples per group. Parameter n indicates
#'              the number of random samples with reposition.
#'              Parameter frac indicates a fraction of random samples
#'              without reposition. If frac > 1, no sampling is done.
#'
#' @param  distances_DT    Distances associated to a time series.
#' @param  frac            Percentage of samples to pick.
#' @return                 Data.table with a fixed quantity of samples
#'                         of informed labels and all other.
.sits_distances_sample <- function(distances_DT, frac){
	# compute sampling
	result_DT <- distances_DT[, .SD[sample(.N, round(frac*.N))], by = reference]

	return(result_DT)
}
