#' @title Cluster a set of satellite image time series using SOM
#' @name sits_kohonen
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Lorena Alves, \email{lorena.santos@@inpe.br}
#'
#' @description This function uses package "kohonen" (self-organized maps) to find clusters in
#' satellite image time series.
#' @references `kohonen` package (https://CRAN.R-project.org/package=kohonen)
#'
#' @param data.tb        a SITS tibble the list of time series to be clustered
#' @param bands          the bands to be clusterized.
#' @param grid_xdim      x dimension of the SOM grid (used only in `kohonen` or `kohonen-dendogram` methods). Defaul is 5.
#' @param grid_ydim      y dimension of the SOM grid
#' @param rlen           the number of times the complete data set will be presented to the SOM grid
#' @param alpha          learning rate, a vector of two numbers indicating the amount of change.
#'                       Default is to decline linearly from 0.05 to 0.01 over rlen updates.
#' @param  ...           Additional parameters to be passed to kohonen::supersom function
#' @return clusters.tb a SITS tibble with the clusters time series or cluster' members time series according to return_member parameter.
#' If return_members are FALSE, the returning SITS tibble will contain a new collumn called `n_members` informing how many members has each cluster.
#' @export
sits_kohonen <- function (data.tb, bands = NULL, grid_xdim = 5, grid_ydim = 5, rlen = 100,
                          alpha = c(0.05, 0.01), ...) {

    # does the input data exist?
    .sits_test_tibble (data.tb)

    # if no bands informed, get all bands available in SITS tibble
    if (purrr::is_null(bands))
        bands <- sits_bands(data.tb)

    # creates the resulting tibble
    cluster.tb <- .sits_tibble()

    # how many different labels are there?
    labels <- dplyr::distinct (data.tb, label)$label

    #
    message("Clustering using SOM...")

    # add a progress bar
    progress_bar <- utils::txtProgressBar(min = 0, max = length(labels), style = 3)

    # traverse labels
    purrr::map2(labels, seq_along(labels), function (lb, i){
        # filter only those rows with the same labels
        # cut time series to fit in one year
        label.tb <- dplyr::filter (data.tb, label == lb)

        # recalculate grid dimension if the number of neurons is greater than the number of data input cases
        #### TO-DO: Document this recalculation!
        if ((grid_xdim * grid_ydim) >= nrow(label.tb)){
            grid_xdim <- trunc(max(sqrt(nrow(label.tb) / (grid_xdim * grid_ydim) / 2) * grid_xdim, 2))
            grid_ydim <- trunc(max(sqrt(nrow(label.tb) / (grid_xdim * grid_ydim) / 2) * grid_ydim, 2))
            message(paste("Kohonen grid dimension reduced to", "grid_xdim =", grid_xdim, "and grid_ydim =", grid_ydim))
        }

        # get the values of the various time series for this band group
        values.tb <- sits_values (label.tb, bands, format = "bands_cases_dates")

        grid <- kohonen::somgrid(xdim = grid_xdim, ydim = grid_ydim, topo = "rectangular")
        kohonen_obj  <- kohonen::supersom (values.tb, grid=grid,
                                           rlen = rlen, alpha = alpha,
                                           keep.data = TRUE, ...)

        # if no original_label exists, creates one
        if (!("original_label" %in% colnames(label.tb)))
            data.tb$original_label <- data.tb$label

        # get label prefix to form sub-class label result
        label_prefix <- data.tb[1,]$label[[1]]

        # create a tibble to store the results
        result.tb <- data.tb

        # assign new labels
        result.tb$label <- paste0(label_prefix, ".", kohonen_obj$unit.classif)

        # organizes the resulting SITS tibble
        result.tb <- dplyr::select(result.tb, longitude, latitude, start_date, end_date, label, coverage, time_series, original_label)

        # append the result
        cluster.tb <<- dplyr::bind_rows(cluster.tb, result.tb)

        # update progress bar
        utils::setTxtProgressBar(progress_bar, i)

    })
    close(progress_bar)

    return (cluster.tb)
}

