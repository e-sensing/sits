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
#' @param data.tb        A tibble with time series to be clustered.
#' @param bands          Bands to be clustered.
#' @param grid_xdim      X dimension of the SOM grid (default = 5).
#' @param grid_ydim      Y dimension of the SOM grid.
#' @param rlen           Number of times the complete data set will be presented to the SOM grid.
#' @param alpha          Learning rate, a vector of two numbers indicating the amount of change.
#'                       Default is to decline linearly from 0.05 to 0.01 over rlen updates.
#' @param  ...           Additional parameters to be passed to kohonen::supersom function.
#' @return A full kohonen object for data analysis.
#' @examples
#' \donttest{
#' # Retrieve a time series to be clusterized
#' data("samples_MT_9classes")
#' # compute a kohonen object
#' koh_obj <- sits_kohonen(samples_MT_9classes)
#' # generate a dataset
#' cluster.tb <- sits_kohonen_cluster(samples_MT_9classes, koh_obj)
#' # plot the result
#' sits_cluster_frequency(cluster.tb)
#' }
#' @export
sits_kohonen <- function (data.tb, bands = NULL, grid_xdim = 5, grid_ydim = 5, rlen = 100,
                          alpha = c(0.05, 0.01), ...) {
    # does the input data exist?
    .sits_test_tibble(data.tb)

    # if no bands informed, get all bands available in sits tibble
    if (purrr::is_null(bands))
        bands <- sits_bands(data.tb)

    # print message
    message("Clustering using SOM...")

    # get the values of the various time series for this band group
    values.tb <- sits_values(data.tb, bands, format = "bands_cases_dates")

    grid <- kohonen::somgrid(xdim = grid_xdim, ydim = grid_ydim, topo = "rectangular")
    kohonen.obj  <- kohonen::supersom (values.tb, grid=grid,
                                       rlen = rlen, alpha = alpha,
                                       keep.data = TRUE, ...)
    # return kohonen object
    return (kohonen.obj)
}

#' @title Cluster a set of satellite image time series using SOM
#' @name sits_kohonen_cluster
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Lorena Alves, \email{lorena.santos@@inpe.br}
#'
#' @description This function uses package "kohonen" (self-organized maps) to find clusters in
#' satellite image time series.
#' @references `kohonen` package (https://CRAN.R-project.org/package=kohonen)
#'
#' @param data.tb        A tibble with time series to be clustered.
#' @param kohonen.obj    A full kohonen object for data analysis.
#' @return A tibble with the clusters or clusters' members.
#' @examples
#' \donttest{
#' # Retrieve a time series to be clusterized
#' data("samples_MT_9classes")
#' # compute a kohonen object
#' koh_obj <- sits_kohonen(samples_MT_9classes)
#' # generate a dataset
#' cluster.tb <- sits_kohonen_cluster(samples_MT_9classes, koh_obj)
#' # plot the result
#' sits_cluster_frequency(cluster.tb)
#' }
#' @export
sits_kohonen_cluster <- function (data.tb, kohonen.obj) {
    # does the input data exist?
    .sits_test_tibble(data.tb)

    #verifies if data.tb has the same size of kohonen.obj
    ensurer::ensure_that(data.tb, (nrow(.) == length(kohonen.obj$unit.classif)),
                         err_desc = "sits_kohonen_cluster: you must provide the same data set used to cluster data.")

    # create a "cluster" column
    data.tb$cluster <- as.integer(kohonen.obj$unit.classif)

    # return kohonen object
    return (data.tb)
}
