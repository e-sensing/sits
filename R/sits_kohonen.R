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
#' @param data.tb        a tibble with time series to be clustered
#' @param bands          bands to be clustered.
#' @param grid_xdim      x dimension of the SOM grid (default = 5)
#' @param grid_ydim      y dimension of the SOM grid
#' @param rlen           number of times the complete data set will be presented to the SOM grid
#' @param alpha          learning rate, a vector of two numbers indicating the amount of change.
#'                       Default is to decline linearly from 0.05 to 0.01 over rlen updates.
#' @param  ...           additional parameters to be passed to kohonen::supersom function
#' @return kohonen.obj   a full kohonen object for data analysis
#' @export
sits_kohonen <- function (data.tb, bands = NULL, grid_xdim = 5, grid_ydim = 5, rlen = 100,
                          alpha = c(0.05, 0.01), ...) {

    # does the input data exist?
    .sits_test_tibble(data.tb)

    # if no bands informed, get all bands available in SITS tibble
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
#' @param data.tb        a tibble with time series to be clustered
#' @param kohonen.obj    a full kohonen object for data analysis
#' @return data.tb       tibble with the clusters or clusters' members
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

