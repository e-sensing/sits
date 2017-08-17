#' @title Cluster a set of time series using kohonen clustering -- self organized maps (SOM)
#' @name .sits_cluster_kohonen
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Lorena Alves, \email{lorena.santos@@inpe.br}
#'
#' @description Cluster time series in SOM mode. SOMs can be thought
#' of as a spatially constrained form of k-means clustering.
#' Even with identical settings, repeated training
#' of a SOM will lead to sometimes even quite different mappings, because of the random initialisation.
#' However, in our experience the conclusions drawn from the map remain remarkably
#' consistent, which makes it a very useful tool in many different circumstances. Nevertheless,
#' it is always wise to train several maps before jumping to conclusions (taken from `kohonen` package docs).
#'
#' @references `kohonen` package (https://CRAN.R-project.org/package=kohonen)
#'
#' @param data.tb        a tibble the list of time series to be clustered
#' @param bands          a vector the bands to be clusterized.
#' @param grid_xdim      x dimension of the SOM grid
#' @param grid_ydim      y dimension of the SOM grid
#' @param rlen           the number of times the complete data set will be presented to the SOM grid
#' @param alpha          learning rate, a vector of two numbers indicating the amount of change.
#' Default is to decline linearly from 0.05 to 0.01 over rlen updates.
#' @param return_members (boolean) should the results be the clusters' members instead of clusters' centroids?
#' @param show           (boolean) should the results be shown?
#' @param ...            any additional parameters to be passed to kohonen::supersom() function
#' @return clusters.tb a SITS tibble with the clusters
.sits_cluster_kohonen <- function (data.tb, bands, grid_xdim, grid_ydim, rlen, alpha, return_members, show, ...){

    # recalculate grid dimension if the number of neurons is greater than the number of data input cases
    #### TO-DO: Document this recalculation!
    if ((grid_xdim * grid_ydim) >= nrow(data.tb)){
        grid_xdim <- trunc(max(sqrt(nrow(data.tb) / (grid_xdim * grid_ydim) / 2) * grid_xdim, 2))
        grid_ydim <- trunc(max(sqrt(nrow(data.tb) / (grid_xdim * grid_ydim) / 2) * grid_ydim, 2))
        message(paste("Kohonen grid dimension reduced to", "grid_xdim =", grid_xdim, "and grid_ydim =", grid_ydim))
    }

    # get the values of the various time series for this band group
    values.tb <- sits_values (data.tb, bands, format = "bands_cases_dates")

    grid <- kohonen::somgrid(xdim = grid_xdim, ydim = grid_ydim, topo = "rectangular")
    kohonen_obj  <- kohonen::supersom (values.tb, grid=grid,
                                       rlen = rlen, alpha = alpha,
                                       keep.data = TRUE, ...)

    # Plot the series and the obtained prototypes
    if (show) .sits_kohonen_show (kohonen_obj)

    # stores in .sits_last_cluster environment
    assign("last_cluster", kohonen_obj, envir = .sits_last_cluster)

    # if return_members parameter is TRUE, returns a sits samples table with updated labels else, returns cluster's centroids
    return (.sits_from_kohonen (data.tb, kohonen_obj = kohonen_obj, return_members = return_members))
}
#' @title Cluster a set of time series using kohonen clustering followed by a hierarchical clustering over resulting neurons
#' @name .sits_cluster_kohodogram
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Lorena Alves, \email{lorena.santos@@inpe.br}
#'
#' @description Cluster time series in SOM mode followed by a hierqarchical clustering. (see `.sits_cluster_kohonen` description.)
#'
#' @references `kohonen` package (https://CRAN.R-project.org/package=kohonen), `dtwclust` package (https://CRAN.R-project.org/package=dtwclust)
#'
#' @param data.tb         a tibble the list of time series to be clustered.
#' @param bands           a vector the bands to be clusterized.
#' @param grid_xdim       x dimension of the SOM grid.
#' @param grid_ydim       y dimension of the SOM grid.
#' @param rlen            the number of times the complete data set will be presented to the SOM grid.
#' @param alpha           learning rate, a vector of two numbers indicating the amount of change.
#' Default is to decline linearly from 0.05 to 0.01 over rlen updates.
#' @param n_clusters      the number of clusters to be identified.
#' @param grouping_method the agglomeration method to be used. Any `hclust` method (see `hclust`).
#' @param return_members  (boolean) should the results be the clusters' members instead of clusters' centroids?
#' @param show            (boolean) should the results be shown?
#' @param  ...            any additional parameters
#' @return result.tb a SITS tibble with the clusters
.sits_cluster_kohodogram <- function (data.tb, bands, grid_xdim, grid_ydim, rlen, alpha, n_clusters,
                                      dist_method, grouping_method, return_members, show, ...){

    # recalculate grid dimension if the number of neurons is greater than the number of data input cases
    #### TO-DO: Document this recalculation!
    if ((grid_xdim * grid_ydim) >= nrow(data.tb)){
        grid_xdim <- trunc(max(sqrt(nrow(data.tb) / (grid_xdim * grid_ydim) / 2) * grid_xdim, 2))
        grid_ydim <- trunc(max(sqrt(nrow(data.tb) / (grid_xdim * grid_ydim) / 2) * grid_ydim, 2))
        message(paste("Kohonen grid dimension reduced to", "grid_xdim =", grid_xdim, "and grid_ydim =", grid_ydim))
    }

    # get the values of the various time series for this band group
    values.tb <- sits_values (data.tb, bands, format = "bands_cases_dates")

    # create a initial random grid
    grid <- kohonen::somgrid(xdim = grid_xdim, ydim = grid_ydim, topo = "rectangular")

    kohonen_obj  <- kohonen::supersom (values.tb, grid=grid,
                                       rlen = rlen, alpha = alpha,
                                       keep.data = TRUE, ...)

    # Plot the series and the obtained prototypes
    if (show) .sits_kohonen_show (kohonen_obj)

    # stores in .sits_last_cluster environment
    assign("last_cluster", kohonen_obj, envir = .sits_last_cluster)

    # returns kohonen's neurons
    neurons.tb <- .sits_from_kohonen (data.tb, kohonen_obj, return_members = FALSE)

    # proceed with unsupervised hierarchical cluster.
    neurons.tb$label <- tools::file_path_sans_ext(neurons.tb[1,]$label[[1]])

    # pass neurons to dendogram clustering
    clusters.tb <- .sits_cluster_dendogram (neurons.tb, bands = bands, n_clusters = n_clusters, dist_method = dist_method,
                                            grouping_method = grouping_method, return_members = return_members, show = FALSE, ... = ...)

    # return a sits table with all input data with new labels
    if (return_members) {
        # set clusters' labels to result data
        result.tb <- data.tb

        # if no original_label exists, creates one
        if (!("original_label" %in% colnames(result.tb)))
            result.tb$original_label <- tools::file_path_sans_ext(result.tb$label)

        # update labels according to clusters
        result.tb$label <- clusters.tb$label[kohonen_obj$unit.classif]

        # return a sits table with clusters' centroids
    } else
        result.tb <- dplyr::select(clusters.tb, longitude, latitude, start_date, end_date, label, coverage, time_series, original_label, n_members)

    return (result.tb)
}

#------------------------------------------------------------------
#' @title Returns a tibble of centroids and its metadata.
#' @name .sits_from_kohonen
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Lorena Alves, \email{lorena.santos@@inpe.br}
#'
#' @description reads a list of clusters provided by the dtwclust
#' package and produces a sits table.
#' @references `kohonen` package (https://CRAN.R-project.org/package=kohonen)
#'
#' @param data.tb          a tibble with input data of `kohonen`.
#' @param kohonen_obj      a kohonen structure returned from `kohonen::supersom`.
#' @param return_members   (boolean) should the results be the clusters' members instead of clusters' centroids?
#' @return result.tb       a SITS table with the clusters or clusters' members
.sits_from_kohonen <-  function (data.tb, kohonen_obj, return_members) {

    # if no original_label exists, creates one
    if (!("original_label" %in% colnames(data.tb)))
        data.tb$original_label <- data.tb$label

    if (return_members){
        # get label prefix to form sub-class label result
        label_prefix <- data.tb[1,]$label[[1]]

        # create a table to store the results
        result.tb <- data.tb

        # assign new labels
        result.tb$label <- paste0(label_prefix, ".", kohonen_obj$unit.classif)

        # organizes the resulting SITS table
        result.tb <- dplyr::select(result.tb, longitude, latitude, start_date, end_date, label, coverage, time_series, original_label)

    } else {
        # get label prefix to form sub-class label result
        label_prefix <- data.tb[1,]$label[[1]]

        # get num of neurons' members
        neurons_size <- rle(sort(kohonen_obj$unit.classif))$lengths

        # unpack output data provided by kohonen_obj
        neurons.lst <- purrr::map(kohonen_obj$codes, function (ts) ts %>% t() %>% as.data.frame())

        # populates the result table with centroids
        # traverse neurons bands
        result_band.lst <- purrr::map2(neurons.lst, names(neurons.lst), function (neurons.df, band) {
            # create a table to store the results
            result_band.tb <- sits_table()

            # return the number of each cluster members
            result_band.tb <- tibble::add_column(result_band.tb, original_label = character(), n_members = integer())

            # traverse neurons time series
            purrr::map2(neurons.df, seq_along(neurons.df), function (ts, i) {

                # populates result
                new_ts <- dplyr::select(data.tb[1,]$time_series[[1]], Index)
                ts.tb <- tibble::as_tibble(ts)
                names(ts.tb) <- band
                new_ts <- dplyr::bind_cols(new_ts, ts.tb)
                result_band.tb <<- tibble::add_row (result_band.tb,
                                                    longitude      = 0.0,
                                                    latitude       = 0.0,
                                                    start_date     = data.tb[1,]$start_date[[1]],
                                                    end_date       = data.tb[1,]$end_date[[1]],
                                                    label          = paste0(label_prefix, ".", i),
                                                    coverage       = data.tb[1,]$coverage[[1]],
                                                    time_series    = list(new_ts),
                                                    original_label = label_prefix,
                                                    n_members      = neurons_size[i])
            })
            return (result_band.tb)
        })

        # merge all bands together
        result.tb <- sits_table()
        result_band.lst %>% purrr::map(function (result_band.tb){
            result.tb <<- sits_merge(result.tb, result_band.tb)
        })

        # organizes the resulting SITS table
        result.tb <- dplyr::select(result.tb, longitude, latitude, start_date, end_date, label, coverage, time_series, original_label, n_members)
    }

    return (result.tb)
}

#' @title Plots a map produced by the kohonen package
#' @name .sits_kohonen_show
#' @author Lorena Alves, \email{lorena.santos@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description plots kohonen clusters produced by the kohonen package
#' @references `kohonen` package (https://CRAN.R-project.org/package=kohonen)
#' @param kohonen_obj     a kohonen structure returned from kohonen::supersom.
.sits_kohonen_show <- function (kohonen_obj) {
    # plot neurons in a map
    graphics::plot (kohonen_obj, type = "codes")

    # plot how many members for each neuron
    graphics::plot (kohonen_obj, type = "counts")

    # plot the distances among imediate neuron neighbors
    graphics::plot (kohonen_obj, type = "dist.neighbours")

    # print information about the kohonen
    .sits_kohonen_info (kohonen_obj)
}

#' @title Prints information about the kohonen neurons generated by the kohonen package
#' @name .sits_kohonen_info
#'
#' @description The kohonen package produces a kohonen object. This function
#' prints information about this object
#' @references `kohonen` package (https://CRAN.R-project.org/package=kohonen)
#' @param kohonen_obj   a kohonen object returned from kohonen::supersom.
.sits_kohonen_info <- function (kohonen_obj) {
    # cat ("-------------------------------\n")
    # # what is the name of the band?
    # band <- tools::file_path_sans_ext(names(clusters@centroids)[1])
    # cat (paste ("Band(s): ", band, "\n", sep = ""))
    #
    # # print the size of each cluster
    # df <- clusters@clusinfo
    # cat ("Clusters' sizes\n")
    # for (i in 1:nrow(df))
    #      cat (paste ("Cluster ", i,": ", df[i,"size"], "\n", sep = ""))
}
