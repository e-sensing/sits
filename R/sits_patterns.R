#' @title Create time series patterns for classification
#' @name sits_patterns
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#'
#' @description This function allows the user to select different alternatives to define a set of
#' patterns, given his samples. The alternatives are:
#' "gam" - uses a generalised additive model to approximate a smooth spline for each pattern
#' "dendogram" - uses a herarchical clustering method to group the patterns
#' "centroids" - uses a positional clustering method to group the patterns
#'
#' @param data.tb          a SITS tibble time series with an alignment column
#' @param bands            the bands to be used for determining patterns
#' @param pt_method        a pattern fitting method
#' @return result          a model fitted into input data given by train_method parameter
#' @export
#'
sits_patterns <- function(data.tb, bands = NULL, pt_method = sits_gam(data.tb = NULL, bands = bands, from = NULL, to = NULL, freq = 8, formula = y ~ s(x))) {

    # does the input data exist?
    .sits_test_table (data.tb)
    # is the train method a function?
    ensurer::ensure_that(pt_method, class(.) == "function", err_desc = "sits_train: train_method is not a valid function")

    # compute the training method by the given data
    result <- pt_method(data.tb)
    return(result)

}

#' @title Create time series patterns for classification
#' @name sits_patt_dendgam
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Michelle Picoli, \email{mipicoli@@gmail.com}
#'
#'
#' @description This function ...
#'
#' @param data.tb                 a SITS tibble time series with an alignment column
#' @param first_cluster_cleaner   first_cluster_cleaner
#' @param cluster_a_labels        cluster_a_labels
#' @param second_cluster_cleaner  second_cluster_cleaner
#' @param k_cluster_a             k_cluster_a
#' @param k_cluster_b             k_cluster_b
#' @return result                 result
#' @export
#'
sits_patt_dendgam <- function(data.tb = NULL, first_cluster_cleaner = 0.027,
                              cluster_a_labels = c("Forest", "Cerrado", "Pasture"),
                              second_cluster_cleaner = 0.035, k_cluster_a = 8, k_cluster_b = 6){

    result_fun <- function(tb){

        # does the input data exist?
        .sits_test_table (tb)

        # first cluster cutree and cleaner
        data_clus.obj <- sits_dendrogram(tb, window.size = 7, step.pattern = symmetric1)
        cluster.tb <- sits_cluster(tb, data_clus.obj, k = 2)
        cluster.tb <- sits_cluster_cleaner(cluster.tb, min_clu_perc = first_cluster_cleaner)

        # cluster partitioning
        frequency.tb <- sits_cluster_frequency(cluster.tb, relative = TRUE)
        if (any(row.names(frequency.tb)[which.max(frequency.tb[1:(NROW(frequency.tb) - 1), 1])] %in% cluster_a_labels)){
            data_a.tb <- dplyr::filter(cluster.tb, cluster == 1)
            data_b.tb <- dplyr::filter(cluster.tb, cluster == 2)
        } else {
            data_a.tb <- dplyr::filter(cluster.tb, cluster == 2)
            data_b.tb <- dplyr::filter(cluster.tb, cluster == 1)
        }

        # second cluster cutree and cleaner
        # cluster_a
        data_clus_a.obj <- sits_dendrogram(data_a.tb, window.size = 7, step.pattern = symmetric1)
        cluster_a.tb <- sits_cluster(data_a.tb, data_clus_a.obj, k = k_cluster_a)
        cluster_a.tb <- sits_cluster_cleaner(cluster_a.tb, min_clu_perc = second_cluster_cleaner)
        # cluster_b
        data_clus_b.obj <- sits_dendrogram(data_b.tb, window.size = 7, step.pattern = symmetric1)
        cluster_b.tb <- sits_cluster(data_b.tb, data_clus_b.obj, k = k_cluster_b)
        cluster_b.tb <- sits_cluster_cleaner(cluster_b.tb, min_clu_perc = second_cluster_cleaner)

        # computing cluster new labels
        # cluster_a
        frequency_a.tb <- sits_cluster_frequency(cluster_a.tb, relative = TRUE)
        cluster_a_labels.lst <- colnames(frequency_a.tb)[1:(NCOL(frequency_a.tb)-1)] %>% purrr::map(function(cluster){
            paste0(row.names(frequency_a.tb)[which.max(frequency_a.tb[1:(NROW(frequency_a.tb) - 1), cluster])], ".", cluster)
        })
        names(cluster_a_labels.lst) <- colnames(frequency_a.tb)[1:(NCOL(frequency_a.tb)-1)]
        # cluster_b
        frequency_b.tb <- sits_cluster_frequency(cluster_b.tb, relative = TRUE)
        cluster_b_labels.lst <- colnames(frequency_b.tb)[1:(NCOL(frequency_b.tb)-1)] %>% purrr::map(function(cluster){
            paste0(row.names(frequency_b.tb)[which.max(frequency_b.tb[1:(NROW(frequency_b.tb) - 1), cluster])], ".", cluster)
        })
        names(cluster_b_labels.lst) <- colnames(frequency_b.tb)[1:(NCOL(frequency_b.tb)-1)]

        # relabel cluster
        # cluster_a
        cluster_a.tb <- sits_cluster_relabel(cluster_a.tb, cluster_a_labels.lst)
        # cluster_b
        cluster_b.tb <- sits_cluster_relabel(cluster_b.tb, cluster_b_labels.lst)

        # bind cluster_a and cluster_b
        combined.tb <- dplyr::bind_rows(cluster_a.tb, cluster_b.tb)

        # generate pattern with gam
        result.tb <- sits_gam(combined.tb)
        return(result.tb)
    }

    result.tb <- .sits_factory_function (data.tb, result_fun)
    return(result.tb)
}
