#' @title compare clusters against original labels and computes a segregation matrix.
#' @name sits_cluster_segregation
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Given a sits table with `original_label` column, computes a segregation matrix
#' between the original labels (`original_label` column) and new labels. This is useful
#' to analyse the separability of samples for a given clustering algorithm.
#'
#' @param  data.tb        a SITS table with the samples to be validated
#' @return result.tb      a tibble with segregation matrix.
#' @export
sits_cluster_segregation <- function (data.tb){
     ensurer::ensure_that(data.tb, !purrr::is_null(.),
                          err_desc = "sits_cluster_segregation: SITS table not provided")
     # do the input data have the `original_label` column?
     ensurer::ensure_that(data.tb, "original_label" %in% colnames(data.tb),
                          err_desc = "sits_cluster_segregation: informed SITS table has not an `original_label` column.")

     # TO-DO: generalize for clusters series and clusters' members series

     result.tb <- data.tb %>%
          dplyr::group_by(original_label, label) %>%
          dplyr::summarise(count = n()) %>%
          tidyr::spread(key = label, value = count) %>%
          dplyr::ungroup()

     return (result.tb)
}

#' @title computes a segregation measure from a clusterized SITS table data.
#' @name sits_segregation_measure
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Computes an measure of seggregation from SITS table based on mean relative Shannon Entropy,
#' where maximum entropy is 1.0 and minimum entropy is 0.0, from each cluster label against
#' original_label occorrences in it. To obtain this measure, first we compute Shannon
#' information entropy from counts of each cluster label. To get the relative entropy, we
#' divide this value by the maximum entropy. After that, we compute the weighted mean relative
#' to the frequency of each cluster label. The final values represents an average of
#' maximum entropy fraction. This is useful to assess the separability of samples for a given clustering algorithm.
#'
#' @param  data.tb        a SITS table with the samples to be validated.
#' @param  measure        an segregation measure. For now, only "entropy" is implemented.
#' @param  per_cluster    (boolean) should return a total average segregation measure, or a per cluster measure?
#' @return result         a segregation measure.
#' @export
sits_segregation_measure <- function (data.tb, measure = "entropy", per_cluster = FALSE){
     ensurer::ensure_that(data.tb, !purrr::is_null(.),
                          err_desc = "sits_segregation_measure: SITS table not provided.")

     # ensure that `measure` parameter is a valid option
     ensurer::ensure_that(measure, . == "entropy",
                          err_desc = "sits_segregation_measure: measure jno supported.")

     # do the input data have the `original_label` column?
     ensurer::ensure_that(data.tb, "original_label" %in% colnames(data.tb),
                          err_desc = "sits_segregation_measure: informed SITS table has not an `original_label` column.")

     # do we have at least two original labels?
     labels_count <- length(table(data.tb$original_label))
     if (labels_count == 1)
          return (0.0)

     if (!("n_members" %in% colnames(data.tb)))
          data.tb$n_members <- 1


     if (measure == "entropy")
          # sum all n_members values grouping by original_label and label fields. After that computes a segregation measure.
          # further measures implementations must return two mandatory fields: segr and frac. The first is the measure itself,
          # the second represents the fraction of cluster members.
          result.tb <- data.tb %>%
               dplyr::group_by(original_label, label) %>%
               dplyr::summarise(count = sum(n_members, na.rm = TRUE)) %>%
               dplyr::ungroup() %>%
               dplyr::group_by(label) %>%
               dplyr::summarise(segr = entropy::entropy(count) / log(labels_count),
                                frac = sum(count, na.rm = TRUE) / sum(sum(count, na.rm = TRUE)), na.rm = TRUE)

     if (per_cluster)
          return (result.tb)

     return (dplyr::summarise(result.tb, mean_segr = sum(segr * frac, na.rm = TRUE)) %>% .$mean_segr)

}

#' @title Do a cross combination of all elements of its input parameters.
#' @name .setup_segregation_expr
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Returns a tibble with all cross combinations of its input parameters informed as lists, vectors, or scalars.
#' This output is used to set the parameters' domain to be used in `sits_segregation_experiment` function.
#'
#' @param method          string - either 'dendogram', 'centroids', 'kohonen', or 'kohonen-dendogram'.
#' @param times           how many times to run an parameterized experiment.
#' @param bands           the bands to be clusterized.
#' @param n_clusters      the number of clusters to be croped from hierarchical clustering (ignored in `kohonen` method). Default is 2.
#' @param grouping_method the agglomeration method to be used. Any `hclust` method (see `hclust`) (ignored in `kohonen` method). Default is 'ward.D2'.
#' @param koh_grid_length grid side dimension of the SOM grid (used only in `kohonen` or `kohonen-dendogram` methods). Defaul is 5.
#' @param koh_rlen        the number of times the complete data set will be presented to the SOM grid
#' (used only in `kohonen` or `kohonen-denddogram` methods). Default is 100.
#' @param koh_alpha_from  starting learning rate. Default is to decline linearly from 0.05 to `koh_alpha_to` over rlen updates.
#' @param koh_alpha_to    ending learning rate. Default is to decline linearly from `koh_alpha_from` to 1.0 over rlen updates.
#' @return result.tb      a tibble with all cross combinations of parameters' values.
.setup_segregation_expr <- function(method, times, bands, n_clusters, grouping_method,
                                    koh_grid_length, koh_rlen, koh_alpha_from, koh_alpha_to){

     if (method == "dendogram" || method == "centroids")
          return (tibble::as_tibble(expand.grid(method = "dendogram", bands = bands, n_clusters = n_clusters,
                                                grouping_method = grouping_method, koh_grid_length = NA,
                                                koh_rlen = NA, koh_alpha_from = NA, koh_alpha_to = NA,
                                                times = 1,
                                                KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)))
     else if (method == "kohonen")
          return (tibble::as_tibble(expand.grid(method = "kohonen", bands = bands, n_clusters = NA,
                                                grouping_method = NA, koh_grid_length = koh_grid_length,
                                                koh_rlen = koh_rlen, koh_alpha_from = koh_alpha_from, koh_alpha_to = koh_alpha_to,
                                                times = times,
                                                KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)))
     else if (method == "kohonen-dendogram")
          return (tibble::as_tibble(expand.grid(method = "kohonen-dendogram", bands = bands, n_clusters = n_clusters,
                                                grouping_method = grouping_method, koh_grid_length = koh_grid_length,
                                                koh_rlen = koh_rlen, koh_alpha_from = koh_alpha_from, koh_alpha_to = koh_alpha_to,
                                                times = times,
                                                KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)))
}

#' @title Runs a single segregation experiment.
#' @name .exec_segregation_expr
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Runs an segregation experiment with a single valued parameters and returns a list with experiment's result.
#'
#' @param data.tb         a SITS table with the samples to be used in experiments.
#' @param method          string - either 'dendogram', 'centroids', 'kohonen', or 'kohonen-dendogram'.
#' @param bands           the bands to be clusterized.
#' @param n_clusters      the number of clusters to be croped from hierarchical clustering (ignored in `kohonen` method). Default is 2.
#' @param grouping_method the agglomeration method to be used. Any `hclust` method (see `hclust`) (ignored in `kohonen` method). Default is 'ward.D2'.
#' @param koh_grid_length grid side dimension of the SOM grid (used only in `kohonen` or `kohonen-dendogram` methods). Defaul is 5.
#' @param koh_rlen        the number of times the complete data set will be presented to the SOM grid
#' (used only in `kohonen` or `kohonen-denddogram` methods). Default is 100.
#' @param koh_alpha_from  starting learning rate. Default is to decline linearly from 0.05 to `koh_alpha_to` over rlen updates.
#' @param koh_alpha_to    ending learning rate. Default is to decline linearly from `koh_alpha_from` to 1.0 over rlen updates.#' @param method
#' @param times           how many times to run an parameterized experiment.
#' @return result.lst     a list with all cross combinations of parameters' values.
.exec_segregation_expr <- function(data.tb, method, bands, n_clusters, grouping_method, koh_grid_length,
                                   koh_rlen, koh_alpha_from, koh_alpha_to, times = 1) {

     segregation <- 0.0
     etime <- as.difftime(0, units = "secs")
     for (i in 1:times){
          stime <- Sys.time()
          clusterized.tb <- sits_cluster(data.tb, method = method, grouping_method = grouping_method,
                                         bands = bands, n_clusters = n_clusters,
                                         show = FALSE, return_members = TRUE, unsupervised = TRUE,
                                         koh_xgrid = koh_grid_length, koh_ygrid = koh_grid_length,
                                         koh_rlen = koh_rlen, koh_alpha = c(koh_alpha_from, koh_alpha_to))
          etime <- Sys.time() - stime + etime
          segregation <- sits_segregation_measure(clusterized.tb) + segregation

     }
     etime <- etime / times
     segregation <- segregation / times

     result.lst <- list(method          = method,
                        bands           = paste0(bands, collapse = "&"),
                        n_clusters      = n_clusters,
                        grouping_method = grouping_method,
                        koh_grid_length = koh_grid_length,
                        koh_rlen        = koh_rlen,
                        koh_alpha_from  = koh_alpha_from,
                        koh_alpha_to    = koh_alpha_to,
                        segregation     = segregation,
                        expr_time       = etime)

     return (result.lst)
}

#' @title Do a set of segregation experiments based on a cross combination of all elements of its input parameters.
#' @name sits_segregation_experiments
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Runs a set of segregation experiments according to informed parameters and returns a tibble with experiment's result.
#'
#' @param data.tb         a SITS table with the samples to be used in experiments.
#' @param method          string - either 'dendogram', 'centroids', 'kohonen', or 'kohonen-dendogram'.
#' @param times           how many times to run an parameterized experiment.
#' @param bands           the bands to be clusterized.
#' @param n_clusters      the number of clusters to be croped from hierarchical clustering (ignored in `kohonen` method). Default is 2.
#' @param grouping_method the agglomeration method to be used. Any `hclust` method (see `hclust`) (ignored in `kohonen` method). Default is 'ward.D2'.
#' @param koh_grid_length grid side dimension of the SOM grid (used only in `kohonen` or `kohonen-dendogram` methods). Defaul is 5.
#' @param koh_rlen        the number of times the complete data set will be presented to the SOM grid
#' (used only in `kohonen` or `kohonen-denddogram` methods). Default is 100.
#' @param koh_alpha_from  starting learning rate. Default is to decline linearly from 0.05 to `koh_alpha_to` over rlen updates.
#' @param koh_alpha_to    ending learning rate. Default is to decline linearly from `koh_alpha_from` to 1.0 over rlen updates.
#' @param .multiproc      (Linux only) numbers of cores to be used in multiprocessing.
#' @return result.tb      a tibble with all cross combinations of parameters' values.
#' @export
sits_segregation_experiments <- function(data.tb, method = "kohonen-dendogram", times = 1,
                                         bands = list("evi", "ndvi", c("evi", "ndvi")),
                                         n_clusters = 5,
                                         grouping_method = c("average", "ward.D2", "complete", "single"),
                                         koh_grid_length = 7,
                                         koh_rlen = 1200,
                                         koh_alpha_from = 0.2,
                                         koh_alpha_to = 0.04, .multiproc = 1){

     # CAUTION: generates all parameters' combinations
     parameters <- .setup_segregation_expr(method = method, times = times, bands = bands,
                                           n_clusters = n_clusters, grouping_method = grouping_method,
                                           koh_grid_length = koh_grid_length, koh_rlen = koh_rlen,
                                           koh_alpha_from = koh_alpha_from, koh_alpha_to = koh_alpha_to)

     # if .multiproc greater than 1, start parallel threads
     if (.multiproc > 1)
          result.lst <- parallel::mcMap(.exec_segregation_expr, list(data.tb), parameters$method, parameters$bands,
                                        parameters$n_clusters, parameters$grouping_method,
                                        parameters$koh_grid_length, parameters$koh_rlen,
                                        parameters$koh_alpha_from, parameters$koh_alpha_to,
                                        mc.cores = .multiproc)

     else
          result.lst <- Map(.exec_segregation_expr, list(data.tb), parameters$method, parameters$bands,
                            parameters$n_clusters, parameters$grouping_method,
                            parameters$koh_grid_length, parameters$koh_rlen, parameters$koh_alpha_from, parameters$koh_alpha_to)

     # composes final result as a tibble
     return (tibble::as_tibble(plyr::rbind.fill(purrr::map(result.lst, tibble::as_tibble))))
}
