#------------------------------------------------------------------
#' Returns a list of centroids and its metadata.
#'
#' \code{sits_fromClusters} reads a set of centroids from clusters output.
#' (See: sits_dendogram(...))
#'
#' A sits table has the metadata and data for each time series centroids
#' <longitude, latitude, start_date, end_date, label, coverage, time_series>
#'
#'
#' @param data.tb   a tibble with input data of dtwclust.
#' @param clusters.lst   a list of cluster structure returned from dtwclust. Each element correspond to a band attribute.
#' @return table     tibble  - a SITS table
#' @keywords SITS
#' @family   SITS main functions
#' @examples sits_fromClusters (savanna_s.tb, clusters.lst)
#' @export

sits_fromClusters <-  function (data.tb, clusters.lst) {

     centroids.lst <- clusters.lst %>%
          purrr::map (function (clu) {
               # what is the name of the band?
               band <- tools::file_path_sans_ext(names(clu@centroids)[1])

               # get only the statistically significant clusters' id
               total <- length(clu@cluster)

               # get the unique id of the clusters
               uniq_clust <- unique(clu@cluster)

               # get the proportions for each cluster (0 to 100% of samples)
               partitions <- purrr::map (uniq_clust, function (c) { sum (clu@cluster == c)/total})

               # select only clusters with more than 10% of the samples
               relevants <- tibble(clust_id=uniq_clust, partitions) %>%
                    dplyr::filter(partitions >= 0.1)

               # get respective clusters centroids ids
               series_id <- as.integer(tools::file_ext(names(clu@centroids))[relevants$clust_id]) + 1

               # copy centroids values
               new_centroids.tb <- data.tb[series_id,]

               # select only cluster band (drops all other bands)
               for (j in 1:nrow(new_centroids.tb)) {
                    new_centroids.tb$time_series[[j]] <- dplyr::select(new_centroids.tb$time_series[[j]], one_of(c("Index", band)))
               }
               return (new_centroids.tb)
       })

     return(centroids.lst)
}
