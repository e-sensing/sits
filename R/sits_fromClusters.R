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
     # create an empty table
     clusters.tb <-  sits_table()
     # get the band names
     #bands <- sits_bands(data.tb)

     for (i in 1:length(clusters.lst)) {

          # get band name
          band <- tools::file_path_sans_ext(names(clusters.lst[[i]]@centroids)[1])

          # get only the statistically significant clusters' id
          total <- length(clusters.lst[[i]]@cluster)
          uniq_clust <- cbind(unique(clusters.lst[[i]]@cluster))
          partition <- apply(uniq_clust, 1, FUN=function (clu){ sum(clusters.lst[[i]]@cluster==clu) / total })
          relevants <- tibble(clust_id=uniq_clust[,1], partition) %>% filter(partition >= 0.1)

          # get respective clusters' centroids ids
          series_id <- as.integer(tools::file_ext(names(clusters.lst[[i]]@centroids))[relevants$clust_id]) + 1

          # copy centroids values
          new_centroids.tb <- data.tb[series_id,]

          # select only cluster band (drops all other bands)
          for (j in 1:nrow(new_centroids.tb)) {
               new_centroids.tb$time_series[[j]] <- select(new_centroids.tb$time_series[[j]], one_of(c("Index", band)))
          }

          # binds the rows of copied centroids to clusters.tb table
          clusters.tb <- bind_rows(clusters.tb, new_centroids.tb)
     }

     return(clusters.tb)
}
