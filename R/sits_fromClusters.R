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
#' @examples sits_fromCSV ("mysamples.csv", n_max = 5)
#' @export

sits_fromClusters <-  function (data.tb, clusters.lst) {
     # create an empty table
     clusters.tb <-  sits_table()
     # get the band names
     bands <- sits_bands(data.tb)

     for (i in 1:length(bands)) {
          band <- tools::file_path_sans_ext(names(clusters.lst[[i]]@centroids)[1])
          series_id <- as.integer(tools::file_ext(names(clusters.lst[[i]]@centroids))) + 1
          print(series_id)
          new_centroids.tb <- data.tb[series_id,]
          for (j in 1:nrow(new_centroids.tb)) {
               new_centroids.tb$time_series[[j]] <- select(new_centroids.tb$time_series[[j]], one_of(c("Index", band)))
          }
          clusters.tb <- bind_rows(clusters.tb, new_centroids.tb)
     }

     return(clusters.tb)
}
