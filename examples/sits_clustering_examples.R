library (sits)

data.tb <- sits_smooth (savanna.tb, lambda = 1.0)
band <- "evi"
n_clusters <- 4

values.tb <- sits_values_rows (data.tb, band)
clusters  <- dtwclust (values.tb,
                       type     = "hierarchical",
                       k        = n_clusters,
                       distance = "dtw_basic",
                       seed     = 899)

# By default, the dendrogram is plotted in hierarchical clustering
plot(clusters)
# The series and the obtained prototypes can be plotted too
plot (clusters, type = "sc")

# Focusing on the first cluster
plot(clusters, type = "series", clus = 1L)
plot(clusters, type = "centroids")

#create a list of each cluster
cl <- lapply (unique (clusters@cluster), function (clu) data.tb[clusters@cluster == clu,] )
