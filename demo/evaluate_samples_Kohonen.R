# satellite image time series package (SITS)
# example of clustering using self-organizin maps
library(sits)
library(inSitu)

# Clustering time series samples using self-organizing maps
som_map <-
    sits_som_map(
        samples_mt_4bands,
        grid_xdim = 10,
        grid_ydim = 10,
        alpha = 1,
        distance = "euclidean"
    )

plot(som_map)

# Remove samples that have  bad quality
new_samples.tb <- sits_som_clean_samples(som_map)

cluster_purity.tb <- sits_som_evaluate_cluster(som_map)
plot(cluster_purity.tb)
