# satellite image time series package (SITS)
# example of clustering using self-organizin maps
library(sits)

# load the sitsdata library
if (!requireNamespace("sitsdata", quietly = TRUE)) {
    if (!requireNamespace("devtools", quietly = TRUE)) {
        install.packages("devtools")
    }
    devtools::install_github("e-sensing/sitsdata")
}
library(sitsdata)
data("br_mt_1_8K_9classes_6bands")

# Clustering time series samples using self-organizing maps
som_map <-
    sits_som_map(
        br_mt_1_8K_9classes_6bands,
        grid_xdim = 12,
        grid_ydim = 12,
        alpha = 1,
        distance = "euclidean"
    )

plot(som_map)

# Remove samples that have  bad quality
new_samples <- sits_som_clean_samples(som_map)

cluster_purity <- sits_som_evaluate_cluster(som_map)
plot(cluster_purity)
