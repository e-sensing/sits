# Retrieve samples used for Mato Grosso study
library(sits)
data("samples_MT_9classes")

samples <- paste0("https://www.dropbox.com/s/addv5lxbpjm85jr/cerrado_13classes_modis_col6.rda?raw=1")

#samples <- paste0("https://www.dropbox.com/s/p1fhzokfwdoum2m/cerrado_13_classes_col6_adj.rda?raw=1")

download.file(samples, destfile = "./cerrado_13classes_col6_adj.rda")
load(file = "./cerrado_13classes_col6_adj.rda")

cerrado_pasture.tb <- sits_select(samples.tb, label %in% c("Cerrado", "Cerrado_Campo", "Cerrado_Rupestre","Pasture"))

# remove confliction samples and keep only "pure" one
dendro <- sits_dendrogram(cerrado_pasture.tb)
sits_plot_dendrogram(cerrado_pasture.tb, dendro)
sits_dendro_bestcut(cerrado_pasture.tb, dendro)
clusters.tb <- sits_cluster(cerrado_pasture.tb, dendro, k = 7)
sits_cluster_frequency(clusters.tb)
cleaned.tb <- sits_cluster_remove(clusters.tb, min_perc = 0.90)
sits_cluster_frequency(cleaned.tb)
new_samples.tb <- cleaned.tb[,1:7]

samples_MT_9classes_reduced <- sits_select(samples_MT_9classes, !(label %in% c("Cerrado", "Pasture")))

samples_Cerrado_11classes <- dplyr::bind_rows(samples_MT_9classes_reduced, new_samples.tb)


