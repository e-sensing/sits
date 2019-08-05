# satellite image time series package (SITS)
# example of clustering using self-organizin maps
library(sits)
library(kohonen)

# Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#select the bands for classification
if (!requireNamespace("inSitu", quietly = TRUE)) {
    if (!requireNamespace("devtools", quietly = TRUE))
        install.packages("devtools")
    devtools::install_github("e-sensing/inSitu")
}
library(inSitu)

#select the bands for classification
samples <- inSitu::br_mt_1_8K_9classes_6bands

#Create cluster with Self-organizing maps (kohonen)
koh <-
    sits::sits_kohonen(
        samples,
        bands = c("evi", "ndvi"),
        grid_xdim = 25,
        grid_ydim = 25,
        rlen = 100,
        distance = "euclidean",
        neighbourhood.fct = "gaussian",
        mode = "online"

    )
sits_plot_kohonen(koh)

#Analyze the mixture between groups and extract informations about confusion matrix
confusion_by_cluster <- sits_evaluate_cluster(koh$info_samples)
confusion_matrix <- confusion_by_cluster$confusion_matrix
# Show the result of the confusion matrix
sits_plot_cluster_info(confusion_by_cluster, "Confusion by cluster")

#Divide groups according to variations
subgroups <- sits_subgroup(koh)

#Get samples tibble with subgroups
samples_subgroup <- subgroups$samples_subgroup.tb

#Get neurons and their patterns
neurons_subgroup <- subgroups$neurons_subgroup.lst

#Number of subgroups for each class
number_of_subgroup <- lengths(neurons_subgroup)

#Plot subgroups
sits_plot_subgroups(neurons_subgroup)


