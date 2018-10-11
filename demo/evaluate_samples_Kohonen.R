library(sits)
library(ggplot2)
library(tibble)
library(zoo)


data.tb<-data("samples_MT_9classes")
data.tb<-samples_MT_9classes

#Get time series
time_series.ts <- sits_values (data.tb, format = "bands_cases_dates")

##To use the DTW distance
sourceCpp(paste(path.package("sits"), "Distances/distance.cpp", sep = "/"))

#Create cluster with Self-organizing maps (kohonen)
koh <-
    sits::sits_kohonen(
        data.tb,
        time_series.ts,
        grid_xdim = 25,
        grid_ydim = 25,
        rlen = 100,
        dist.fcts="euclidean"
    )

#Analyze the mixture between groups and extract informations about confusion matrix
confusion_by_cluster<-sits_metrics_by_cluster(koh$info_samples)
confusion_matrix<-confusion_by_cluster$confusion_matrix

sits_plot_clusterInfo(confusion_by_cluster)

#Divide groups according to variations
subgroups <- sits_subgroup(koh)

