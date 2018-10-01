library(kohonen)
library(dplyr)
library(sits)
library (ggplot2)
library(caret)
library(proxy)
library(zoo)
library(sp)
library(wtss)

data.tb<-data("samples_MT_9classes")
data.tb<-samples_MT_9classes
time_series.ts <- sits_values (data.tb, format = "bands_cases_dates")

#Create cluster with Self-organizing maps (kohonen)
koh <-
    sits::sits_kohonen(
        data.tb,
        time_series.ts,
        grid_xdim = 25,
        grid_ydim = 25,
        rlen = 200,
        dist.fcts="euclidean",
        radius = 5

    )

sourceCpp(paste(path.package("sits"), "Distances/distance.cpp", sep = "/"))
koh_teste_distance <-
    sits::sits_kohonen(
        data.tb,
        time_series.ts,
        grid_xdim = 25,
        grid_ydim = 25,
        rlen = 10,
        dist.fcts="dtw",
        radius = 5

    )

sits_plot_kohonen(koh_teste_distance)
confusion_by_cluster2<-sits_metrics_by_cluster(koh_teste_distance$info_samples)
sits_plot_clusterInfo(confusion_by_cluster2)

confusion_by_cluster<-sits_metrics_by_cluster(koh$info_samples)
sits_plot_clusterInfo(confusion_by_cluster)

subgroups<-sits_subgroup(koh)
info_samples<-sits_evaluate_samples( data.tb,
                                     time_series.ts,
                                     grid_xdim = 25,
                                     grid_ydim = 25,
                                     rlen = 200,
                                     radius = 5,
                                     iterations=100
)
