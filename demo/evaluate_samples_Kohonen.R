library(kohonenDTW)
library(dplyr)
library(sits)
library(ggplot2)
library(caret)
library(proxy)
library(zoo)
library(sp)

data.tb<-data("samples_MT_9classes")
data.tb<-samples_MT_9classes

#Get time series
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
sits_plot_kohonen(koh)

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

varesom <- supersom(list(species = specmat.tr, soil = chemmat.tr),
                    somgrid(4, 3, "hexagonal"),
                    dist.fcts = c("BrayCurtis"))


sits_plot_kohonen(koh)

confusion_by_cluster<-sits_metrics_by_cluster(koh$info_samples)
sits_plot_clusterInfo(confusion_by_cluster)

subgroups<-sits_subgroup(koh)

subgroups_bands<-sits_select_bands(subgroups, bands=c("evi", "ndvi"))
Pasture1<-filter(subgroups_bands, subgroups_bands$label2=="Forest_3")
sits_plot(Pasture1,"together")

info_samples<-sits_evaluate_samples( data.tb,
                                     time_series.ts,
                                     grid_xdim = 25,
                                     grid_ydim = 25,
                                     rlen = 200,
                                     radius = 5,
                                     iterations=100
)

metrics_samples<-info_samples$metrics_by_samples
confusion <-
    dplyr::filter(
        metrics_samples,
        metrics_samples$original_label != metrics_samples$cluster_label
    )

data.tb[87,]
filter(subgroups, subgroups$id_sample==87)
sits_plot(data.tb[87,],band = c("evi"))
