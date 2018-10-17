library(kohonen)
library(sits)
library(Rcpp)

data.tb<-data("samples_MT_9classes")
data.tb<-samples_MT_9classes
time_series.ts <- sits_values (data.tb, format = "bands_cases_dates")

#If the DTW distance is chosen, call the distance.cpp from sits package
sourceCpp(paste(path.package("sits"), "Distances/distance.cpp", sep = "/"))

#Create cluster with Self-organizing maps (kohonen)
koh <-
    sits::sits_kohonen(
        data.tb,
        time_series.ts,
        grid_xdim = 25,
        grid_ydim = 25,
        rlen = 5,
        dist.fcts="euclidean"
)

sits_plot_kohonen(koh_exponential_euclidena)
confusion_by_cluster_exponential<-sits_metrics_by_cluster(koh_exponential_euclidena$info_samples)
sits_plot_clusterInfo(confusion_by_cluster_exponential)


koh_linear_euclidean <-
    sits::sits_kohonen(
        data.tb,
        time_series.ts,
        grid_xdim = 25,
        grid_ydim = 25,
        rlen = 50,
        dist.fcts="euclidean",
        radius = 6,
        decay.fcts = "linear"

    )

sits_plot_kohonen(koh_linear_euclidean)
confusion_by_cluster_linear<-sits_metrics_by_cluster(koh_linear_euclidean$info_samples)
sits_plot_clusterInfo(confusion_by_cluster_linear)



informations<-sits_evaluate_samples(data.tb,
                                    time_series.ts,
                                    grid_xdim = 25,
                                    grid_ydim = 25,
                                    rlen = 1,
                                    iterations=5


)





subgroups<-sits_subgroup(koh)


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


info_samples<-sits_evaluate_samples( data.tb,
                                     time_series.ts,
                                     grid_xdim = 25,
                                     grid_ydim = 25,
                                     rlen = 200,
                                     radius = 5,
                                     iterations=100
)

samples_matching.tb <-
    dplyr::filter(
        info_samples$samples,
        info_samples$samples$label == info_samples$samples$cluster_label &
            !(info_samples$samples$id_sample %in% c(13,20,32,65,78,315,658,818,880,895,942,977,994,1123,1425,1596)
    )
    )
    matching_bools <-
        dplyr::filter(
            info_samples$samples,
            info_samples$samples$label == info_samples$samples$cluster_label &
                !(info_samples$samples$id_sample %in% c(13,20,32,65,78,315,658,818,880,895,942,977,994,1123,1425,1596)
                )



matching_metrics<-
    dplyr::filter(
        info_samples$metrics_by_samples,
        info_samples$metrics_by_samples$original_label == info_samples$metrics_by_samples$cluster_label
    )

matching_metrics<-
    dplyr::filter(
        info_samples$metrics_by_samples,
        info_samples$metrics_by_samples$original_label == info_samples$metrics_by_samples$cluster_label &
            !(info_samples$metrics_by_samples$id_sample %in% c(13,20,32,65,78,315,658,818,880,895,942,977,994,1123,1425,1596))
    )


confusion_samples <-
    dplyr::filter(
        confusion_samples,
        confusion_samples$original_label != confusion_samples$cluster_label
    )

unique(confusion_samples$id_sample)
confusion_samples<-info_samples$metrics_by_samples
