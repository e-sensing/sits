#' @title Cluster a set of satellite image time series using SOM
#' @name sits_kohonen
#' @author Lorena Alves, \email{lorena.santos@@inpe.br}
#'
#' @description This function uses package "kohonen" (self-organized maps) to find clusters in
#' satellite image time series.
#' @references `kohonen` package (https://CRAN.R-project.org/package=kohonen)
#'
#' @param data.tb        a tibble with time series to be clustered
#' @param bands          bands to be clustered.
#' @param grid_xdim      x dimension of the SOM grid (default = 5)
#' @param grid_ydim      y dimension of the SOM grid
#' @param rlen           number of times the complete data set will be presented to the SOM grid
#' @param alpha          learning rate, a vector of two numbers indicating the amount of change.
#'                       Default is to decline linearly from 0.05 to 0.01 over rlen updates.
#' @param  ...           additional parameters to be passed to kohonen::supersom function
#' @return clusters.tb   a tibble with the clusters time series or cluster' members time series according to return_member parameter.
#' If return_members are FALSE, the returning tibble will contain a new collumn called `n_members` informing how many members has each cluster.
#' @export
sits_kohonen <- function (data.tb, ts.tb, bands = NULL, grid_xdim = 25, grid_ydim = 25, rlen = 100,dist.fcts="euclidean",
                          alpha = 1, ...) {

    #estimate the number of neurons from data.tb size

    #paletteVizin<-rainbow(20)
    paletteVizin <- colors()[c(8, 5,12,26,31,30,33,37,43,47,51,52, 56,76,84,118,142,419,625,404,656, 72)] #

    # does the input data exist?
    .sits_test_tibble (data.tb)

    # if no bands informed, get all bands available in SITS tibble
    if (purrr::is_null(bands))
        bands <- sits_bands(data.tb)

    # creates the resulting tibble
    cluster.tb <- sits_tibble()

    # get the values of the various time series for this band group
    #values.tb <- sits_values (data.tb, format = "bands_cases_dates")


    grid <- kohonen::somgrid(xdim = grid_xdim, ydim = grid_ydim, topo = "rectangular")
    kohonen_obj  <- kohonen::supersom (ts.tb, grid=grid,
                                       rlen = rlen, alpha = alpha,
                                       dist.fcts=dist.fcts,
                                       keep.data = TRUE, ...)


    # create a tibble to store the results
    result.tb <- data.tb


    #add the in sample the id of neuron that the sample was allocated
    result.tb$id_neuron <- kohonen_obj$unit.classif
    result.tb$neuron_label<-"neuron_label"


    #get the size grid
    grid_size <- dim(kohonen_obj$grid$pts)[1]

    # create an id for each neuron
    init_neuron_id <- rep(1:grid_size)

    #create an id for vinicity
    neighborhood <- rep(1:grid_size)

    #put id in samples here, because the labell needs
    result.tb$id_sample<-1:(dim(result.tb)[1])

    #get label
    neurons_labelled <- .sits_labelling_neurons (result.tb, grid_size)

    #create an integer to correspond a class
    class_vector_int <- as.integer(factor(neurons_labelled))

    # Matrix with label and the id that correspondent
    class_matrix <- cbind(neurons_labelled, class_vector_int)

    table_class_matrix_id <- (unique(class_matrix))

    #Add at the sample the label of neuron
    Neurons_ <- result.tb$id_neuron

    # Which cluster the samples was allocated?
    Cluster_sample <- class_matrix[Neurons_]
    result.tb$neuron_label <- Cluster_sample

    j=1
    for (j in 1:length(unique(neurons_labelled)))
    {
        paint_neurons <- which(class_matrix[, 2] == j)
        neighborhood[paint_neurons] <- paletteVizin[j]

    }
    Empty_neuron <- which(neurons_labelled == "Noclass")
    neighborhood[Empty_neuron] <- " White"


    kohonen_obj$paint_map<- neighborhood
    kohonen_obj$neurons_labelled<- neurons_labelled

    info_samples_tables <-  structure(list(
          kohonen_obj = kohonen_obj,
          info_samples = result.tb
         ),
        class = "sits")


    #confusion matrix

   return (info_samples_tables)
}

#' @title Labelling neurons using majority voting
#' @name .sits_labelling_neurons
#' @author Lorena Santos, \email{lorena.santos@@inpe.br}
#'
#' @description Labelling all neurons using the majority voting.
#' If the neuron does not have samples  allocated him, to label as "NoClass".
#'
#' @param data.tb  a SITS tibble with info of samples and kohonen.obj
#' @param size_grid the size of kohonen map
#' @return returns TRUE if data.tb has data.
#'
.sits_labelling_neurons <- function (data.tb, grid_size)
{
    i=1
    class_vector <- vector()
    for (i in 1:grid_size)
    {
        #Get the id of samples that were allocated in neuron i
        neuron_i <- dplyr::filter(data.tb, data.tb$id_neuron == i)$id_sample
        vb = neuron_i

        #o neuronio ? vazio?
        # vb <- is.null(neuron_i)
        if (length(vb) != 0)
        {
            alloc_neurons_i <- data.tb[neuron_i,]
            #count_i<- as.matrix(sits_labels(alloc_neurons_i))

            data.vec <- table(alloc_neurons_i$label)
            result.tb <- tibble::as_tibble(list(
                label = names(data.vec),
                count = as.integer(data.vec),
                freq  = as.numeric(prop.table(data.vec))
            ))

            max_class <- dplyr::summarize(result.tb, max.pt = max(count))
            neuron_class <-
                dplyr::filter(result.tb, result.tb$count == as.integer(max_class))$label
        } else if (length(vb) == 0)
        {
            neuron_class <- 'Noclass'

        }
        #this vector contains the label of each neuron
        class_vector[i] <- neuron_class


    }

    return (class_vector)

}
#' @title Evaluate samples
#' @name sits_evaluate_samples
#' @author Lorena Santos, \email{lorena.santos@@inpe.br}
#'
#' @description This function evaluate the samples and extract metrics from SOM cluster.
#' The user can choose the amount of time that the kohonen process will be run.
#'
#' @param data.tb        a SITS tibble with info of samples
#' @param time_series    the time series extracted from SITS tibble
#' @param grid_xdim      x dimension of the SOM grid (default = 5)
#' @param grid_ydim      y dimension of the SOM grid
#' @param rlen           number of times the complete data set will be presented to the SOM grid
#' @param alpha          learning rate, a vector of two numbers indicating the amount of change.
#' @param radius         the radius of kohonen map.
#' @param distance       the similiraty measure (Euclidean or DTW)
#' @param iterations     the number of time to run the SOM cluster
#' @return returns a SITS tibble with a new column of label and a table with information about the sammples
#' @export
sits_evaluate_samples<-function(data.tb,
                                time_series,
                                grid_xdim = 5,
                                grid_ydim = 5,
                                rlen = 100,
                                alpha = c(1),
                                radius = 6,
                                distance = "euclidean",
                                iterations = 1,
                                mode="online")
{
    #create an id for each sample
    data.tb$id_sample<- rep(1:dim(data.tb)[1])

    #Inicializar as tibbles
    neurons_info_t.tb<-as_tibble()
    samples_info_t.tb<-as_tibble()

    #Create an if here. The user can be enter with only data.tb, then we need
    # to extract the time series here using sits_value

    k = 1
    for (k in 1:iterations)
    {
        kohonen_obj <-
            kohonen::supersom(
                time_series,
                grid = somgrid(grid_xdim, grid_ydim , "rectangular", "gaussian", toroidal = FALSE),
                rlen = rlen,
                alpha = alpha,
                radius = radius,
                dist.fcts = distance,
                normalizeDataLayers = TRUE
            )


        # create a tibble to store the results
        result.tb <- data.tb

        #add the in sample the id of neuron that the sample was allocated
        result.tb$id_neuron <- kohonen_obj$unit.classif
        result.tb$neuron_label<-"neuron_label"

        #get the size grid
        grid_size <- dim(kohonen_obj$grid$pts)[1]

        #get label
        neurons_labelled <- .sits_labelling_neurons (result.tb, grid_size)

        #create an integer to correspond a class
        class_vector_int <- as.integer(factor(neurons_labelled))

        # Matrix with label and the id that correspondent
        class_matrix <- cbind(neurons_labelled, class_vector_int)
        table_class_matrix_id <- (unique(class_matrix))


        #Add at the sample the label of neuron
        Neurons_ <- result.tb$id_neuron

        # Which cluster the samples was allocated?
        Cluster_sample <- class_matrix[Neurons_]
        result.tb$neuron_label <- Cluster_sample

        #until here is common sits_kohonen (improve this function)


        table_samples<-tibble::as_tibble(list(
            id_sample= as.integer(result.tb$id_sample),
            original_label = as.character(result.tb$label),
            neuron_label = as.character(result.tb$neuron_label),
            id_neuron = as.integer(result.tb$id_neuron),
            iteration = as.integer(k)

        ))

        neighborhood<-.sits_neighbor_neurons(neurons_labelled, kohonen_obj)
        table_neurons<-neighborhood
        table_neurons$iteration<-k

        #These tables contain informtion with all samples over all iterations
        #id_sample, original_label, neuron_label, id_neuron, Iteration
        #id_neuron, neuron_label and iteration_Neuron
        samples_info_t.tb <- rbind(samples_info_t.tb,table_samples)
        neurons_info_t.tb <- rbind(neurons_info_t.tb,table_neurons)

        #end of loop
    }

    sample_t<-samples_info_t.tb
    neuron_t<-neurons_info_t.tb
    table_sample_neuron<- sample_t%>% dplyr::inner_join(neuron_t)


    samples_iteration.tb<-samples_info_t.tb
    j = 1
    i=1
    samples_cluster_t.tb <- dplyr::tibble()
    info_samples.tb <- dplyr::tibble()
    for (j in 1:max(samples_iteration.tb$id_sample))
    {
        #filter the samples whose id is j
        count_sample <-
            dplyr::filter(samples_iteration.tb,
                          samples_iteration.tb$id_sample == j)
        majority_label <-
            sort(table(count_sample$neuron_label), decreasing = TRUE)[1]

        name_majority_label <- names(majority_label)

        #add into sample whose id is j the column cluster_label (majority_label)
        count_sample$cluster_label <- name_majority_label

        #create table with all samples and de majority label
         samples_cluster_t.tb <- rbind(samples_cluster_t.tb, count_sample)
    }

    #summary samples_cluster_t.tb
    for (i in 1:max(samples_iteration.tb$id_sample))
    {
        #get samples with id = i from samples_cluster_t.tb
        filter_samples <-
            dplyr::filter (samples_cluster_t.tb, samples_cluster_t.tb$id_sample == i)

        #count the number of labels per class (summary in a table)
        count_labels_id <-
            sort(table(filter_samples$neuron_label), decreasing = TRUE)

        summarized_samples.tb <- tibble::as_tibble(list(
            id_sample= as.integer(i),
            original_label= as.character(unique(filter_samples$original_label)),
            neuron_label = names(count_labels_id),
            frequency = as.integer(count_labels_id),
            percentage = as.numeric(prop.table(count_labels_id) * 100)
        ))

        #Each sample has id,original_label, summarized neuron_label
        # with amount e percertagem of neuron_label.
        summarized_samples.tb <-
            dplyr::filter(summarized_samples.tb, summarized_samples.tb$frequency > 0)

        info_samples.tb <- rbind(info_samples.tb, summarized_samples.tb)

    }

    #get id and cluster_label of each sample
    #this tale should have the same size of input data samples.
    samples_id_cluster_label <-
        unique(dplyr::select(samples_cluster_t.tb, id_sample, cluster_label))

    #this table contains summarized samples and
    #cluster_label (defined by majority neuron_label)

    info_sample_cluster.tb <-
        samples_id_cluster_label %>% dplyr::inner_join(info_samples.tb)

    info_sample_cluster.tb <-
        dplyr::select(info_sample_cluster.tb,
                      id_sample,
                      original_label,
                      neuron_label,
                      frequency,
                      percentage,
                      cluster_label)


    #join samples with new cluster
    info_samples_id_cluster<-unique(dplyr::select(info_sample_cluster.tb, id_sample, cluster_label))

    #here sample must have an id (SITS tibble)
    samples_new_label<-info_samples_id_cluster %>% dplyr::inner_join(data.tb)


    #Analyse samples

    #confusion

    confusion_between_samples.temp <-
        dplyr::filter(
            info_sample_cluster.tb,
            info_sample_cluster.tb$original_label != info_sample_cluster.tb$cluster_label
        )

    confusion_between_samples.tb<-
        confusion_between_samples.temp %>%
        dplyr::select(id_sample,
               original_label,
               percentage,
               cluster_label)%>%
        dplyr::group_by(id_sample) %>%
        dplyr::filter(percentage == max(percentage)) %>%
        arrange(desc(percentage))

    matching_between_samples.temp <-
        dplyr::filter(
            info_sample_cluster.tb,
            info_sample_cluster.tb$original_label == info_sample_cluster.tb$cluster_label
        )

    matching_between_samples.tb<-
        matching_between_samples.temp %>%
        dplyr::select(id_sample,
                      original_label,
                      percentage,
                      cluster_label)%>%
        dplyr::group_by(id_sample) %>%
        dplyr::filter(percentage == max(percentage)) %>%
        arrange(desc(percentage))

    #grouping the samples and neuron by id_sample and iteration.
    #The result is a table with the samples in all iterations e the majority
    #vincinity of neuron  where the sample was allocated in each iteration.
    sample_neuron_t<-table_sample_neuron %>%
        dplyr::group_by(id_sample, iteration) %>%
        dplyr::filter(P_Neighbor == max(P_Neighbor)) %>%
        arrange(id_sample)

    #grouping the result of sample_neuron_t and summary the
    #number of neighbor in general and extract the percentage by label.
    count_all_neighbor<-sample_neuron_t %>%
        dplyr::group_by(id_sample, label_neighbor_neuron) %>%
        dplyr::summarise (n = n()) %>%
        dplyr::mutate(freq = (n / sum(n))*100)

    #select the id_sample and the most frequent label neighbor neuron during all
    #iterations. In general_vinicity the percentage of the most frequent neighbor
    #is extracted.

    general_vinicity<-dplyr::select(count_all_neighbor, id_sample,label_neighbor_neuron, freq)%>%
        dplyr::group_by(id_sample) %>%
        dplyr::filter(freq == max(freq)) %>%
        arrange(id_sample)

    Confusion_and_neighboor <-
        general_vinicity %>% dplyr::inner_join(confusion_between_samples.tb)

    confusion.tb<- dplyr::select(
        Confusion_and_neighboor,
        id_sample,
        original_label,
        cluster_label,
        P_sample=percentage,
        label_neighbor_neuron,
        P_Neighbor=(freq)
    )%>%dplyr::arrange(desc(P_sample))


    Matching_and_neighboor <-
        general_vinicity %>% dplyr::inner_join(matching_between_samples.tb)

    matching.tb<- dplyr::select(
        Matching_and_neighboor,
        id_sample,
        original_label,
        cluster_label,
        P_sample=percentage,
        label_neighbor_neuron,
        P_Neighbor=(freq)
    )%>%dplyr::arrange(desc(P_sample))


    evaluated_sample <-  structure(list(
        table_sample_neuron= table_sample_neuron,
        metrics_by_samples = info_sample_cluster.tb,
        samples.tb = samples_new_label
    ),
    class = "sits")
}


#' @title Get the neighbor of neurons
#' @name .sits_neighbor_neurons
#' @author Lorena Santos, \email{lorena.santos@@inpe.br}
#'
#' @description This function create a table contained the information about all
#' neighbor of one neuron of radius 1
#'
#' @param data.tb  a SITS tibble with info of samples and kohonen.obj
#'
#' @return returns a SITS tibble with subgroups generated by hierarchical clustering.
#' @export

.sits_neighbor_neurons <- function (class_vector,koh,radius=1)
{
    neuron_vicinity.tb <- as_tibble()
    grid_size <- dim(koh$grid$pts)[1]
    neuron=1
    for (neuron in 1:grid_size)
    {
        neurons_neighbors<-which(unit.distances(koh$grid)[,neuron] == radius)
        class_neighbors<-class_vector[neurons_neighbors]

        count_neighbors<-table(class_neighbors)
        result_vizinhos_temporary.tb <- tibble::as_tibble(list(
            id_neuron= as.integer(neuron),
            neuron_label= as.character(class_vector[neuron]),
            label_neighbor_neuron =names(count_neighbors),
            F_Neighbor = as.integer(count_neighbors),
            P_Neighbor  = as.numeric(prop.table(count_neighbors))
        ))

        neuron_vicinity.tb <- rbind(neuron_vicinity.tb, result_vizinhos_temporary.tb)

    }
    return (neuron_vicinity.tb)

}

#' @title Create new groups from kohonen maps
#' @name sits_subgroup
#' @author Lorena Santos, \email{lorena.santos@@inpe.br}
#'
#' @description Create new groups to identify variations in a same group
#'
#' @param data.tb  a SITS tibble with info of samples and kohonen.obj
#' @return returns a SITS tibble with subgroups generated by hierarchical clustering.
#' @export
sits_subgroup<-function(koh)
{
    cluster_label_analysis<-koh$info_samples$neuron_label
    class_neurons <- (unique(cluster_label_analysis))

    Class_group <- as_tibble()
    k = 1
    for (k in 1:length(unique(class_neurons)))
    {
        print(class_neurons[k])

        #filter the samples by neuron_label from class_neurons
        current_class <- dplyr::filter(koh$info_samples,
                                       koh$info_samples$neuron_label == class_neurons[k])

        #get the neuron's id of where these samples were allocated
        neurons_class<-unique(current_class$id_neuron)

        #get the  weigth's vector of neurons
        codebooks<-koh$kohonen_obj$codes

        #get the number of list that represents evi and ndvi
        position_evi<-which(names(codebooks) == "evi")
        position_ndvi<-which(names(codebooks) == "ndvi")

        #get the ndvi weight
        weight_ndvi<-koh$kohonen_obj$codes$ndvi

        #only neurons of current class
        weight_ndvi.ts<-weight_ndvi[neurons_class,]

        #get the evi weight
        weight_evi<-koh$kohonen_obj$codes$evi
        weight_evi.ts<-weight_evi[neurons_class,]

        codes_ndvi_evi<-cbind(weight_ndvi,weight_evi)
        codes_ndvi_evi.ts<-codes_ndvi_evi[neurons_class,]


        if (length(neurons_class) <= 10)
        {
            min_group<-1
            max_group<-length(neurons_class)-1

        }else {
            min_group<-2
            max_group<-10
        }

        # indice <- pmatch(index, c("kl", "ch", "hartigan", "ccc",
        #                           "scott", "marriot", "trcovw", "tracew", "friedman",
        #                           "rubin", "cindex", "db", "silhouette", "duda", "pseudot2",
        #                           "beale", "ratkowsky", "ball", "ptbiserial", "gap", "frey",
        #                           "mcclain", "gamma", "gplus", "tau", "dunn", "hubert",
        #                           "sdindex", "dindex", "sdbw", "all", "alllong"))
        # #estimates the number of groups ward.D2 weight_ndvi.ts  codes_ndvi_evi.ts
        nb_all <- NbClust::NbClust(weight_evi.ts, distance = "euclidean", min.nc = min_group,
                                   max.nc = max_group, method = "ward.D2" , index=c("dunn","sdbw","sdindex"))


        number_of_cluster<- nb_all$Best.nc[1]
        number_of_cluster

        distance_atrributes<-proxy::dist(codes_ndvi_evi.ts,  distance = "euclidean")
        hc<-stats::hclust(distance_atrributes,"ward.D2")
        cut_hc<-stats::cutree(hc, k = number_of_cluster)

        id_neurons<-names(cut_hc)
        j = 1
        #separar os grupos
        for (j in 1:length(unique(cut_hc)))
        {
            #transformar o nome do neuronio V1 para 1
            neuron_id_string <- which(cut_hc == j)
            neuron_id_string <- names(neuron_id_string)
            neuron_id_int <- as.integer(substring(neuron_id_string, 2))

            #get the weight time series
            ts<-codes_ndvi_evi[neuron_id_int,]

            if (length(neuron_id_int)==1)
            {

                ts_ndvi.ts<-as.matrix(ts[1:23])
                colnames(ts_ndvi.ts)<- paste ("V",neuron_id_int, sep = "", collapse = NULL)
                #get only evi
                ts_evi.ts<-as.matrix(ts[24:46])
                colnames(ts_evi.ts)<- paste ("V",neuron_id_int, sep = "", collapse = NULL)

                ts_group_ndvi.ts <-zoo::zoo((ts_ndvi.ts))
                ts_group_evi.ts <- zoo::zoo((ts_evi.ts))

            } else{

                #get only ndvi
                ts_ndvi.ts<-ts[,1:23]
                #get only evi
                ts_evi.ts<-ts[,24:46]

                ts_group_ndvi.ts <- zoo::zoo(t(ts_ndvi.ts))
                ts_group_evi.ts <- zoo(t(ts_evi.ts))
            }




            groupts_ndvi.df <- data.frame(value = as.vector(ts_group_ndvi.ts),
                                          time = time(ts_group_ndvi.ts),
                                          neurons = rep(names(ts_group_ndvi.ts),
                                                        each = nrow(ts_group_ndvi.ts)))


            p.ndvi <- ggplot2::ggplot(groupts_ndvi.df, aes(x = time, y = value)) +
                stat_summary(fun.data = "mean_cl_boot",
                             mult = 1,
                             geom = "smooth") + labs(x = "Time", y = "NDVI") + ggtitle(paste(class_neurons[k], " Group ", j , sep = ''))

            ggplot2::ggsave(
                paste(class_neurons[k], "_plot.NDVI", j, ".png" , sep = ''),
                plot = p.ndvi,
                device = "png"
            )



            #-------------------------------- evi -----------------------------------------


            groupts_evi.df <- data.frame(value = as.vector(ts_group_evi.ts),
                                         time = time(ts_group_evi.ts),
                                         neurons = rep(names(ts_group_evi.ts), each = nrow(ts_group_evi.ts)))

            p.evi <- ggplot2::ggplot(groupts_evi.df, aes(x = time, y = value)) +
                stat_summary(fun.data = "mean_cl_boot",
                             mult = 1,
                             geom = "smooth") + labs(x="Time", y="EVI")+ ggtitle(paste(class_neurons[k], " Group ", j , sep = ''))

            ggplot2::ggsave(
                paste(class_neurons[k], "_plot.EVI", j, ".png" , sep = ''),
                plot = p.evi,
                device = "png"
            )


            #-------------------------------------------------------------------------------
            #get samples of current group
            #s_group_cluster<-.sits_get_samples (neuron_id_int,j)
            s = 1
            s_group <- as_tibble()
            for (s in 1:length(neuron_id_int))
            {
                temporary_samples <-
                    dplyr::filter(current_class, current_class$id_neuron == neuron_id_int[s])
                s_group <- rbind(s_group, temporary_samples)
            }
            s_group_cluster <- s_group
            s_group_cluster$label2 <-paste(class_neurons[k], "_", j,  sep = '')

            Class_group <-rbind(Class_group, s_group_cluster)
        }
    }
    return(Class_group)
}


#' @title Metrics by cluster
#' @name sits_metrics_by_cluster
#' @author Lorena Santos, \email{lorena.santos@@inpe.br}
#'
#' @description Create new groups to identify variations in a same group
#'
#' @param data.tb info_samples_cluster.tb
#' @param size_grid the size of kohonen map
#' @return returns TRUE if data.tb has data.
#' @export

sits_metrics_by_cluster<-function(info_sample_cluster.tb)
{

    #get only id, label and neuron_label
    temp.data.tb<-unique(dplyr::select(info_sample_cluster.tb, id_sample, label, neuron_label))


    #get label that no have cluster
    no_cluster<-dplyr::setdiff(temp.data.tb$label, temp.data.tb$neuron_label)

    confusion.matrix<-table(temp.data.tb$label, temp.data.tb$neuron_label)

    #get the names of classes (original labels from samples)
    Label_table<-rownames(confusion.matrix)

    if (length(no_cluster)>0){

        #number of class that dont have cluster
        size_vector<-length(no_cluster)

        #Add columns in confusion matrix
        sv=1
        for(sv in 1:size_vector)
        {
            #position to add column in confusion matrix
            positon_to_add<-which(Label_table==no_cluster[sv])

            #trasform in df.array to add a column
            cf<-as.data.frame.array(confusion.matrix)

            #add a column in same position of line
            cf<-add_column(cf, d = 0, .after =positon_to_add-1 )

            #rename de column to name of cluster
            names(cf)[positon_to_add]<-no_cluster[sv]
            confusion.matrix<-as.table(as.matrix(cf))

        }

    }

    #Add the total number of samples in table
    confusion.matrix.tb <- addmargins(confusion.matrix, FUN = list(Total = sum), quiet = TRUE)

    #get dimensions (rows and col)

    #represents the original classes of samples
    dim_row <-dim(confusion.matrix.tb)[1]

    #represents clusters
    dim_col <-dim(confusion.matrix.tb)[2]

    #get the names of classes (original labels from samples)
    Label_table<-rownames(confusion.matrix.tb)[1:dim_row-1]

    Mix_class <- dplyr::tibble()
    d = 1
    for (d in 1:(dim_row - 1))
    {
        #sum the samples of label "d" by cluster.
        #each column represents the cluster where the sample was allocated
        current_row <- confusion.matrix.tb[d, 1:dim_col-1]

        #get the value total
        current_row_Total <- confusion.matrix.tb[d, dim_col]

        current_class_ambiguity <-
            tibble::as.tibble(list(
                id_class= as.integer(d),
                cluster=  Label_table[d],
                original_class = names(current_row),
                mixture_percentage = as.numeric((current_row / current_row_Total) *
                                                    100)
            ))

        #sort the table in decending order
        current_class_ambiguity<- dplyr::arrange(current_class_ambiguity,
                                                 desc(current_class_ambiguity$mixture_percentage))

        #remove lines that mix_percentege is zero
        current_class_ambiguity <-
            dplyr::filter(current_class_ambiguity,
                          current_class_ambiguity$mixture_percentage > 0)


        Mix_class<-rbind(Mix_class,current_class_ambiguity)

    }


    palette <- colors()[c(8, 5,12,26,31,30,33,37,43,47,51,52, 56,76,84,118,142,419,625,404,656, 72)]

    info_confusion_matrix <-
        caret::confusionMatrix(confusion.matrix.tb[1:dim_row,1:dim_col])


    return (Mix_class)

}


#' @title Mixed samples
#' @name sits_confusion_by_samples
#' @author Lorena Santos, \email{lorena.santos@@inpe.br}
#'
#' @description Extract metrics about confusion between the samples
#'
#' @param data.tb info_samples_cluster.tb
#' @param size_grid the size of kohonen map
#' @return returns TRUE if data.tb has data.
#' @export

sits_confusion_by_samples<-function(info_samples.tb)
{

    metrics_samples<-info_samples$metrics_by_samples
    confusion <-
        dplyr::filter(
            metrics_samples,
            metrics_samples$original_label != metrics_samples$cluster_label
        )

    Confusion_desc<-confusionok %>% select(id_sample, original_label,percentage, cluster_label)%>%
        group_by(id_sample) %>%
        filter(percentage == max(percentage)) %>%
        arrange(desc(percentage))

}
#
# sits_plot_kohonen <-function (data.tb)
# {
#
#     p <-
#         ggplot2::ggplot() + geom_bar(
#             aes(
#                 y = Mix_class$mixture_percentage,
#                 x = Mix_class$cluster,
#                 fill = Mix_class$original_class
#             ),
#             data = Mix_class,
#             stat = "identity",
#             position = position_dodge()
#         ) + theme_minimal() + theme(axis.text.x = element_text(angle = 60, hjust = 1)) +  labs(x = "Clusters", y="Percentage of Mixture", colour="cluster")
#     p + scale_fill_manual("Labels", values = palette)
#
#
# }
