#' @title Clustering a set of satellite image time series using SOM
#' @name sits_cluster_som
#' @author Lorena Alves, \email{lorena.santos@@inpe.br}
#' @author Karine Ferreira. \email{karine.ferreira@@inpe.br}
#' @description This function uses self-organized maps to find clusters in
#' satellite image time series for quality control of the  samples.
#' It calls the function \code{\link[sits]{sits_som_map}} to generate the som map and
#' \code{\link[sits]{sits_clean_samples_som}} to produce a cleaned set of samples.
#' The parameters "grid_xdim", "grid_ydim", "rlen", "distance", "alpha", and
#' "iterations are used by \code{\link[sits]{sits_som_map}} to control how the Kohonen map
#' is generated. The parameters "prob_label_change" and "min_cluster_prob" control how the
#' good quality samples are selected, based on the Kohonen map.
#'
#' @references `kohonen` package (https://CRAN.R-project.org/package=kohonen)
#'
#' @param data.tb        A tibble with samples to be clustered.
#' @param grid_xdim      X dimension of the SOM grid (default = 25).
#' @param grid_ydim      Y dimension of the SOM grid.
#' @param alpha          Starting learning rate, which decreases according to number of iterations.
#' @param distance       The similarity measure (distance).
#' @param iterations     The number of time to run the SOM cluster.
#' @param prob_label_change      Threshold of probability to change the label
#' @param min_cluster_prob       Minimum probability for a good sample to belong to a cluster
#' @return Returns a sits tibble with a new subset of samples and a new column
#' presenting the probability of each sample belongs to a class described in column label.
#' @examples
#' \donttest{
#' # Read a set of samples
#' # Get a new subset of samples evaluated by SOM clustering methods
#' new_samples.tb <- sits_cluster_som(prodes_226_064)
#' }
#' @export

sits_cluster_som <- function(data.tb,  grid_xdim = 10, grid_ydim = 10, alpha = 1.0,
                              distance = "euclidean", iterations = 50,
                              prob_label_change = 0.8, min_cluster_prob = 0.6){

    som_cluster <- sits_som_map(data.tb,  grid_xdim, grid_ydim, alpha,
                                 distance, iterations)

    data_clean <- sits_clean_samples_som(som_cluster, prob_label_change, min_cluster_prob)

    return(data_clean)
}



#' @title Generate a Kohonen map for quality control of the samples and calculate parameters
#' @name sits_som_map
#' @author Lorena Alves, \email{lorena.santos@@inpe.br}
#' @author Karine Ferreira. \email{karine.ferreira@@inpe.br}
#'
#' @description This function uses package self-organized maps to find clusters in
#' satellite image time series to cluster the  samples. Besides that, this function
#' evaluates the quality of each sample through SOM properties, such as evaluate each
#' sample allocated in aneuron-cluster and its neighbourhood.
#' @references `kohonen` package (https://CRAN.R-project.org/package=kohonen)
#'
#' @param data.tb        A tibble with samples to be clustered.
#' @param grid_xdim      X dimension of the SOM grid (default = 25).
#' @param grid_ydim      Y dimension of the SOM grid.
#' @param alpha          Starting learning rate, which decreases according to number of iterations.
#' @param distance       The type of similarity measure (distance).
#' @param iterations     The number of time to run the SOM cluster.
#' @param  ...           Additional parameters to be passed to kohonen::supersom function.
#' @return               Returns a list of sits tibbles containing statistics about the samples and the neuron in each iteration.
#' Besides that, the data.tb is returned with the columns presenting the probability of a sample belongs to a cluster based on a frequency
#' that a sample was allocated in a neuron, and finally a column presenting the total probability of a sample belongs to a cluster using data of neighbourhood.
#'
#' @examples
#' \donttest{
#' # Produce a cluster map
#' som_cluster <- sits_som_map(prodes_226_064)
#' # Clean the samples to get better quality ones
#' clean_samples <- sits_clean_samples_som(som_cluster)
#' }
#' @export
sits_som_map <- function(data.tb,
                             grid_xdim = 10,
                             grid_ydim = 10,
                             alpha = 1.0,
                             distance = "euclidean",
                             iterations = 50)

    {
    # backward compatibility
    if ("coverage" %in% names(data.tb))
        data.tb <- .sits_tibble_rename(data.tb)
    # does the input data exist?
    .sits_test_tibble(data.tb)
    # get the time series
    time_series <- sits_values(data.tb, format = "bands_cases_dates")

    #initialize tibbles
    neurons_info_t.tb <- tibble::as_tibble()
    samples_info_t.tb <- tibble::as_tibble()

    #Create an if here. The user can be enter with only data.tb, then we need
    # to extract the time series here using sits_value
    for (k in 1:iterations)
    {
        kohonen_obj <-
            kohonen::supersom(
                time_series,
                grid = kohonen::somgrid(grid_xdim, grid_ydim , "rectangular", "gaussian", toroidal = FALSE),
                rlen = 100,
                alpha = alpha,
                dist.fcts = distance,
                normalizeDataLayers = TRUE,
                mode = "pbatch"
            )

        #put id in samples here, because the labell needs
        data.tb$id_sample <- 1:rep(dim(data.tb)[1])

        # create a tibble to store the results

        result.tb <- data.tb
        #result.tb$id_sample <- 1:(dim(result.tb)[1])

        #add the in sample the id of neuron that the sample was allocated
        result.tb$id_neuron <- kohonen_obj$unit.classif
        #result.tb$neuron_label <- "neuron_label"

        #get the size grid
        grid_size <- dim(kohonen_obj$grid$pts)[1]

        #create an id for vinicity
        neighborhood_neurons <- rep(1:grid_size)

        #get label
        neurons_labelled <- .sits_labelling_neurons_frequency(result.tb,kohonen_obj)

        neuron_id_class <-  unique(dplyr::select(neurons_labelled,id_neuron, neuron_class))
        duplicated_id_neuron <- (neuron_id_class %>%
                                     dplyr::mutate(dup_neuron = duplicated(id_neuron)) %>%
                                     dplyr::filter(dup_neuron))$id_neuron

        if(length(duplicated_id_neuron) != 0){
            neurons_labelled.tb <- .sits_tie_breaking_neuron_labelling (neurons_labelled, kohonen_obj, duplicated_id_neuron)
        }else{
            neurons_labelled.tb <- neurons_labelled
        }

        #provavelmente alterar a funcao de vizinhança - corrigir_
        neighborhood <- .sits_neighbor_neurons(neurons_labelled.tb, kohonen_obj, duplicated_id_neuron, 1)
        table_neurons <- neighborhood

        #This table contain id and neuron label
        id_neuron_class.tb <- (unique(dplyr::select(neurons_labelled.tb, id_neuron, neuron_class)))

        #Vector containing only the label of each neuron
        neurons_label.vec <- (unique(dplyr::select(neurons_labelled.tb, id_neuron, neuron_class)))$neuron_class

        #Create an integer to correspond a class
        class_vector_int <- as.integer(factor(neurons_label.vec))

        # Matrix with label and its correspondent factor
        neuron_label_id.mt <- cbind(neurons_label.vec, class_vector_int)

        #Por euqnaunto nao ta usando pra nada (apagar)
        table_class_matrix_id <- (unique(neuron_label_id.mt))

        #Get the id of neuron of each sample.
        #This vector must have the same lengh of  samples.tb
        neurons_ <- result.tb$id_neuron

        # The neuron(label) where the samples was allocated
        neuron_label_for_each_sample <- neuron_label_id.mt[neurons_]
        result.tb$neuron_label <- neuron_label_for_each_sample

        kohonen_obj$neuron_label <- neurons_label.vec
        kohonen_obj$neuron_label_id.mt <- neuron_label_id.mt
        kohonen_obj$neighborhood_neurons <- neighborhood_neurons

        #Bayesian filter
        samples_probability_i.tb <- .sits_bayesian_som_neighbourhood(kohonen_obj, neurons_labelled.tb, result.tb)

        #In the last iteration, the neuros will be painted.
        #The map plot will be of the last iteration
        if (iterations == k)
            kohonen_obj <- .sits_paint_neurons(kohonen_obj)

        #until here is common sits_kohonen (improve this function)
        table_samples <- tibble::as_tibble(
            list(
                id_sample = as.integer(samples_probability_i.tb$id_sample),
                original_label = as.character(samples_probability_i.tb$label),
                neuron_label = as.character(samples_probability_i.tb$neuron_label),
                id_neuron = as.integer(samples_probability_i.tb$id_neuron),
                probability = as.numeric(samples_probability_i.tb$probability_cluster),
                iteration = as.integer(k)
            )
        )

        table_neurons$iteration <- k

        #These tables contain informtion with all samples over all iterations
        #id_sample, original_label, neuron_label, id_neuron, Iteration
        #id_neuron, neuron_label and iteration_Neuron
        samples_info_t.tb <- rbind(samples_info_t.tb,table_samples)
        neurons_info_t.tb <- rbind(neurons_info_t.tb,table_neurons)
    }

    #Get the median of the samples during all iterations by id
    id_median.tb <- samples_info_t.tb %>%
        dplyr::group_by(id_sample) %>%
        dplyr::summarise(median = median(probability, na.rm = TRUE))

    cluster_som <- NULL
    cluster_som$samples <- samples_info_t.tb
    cluster_som$neuron <- neurons_info_t.tb

    #This table shows the probability of a sample belongs a classes without look neighbourhood
    sample_cluster_probability.tb <- .sits_cluster_probability(cluster_som)

    cluster_sample_probability <- sample_cluster_probability.tb %>%
        dplyr::inner_join(id_median.tb, by = "id_sample")
    samples_statistics_overall.tb <- dplyr::select(cluster_sample_probability, id_sample,
                                                   som_label = neuron_label,
                                                   cluster_probability = percentage_s,
                                                   total_probability = median)

    samples_statistics_overall.tb <- samples_statistics_overall.tb %>%
        dplyr::mutate(total_probability = total_probability*100)
    samples.tb <- data.tb %>%
        dplyr::inner_join(samples_statistics_overall.tb, by = "id_sample")

    cluster_som$cluster_sample_probability <- cluster_sample_probability

    som_map.lst <-
        structure(
            list(
                statistics_samples = cluster_som,
                som_properties = kohonen_obj,
                samples.tb = samples.tb
            ),
            class = "sits"
        )

    return(som_map.lst)
}

#' @title Clean samples
#' @name sits_clean_samples_som
#' @author Lorena Santos, \email{lorena.santos@@inpe.br}
#' @author Karine Ferreira. \email{karine.ferreira@@inpe.br}
#'
#' @description This function remove sample that do not have good quality based
#' on the statistics of the result of the clustering using SOM.
#'
#' @param som_cluster            A sits tibble returned by \code{\link[sits]{sits_som_map}}
#' @param prob_label_change      Threshold of probability to change the label
#' @param min_cluster_prob       Minimum probability for a good sample to belong to a cluster
#' @return Returns a sits tibble with a new subset of samples and a new column
#' presenting the probability of each sample belongs to a class described in column label.
#'
#' @examples
#' \donttest{
#' # Read a set of samples
#' # Get a new subset of samples evaluated by clustering methods
#' som_cluster <- sits_som_map(prodes_226_064, grid_xdim = 10, grid_ydim = 10, rlen = 100,
#'        distance = "euclidean", iterations = 50)
#' new_samples.tb <- sits_clean_samples_som(som_cluster)
#' }
#' @export

sits_clean_samples_som <- function(som_cluster, prob_label_change = 0.8, min_cluster_prob = 0.6) {

    data.tb <-
        unique(
            dplyr::select(
                som_cluster$samples.tb,
                id_sample,
                longitude,
                latitude,
                start_date,
                end_date,
                label,
                cube,
                time_series
            )
        )
    data.tb$id_sample <- 1:rep(dim(data.tb)[1])

    #Get samples that have different original_label and majority cluster
    confused_samples <-
        dplyr::filter(
            som_cluster$statistics_samples$cluster_sample_probability,
            som_cluster$statistics_samples$cluster_sample_probability$original_label
              != som_cluster$statistics_samples$cluster_sample_probability$cluster
        )

    #Get sample with high value of probability to change the label
    samples_to_change_label <- dplyr::filter(confused_samples, confused_samples$median > prob_label_change)
    id_change_samples <- unique(samples_to_change_label$id_sample)

    #Get the "good samples"
    matching_samples.tb <-
        dplyr::filter(
            som_cluster$statistics_samples$cluster_sample_probability,
            som_cluster$statistics_samples$cluster_sample_probability$original_label
                    == som_cluster$statistics_samples$cluster_sample_probability$cluster &
                som_cluster$statistics_samples$cluster_sample_probability$percentage_s > min_cluster_prob
        )

    id_matching_samples.tb <- unique(matching_samples.tb$id_sample)
    samples_cleaned.tb <- dplyr::filter(data.tb, data.tb$id_sample %in% id_matching_samples.tb)
    samples_median.tb <- unique(dplyr::select(som_cluster$statistics_samples$cluster_sample_probability, id_sample, probability = median))

    if ( length(id_change_samples) != 0)
    {
        samples_to_change_id_cluster <- dplyr::arrange(unique(dplyr::select(samples_to_change_label,
                                                                            id_sample, cluster)), id_sample)

        samples_label_changed.tb <- dplyr::arrange(dplyr::filter(data.tb,
                                                                 data.tb$id_sample %in% id_change_samples),
                                                   id_sample)

        samples_temp.tb <- dplyr::arrange(rbind(samples_cleaned.tb,samples_label_changed.tb ), id_sample)
    } else {
        samples_temp.tb <- dplyr::arrange((samples_cleaned.tb ), id_sample)
    }
    samples.tb <- samples_temp.tb %>%
        dplyr::inner_join(samples_median.tb, by = "id_sample")

    return(samples.tb)

}

#' @title Labelling neurons
#' @name .sits_labelling_neurons_frequency
#' @author Lorena Santos, \email{lorena.santos@@inpe.br}
#'
#' @description Compute the probability of a neuron belongs to a class.
#' However the neuron is labelled using the majority voting.
#' If the neuron is empty, it will labeled as "NoClass".
#'
#' @param data.tb      A SITS tibble with info of samples and kohonen.obj.
#' @param kohonen_obj  The kohonen object returned by kohonen package.
#' @return Returns a tibble with the probability of each neuron belongs to class
#' and a majority label which is the neuron is labelled.
#'
.sits_labelling_neurons_frequency <- function(data.tb, kohonen_obj)
{
    neuron_label.tb <- tibble::as_tibble()
    grid_size <- dim(kohonen_obj$grid$pts)[1]

    #class_vector <- vector()
    for (i in 1:grid_size)
    {
        #Get the id of samples that were allocated in neuron i
        neuron_i <- dplyr::filter(data.tb, data.tb$id_neuron == i)$id_sample
        vb <- neuron_i

        #Check if the neuron is empty or full
        if (length(vb) != 0)
        {
            alloc_neurons_i <- data.tb[neuron_i, ]
            data.vec <- table(alloc_neurons_i$label)

            result.tb <- tibble::as_tibble(list(
                id_neuron = as.numeric(i),
                label_samples = names(data.vec),
                count = as.integer(data.vec),
                freq  = as.numeric(prop.table(data.vec))
            ))

            max_class <- dplyr::summarize(result.tb, max.pt = max(count))

            neuron_class <- dplyr::filter(result.tb, result.tb$count == as.integer(max_class))$label_samples

        } else if (length(vb) == 0){
            neuron_class <- 'Noclass'
            result.tb <- tibble::as_tibble(list(
                id_neuron = as.numeric(i),
                label_samples = "NoSamples",
                count = 0,
                freq  = 0
            ))
        }

        if ((length(neuron_class) != 1 ) &  (length(neuron_class) < dim(result.tb)[1]))
        {
            neuron_class <- append(neuron_class, neuron_class, after = length(neuron_class) + 1)
            neuron_class <- neuron_class[1:dim(result.tb)[1]]
        }
        result.tb$neuron_class <- neuron_class
        neuron_label.tb <- rbind(neuron_label.tb,result.tb)
    }

    return(neuron_label.tb)
}

#' @title Tie breaking neuron labelling
#' @name .sits_tie_breaking_neuron_labelling
#' @author Lorena Santos, \email{lorena.santos@@inpe.br}
#'
#' @description This function tie breaking the neurons that has the same rate to belong a class.
#'
#' @param class_vector  A vector contained the labels of each neuron.
#' @param kohonen_obj   The kohonen object returned by kohonen package.
#' @param duplicated_id_neuron  A vector containing all the id of each neuron that has two majority labels.
#'
#' @return Return a new majority label of a neuron based on its neighbourhood
#'
.sits_tie_breaking_neuron_labelling <- function(class_vector, kohonen_obj, duplicated_id_neuron)
{
    for (i in  1:length(duplicated_id_neuron))
    {
        neurons_neighbors <- which(kohonen::unit.distances(kohonen_obj$grid)[, duplicated_id_neuron[i]] == 1)
        neuron <- dplyr::filter(class_vector, class_vector$id_neuron == duplicated_id_neuron[i])

        #tiebreaker
        viz_neu <- which(kohonen::unit.distances(kohonen_obj$grid)[, duplicated_id_neuron[i]] == 1)

        #remove duplicate neigbour
        datvec = viz_neu[!(viz_neu %in% duplicated_id_neuron)]

        #What is the class of this neihgbour?
        class_neighbors.tb <- dplyr::filter(class_vector, class_vector$id_neuron %in% datvec)
        class_neighbors.tb <- unique(dplyr::select(class_neighbors.tb, id_neuron, neuron_class))

        label_samples_current_neuron <- dplyr::filter(class_vector,
                                            class_vector$id_neuron == duplicated_id_neuron[i])$neuron_class
        summary_frequency <- table(class_neighbors.tb$neuron_class)

        majority_vincinity <- sort(summary_frequency, decreasing = TRUE)[1]

        #Verify if the majority neighbourhood is the same of the label that was tie-break
        verify_labels <- (label_samples_current_neuron %in% names(summary_frequency))
        verify_labels <- all(verify_labels)

        neuron_label <- names(majority_vincinity)
        if (verify_labels == FALSE)
            neuron_label <- label_samples_current_neuron[1]

        if(neuron_label == "Noclass")
            neuron_label <- unique(neuron$label_samples)[1]

        class_vector$neuron_class[class_vector$id_neuron == duplicated_id_neuron[i]] <- neuron_label
    }
    return(class_vector)
}

#' @title Get the neighbor of neurons
#' @name .sits_neighbor_neurons
#' @author Lorena Santos, \email{lorena.santos@@inpe.br}
#'
#' @description This function create a table contained the information about all
#' neighbor of one neuron of radius 1.
#'
#' @param class_vector  A vector contained the labels of each neuron.
#' @param kohonen_obj   The kohonen object returned by kohonen package.
#' @param duplicated_id_neuron  A vector containing all the id of each neuron that has two majority labels.
#' @param radius radius of neigbourhood of each neuron.
#'
#' @return Returns a sits tibble with informations about the vinicity of each neuron.
.sits_neighbor_neurons <- function(class_vector, kohonen_obj, duplicated_id_neuron, radius = 1)
{
    neuron_vicinity.tb <- tibble::as_tibble()
    grid_size <- dim(kohonen_obj$grid$pts)[1]

    for (neuron in 1:grid_size)
    {
        neurons_neighbors <- which(kohonen::unit.distances(kohonen_obj$grid)[, neuron] == radius)

        #Get the neigbourhood classes
        current_class_neighbors.tb <- dplyr::filter(class_vector, class_vector$id_neuron %in% neurons_neighbors)
        current_class_neighbors2.tb <- unique(dplyr::select(current_class_neighbors.tb, id_neuron, neuron_class))

        current_neuron.tb <- dplyr::filter(class_vector, class_vector$id_neuron == neuron)
        count_neighbors <- table(current_class_neighbors2.tb$neuron_class)

        vicinity_temp.tb <- tibble::as_tibble(
            list(
                id_neuron = as.integer(neuron),
                neuron_label = as.character(unique(current_neuron.tb$neuron_class)),
                label_neighbor_neuron = names(count_neighbors),
                f_Neighbor = as.integer(count_neighbors),
                p_Neighbor  = as.numeric(prop.table(count_neighbors)),
                total_Neighbor = as.integer(sum(count_neighbors))
            )
        )
        neuron_vicinity.tb <- rbind(neuron_vicinity.tb, vicinity_temp.tb)
    }

    return(neuron_vicinity.tb)
}

#' @title Create new groups from kohonen maps
#' @name sits_evaluate_som_subgroups
#' @author Lorena Santos, \email{lorena.santos@@inpe.br}
#'
#' @param som_cluster Kohonen map and associated information created by \code{\link[sits]{sits_som_map}}
#'
#' @description Create subgroups from a cluster created by SOM.
#' @return Returns a sits tibble with a new column presenting the subgroups that each sample belongs.
#'
#' @examples
#' \donttest{
#' # Create a Kohonen map
#' som_cluster <- sits_som_map(prodes_226_064)
#' # Create subgroups
#' subgroups.tb <- sits_evaluate_som_subgroups(som_cluster)
#' }
#' @export
sits_evaluate_som_subgroups <- function(som_cluster)
{
    #subgroup.lst <- list()
    cluster_labels <- som_cluster$statistics_samples$cluster_sample_probability$cluster
    class_neurons <- (unique(cluster_labels))
    class_group <- tibble::as_tibble()

    #Get the same information samples of som_properties
    samples_temp.tb <- som_cluster$statistics_samples$samples
    last_iteration <- max(unique(samples_temp.tb$iteration))
    samples_temp.tb <- dplyr::filter(samples_temp.tb, samples_temp.tb$iteration == last_iteration)

    for (k in 1:length(unique(class_neurons)))
    {

        #filter the samples by neuron_label from class_neurons
        current_class <- dplyr::filter(samples_temp.tb,
                                       samples_temp.tb$neuron_label == class_neurons[k])

        #get the neuron's id of where these samples were allocated
        neurons_class <- unique(current_class$id_neuron)

        #get the  weigth's vector of neurons
        codebooks <- som_cluster$som_properties$codes

        #get the number of list that represents evi and ndvi
        #position_evi <- which(names(codebooks) == "evi")
        #position_ndvi <- which(names(codebooks) == "ndvi")

        #get the ndvi weight
        weight_ndvi <- som_cluster$som_properties$codes$ndvi

        #only neurons of current class
        #weight_ndvi.ts <- weight_ndvi[neurons_class, ]

        #get the evi weight
        weight_evi <- som_cluster$som_properties$codes$evi
        weight_evi.ts <- weight_evi[neurons_class, ]
        codes_ndvi_evi <- cbind(weight_ndvi, weight_evi)
        codes_ndvi_evi.ts <- codes_ndvi_evi[neurons_class, ]

        if (length(neurons_class) <= 10)
        {
            min_group <- 1
            max_group <- length(neurons_class) - 1
        } else {
            min_group <- 2
            max_group <- 10
        }

        #other index can be used "sdbw", "sdindex"
        nb_all <-
            NbClust::NbClust(
                t(weight_evi.ts),
                distance = "euclidean",
                min.nc = min_group,
                max.nc = max_group,
                method = "ward.D2",
                index = "dunn"
            )

        number_of_cluster <- nb_all$Best.nc[1]
        distance_atrributes <- proxy::dist(codes_ndvi_evi.ts, distance = "euclidean")
        hc <- stats::hclust(distance_atrributes, "ward.D2")
        cut_hc <- stats::cutree(hc, k = number_of_cluster)
        #id_neurons <- names(cut_hc)

        #Divide groups
        temp.lst <- list()
        for (j in 1:length(unique(cut_hc)))
        {
            #Convert neuron name to integer for example V1 to 1
            neuron_id_string <- which(cut_hc == j)
            neuron_id_string <- names(neuron_id_string)
            neuron_id_int <- as.integer(substring(neuron_id_string, 2))

            #get the weight time series. The evi and ndvi are concatened
            ts <- codes_ndvi_evi[neuron_id_int, ]

            temp.lst[[j]] <- ts
            names(temp.lst)[j] <- paste(" - Group ", j, sep = '')

            #get samples of current group
            s_group <- tibble::as_tibble()

            temporary_samples <- dplyr::filter(current_class,current_class$id_neuron %in% neuron_id_int)
            s_group <- rbind(s_group, temporary_samples)


            s_group_cluster <- s_group
            s_group_cluster$label_subgroup <- paste(class_neurons[k], "_", j,  sep = '')
            class_group <- rbind(class_group, s_group_cluster)
        }

        #subgroup.lst[[k]] <- temp.lst
        #names(subgroup.lst)[k] <- paste(class_neurons[k], sep = '')
    }

    class_group <- dplyr::arrange(dplyr::select(class_group, id_sample, neuron_label, id_neuron, label_subgroup), id_sample)

    samples.tb <- unique(dplyr::select(som_cluster$samples.tb,id_sample,longitude,latitude, start_date,end_date, label, cube, time_series))
    samples.tb$som_label <- class_group$neuron_label
    samples.tb$label_subgroup <- class_group$label_subgroup
    samples.tb$id_neuron <- class_group$id_neuron

    subgroups.tb <-
        structure(list(
            samples_subgroups.tb = samples.tb,
            som_properties = som_cluster$som_properties
        ))
    return(subgroups.tb)
}

#' @title Cluster evaluation
#' @name sits_evaluate_cluster
#' @author Lorena Santos, \email{lorena.santos@@inpe.br}
#'
#' @description This function extracts metrics about the clusters calculating
#' the percentage of mixture between a cluster and others.
#'
#' @param som_cluster Kohonen map and associated information created by \code{\link[sits]{sits_som_map}}
#' @return Returns the confusion matrix and a table with percentage of mixture between the clusters.
#'
#' @examples
#' \donttest{
#' # Produce a Kohonen map for the time series samples
#' som_cluster <- sits_som_map(prodes_226_064)
#' # Extract metrics about the clusters
#' confusion_by_cluster <- sits_evaluate_cluster(som_cluster)
#' # Show confusion matrix
#' confusion_matrix <- confusion_by_cluster$confusion_matrix
#' }
#' @export
sits_evaluate_cluster <- function(som_cluster)
{
    data.tb <- som_cluster$statistics_samples$cluster_sample_probability
    #id_sample original_label neuron_label percentage_s cluster
    #Initialize variables
    id_sample <- NULL
    neuron_label <- NULL

    #get only id, label and neuron_label
    temp.data.tb <- unique(dplyr::select(data.tb, id_sample, original_label, cluster))

    #get label that no have cluster
    no_cluster <- dplyr::setdiff(temp.data.tb$original_label, temp.data.tb$cluster)

    confusion.matrix <- table(temp.data.tb$original_label, temp.data.tb$cluster)

    #get the names of classes (original labels from samples)
    label_table <- rownames(confusion.matrix)

    if (length(no_cluster) > 0) {
        #number of class that dont have cluster
        size_vector <- length(no_cluster)

        #Add columns in confusion matrix
        for (sv in 1:size_vector)
        {
            #position to add column in confusion matrix
            positon_to_add <- which(label_table == no_cluster[sv])

            #trasform in df.array to add a column
            cf <- as.data.frame.array(confusion.matrix)

            #add a column in same position of line
            cf <- tibble::add_column(cf, d = 0, .after = positon_to_add - 1)

            #rename de column to name of cluster
            names(cf)[positon_to_add] <- no_cluster[sv]
            confusion.matrix <- as.table(as.matrix(cf))
        }
    }

    #Add the total number of samples in table
    confusion.matrix.tb <- stats::addmargins(confusion.matrix, FUN = list(Total = sum), quiet = TRUE)

    #get dimensions (rows and col)
    #represents the original classes of samples
    dim_row <- dim(confusion.matrix.tb)[1]

    #represents clusters
    dim_col <- dim(confusion.matrix.tb)[2]

    #get the names of classes (original labels from samples)
    label_table <- rownames(confusion.matrix.tb)[1:dim_row - 1]

    mix_class <- dplyr::tibble()
    for (d in 1:(dim_row - 1))
    {
        #sum the samples of label "d" by cluster.
        #each column represents the cluster where the sample was allocated
        current_row <- confusion.matrix.tb[d, 1:dim_col - 1]
        current_col <- confusion.matrix.tb[1:dim_row - 1, d]

        #get the value total
        current_row_Total <- confusion.matrix.tb[d, dim_col]
        current_col_Total <- confusion.matrix.tb[dim_row, d]

        current_class_ambiguity <-
            tibble::as_tibble(
                list(
                    id_class = as.integer(d),
                    cluster = label_table[d],
                    original_class = names(current_col),
                    mixture_percentage = as.numeric((current_col / current_col_Total) * 100)
                )
            )

        #sort the table in decending order
        current_class_ambiguity <-
            dplyr::arrange(current_class_ambiguity,
                           dplyr::desc(current_class_ambiguity$mixture_percentage))

        #remove lines that mix_percentege is zero
        current_class_ambiguity <-
            dplyr::filter(current_class_ambiguity,
                          current_class_ambiguity$mixture_percentage > 0)

        mix_class <- rbind(mix_class, current_class_ambiguity)
    }

    #Remove the row and column toal and get the confusion matrix from caret,
    info_confusion_matrix <- caret::confusionMatrix(confusion.matrix.tb[1:dim_row - 1, 1:dim_col - 1])

    metrics_by_cluster <-  structure(list(mixture_cluster = mix_class,
                                          confusion_matrix = info_confusion_matrix),
                                     class = "sits")
    return(metrics_by_cluster)
}

#' @title Get cluster probability
#' @name .sits_cluster_probability
#' @author Lorena Santos, \email{lorena.santos@@inpe.br}
#'
#' @description This function summarize the probability a sample belongs to a cluster from the label of the neuron
#' of several iterations.
#'
#' @param data       A sits tibble with info of samples provided by sits_cluster_som.
#' @return Returns a sits tibble with a new column indicating the probability of a sample belongs to a cluster and the majority cluster of a sample
#'

.sits_cluster_probability <- function(data)
{

    #Count samples to extract the percentage of a sample belong to a cluster
    samples_probability.tb <- tibble::as_tibble(unique(data$samples) %>%
                                                    dplyr::group_by(id_sample,original_label,neuron_label) %>%
                                                    dplyr::summarise(count = dplyr::n()) %>%
                                                    dplyr::mutate(percentage_s = (count / sum(count))*100))
    samples_probability.tb <- dplyr::select(samples_probability.tb, id_sample, original_label, neuron_label, percentage_s)
    cluster.temp.tb <- dplyr::group_by(samples_probability.tb, id_sample)
    cluster_majority.tb <- unique(dplyr::filter(cluster.temp.tb, percentage_s == max(percentage_s)))
    cluster.tb <- dplyr::select(cluster_majority.tb, id_sample, cluster = neuron_label)
    cluster_probability <- samples_probability.tb %>% dplyr::inner_join(cluster.tb, by = "id_sample")

    return(cluster_probability)

}


#' @title SOM neighbourhood
#' @name sits_som_cluster_neighbourhood
#' @author Lorena Santos, \email{lorena.santos@@inpe.br}
#'
#' @description This fuction presents the overall statistics of neigbourhood for each sample
#'
#' @param som_cluster Kohonen map and associated information created by \code{\link[sits]{sits_som_map}}
#' @return Returns a sits tibble with a overall percentage of the majority neighbourhood of each sample of all iterations.
.sits_som_cluster_neighbourhood <- function(som_cluster)
{
    s_samples <- som_cluster$statistics_samples
    #number of iterations
    k = length(unique(s_samples$samples$iteration))

    table_sample_neuron <- s_samples$sample %>%
        dplyr::inner_join(s_samples$neuron)
    # Table with the % of neighbourhood of a sample
    # id_sample, label_neighbor_neuron, % vincinity
    info_neighbourhood.tb <- tibble::as_tibble(
        dplyr::arrange(
            table_sample_neuron %>%
                dplyr::group_by(id_sample, label_neighbor_neuron) %>%
                dplyr::summarize(percentage_n = (sum(p_Neighbor) / k) * 100),
            id_sample,
            desc(percentage_n)
        )
    )
    return(info_neighbourhood.tb)
}


#' @title SOM neuron unitary
#' @name .sits_som_cluster_unitary
#' @author Lorena Santos, \email{lorena.santos@@inpe.br}
#'
#' @description This function shows how many times a sample was allocated in a neuron alone.
#' @param som_cluster Kohonen map and associated information created by \code{\link[sits]{sits_som_map}}
#' @return Returns a sits tibble with the amount of times a sample was allocated alone in a neuron during the
#' clustering process.
#'
.sits_som_cluster_unitary <- function(som_cluster)
{
    s_samples <- som_cluster$statistics_samples

    #number of iterations
    k = length(unique(s_samples$samples$iteration))
    table_sample_neuron <- s_samples$sample %>%
        dplyr::inner_join(s_samples$neuron)

    temp.tb <-
        dplyr::arrange(unique(
            dplyr::select(
                table_sample_neuron,
                id_neuron,
                neuron_label,
                samples_label = original_label,
                id_sample,
                iteration
            )
        ), id_neuron)

    table_neuron_samples <- tibble::as_tibble(temp.tb %>%
                              dplyr::group_by(id_neuron,iteration,neuron_label,samples_label) %>%
                              dplyr::summarise(count = dplyr::n()) %>%
                              dplyr::mutate(percentage_n = count / sum(count)))

    neuron_alone <- dplyr::filter (table_neuron_samples, table_neuron_samples$percentage_n == 1 & table_neuron_samples$count == 1)
    table_score_sample_neuron_alone <- temp.tb %>% dplyr::inner_join(neuron_alone)
    alone_by_samples <- tibble::as_tibble(table_score_sample_neuron_alone %>%
                                              dplyr::group_by(id_sample) %>%
                                              dplyr::summarise(count = dplyr::n()) %>%
                                              dplyr::mutate(percentage_n = count /k))

    alone_by_samples.tb <- tibble::as_tibble(table_score_sample_neuron_alone %>%
                                                 dplyr::group_by(id_sample, neuron_label) %>%
                                                 dplyr::summarise(count = dplyr::n()) %>%
                                                 dplyr::mutate(percentage_n = count /100))

    cluster_unitary.tb <- dplyr::arrange(alone_by_samples.tb, desc(percentage_n))

    return(cluster_unitary.tb)

}

#' @title Paint neurons
#' @name .sits_paint_neurons
#' @author Lorena Santos, \email{lorena.santos@@inpe.br}
#'
#' @description This function paint all neurons of the last iteration of SOM in function sits_cluster_som
#' @param kohonen_obj    Object Kohonen, this object contains all parameters of SOM provided by package Kohonen
#' @return Returns kohonen_obj with a new parameter with the colour of the neuron.
#'
.sits_paint_neurons <- function(kohonen_obj)
{
    #set colors to paint neurons
    pallete1 <- .sits_brewerRGB[[.sits_color_name("Set1")]]
    set1 <- utils::head(unique(unlist(pallete1, use.names = FALSE)), -1)

    pallete2 <- .sits_brewerRGB[[.sits_color_name("Accent")]]
    accent <- utils::head(unique(unlist(pallete2, use.names = FALSE)), -1)

    pallete3 <- .sits_brewerRGB[[.sits_color_name("Pastel1")]]
    pastel1 <- utils::head(unique(unlist(pallete3, use.names = FALSE)), -1)

    pallete_neighbors <- c(pastel1, set1, accent)

    #Paint
    for (j in 1:length(unique(kohonen_obj$neuron_label)))
    {
        paint_neurons <- which(kohonen_obj$neuron_label_id.mt[, 2] == j)
        kohonen_obj$neighborhood_neurons[paint_neurons] <- pallete_neighbors [j]
    }

    empty_neuron <- which(kohonen_obj$neuron_label == "No_class")
    kohonen_obj$neighborhood_neurons[empty_neuron] <- " White"
    kohonen_obj$paint_map <-  kohonen_obj$neighborhood_neurons
    kohonen_obj$neuron_label <- kohonen_obj$neuron_label

    return(kohonen_obj)
}


#' @title Paint neurons
#' @name .sits_bayesian_som_neighbourhood
#' @author Lorena Santos, \email{lorena.santos@@inpe.br}
#'
#' @description This function compute the probability of a sample belongs to a cluster using bayesian filter.
#' @param kohonen_obj    Object Kohonen, this object contains all parameters of SOM provided by package Kohonen
#' @param neurons_labelled.tb A tibble containing informations about each neuron.
#' @param result.tb A tibble with samples.
#' @return Returns the probability of a sample belongs to a cluster based on class of neuron and its neighborhood.

.sits_bayesian_som_neighbourhood <- function (kohonen_obj, neurons_labelled.tb, result.tb)
{
    grid_size <- dim(kohonen_obj$grid$pts)[1]
    samples_probability_i.tb <- tibble::as_tibble()


    for (neuron_i in 1:grid_size)
    {
        #get general information about each neuron
        current_id_neighbourhood.tb <-which(kohonen::unit.distances(kohonen_obj$grid)[, neuron_i] == 1)

        #amount of neighourhood
        size_neighbourhood <- length(current_id_neighbourhood.tb)

        data_neuron_i.tb <- dplyr::filter(neurons_labelled.tb, neurons_labelled.tb$id_neuron == neuron_i)

        #if ((data_neuron_i.tb$count[1]) != 0)
        if ((data_neuron_i.tb$neuron_class[1])!= "Noclass")
        {
            for (i in 1:dim(data_neuron_i.tb)[1])
            {
                current_class_neuron.tb <- data_neuron_i.tb[i,]
                #get sample with the same label of current class of neuron
                samples_inside_neuron.tb <-
                    dplyr::filter(
                        result.tb,
                        result.tb$id_neuron == neuron_i &
                            result.tb$label == current_class_neuron.tb$label_samples
                    )

                get_data_neighbouhood_current_class.tb <-
                    dplyr::filter(neurons_labelled.tb,
                                  neurons_labelled.tb$id_neuron %in% current_id_neighbourhood.tb &
                                      neurons_labelled.tb$label_samples == current_class_neuron.tb$label_samples )

                #create vector of probability about the neigbourhood of current neuron
                get_freq <- get_data_neighbouhood_current_class.tb$freq

                #get the amount of zeros probabilites in this neigbourhood for class k
                get_amount_zeros <- size_neighbourhood - length(get_freq)

                neighbourhood_probaility.vec <- c(get_freq, rep(0, get_amount_zeros))
                variance_neighbourhood.vec <- stats::var(neighbourhood_probaility.vec)

                #Bayesian Filter equations
                probability_sample_k <- current_class_neuron.tb$freq

                w1 = (variance_neighbourhood.vec/(1+variance_neighbourhood.vec))*probability_sample_k
                w2 = (1/(1+variance_neighbourhood.vec))*mean(neighbourhood_probaility.vec)
                probality_samples <- w1 + w2

                #add this value in samples that belongs to this neuron
                samples_inside_neuron.tb$probability_cluster <- probality_samples
                samples_probability_i.tb <- rbind(samples_probability_i.tb,samples_inside_neuron.tb)
            }
        }
    }
    return(samples_probability_i.tb)
}

