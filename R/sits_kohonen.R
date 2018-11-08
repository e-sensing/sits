#' @title Clustering a set of satellite image time series using SOM
#' @name sits_kohonen
#' @author Lorena Alves, \email{lorena.santos@@inpe.br}
#'
#' @description This function uses package self-organized maps to find clusters in
#' satellite image time series to cluster samples.
#' @references `kohonen` package (https://CRAN.R-project.org/package=kohonen)
#'
#' @param data.tb        A tibble with samples to be clustered.
#' @param time_series    Time series extracted from tibble data.
#' @param bands          Bands to be clustered.
#' @param grid_xdim      X dimension of the SOM grid (default = 5).
#' @param grid_ydim      Y dimension of the SOM grid.
#' @param rlen           Number of times the complete data set will be presented to the SOM grid
#' @param dist.fcts      The similiraty measure (distance).
#' @param alpha          Learning rate, a vector of two numbers indicating the amount of change.
#' @param neighbourhood.fct Type of neighbourhood function (bubble or gaussian).
#' @param  ...           Additional parameters to be passed to kohonen::supersom function.
#' @return  A tibble with the clusters time series or cluster' members time series according to return_member parameter.
#' If return_members are FALSE, the returning tibble will contain a new collumn called `n_members` informing how many members has each cluster.
#' @export
sits_kohonen <- function (data.tb, time_series, bands = NULL, grid_xdim = 25, grid_ydim = 25, rlen = 100, dist.fcts = "euclidean",
                          alpha = 1, neighbourhood.fct = "bubble", ...) {

    # verifies if dtwSat package is installed
    if (!base::requireNamespace("kohonen", quietly = TRUE)) {
        stop("kohonen needed for this function to work. Please install it.", call. = FALSE)
    }

    #set colors to paint neurons
    pallete1 <- .sits_brewerRGB[[.sits_color_name("Set1")]]
    set1 <- utils::head(unique(unlist(pallete1, use.names = FALSE)), -1)

    pallete2 <- .sits_brewerRGB[[.sits_color_name("Accent")]]
    accent <- utils::head(unique(unlist(pallete2, use.names = FALSE)), -1)

    pallete3 <- .sits_brewerRGB[[.sits_color_name("Pastel1")]]
    pastel1 <- utils::head(unique(unlist(pallete3, use.names = FALSE)), -1)

    pallete_neighbors <- c(set1, pastel1, accent)

    # does the input data exist?
    .sits_test_tibble (data.tb)

    # if no bands informed, get all bands available in SITS tibble
    if (purrr::is_null(bands))
        bands <- sits_bands(data.tb)

    # creates the resulting tibble
    cluster.tb <- sits_tibble()

    grid <- kohonen::somgrid(xdim = grid_xdim, ydim = grid_ydim, topo = "rectangular", neighbourhood.fct = neighbourhood.fct)
    kohonen_obj  <- kohonen::supersom (
        time_series,
        grid = grid,
        rlen = rlen,
        alpha = alpha,
        dist.fcts = dist.fcts,
        keep.data = TRUE,
        ...
    )

    # create a tibble to store the results
    result.tb <- data.tb

    #add the in sample the id of neuron that the sample was allocated
    result.tb$id_neuron <- kohonen_obj$unit.classif
    result.tb$neuron_label <- "neuron_label"

    #get the size grid
    grid_size <- dim(kohonen_obj$grid$pts)[1]

    # create an id for each neuron
    init_neuron_id <- rep(1:grid_size)

    #create an id for vinicity
    neighborhood <- rep(1:grid_size)

    #put id in samples here, because the labell needs
    result.tb$id_sample <- 1:(dim(result.tb)[1])

    #get label
    neurons_labelled <- .sits_labelling_neurons (result.tb, grid_size)

    #create an integer to correspond a class
    class_vector_int <- as.integer(factor(neurons_labelled))

    # Matrix with label and the id that correspondent
    class_matrix <- cbind(neurons_labelled, class_vector_int)

    table_class_matrix_id <- (unique(class_matrix))

    #Add at the sample the label of neuron
    neurons_ <- result.tb$id_neuron

    # Which cluster the samples was allocated?
    cluster_sample <- class_matrix[neurons_]
    result.tb$neuron_label <- cluster_sample

    for (j in 1:length(unique(neurons_labelled)))
    {
        paint_neurons <- which(class_matrix[, 2] == j)
        neighborhood[paint_neurons] <- pallete_neighbors [j]
    }

    empty_neuron <- which(neurons_labelled == "Noclass")
    neighborhood[empty_neuron] <- " White"
    kohonen_obj$paint_map <- neighborhood
    kohonen_obj$neurons_labelled <- neurons_labelled

    info_samples_tables <-
        structure(list(kohonen_obj = kohonen_obj, info_samples = result.tb),
                  class = "sits")

    return (info_samples_tables)
}

#' @title Labelling neurons using majority voting
#' @name .sits_labelling_neurons
#' @author Lorena Santos, \email{lorena.santos@@inpe.br}
#'
#' @description Labelling all neurons using the majority voting.
#' If the neuron is empty, it will labeled as "NoClass".
#'
#' @param data.tb    A SITS tibble with info of samples and kohonen.obj.
#' @param grid_size  The size of kohonen map.
#' @return Returns the labels of each neuron.
#'
.sits_labelling_neurons <- function (data.tb, grid_size)
{
    class_vector <- vector()
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
        class_vector[i] <- neuron_class[1]
    }

    return (class_vector)
}

#' @title Evaluate samples
#' @name sits_evaluate_samples
#' @author Lorena Santos, \email{lorena.santos@@inpe.br}
#'
#' @description This function evaluate the samples and extract metrics from SOM cluster.
#' The user can choose the amount of time that the kohonen process will be run.
#' This function allows check if the sample reliable or not
#'
#' @param data.tb        A sits tibble with info of samples.
#' @param time_series    The time series extracted from sits tibble.
#' @param grid_xdim      X dimension of the SOM grid (default = 5).
#' @param grid_ydim      Y dimension of the SOM grid.
#' @param rlen           Number of times the complete data set will be presented to the SOM grid
#' @param alpha          Learning rate, a vector of two numbers indicating the amount of change.
#' @param radius         The radius of kohonen map.
#' @param distance       The similiraty measure.
#' @param iterations     The number of time to run the SOM cluster.
#' @param mode           Type of learning algorithm (online or batch).
#' @return Returns a sits tibble with a new column of label and a table with information about the
#' confiability of each samples.
#' @export
sits_evaluate_samples <- function(data.tb,
                                  time_series,
                                  grid_xdim = 5,
                                  grid_ydim = 5,
                                  rlen = 100,
                                  alpha = c(1),
                                  radius = 6,
                                  distance = "euclidean",
                                  iterations = 1,
                                  mode = "online")
{
    # verifies if dtwSat package is installed
    if (!base::requireNamespace("kohonen", quietly = TRUE)) {
        stop("kohonen needed for this function to work. Please install it.", call. = FALSE)
    }

    #create an id for each sample
    #data.tb$id_sample <- rep(1:dim(data.tb)[1])

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
                rlen = rlen,
                alpha = alpha,
                radius = radius,
                dist.fcts = distance,
                normalizeDataLayers = TRUE
            )

        # create a tibble to store the results
        result.tb <- data.tb

        #put id in samples here, because the labell needs
        result.tb$id_sample <- 1:(dim(result.tb)[1])

        #add the in sample the id of neuron that the sample was allocated
        result.tb$id_neuron <- kohonen_obj$unit.classif
        #result.tb$neuron_label <- "neuron_label"

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
        neurons_ <- result.tb$id_neuron

        # Which cluster the samples was allocated?
        cluster_sample <- class_matrix[neurons_]
        result.tb$neuron_label <- cluster_sample

        #until here is common sits_kohonen (improve this function)
        table_samples <- tibble::as_tibble(list(
            id_sample = as.integer(result.tb$id_sample),
            original_label = as.character(result.tb$label),
            neuron_label = as.character(result.tb$neuron_label),
            id_neuron = as.integer(result.tb$id_neuron),
            iteration = as.integer(k)
        ))

        neighborhood <- .sits_neighbor_neurons(neurons_labelled, kohonen_obj)
        table_neurons <- neighborhood
        table_neurons$iteration <- k

        #These tables contain informtion with all samples over all iterations
        #id_sample, original_label, neuron_label, id_neuron, Iteration
        #id_neuron, neuron_label and iteration_Neuron
        samples_info_t.tb <- rbind(samples_info_t.tb,table_samples)
        neurons_info_t.tb <- rbind(neurons_info_t.tb,table_neurons)
    }

    sample_t <- samples_info_t.tb
    neuron_t <- neurons_info_t.tb
    table_sample_neuron <- sample_t %>% dplyr::inner_join(neuron_t)
    samples_iteration.tb <- samples_info_t.tb

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

    #initialize variables
    id_sample <- 0
    frequency <- 0
    percentage <- 0
    neuron_label <- as.character()
    cluster_label <- as.character()
    #summary samples_cluster_t.tb
    for (i in 1:max(samples_iteration.tb$id_sample))
    {
        #get samples with id equal i from samples_cluster_t.tb
        filter_samples <- dplyr::filter (samples_cluster_t.tb, samples_cluster_t.tb$id_sample == i)

        #count the number of labels per class (summary in a table)
        count_labels_id <- sort(table(filter_samples$neuron_label), decreasing = TRUE)

        summarized_samples.tb <- tibble::as_tibble(list(
            id_sample = as.integer(i),
            original_label = as.character(unique(filter_samples$original_label)),
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
    info_samples_id_cluster <- unique(dplyr::select(info_sample_cluster.tb, id_sample, cluster_label))

    #here sample must have an id (SITS tibble)
    samples_new_label<-info_samples_id_cluster %>% dplyr::inner_join(result.tb [,1:8])

    evaluated_sample <-  structure(
        list(
            table_sample_neuron = table_sample_neuron,
            metrics_by_samples = info_sample_cluster.tb,
            samples.tb = samples_new_label
        ),
        class = "sits"
    )
}

#' @title Get the neighbor of neurons
#' @name .sits_neighbor_neurons
#' @author Lorena Santos, \email{lorena.santos@@inpe.br}
#'
#' @description This function create a table contained the information about all
#' neighbor of one neuron of radius 1.
#'
#' @param class_vector  A vector contained the labels of each neuron.
#' @param koh An object kohonen.
#' @param radius Radius of neigbourhood of each neuron.
#'
#' @return Returns a sits tibble with informations about the vinicity of each neuron.
.sits_neighbor_neurons <- function (class_vector, koh, radius = 1)
{
    neuron_vicinity.tb <- tibble::as_tibble()
    grid_size <- dim(koh$grid$pts)[1]

    for (neuron in 1:grid_size)
    {
        neurons_neighbors <-
            which(kohonen::unit.distances(koh$grid)[, neuron] == radius)
        class_neighbors <- class_vector[neurons_neighbors]

        count_neighbors <- table(class_neighbors)
        result_vizinhos_temporary.tb <- tibble::as_tibble(
            list(
                id_neuron = as.integer(neuron),
                neuron_label = as.character(class_vector[neuron]),
                label_neighbor_neuron = names(count_neighbors),
                f_Neighbor = as.integer(count_neighbors),
                p_Neighbor  = as.numeric(prop.table(count_neighbors))
            )
        )
        neuron_vicinity.tb <-
            rbind(neuron_vicinity.tb, result_vizinhos_temporary.tb)
    }

    return (neuron_vicinity.tb)
}

#' @title Create new groups from kohonen maps
#' @name sits_subgroup
#' @author Lorena Santos, \email{lorena.santos@@inpe.br}
#'
#' @description Create new groups to identify variations in a same group.
#'
#' @param koh  An object with informations about kohonen clustering and samples.

#' @return Returns a sits tibble with subgroups generated by hierarchical clustering.
#' @export
sits_subgroup <- function(koh)
{
    subgroup.lst <- list()
    cluster_label_analysis <- koh$info_samples$neuron_label
    class_neurons <- (unique(cluster_label_analysis))
    class_group <- tibble::as_tibble()

    for (k in 1:length(unique(class_neurons)))
    {
        #filter the samples by neuron_label from class_neurons
        current_class <- dplyr::filter(koh$info_samples,
                                       koh$info_samples$neuron_label == class_neurons[k])

        #get the neuron's id of where these samples were allocated
        neurons_class <- unique(current_class$id_neuron)

        #get the  weigth's vector of neurons
        codebooks <- koh$kohonen_obj$codes

        #get the number of list that represents evi and ndvi
        position_evi <- which(names(codebooks) == "evi")
        position_ndvi <- which(names(codebooks) == "ndvi")

        #get the ndvi weight
        weight_ndvi <- koh$kohonen_obj$codes$ndvi

        #only neurons of current class
        weight_ndvi.ts <- weight_ndvi[neurons_class, ]

        #get the evi weight
        weight_evi <- koh$kohonen_obj$codes$evi
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
        id_neurons <- names(cut_hc)

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
            for (s in 1:length(neuron_id_int))
            {
                temporary_samples <-
                    dplyr::filter(current_class,
                                  current_class$id_neuron == neuron_id_int[s])
                s_group <- rbind(s_group, temporary_samples)
            }

            s_group_cluster <- s_group
            s_group_cluster$label_subgroup <- paste(class_neurons[k], "_", j,  sep = '')
            class_group <- rbind(class_group, s_group_cluster)
        }

        subgroup.lst[[k]] <- temp.lst
        names(subgroup.lst)[k] <- paste(class_neurons[k], sep = '')
    }

    subgroups <- structure(list(samples_subgroup.tb = class_group, neurons_subgroup.lst = subgroup.lst))
    return (subgroups)
}

#' @title Metrics by cluster
#' @name sits_metrics_by_cluster
#' @author Lorena Santos, \email{lorena.santos@@inpe.br}
#'
#' @description This function extracts metrics about the clusters calculating
#' the percentage of mixture between a cluster and others.
#'
#' @param data.tb Tibble containg information about evaluation of samples.
#' @return Returns the confusion matrix and a table with percentage of mixture between the clusters.
#' @export
sits_metrics_by_cluster <- function(data.tb)
{
    #Initialize variables
    id_sample <- NULL
    neuron_label <- NULL

    #get only id, label and neuron_label
    temp.data.tb <- unique(dplyr::select(data.tb, id_sample, label, neuron_label))

    #get label that no have cluster
    no_cluster <- dplyr::setdiff(temp.data.tb$label, temp.data.tb$neuron_label)

    confusion.matrix <- table(temp.data.tb$label, temp.data.tb$neuron_label)

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
            tibble::as.tibble(
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
    info_confusion_matrix <- caret::confusionMatrix(confusion.matrix.tb [1:dim_row - 1, 1:dim_col - 1])

    metrics_by_cluster <-  structure(list(mixture_cluster = mix_class,
                                          confusion_matrix = info_confusion_matrix),
                                     class = "sits")
    return (metrics_by_cluster)
}
