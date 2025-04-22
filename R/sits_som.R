#' @title Build a SOM for quality analysis of time series samples
#' @name sits_som_map
#'
#' @author Lorena Alves, \email{lorena.santos@@inpe.br}
#' @author Karine Ferreira. \email{karine.ferreira@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description These function use self-organized maps to perform
#' quality analysis in satellite image time series.
#'
#'
#'
#' @param data           A tibble with samples to be clustered.
#' @param grid_xdim      X dimension of the SOM grid (default = 25).
#' @param grid_ydim      Y dimension of the SOM grid.
#' @param alpha          Starting learning rate
#'                       (decreases according to number of iterations).
#' @param distance       The type of similarity measure (distance). The
#'                       following similarity measurements are supported:
#'                       \code{"euclidean"} and \code{"dtw"}. The default
#'                       similarity measure is \code{"dtw"}.
#' @param rlen           Number of iterations to produce the SOM.
#' @param som_radius     Radius of SOM neighborhood.
#' @param mode           Type of learning algorithm. The
#'                       following learning algorithm are available:
#'                       \code{"online"}, \code{"batch"}, and \code{"pbatch"}.
#'                       The default learning algorithm is \code{"online"}.
#'
#' @note
#' \code{\link[sits]{sits_som_map}} creates a SOM map, where
#' high-dimensional data is mapped into a two dimensional map,
#' keeping the topological relations
#' between data patterns. Each sample is assigned to a neuron,
#' and neurons are placed in the grid based on similarity.
#'
#' \code{\link[sits]{sits_som_evaluate_cluster}} analyses the neurons of
#' the SOM map, and builds clusters based on them. Each cluster is a neuron
#' or a set of neuron categorized with same label.
#' It produces a tibble with the percentage of mixture of classes
#' in each cluster.
#'
#' \code{\link[sits]{sits_som_clean_samples}} evaluates sample quality
#' based on the results of the SOM map.  The algorithm identifies noisy samples,
#' using `prior_threshold` for the prior probability
#' and `posterior_threshold` for the posterior probability.
#' Each sample receives an evaluation tag, according to the following rule:
#' (a) If the prior probability is < `prior_threshold`, the sample is tagged
#' as "remove";
#' (b) If the prior probability is >= `prior_threshold` and the posterior
#' probability is >=`posterior_threshold`, the sample is tagged as "clean";
#' (c) If the prior probability is >= `posterior_threshold` and
#' the posterior probability is < `posterior_threshold`, the sample is tagged as
#' "analyze" for further inspection.
#'
#' The user can define which tagged samples will be returned using the "keep"
#' parameter, with the following options: "clean", "analyze", "remove".
#'
#' To learn more about the learning algorithms, check the
#'       \code{\link[kohonen:supersom]{kohonen::supersom}} function.
#'
#' The \code{sits} package implements the \code{"dtw"} (Dynamic Time
#'       Warping) similarity measure. The \code{"euclidean"} similarity
#'       measurement come from the
#'       \code{\link[kohonen:supersom]{kohonen::supersom (dist.fcts)}} function.
#'
#' @references
#' Lorena Santos, Karine Ferreira, Gilberto Camara, Michelle Picoli,
#' Rolf Simoes, “Quality control and class noise reduction of satellite
#' image time series”. ISPRS Journal of Photogrammetry and Remote Sensing,
#' vol. 177, pp 75-88, 2021. https://doi.org/10.1016/j.isprsjprs.2021.04.014.
#'
#'
#' @return
#' \code{sits_som_map()} produces a list with three members:
#' (1) the samples tibble, with one additional column indicating
#' to which neuron each sample has been mapped;
#' (2) the Kohonen map, used for plotting and cluster quality measures;
#' (3) a tibble with the labelled neurons,
#' where each class of each neuron is associated to two values:
#' (a) the prior probability that this class belongs to a cluster
#' based on the frequency of samples of this class allocated to the neuron;
#' (b) the posterior probability that this class belongs to a cluster,
#' using data for the neighbours on the SOM map.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # create a som map
#'     som_map <- sits_som_map(samples_modis_ndvi)
#'     # plot the som map
#'     plot(som_map)
#'     # evaluate the som map and create clusters
#'     clusters_som <- sits_som_evaluate_cluster(som_map)
#'     # plot the cluster evaluation
#'     plot(clusters_som)
#'     # clean the samples
#'     new_samples <- sits_som_clean_samples(som_map)
#' }
#'
#' @export
sits_som_map <- function(data,
                         grid_xdim = 10L,
                         grid_ydim = 10L,
                         alpha = 1.0,
                         rlen = 100L,
                         distance = "dtw",
                         som_radius = 2L,
                         mode = "online") {
    # set caller to show in errors
    .check_set_caller("sits_som_map")
    # verifies if kohonen package is installed
    .check_require_packages("kohonen")
    # does the input data exist?
    .check_samples_train(data)
    # check distance
    .check_chr_within(distance, c("euclidean", "dtw"))
    # check mode
    .check_chr_within(mode, c("online", "batch", "pbatch"))
    # is are there more neurons than samples?
    n_samples <- nrow(data)
    # check recommended grid sizes
    min_grid_size <- floor(sqrt(5L * sqrt(n_samples))) - 2L
    max_grid_size <- ceiling(sqrt(5L * sqrt(n_samples))) + 2L
    if (grid_xdim < min_grid_size || grid_xdim > max_grid_size)
        warning(.conf("messages", "sits_som_map_grid_size"),
                "(", min_grid_size, " ...", max_grid_size, ")"
        )
    .check_that(n_samples > grid_xdim * grid_ydim)
    # get the time series
    time_series <- .values_ts(data, format = "bands_cases_dates")
    # create the kohonen map
    kohonen_obj <-
        .kohonen_supersom(
            time_series,
            grid = kohonen::somgrid(
                xdim = grid_xdim,
                ydim = grid_ydim,
                topo = "rectangular",
                neighbourhood.fct = "gaussian",
                toroidal = FALSE
            ),
            distance = distance,
            rlen = rlen,
            alpha = alpha,
            mode = mode
        )
    # put id in samples
    data[["id_sample"]] <- seq_len(nrow(data))
    # add id of neuron that the sample was allocated
    data[["id_neuron"]] <- kohonen_obj[["unit.classif"]]
    # get labels and frequencies for the neuron
    labelled_neurons <- .som_label_neurons(
        data,
        kohonen_obj
    )
    # bayesian inference to calculate the posterior prob
    labelled_neurons <- .som_bayes_estimate(
        data,
        kohonen_obj,
        labelled_neurons,
        som_radius
    )
    # get the list of labels for maximum a priori probability
    lab_max <- seq_len(grid_xdim * grid_ydim) |>
        purrr::map(function(neuron_id) {
            labels_neuron <- dplyr::filter(
                labelled_neurons,
                .data[["id_neuron"]] == neuron_id
            )
            # Get the maximum value of the prior probability
            max_prob_index <- which.max(labels_neuron[["prior_prob"]])
            prob_max <- labels_neuron[max_prob_index, ][["prior_prob"]]
            # How many elements there are with the maximumn value?
            number_of_label_max <- which(
                labels_neuron[["prior_prob"]] == prob_max
            )
            label_max_final <- which.max(labels_neuron[["prior_prob"]])
            # if more than one sample has been mapped AND their max are the
            # same, then a posteriori probability is considered
            if (length(number_of_label_max) > 1L) {
                # Get the maximum posterior among the tied classes
                max_post <- max(
                    labels_neuron[number_of_label_max, ][["post_prob"]]
                )
                # Where are the duplicated values?
                label_max_post <- which(
                    labels_neuron[["post_prob"]] == max_post
                )
                # Is this value are in the maximum vector of the prior
                # probability?
                index_prior_max <-
                    which(label_max_post %in% number_of_label_max)
                label_max_final <- label_max_post[index_prior_max]
            } else {
                label_max_final <- which.max(labels_neuron[["prior_prob"]])
            }
            labels_neuron[label_max_final, ][["label_samples"]]
        })
    labels_max <- unlist(lab_max)
    # prepare a color assignment to the SOM map
    kohonen_obj[["neuron_label"]] <- labels_max
    # return the som_map object
    som_map <-
        list(
            data = data,
            labelled_neurons = labelled_neurons,
            som_properties = kohonen_obj
        )
    class(som_map) <- c("som_map", class(som_map))
    return(som_map)
}

#' @title Cleans the samples based on SOM map information
#' @name sits_som_clean_samples
#' @author Lorena Alves, \email{lorena.santos@@inpe.br}
#' @author Karine Ferreira. \email{karine.ferreira@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description
#' \code{sits_som_clean_samples()} evaluates the quality of the samples
#' based on the results of the SOM map.
#'
#' @note
#' The algorithm identifies noisy samples,
#' using `prior_threshold` for the prior probability
#' and `posterior_threshold` for the posterior probability.
#' Each sample receives an evaluation tag, according to the following rule:
#' (a) If the prior probability is < `prior_threshold`, the sample is tagged
#' as "remove";
#' (b) If the prior probability is >= `prior_threshold` and the posterior
#' probability is >=`posterior_threshold`, the sample is tagged as "clean";
#' (c) If the prior probability is >= `posterior_threshold` and
#' the posterior probability is < `posterior_threshold`, the sample is tagged as
#' "analyze" for further inspection.
#' The user can define which tagged samples will be returned using the "keep"
#' parameter, with the following options: "clean", "analyze", "remove".
#'
#' @param som_map              Returned by \code{\link[sits]{sits_som_map}}.
#' @param prior_threshold      Threshold of conditional probability
#'                             (frequency of samples assigned to the
#'                              same SOM neuron).
#' @param posterior_threshold   Threshold of posterior probability
#'                              (influenced by the SOM neighborhood).
#' @param keep      Which types of evaluation to be maintained in the data.
#'
#' @return tibble with an two additional columns.
#' The first indicates if each sample is clean, should be analyzed or
#' should be removed. The second is the posterior probability of the sample.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # create a som map
#'     som_map <- sits_som_map(samples_modis_ndvi)
#'     # plot the som map
#'     plot(som_map)
#'     # evaluate the som map and create clusters
#'     clusters_som <- sits_som_evaluate_cluster(som_map)
#'     # plot the cluster evaluation
#'     plot(clusters_som)
#'     # clean the samples
#'     new_samples <- sits_som_clean_samples(som_map)
#' }
#'
#' @export
sits_som_clean_samples <- function(som_map,
                                   prior_threshold = 0.6,
                                   posterior_threshold = 0.6,
                                   keep = c("clean", "analyze")) {
    # set caller to show in errors
    .check_set_caller("sits_som_clean_samples")
    # Sanity check
    .check_that(inherits(som_map, "som_map"))
    .check_chr_within(
        x = keep,
        within = .conf("som_outcomes"),
        msg = .conf("messages", "sits_som_clean_samples_keep")
    )
    # function to detect of class noise
    .detect_class_noise <- function(prior_prob, post_prob) {
        if (prior_prob >= prior_threshold &
                post_prob >= posterior_threshold)
            return ("clean")
        else if (prior_prob >= prior_threshold &
                 post_prob < posterior_threshold)
            return("analyze")
        else
            "remove"
    }
    # extract tibble from SOM map
    data <- som_map[["data"]] |>
        dplyr::select(
            "longitude",
            "latitude",
            "start_date",
            "end_date",
            "label",
            "cube",
            "time_series",
            "id_sample",
            "id_neuron"
        ) |>
        dplyr::inner_join(som_map[["labelled_neurons"]],
            by = c("id_neuron", label = "label_samples")
        ) |>
        dplyr::mutate(
            eval = .detect_class_noise(
                .data[["prior_prob"]],
                .data[["post_prob"]]
            )
        ) |>
        dplyr::select(
            -"count",
            -"prior_prob"
        ) |>
        dplyr::filter(.data[["eval"]] %in% keep)

    # include class for plotting
    class(data) <- c("som_clean_samples", class(data))

    return(data)
}

#' @title Evaluate cluster
#' @name sits_som_evaluate_cluster
#' @author Lorena Alves, \email{lorena.santos@@inpe.br}
#' @author Karine Ferreira. \email{karine.ferreira@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description
#' \code{sits_som_evaluate_cluster()} produces a tibble with the clusters
#' found by the SOM map. For each cluster, it provides the percentage
#' of classes inside it.
#' @param som_map   A SOM map produced by the som_map() function
#' @return A tibble stating the purity for each cluster
#'
#' @examples
#' if (sits_run_examples()) {
#'     # create a som map
#'     som_map <- sits_som_map(samples_modis_ndvi)
#'     # plot the som map
#'     plot(som_map)
#'     # evaluate the som map and create clusters
#'     clusters_som <- sits_som_evaluate_cluster(som_map)
#'     # plot the cluster evaluation
#'     plot(clusters_som)
#'     # clean the samples
#'     new_samples <- sits_som_clean_samples(som_map)
#' }
#' @export
sits_som_evaluate_cluster <- function(som_map) {
    .check_set_caller("sits_som_evaluate_cluster")
    # Sanity check
    .check_that(inherits(som_map, "som_map"))
    # Get neuron labels
    neuron_label <- som_map[["som_properties"]][["neuron_label"]]
    id_neuron_label_tb <- tibble::tibble(
        id_neuron = seq_along(neuron_label),
        neuron_label = neuron_label
    )
    # Aggregate in the sample dataset the label of each neuron
    data <- som_map[["data"]] |>
        dplyr::inner_join(id_neuron_label_tb, by = "id_neuron")
    # Get only id, label and neuron_label
    temp_data <- unique(dplyr::select(
        data,
        "id_sample",
        "label",
        "neuron_label"
    ))
    # get confusion matrix
    confusion_matrix <- stats::addmargins(table(
        temp_data[["label"]],
        temp_data[["neuron_label"]]
    ))
    # get dimensions (rows and col)
    # rows are  original classes of samples
    dim_row <- dim(confusion_matrix)[[1L]]
    #  cols are  clusters
    dim_col <- dim(confusion_matrix)[[2L]]
    # estimate the purity index per cluster
    cluster_purity_lst <- seq_len(dim_col - 1L) |>
        purrr::map(function(d) {
            current_col <- confusion_matrix[seq_len(dim_row - 1L), d]
            current_col_total <- confusion_matrix[dim_row, d]
            # get mixture percentage per cluster
            mixture_percentage <- as.numeric(
                (current_col / current_col_total) * 100L
            )
            nrows <- length(mixture_percentage)
            current_class_ambiguity <- tibble::tibble(
                id_cluster = rep(d, nrows),
                cluster = rep(colnames(confusion_matrix)[d], nrows),
                class = names(current_col),
                mixture_percentage = mixture_percentage
            )
            # remove lines where mix_percentege is zero
            dplyr::filter(current_class_ambiguity,
                .data[["mixture_percentage"]] > 0.0
            )
        })
    purity_by_cluster <- do.call(rbind, cluster_purity_lst)
    class(purity_by_cluster) <- c(
        "som_evaluate_cluster",
        class(purity_by_cluster)
    )
    purity_by_cluster
}
#' @title Evaluate cluster
#' @name sits_som_remove_samples
#' @author Lorena Alves, \email{lorena.santos@@inpe.br}
#' @author Karine Ferreira. \email{karine.ferreira@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description
#' Remove samples from a given class inside a neuron of another class
#' @param som_map   A SOM map produced by the som_map() function
#' @param som_eval  An evaluation produced by the som_eval() function
#' @param class_cluster Dominant class of a set of neurons
#' @param class_remove  Class to be removed from the neurons of "class_cluster"
#' @return A new set of samples with the desired class neurons remove
#' @examples
#' if (sits_run_examples()) {
#'     # create a som map
#'     som_map <- sits_som_map(samples_modis_ndvi)
#'     # evaluate the som map and create clusters
#'     som_eval <- sits_som_evaluate_cluster(som_map)
#'     # clean the samples
#'     new_samples <- sits_som_remove_samples(som_map, som_eval,
#'                    "Pasture", "Cerrado")
#' }
#' @export
sits_som_remove_samples <- function(som_map,
                                    som_eval,
                                    class_cluster,
                                    class_remove) {

    # get the samples with id_neuron
    data <- som_map$data
    # get the samples by neurons
    neurons <- som_map$labelled_neurons

    neurons_class_1 <- dplyr::filter(neurons,
                                     .data[["label_samples"]] == class_cluster,
                                     .data[["prior_prob"]] > 0.50)
    id_neurons_class_1 <- neurons_class_1[["id_neuron"]]
    # find samples of class2 in neurons of class1
    samples_remove <- dplyr::filter(data,
                                .data[["label"]] == class_remove,
                                .data[["id_neuron"]] %in% id_neurons_class_1)
    # get the id of the samples to be removed
    id_samples_remove <- samples_remove[["id_sample"]]
    # obtain the new samples
    dplyr::filter(data, !(.data[["id_sample"]] %in% id_samples_remove))
}
