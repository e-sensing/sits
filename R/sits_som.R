#' @title Clustering a set of satellite image time series using SOM
#' @name sits_som_cluster
#' @author Lorena Alves, \email{lorena.santos@@inpe.br}
#' @author Karine Ferreira. \email{karine.ferreira@@inpe.br}
#' @description This function uses self-organized maps to find clusters in
#' satellite image time series for quality control of the  samples.
#' Calls \code{\link[sits]{sits_som_map}} to generate the som map and
#' \code{\link[sits]{sits_som_clean_samples}} to produce a clean set of samples.
#' The parameters "grid_xdim", "grid_ydim", "rlen", "distance", "alpha", and
#' "iterations" are used by \code{\link[sits]{sits_som_map}} to control
#' how the Kohonen map is generated.
#' The parameters "prior_threshold" and "posterior_threshold" control
#' how the good quality samples are selected, based on the Kohonen map.
#'
#' @references `kohonen` package (https://CRAN.R-project.org/package=kohonen)
#'
#' @param data           A tibble with samples to be clustered.
#' @param grid_xdim      X dimension of the SOM grid (default = 25).
#' @param grid_ydim      Y dimension of the SOM grid.
#' @param alpha          Starting learning rate,
#'                       which decreases according to number of iterations.
#' @param distance       The similarity measure (distance).
#' @param rlen           How many times dataset will be presented to the SOM.
#' @param prior_threshold      Threshold of priot probability
#'                          (frequency of samples assigned to a same SOM neuron)
#' @param posterior_threshold       Threshold of posterior probability
#'                                  (influenced by the SOM neighborhood)
#' @param som_radius    Radius of neighborhood on the SOM map
#'                      (controls the size of the neighbourhood)
#' @return              A sits tibble with an evaluation column indicating if
#'                      each samples is clean, should be analyzed or
#'                      should be removed, and with a new column indicating
#'                      the posterior probability of the sample
#' @examples
#' \dontrun{
#' # Evaluate the quality of the samples using SOM clustering
#' new_samples <- sits_som_cluster(samples_modis_4bands)
#' }
#' @export
sits_som_cluster <- function(data,
                             grid_xdim = 10,
                             grid_ydim = 10,
                             alpha = 1.0,
                             distance = "euclidean",
                             rlen = 100,
                             prior_threshold = 0.6,
                             posterior_threshold = 0.6,
                             som_radius = 2) {

    # generate a som map where each neuron is associated to one or
    # more samples
    som_map <- sits_som_map(
        data,
        grid_xdim,
        grid_ydim,
        alpha,
        rlen,
        distance,
        som_radius
    )
    plot(som_map)
    # clean the data based on the prior and posterior probabilities
    data_clean <- sits_som_clean_samples(
        som_map,
        prior_threshold,
        posterior_threshold
    )

    return(data_clean)
}
#' @title Generate a Kohonen map for sample quality control
#' @name sits_som_map
#' @author Lorena Alves, \email{lorena.santos@@inpe.br}
#' @author Karine Ferreira. \email{karine.ferreira@@inpe.br}
#'
#' @description This function uses package self-organized maps
#' to find clusters in satellite image time series to cluster the  samples.
#' It also evaluates the quality of each sample using SOM properties.
#'
#' The results is a list with three members:
#' (1) the samples tibble, with one additional column indicating
#' to which neuron it has been mapped;
#' (2) the Kohonen map, used for plotting and cluster quality measures;
#' (3) a tibble with the labelled neurons,
#' where each class of each neuron is associated to two values:
#' (a) the prior probability that this class belongs to a cluster
#' based on the frequency of samples of this class allocated to the neuron;
#' (b) the posterior probability that this class belongs to a cluster,
#' using data for the neighbours on the SOM map.
#'
#' @references `kohonen` package (https://CRAN.R-project.org/package=kohonen)
#'
#' @param data           A tibble with samples to be clustered.
#' @param grid_xdim      X dimension of the SOM grid (default = 25).
#' @param grid_ydim      Y dimension of the SOM grid.
#' @param alpha          Starting learning rate
#'                       (decreases according to number of iterations).
#' @param distance       The type of similarity measure (distance).
#' @param rlen           Number of iterations to produce the SOM.
#' @param som_radius     Radius of SOM neighborhood
#' @param mode           Type of learning algorithm (default = "online")
#' @return               A list of tibbles containing statistics
#'                       about the samples and the neuron in each iteration.

#'
#' @examples
#' \dontrun{
#' # Produce a cluster map
#' som_cluster <- sits_som_map(samples_modis_4bands)
#' # plot the som map
#' plot(som_cluster)
#' # Clean the samples to get better quality ones
#' clean_samples <- sits_som_clean_samples(som_cluster)
#' }
#' @export
sits_som_map <- function(data,
                         grid_xdim = 10,
                         grid_ydim = 10,
                         alpha = 1.0,
                         rlen = 100,
                         distance = "euclidean",
                         som_radius = 2,
                         mode = "online") {

    # set caller to show in errors
    .check_set_caller("sits_som_map")

    # verifies if kohonen package is installed
    if (!requireNamespace("kohonen", quietly = TRUE)) {
        stop("kohonen needed for this function to work.
              Please install it.", call. = FALSE)
    }

    # does the input data exist?
    .sits_tibble_test(data)

    # is are there more neurons than samples?
    n_samples <- nrow(data)
    .check_that(
        n_samples > grid_xdim * grid_ydim,
        msg = paste("number of samples should be",
                    "greater than number of neurons")
    )

    # get the time series
    time_series <- sits_values(data, format = "bands_cases_dates")

    # create the kohonen map
    kohonen_obj <-
        kohonen::supersom(
            time_series,
            grid = kohonen::somgrid(grid_xdim, grid_ydim,
                "rectangular", "gaussian",
                toroidal = FALSE
            ),
            rlen = rlen,
            alpha = alpha,
            dist.fcts = distance,
            normalizeDataLayers = TRUE,
            mode = mode
        )

    # put id in samples
    data$id_sample <- seq_len(nrow(data))

    # add id of neuron that the sample was allocated
    data$id_neuron <- kohonen_obj$unit.classif

    # get labels and frequencies for the neuron
    labelled_neurons <- .sits_som_label_neurons(
        data,
        kohonen_obj
    )

    # bayesian inference to calculate the posterior prob
    labelled_neurons <- .sits_som_bayes_estimate(
        data,
        kohonen_obj,
        labelled_neurons,
        som_radius
    )

    # get the list of labels for maximum a priori probability
    lab_max <- seq(1:(grid_xdim * grid_ydim)) %>%
        purrr::map(function(neuron_id) {
            labels_neuron <- dplyr::filter(labelled_neurons,
                                           id_neuron == neuron_id
            )

            #Get the maximum value of the prior probability
            max_prob_index <- which.max(labels_neuron$prior_prob)
            prob_max <- labels_neuron[max_prob_index, ]$prior_prob

            #How many elements there are with the maximumn value?
            number_of_label_max <- which(labels_neuron$prior_prob == prob_max )
            label_max_final <- nnet::which.is.max(labels_neuron$prior_prob)


            # if more than one sample has been mapped AND their max are the same,
            # then a posteriori probability is considered
            if (length(number_of_label_max) > 1)
            {
                #Get the maximum posterior among the tied classes
                max_post <- max(labels_neuron[number_of_label_max, ]$post_prob)

                # Where are the duplicated values?
                label_max_post <- which(labels_neuron$post_prob == max_post )

                #Is this value are in the maximum vector of the prior probability?
                index_prior_max <- which(label_max_post %in% number_of_label_max == TRUE)
                label_max_final <- label_max_post[index_prior_max]
            }else
                label_max_final <- nnet::which.is.max(labels_neuron$prior_prob)

            return(labels_neuron[label_max_final, ]$label_samples)
        })
    labels_max <- unlist(lab_max)

    # prepare a color assignment to the SOM map
    kohonen_obj$neuron_label <- labels_max
    kohonen_obj <- .sits_som_paint_neurons(kohonen_obj)

    som_map <-
        list(
            data = data,
            labelled_neurons = labelled_neurons,
            som_properties = kohonen_obj
        )
    class(som_map) <- c("som_map", class(som_map))
    return(som_map)
}

#' @title Clean samples
#' @name sits_som_clean_samples
#' @author Lorena Santos, \email{lorena.santos@@inpe.br}
#' @author Karine Ferreira. \email{karine.ferreira@@inpe.br}
#'
#' @description This function evaluate the quality of the samples
#' based on the results of the SOM map. It produces
#' a sits tibble with an evaluation column indicating if
#' each sample is clean, should be analyzed or
#' should be removed, and with a new column indicating
#' the posterior probability of the sample
#'
#' @param som_map                   An object returned
#'                                  by \code{\link[sits]{sits_som_map}}
#' @param prior_threshold      Threshold of conditional probability
#'                (frequency of samples assigned to the same SOM neuron)
#' @param posterior_threshold       Threshold of posterior probability
#'                                  (influenced by the SOM neighborhood)
#' @param keep                 Samples evaluation to be maintained in the data
#'
#' @return List with two sits tibbles.
#'         The first tibble has clean samples
#'         The second has samples that need to be analysed.
#'
#' @note
#'     The algorithm identifies noisy samples, using `prior_threshold` for
#'     the prior probability and `posterior_threshold` for the posterior probability.
#'     Each sample receives an evaluation tag, according to the following rule:
#'     (a) If the prior probability is < `prior_threshold`, the sample is tagged as "remove";
#'     (b) If the prior probability is >= `prior_threshold` and the posterior probability
#'     is >=`posterior_threshold`, the sample is tagged as "clean";
#'     (c) If the prior probability is >= `posterior_threshold` and
#'     the posterior probability is < `posterior_threshold`,
#'     the sample is tagged as "analyze" for further inspection.
#'
#'     The user can define which tagged samples will be returned using the "keep"
#'     parameter, with the following options: "clean", "analyze", "remove".
#'
#' @examples
#' \dontrun{
#' # Read a set of samples
#' # Get a new subset of samples evaluated by clustering methods
#' som_map <- sits_som_map(samples_modis_4bands,
#'     grid_xdim = 10, grid_ydim = 10,
#'     distance = "euclidean"
#' )
#' new_samples <- sits_som_clean_samples(som_map)
#' }
#' @export

sits_som_clean_samples <- function(som_map,
                                   prior_threshold = 0.6,
                                   posterior_threshold = 0.6,
                                   keep = c("clean", "analyze")) {

    # set caller to show in errors
    .check_set_caller("sits_som_clean_samples")

    # Sanity check
    if (!inherits(som_map, "som_map")) {
        message("wrong input data; please run sits_som_map first")
        return(invisible(NULL))
    }
    .check_chr_within(
        x = keep,
        within = c("clean", "analyze", "remove"),
        msg = "invalid keep parameter"
    )

    # original_samples
    data <- som_map$data

    # obtain the evaluation for each sample
    eval_lst <- slider::slide(data, function(row) {
        neuron_id <- row$id_neuron
        sample_probs <-
            som_map$labelled_neurons %>%
            dplyr::filter(id_neuron == neuron_id) %>%
            dplyr::filter(label_samples == row$label)
        if (sample_probs$prior_prob >= prior_threshold &
            sample_probs$post_prob >= posterior_threshold) {
            return("clean")
        } else {
            if (sample_probs$prior_prob >= prior_threshold &
                sample_probs$post_prob < posterior_threshold) {
                  return("analyze")
              } else {
                  return("remove")
              }
        }
    })
    data$eval <- unlist(eval_lst)

    # obtain the posterior probability for each sample
    post_probs <- slider::slide(data, function(row) {
        neuron_id <- row$id_neuron
        sample_prob <-
            som_map$labelled_neurons %>%
            dplyr::filter(id_neuron == neuron_id) %>%
            dplyr::filter(label_samples == row$label)

        return(sample_prob$post_prob)
    })
    data$post_prob <- unlist(post_probs)

    data <- dplyr::filter(data, eval %in% keep)
    return(data)
}

#' @title Evaluate cluster
#' @name sits_som_evaluate_cluster
#' @author Lorena Santos, \email{lorena.santos@@inpe.br}
#' @author Karine Ferreira. \email{karine.ferreira@@inpe.br}
#'
#' @description This function evaluate the clusters created by
#' SOM. Each cluster is a neuron or a set of neuron categorized with same label.
#' It produces a sits tibble indicating the percentage of mixture
#' of classes in each cluster.
#'
#' @param som_map                   An object returned
#'                                  by \code{\link[sits]{sits_som_map}}
#' @return A tibble with the cluster and the percentage of classes
#'  mixtured in each cluster.

#' @examples
#' \dontrun{
#' # Read a set of samples
#' # Get a new subset of samples evaluated by clustering methods
#' som_map <- sits_som_map(samples_modis_4bands,
#'     grid_xdim = 10, grid_ydim = 10,
#'     distance = "euclidean"
#' )
#' cluster_purity <- sits_som_evaluate_cluster(som_map)
#' }
#' @export
sits_som_evaluate_cluster <- function(som_map) {
    # Sanity check
    if (!inherits(som_map, "som_map")) {
        message("wrong input data; please run sits_som_map first")
        return(invisible(NULL))
    }

    # Get neuron labels
    neuron_label <- som_map$som_properties$neuron_label
    id_neuron_label_tb <- tibble::tibble(id_neuron = seq_along(neuron_label),
                                         neuron_label = neuron_label)

    # Agreegate in the sample dataset the label of each neuron
    data <- som_map$data %>% dplyr::inner_join(id_neuron_label_tb)

    # Get only id, label and neuron_label
    temp_data <- unique(dplyr::select(data, id_sample, label, neuron_label))

    # Get sample labels that was not assigned to a cluster
    no_cluster <- dplyr::setdiff(temp_data$label,
                                 temp_data$neuron_label
    )

    confusion_matrix <- stats::addmargins(table(temp_data$label,
                                         temp_data$neuron_label)
    )

    #	get dimensions (rows and col)
    #	represents the original classes of samples
    dim_row <- dim(confusion_matrix)[1]

    # represents clusters
    dim_col <- dim(confusion_matrix)[2]

    cluster_purity_lst <- seq_len(dim_col - 1) %>%
        purrr::map(function(d) {

            current_col <- confusion_matrix[1:dim_row - 1, d]
            current_col_total <- confusion_matrix[dim_row, d]

            mixture_percentage <- as.numeric(
                (current_col / current_col_total) * 100
            )
            nrows <- length(mixture_percentage)
            current_class_ambiguity <- tibble::tibble(
                id_cluster = rep(d, nrows),
                cluster = rep(colnames(confusion_matrix)[d], nrows),
                class = names(current_col),
                mixture_percentage = mixture_percentage
            )
            # remove lines where mix_percentege is zero
            current_class_ambiguity <- dplyr::filter(
                current_class_ambiguity,
                current_class_ambiguity$mixture_percentage > 0
            )

            return(current_class_ambiguity)
        })

    purity_by_cluster <- do.call(rbind, cluster_purity_lst)
    class(purity_by_cluster) <- c("som_evaluate_cluster",
                                  class(purity_by_cluster)
    )
    return(purity_by_cluster)
}


#' @title Label neurons
#' @name .sits_som_label_neurons
#' @keywords internal
#' @author Lorena Santos, \email{lorena.santos@@inpe.br}
#'
#' @description Compute the probability of a neuron belongs to a class.
#' The neuron is labelled using the majority voting.
#' If the neuron is empty, it will labeled as "NoClass".
#'
#' @param data         A SITS tibble with info of samples and kohonen.obj.
#' @param kohonen_obj  The kohonen object returned by kohonen package.
#' @return Tibble with the probability of each neuron belongs to class
#' and a majority label which is the neuron is labelled.
#'
.sits_som_label_neurons <- function(data, kohonen_obj) {
    grid_size <- dim(kohonen_obj$grid$pts)[1]

    labels_lst <- seq_len(grid_size) %>%
        purrr::map(function(i) {
            # Get the id of samples that were allocated in neuron i
            neuron_i <- dplyr::filter(data, data$id_neuron == i)$id_sample

            # 	Check if the neuron is empty or full
            if (length(neuron_i) != 0) {
                alloc_neurons_i <- data[neuron_i, ]
                data_vec <- table(alloc_neurons_i$label)

                label_neuron <- tibble::tibble(
                    id_neuron = as.numeric(i),
                    label_samples = names(data_vec),
                    count = as.integer(data_vec),
                    prior_prob = as.numeric(prop.table(data_vec))
                )
            } else {
                label_neuron <- tibble::tibble(
                    id_neuron = as.numeric(i),
                    label_samples = "NoSamples",
                    count = 0,
                    prior_prob = 0
                )
            }
            return(label_neuron)
        })

    labelled_neurons <- do.call(rbind, labels_lst)
    return(labelled_neurons)
}

#' @title Probability of a sample belongs to a cluster using bayesian filter
#' @name .sits_som_bayes_estimate
#' @keywords internal
#' @author Lorena Santos, \email{lorena.santos@@inpe.br}
#'
#' @description          Computes the probability of a sample belongs
#'                       to a cluster using bayesian filter.
#' @param data             A tibble with samples.
#' @param kohonen_obj      Object that contains all parameters of SOM
#'                         provided by "kohonen" package
#' @param labelled_neurons A tibble containing information about each neuron.
#' @param som_radius       Distance in the SOM map to consider neighbours

#' @return                 Returns the probability of a sample belongs
#'                         to a cluster based on the class of neuron
#'                         and its neighborhood.

.sits_som_bayes_estimate <- function(data,
                                kohonen_obj,
                                labelled_neurons,
                                som_radius) {
    # get the grid size
    grid_size <- dim(kohonen_obj$grid$pts)[1]

    post_probs_lst <- seq_len(grid_size) %>%
        purrr::map(function(neuron_id) {
            # get a list of neighbors of each neuron
            neighbours <-
                unname(
                    which(
                        kohonen::unit.distances(
                            kohonen_obj$grid)[, neuron_id] == som_radius
                    )
                )
            # get information on the samples that are mapped to the neuron
            data_neuron_i <- labelled_neurons %>%
                dplyr::filter(id_neuron == neuron_id)
            if ((data_neuron_i$label_samples[1]) == "Noclass") {
                  return(NULL)
              }
            # calculate the smoothing factor to be used to the posterior prob
            eta <- abs(0.9999999 - max(data_neuron_i$prior_prob))
            # get the posterior probabilities for each label of the neuron
            post_probs <- slider::slide(data_neuron_i, function(row) {
                # get the labels and frequency of all neighbours
                neigh_label <- dplyr::filter(
                    labelled_neurons,
                    id_neuron %in% neighbours &
                        label_samples == row$label_samples
                )
                # how many neighbours with zero probabilities?
                n_zeros <- length(neighbours) - nrow(neigh_label)
                # get the prior probability vector considering the zero probs
                prior_probs <- c(neigh_label$prior_prob, rep(0, n_zeros))
                # neighborhood label frequency variance
                var_neig <- stats::var(prior_probs)
                # neighborhood label frequency mean
                mean_neig <- mean(prior_probs)

                # if the variance and mean are undefined
                # posterior is equal to the prior
                if ((is.na(var_neig)) || (is.nan(mean_neig))) {
                    return(row$prior_prob)
                }
                # mean and variance are valid
                # calculate the estimated variance and mean of the neighbours
                w1 <- (var_neig / (eta + var_neig)) * row$prior_prob
                w2 <- (eta / (eta + var_neig)) * mean_neig
                post_prob <- w1 + w2
                return(post_prob)
            })
            # get the posterior probabilities for the neuron
            post_probs_neu <- unlist(post_probs)
            # add to the list
            return(post_probs_neu)
        })
    # get the posterior probabilities for all the neurons
    post_probs <- unlist(post_probs_lst)

    # include the probabilities in the labeled neurons
    labelled_neurons$post_prob <- post_probs
    # return the updated labeled neurons
    return(labelled_neurons)
}

#' @title Paint neurons
#' @name .sits_som_paint_neurons
#' @keywords internal
#' @author Lorena Santos, \email{lorena.santos@@inpe.br}
#'
#' @description This function paints all neurons  of the last iteration of SOM
#'              in function sits_cluster_som
#'
#' @param kohonen_obj    Object kohonen, this object contains parameters of SOM
#'                       provided by package Kohonen
#' @return               kohonen_obj with a new parameter with the
#'                       colour of the neuron.
#'
.sits_som_paint_neurons <- function(kohonen_obj) {
    # set colors to paint neurons
    pallete1 <- .sits_brewer_rgb[[.sits_brewer_color_name("Set1")]]
    set1 <- utils::head(unique(unlist(pallete1, use.names = FALSE)), -1)

    pallete2 <- .sits_brewer_rgb[[.sits_brewer_color_name("Accent")]]
    accent <- utils::head(unique(unlist(pallete2, use.names = FALSE)), -1)

    pallete3 <- .sits_brewer_rgb[[.sits_brewer_color_name("Pastel1")]]
    pastel1 <- utils::head(unique(unlist(pallete3, use.names = FALSE)), -1)

    # build a mixed pallete with different colors
    pallete <- c(accent, pastel1, set1)

    # unique label
    labels <- unique(kohonen_obj$neuron_label)

    # Paint
    colors <- kohonen_obj$neuron_label %>%
        purrr::map(function(label) {
            if (label == "NoSamples") {
                  return("White")
              } else {
                  return(pallete[which(labels == label)])
              }
        })
    kohonen_obj$paint_map <- unlist(colors)

    return(kohonen_obj)
}
