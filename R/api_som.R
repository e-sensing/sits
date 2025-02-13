#' @title Label neurons
#' @name .som_label_neurons
#' @keywords internal
#' @noRd
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
.som_label_neurons <- function(data, kohonen_obj) {
    grid_size <- dim(kohonen_obj[["grid"]][["pts"]])[[1]]

    labels_lst <- seq_len(grid_size) |>
        purrr::map(function(i) {
            # Get the id of samples that were allocated in neuron i
            neuron_c <- dplyr::filter(data, .data[["id_neuron"]] == i)
            neuron_i <- neuron_c[["id_sample"]]

            # 	Check if the neuron is empty or full
            if (length(neuron_i) != 0) {
                alloc_neurons_i <- data[neuron_i, ]
                data_vec <- table(alloc_neurons_i[["label"]])

                label_neuron <- tibble::tibble(
                    id_neuron = as.numeric(i),
                    label_samples = names(data_vec),
                    count = as.integer(data_vec),
                    prior_prob = as.numeric(prop.table(data_vec))
                )
            } else {
                label_neuron <- tibble::tibble(
                    id_neuron = as.numeric(i),
                    label_samples = "No_Samples",
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
#' @name .som_bayes_estimate
#' @keywords internal
#' @noRd
#' @author Lorena Santos, \email{lorena.santos@@inpe.br}
#'
#' @description          Computes the probability of a sample belongs
#'                       to a cluster using bayesian filter.
#' @param data             A tibble with samples.
#' @param kohonen_obj      Object that contains all parameters of SOM
#'                         provided by "kohonen" package
#' @param labelled_neurons A tibble containing information about each neuron.
#' @param som_radius       Distance in the SOM map to consider neighbours

#' @return                 The probability of a sample belongs
#'                         to a cluster based on the class of neuron
#'                         and its neighborhood.

.som_bayes_estimate <- function(data,
                                kohonen_obj,
                                labelled_neurons,
                                som_radius) {
    # get the grid size
    grid_size <- dim(kohonen_obj[["grid"]][["pts"]])[[1]]

    post_probs_lst <- seq_len(grid_size) |>
        purrr::map(function(neuron_id) {
            # get a list of neighbors of each neuron
            neighbours <-
                unname(
                    which(
                        kohonen::unit.distances(
                            kohonen_obj[["grid"]]
                        )[, neuron_id] == som_radius
                    )
                )
            # get information on the samples that are mapped to the neuron
            data_neuron_i <- labelled_neurons |>
                dplyr::filter(.data[["id_neuron"]] == neuron_id)
            if ((data_neuron_i[["label_samples"]][[1]]) == "Noclass") {
                return(NULL)
            }
            # calculate the smoothing factor to be used to the posterior prob
            eta <- abs(0.9999999 - max(data_neuron_i[["prior_prob"]]))
            # get the posterior probabilities for each label of the neuron
            post_probs <- slider::slide(data_neuron_i, function(row) {
                # get the labels and frequency of all neighbours
                neigh_label <- dplyr::filter(
                    labelled_neurons,
                    .data[["id_neuron"]] %in% neighbours,
                    .data[["label_samples"]] == row[["label_samples"]]
                )
                # how many neighbours with zero probabilities?
                n_zeros <- length(neighbours) - nrow(neigh_label)
                # get the prior probability vector considering the zero probs
                prior_probs <- c(neigh_label[["prior_prob"]], rep(0, n_zeros))
                # neighborhood label frequency variance
                var_neig <- stats::var(prior_probs)
                # neighborhood label frequency mean
                mean_neig <- mean(prior_probs)

                # if the variance and mean are undefined
                # posterior is equal to the prior
                if ((is.na(var_neig)) || (is.nan(mean_neig))) {
                    return(row[["prior_prob"]])
                }
                # mean and variance are valid
                # calculate the estimated variance and mean of the neighbours
                w1 <- (var_neig / (eta + var_neig)) * row[["prior_prob"]]
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
    labelled_neurons[["post_prob"]] <- post_probs
    # return the updated labeled neurons
    return(labelled_neurons)
}

#' @title Paint neurons
#' @name .som_paint_neurons
#' @keywords internal
#' @noRd
#' @author Lorena Santos, \email{lorena.santos@@inpe.br}
#'
#' @description This function paints all neurons
#'              of the last iteration of SOM
#'              in function sits_cluster_som
#'
#' @param kohonen_obj    Object kohonen
#'                       provided by package Kohonen
#' @return               kohonen_obj with a new parameter with the
#'                       colour of the neuron.
#'
.som_paint_neurons <- function(kohonen_obj) {
    # assign one color per unique label

    colors <- .colors_get(
        labels = kohonen_obj[["neuron_label"]],
        legend = NULL,
        palette = "Set3",
        rev = TRUE
    )
    labels <- kohonen_obj[["neuron_label"]]
    kohonen_obj[["paint_map"]] <- unname(colors[labels])

    return(kohonen_obj)
}

#' @title Adjacency matrix
#' @name .som_adjacency
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function calculates the adjacency matrix for the SOM
#'
#' @param som_map        kohonen_map
#' @return               adjacency matrix with the distances btw neurons.
#'
.som_adjacency <- function(som_map) {
    koh <- som_map$som_properties
    adjacency <- proxy::as.matrix(proxy::dist(koh$codes$NDVI, method = "dtw"))
}

#' @title Transform SOM map into sf object.
#' @name .som_to_sf
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function transforms a SOM map into an sf object
#'
#' @param som_map        kohonen_map
#' @return               sf object with same geometry and attributes as SOM map
#'
.som_to_sf <- function(som_map) {
    koh <- som_map$som_properties

    grid_idx <- 0

    neuron_ids <- koh$grid$pts
    neuron_pols <- purrr::map(1:nrow(neuron_ids), function(id) {
        x <- neuron_ids[id,"x"]
        y <- neuron_ids[id,"y"]
        pol <- rbind(c((x - 1), (y - 1)),
                     c(x, (y - 1)),
                     c(x, y),
                     c((x - 1), y),
                     c((x - 1), (y - 1)))
        pol = sf::st_polygon(list(pol))
        return(pol)
    })
    neuron_attr <- as.data.frame(koh$codes)
    neuron_attr$geometry <- sf::st_sfc(neuron_pols)

    sf_neurons <- sf::st_sf(neuron_attr, geometry = neuron_attr$geometry)
    return(sf_neurons)
}
#' @title Use SOM to undersample classes with many samples
#' @name .som_undersample
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function uses a SOM to reduce the number of samples
#' per class
#'
#' @param samples         Training data
#' @param classes_under   Classes to undersample
#' @param n_samples_under Number of samples for each class
#' @param multicores      Number of cores
#' @return                Samples for chosen classes with reduced number
#'
.som_undersample <- function(samples, classes_under,
                             n_samples_under, multicores){
    # for each class, select some of the samples using SOM
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop())
    samples_under_new <- .parallel_map(classes_under, function(cls) {
        # select the samples for the class
        samples_cls <- dplyr::filter(samples, .data[["label"]] == cls)
        # set the dimension of the SOM grid
        grid_dim <- ceiling(sqrt(n_samples_under / 4))
        # build the SOM map
        som_map <- suppressWarnings(
            sits_som_map(
                samples_cls,
                grid_xdim = grid_dim,
                grid_ydim = grid_dim,
                distance = "dtw",
                rlen = 10,
                mode = "pbatch"
            )
        )
        # select samples on the SOM grid using the neurons
        samples_under <- som_map[["data"]] |>
            dplyr::group_by(.data[["id_neuron"]]) |>
            dplyr::slice_sample(n = 4, replace = TRUE) |>
            dplyr::ungroup()
        return(samples_under)
    })
    # bind undersample results
    samples_under_new <- dplyr::bind_rows(samples_under_new)
    return(samples_under_new)
}
