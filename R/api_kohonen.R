
# ---- kohonen utilities ----
#' @title Get a shared pointer of a distance function.
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @noRd
#' @keywords internal
#' @note
#'  The implementation of this function was adapted from the `kohonen` R
#'  Package. The code is open-source, under the GPL license, and is available on
#'  GitHub (https://github.com/rwehrens/kohonen).
#' @description
#'  This auxiliary function gets a shared pointer of a distance function
#'  implemented in C++. This pointer can then be used as input for the
#'  `RcppSupersom`, `RcppBatchSupersom`, and `RcppParallelBatchSupersom`
#'  functions.
#' @param distance   Distance name. The possible values are `euclidean`
#'                   and `dtw`.
#' @return           Shared pointer to a given distance function.
.kohonen_get_distance <- function(distance) {
    distance_fnc <- kohonen_dtw

    if (distance == "euclidean") {
        distance_fnc <- kohonen_euclidean
    }

    distance_fnc()
}

#' @title Get number of NA values in a given data matrix.
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @noRd
#' @keywords internal
#' @note
#'  The implementation of this function was adapted from the `kohonen` R
#'  Package. The code is open-source, under the GPL license, and is available on
#'  GitHub (https://github.com/rwehrens/kohonen).
#' @description
#'  This function calculates the number of NA values in a data matrix
#'  variable. The operation only calculates if a `max_na_fraction` is specified;
#'  otherwise, a matrix of zeros is returned.
#' @param data              Data matrix.
#' @param max_na_fraction   Max fraction of NA values.
#' @param nobjects          Number of objects associated with the data matrix.
#' @return                  Matrix with the number of NA values in the given
#'                          `data matrix`.
.kohonen_get_n_na <- function(data, max_na_fraction, nobjects) {
    if (max_na_fraction > 0L) {
        res <- data |>
            purrr::map(function(x){
                apply(x, 1, function(y)
                    sum(is.na(y))
                )
            }) |>
            dplyr::bind_rows() |>
            as.matrix() |>
            t()
    } else {
        res <- matrix(0, length(data), nobjects)
    }
    return(res)
}

#' @title Transform a Kohonen classes vector in a compatible classes matrix
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @noRd
#' @keywords internal
#' @note
#'  The implementation of this function was adapted from the `kohonen` R
#'  Package. The code is open-source, under the GPL license, and is available on
#'  GitHub (https://github.com/rwehrens/kohonen).
#' @description
#'  This auxiliary function transforms a vector of classes from a Kohonen map
#'  object into a compatible matrix.
#' @param yvec   Kohonen classes vector.
#' @return       Classes matrix.
.kohonen_classvec2classmat <- function(yvec)
{
    if (!is.factor(yvec)) {
        yvec <- factor(yvec)
    }

    nclasses <- nlevels(yvec)

    outmat <- matrix(0, length(yvec), nclasses)
    dimnames(outmat) <- list(NULL, levels(yvec))

    for (i in 1:nclasses) {
        outmat[which(as.integer(yvec) == i), i] <- 1
    }
    outmat
}

#' @title Calculate distances between Kohonen objects weights.
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @noRd
#' @keywords internal
#' @note
#'  The implementation of this function was adapted from the `kohonen` R
#'  Package. The code is open-source, under the GPL license, and is available on
#'  GitHub (https://github.com/rwehrens/kohonen).
#' @description
#'  This auxiliary function calculates the distance between the initial selected
#'  weights (samples from the user-defined data) and the user-defined data.
#' @param kohobj     Kohonen object.
#' @param type       Kohonen object type. The possible values are `data` and
#'                   `codes`
#' @param whatmap    Data layers to use. If unspecified all layers are used.
#' @return           Distances objects containing the weight distances, and
#'                   related metadata.
.kohonen_object_distances <- function(kohobj,
                                      type = c("data", "codes"),
                                      whatmap)
{
    # validate type
    type <- match.arg(type)
    # define the layer to be used based on the `whatmap`
    if (missing(whatmap)) {
        whatmap <- kohobj$whatmap
    } else {
        whatmap <- kohonen::check.whatmap(kohobj, whatmap)
    }
    # extract distance function (shared pointer) from the kohonen object
    distance_ptr <- kohobj$distance_fnc
    # extract the max fraction of NA values allowed in the map data
    max_na_fraction <- kohobj$max_na_fraction
    # prepare the weights
    weights <- kohobj$user_weights[whatmap] * kohobj$distance_weights[whatmap]
    # get data
    data <- kohobj[[type]][whatmap]
    # calculate the number of variables, objects and, NA values in the map data
    nvars <- purrr::map_int(data, ncol)
    nobjects <- nrow(data[[1]])
    n_na <- .kohonen_get_n_na(data, max_na_fraction, nobjects)
    # prepare data matrix
    datamat <- matrix(unlist(data), ncol = nobjects, byrow = TRUE)
    # calculate distances
    res <- kohonen_object_distances(
        data = datamat,
        numVars = nvars,
        numNAs = n_na,
        distanceFunction = distance_ptr,
        weights = weights
    )
    # prepare the result and return it
    attributes(res) <- list(
        "Size" = nobjects,
        "Diag" = FALSE,
        "Upper" = FALSE,
        "method" = "supersom",
        "call" = match.call(),
        "class" = "dist"
    )
    res
}

# ---- kohonen operations ----
#' @title Create SOM Map
#' @author Lorena Alves, \email{lorena.santos@@inpe.br}
#' @author Karine Ferreira. \email{karine.ferreira@@inpe.br}
#' @noRd
#' @keywords internal
#' @note
#'  The implementation of this function was adapted from the `kohonen` R
#'  Package. The code is open-source, under the GPL license, and is available on
#'  GitHub (https://github.com/rwehrens/kohonen).
#' @description
#'  This function creates a SOM map based on `codes` from a `Supersom` function.
#' @param x                 Kohonen object.
#' @param whatmap           What data layers to use. If unspecified all layers
#'                          are used.
#' @param user_weights      Train weights used in the `Supersom`
#' @param max_na_fraction   Max fraction of NA values.
#' @return                  Kohonen map object.
.kohonen_map <- function(x, whatmap = NULL, user_weights = NULL,
                         max_na_fraction = x$max_na_fraction, ...)
{
    # extract relevant info from the kohonen object
    codes <- x$codes
    newdata <- x$data
    nlayers <- length(codes)
    # check whatmap
    whatmap_tr <- kohonen::check.whatmap(x, whatmap)
    whatmap <- kohonen::check.whatmap(newdata, whatmap)
    # select data from a given layer.
    newdata <- newdata[whatmap]
    # select distance (shared pointer)
    distance_ptr <- .kohonen_get_distance(x$distance_fnc)
    # define if train weights should be used in the creation of the
    # kohonen map
    if (is.null(user_weights)) {
        user_weights <- x$user_weights
        use_train_weights <- TRUE
    } else {
        use_train_weights <- FALSE
    }
    # if only one layer of weights is defined, then, replicate it to all layers
    if (length(user_weights) == 1) {
        user_weights <- rep(1, nlayers)
    }
    # validate if new data is being used to create the kohonen map
    if (use_train_weights & any(user_weights[whatmap_tr] < 1e-8)) {
        warning("Mapping new data using data layers not involved in training")
    }
    # select `codes` and `weights` from a given layer
    codes <- codes[whatmap_tr]
    user_weights_original <- user_weights
    user_weights <- user_weights[whatmap_tr]
    # validate `weights` from a given layer
    if (length(whatmap_tr) == 1) {
        user_weights <- 1
    } else {
        if (sum(user_weights >= 1e-8) == 0) {
            stop("Only user_weights of zero given")
        }
    }
    # calculate the number of variables and codes in the codes data
    nvars <- purrr::map_int(codes, ncol)
    ncodes <- nrow(codes[[1]])
    # calculate the number of objects and NA values in the map data
    nobjects <- nrow(newdata[[1]])
    n_na <- .kohonen_get_n_na(newdata, max_na_fraction, nobjects)
    # prepare codes and map data
    newdata <- matrix(unlist(newdata), ncol = nobjects, byrow = TRUE)
    codes <- matrix(unlist(codes), ncol = ncodes, byrow = TRUE)
    # prepare the weights
    weights <- user_weights * x$distance_weights[whatmap_tr]
    weights <- weights / sum(weights)
    # create som map
    res <- RcppMap(
        data = newdata,
        numVars = nvars,
        numNAs = n_na,
        codes = codes,
        weights = weights,
        distanceFunction = distance_ptr
    )
    # prepare the result and return it
    list(
        unit.classif = res$winners + 1,
        distances = res$unitdistances,
        whatmap = whatmap,
        user_weights = user_weights_original
    )
}

#' @title Self- and super-organizing maps
#' @author Lorena Alves, \email{lorena.santos@@inpe.br}
#' @author Karine Ferreira. \email{karine.ferreira@@inpe.br}
#' @noRd
#' @keywords internal
#' @note
#'  The implementation of this function was adapted from the `kohonen` R
#'  Package. The code is open-source, under the GPL license, and is available on
#'  GitHub (https://github.com/rwehrens/kohonen).
#' @description
#'  A supersom is an extension of self-organising maps (SOMs) to multiple data
#'  layers, possibly with different numbers and different types of variables
#'  (though equal numbers of objects). NAs are allowed. A weighted distance
#'  over all layers is calculated to determine the winning units during
#'  training. Functions som and xyf are simply wrappers for supersoms with one
#'  and two layers, respectively. Function nunits is a utility function
#'  returning the number of units in the map.
#' @param data       list of data matrices (numerical) of factors. If a vector
#'                   is entered, it will be converted to a one-column matrix.
#'                   No data.frame objects are allowed.
#' @param grid       a grid for the codebooks (See \code{\link[kohonen:grid]})
#' @param distance   distance name used in all SOM operations. The possible
#'                   values are `euclidean` and `dtw`
#' @param rlen       the number of times the complete data set will be
#'                   presented to the network.
#' @param alpha      learning rate, a vector of two numbers indicating the
#'                   amount of change. Default is to decline linearly from 0.05
#'                   to 0.01 over rlen updates. Not used for the batch
#'                   algorithm.
#' @param radius     the radius of the neighbourhood, either given as a single
#'                   number or a vector (start, stop). If it is given as a
#'                   single number the radius will change linearly from radius
#'                   to zero; as soon as the neighbourhood gets smaller than one
#'                   only the winning unit will be updated. Note that the
#'                   default before version 3.0 was to run from radius to
#'                   -radius. If nothing is supplied, the default is to start
#'                   with a value that covers 2/3 of all unit-to-unit distances.
#' @return           Complete kohonen object.
.kohonen_supersom <- function(data,
                              grid = kohonen::somgrid(),
                              distance = "dtw",
                              rlen = 100,
                              alpha = c(0.05, 0.01),
                              radius = stats::quantile(nhbrdist, 2 / 3),
                              mode = NULL)
{
    # define the initial weights
    user_weights <- 1
    # define the max NA fraction. In `sits`, no NA values are allowed.
    max_na_fraction <- 0
    # define layers to be used. In `sits`, all data layers are used (i.e., NULL)
    whatmap <- NULL
    # save original input data
    full_data <- data
    nmat <- length(data)
    # check whatmap
    whatmap <- kohonen::check.whatmap(data, whatmap)
    nmap <- length(whatmap)
    # get data to be used
    data <- data[whatmap]
    # get a (symmetrical) matrix containing distances from the
    # user-defined grid.
    nhbrdist <- kohonen::unit.distances(grid)
    # check radius and fix it
    if (length(radius) == 1) {
        radius <- c(radius, 0)
    }
    # calculate the number of variables, objects and, NA values in the map data
    nobjects <- nrow(data[[1]])
    nvar <- purrr::map_int(data, ncol)
    n_na <- .kohonen_get_n_na(data, max_na_fraction, nobjects)
    # transform the user-defined data in a matrix
    data_matrix <- matrix(unlist(data), ncol = nobjects, byrow = TRUE)
    # select distance (shared pointer)
    distance_ptr <- .kohonen_get_distance(distance)
    # get or create initial codebooks
    ncodes <- nrow(grid$pts)
    starters <- sample(1:nobjects, ncodes, replace = FALSE)
    init <- lapply(data, function(x) x[starters, , drop = FALSE])
    init_matrix <- matrix(unlist(init), ncol = ncodes, byrow = TRUE)
    # define the initial weights
    distance_weights <- original_user_weights <- rep(0, nmat)
    # prepare `weights` and `distances` based on data from layers.
    if (length(whatmap) == 1) {
        weights <- user_weights <- 1
        distance_weights[whatmap] <- original_user_weights[whatmap] <- 1
    } else {
        if (length(user_weights) == 1) {
            user_weights <- rep(user_weights, length(whatmap))
        } else {
            if (length(user_weights) == nmat)
                user_weights <- user_weights[whatmap]
        }

        if (any(user_weights == 0)) {
            stop("Incompatibility between whatmap and user_weights")
        }

        if (abs(sum(user_weights)) < .Machine$double.eps) {
            stop("user_weights sum to zero")
        }

        user_weights <- user_weights / sum(user_weights)
        original_user_weights[whatmap] <- user_weights

        # comment from the `kohonen` package
        # calculate distance weights from the init data.
        # the goal is to bring each data layer to more or less the same scale,
        # after which the user weights are applied. We call object.distances
        # layer by layer here, which leads to a list of distance vectors.
        meanDistances <-
            lapply(seq(along = init), function(ii)
               .kohonen_object_distances(
                   list(
                       data = init[ii],
                       whatmap = 1,
                       user_weights = 1,
                       distance_weights = 1,
                       max_na_fraction = max_na_fraction,
                       distance_fnc = distance_ptr
                   ),
                   type = "data"
               )
            )

        if (any(purrr::map_dbl(meanDistances, mean) < .Machine$double.eps)) {
            stop("Non-informative layers present: mean distance between
                 objects zero")
        }

        # comment from the `kohonen` package
        ## the distance weights are then the reciprocal values of the mean
        ## distances per layer. We no longer use median distances since
        ## there is a real chance that for factor data the median equals zero
        distance_weights[whatmap] <- 1 / purrr::map_dbl(meanDistances, mean)

        weights <- user_weights * distance_weights[whatmap]
        weights <- weights / sum(weights)
    }
    # create supersom
    switch (mode,
        online = {
            res <- suppressWarnings({RcppSupersom(
                data = data_matrix,
                codes = init_matrix,
                numVars = nvar,
                weights = weights,
                numNAs = n_na,
                neighbourhoodDistances = nhbrdist,
                alphas = alpha,
                radii = radius,
                numEpochs = rlen,
                distanceFunction = distance_ptr
            )})
        },
        batch = {
            res <- suppressWarnings({RcppBatchSupersom(
                data = data_matrix,
                codes = init_matrix,
                numVars = nvar,
                weights = weights,
                numNAs = n_na,
                neighbourhoodDistances = nhbrdist,
                radii = radius,
                numEpochs = rlen,
                distanceFunction = distance_ptr
            )})
        },
        pbatch = {
            res <- suppressWarnings({RcppParallelBatchSupersom(
                data = data_matrix,
                codes = init_matrix,
                numVars = nvar,
                weights = weights,
                numNAs = n_na,
                neighbourhoodDistances = nhbrdist,
                radii = radius,
                numEpochs = rlen,
                numCores = -1,
                distanceFunction = distance_ptr
            )})
        }
    )
    # extract changes
    changes <- matrix(res$changes, ncol = nmap, byrow = TRUE)
    colnames(changes) <- names(data)
    mycodes <- res$codes
    # format codes
    layerID <- rep(1:nmap, nvar)
    mycodes2 <- split(as.data.frame(mycodes), layerID)
    mycodes3 <- lapply(mycodes2, function(x) t(as.matrix(x)))
    # codes as vector
    codes <- vector(length(full_data), mode = "list")
    names(codes) <- names(full_data)
    codes[whatmap] <- mycodes3
    # format code names
    for (ii in seq(along = whatmap)) {
        colnames(codes[[whatmap[ii]]]) <- colnames(data[[ii]])
    }
    # prepare kohonen map input
    map_data <- structure(
        list(
            codes = codes,
            distance_weights = distance_weights,
            distance_fnc = distance,
            data = full_data
        ),
        class = "kohonen"
    )
    # create kohonen map
    mapping <- .kohonen_map(
        map_data,
        whatmap = whatmap,
        user_weights = original_user_weights,
        max_na_fraction = max_na_fraction
    )
    # prepare the result and return it
    structure(
        list(
            data = full_data,
            unit.classif = mapping$unit.classif,
            distances = mapping$distances,
            grid = grid,
            codes = codes,
            changes = changes,
            alpha = alpha,
            radius = radius,
            user_weights = original_user_weights,
            distance_weights = distance_weights,
            whatmap = whatmap,
            max_na_fraction = max_na_fraction,
            distance_fnc = distance
        ),
        class = "kohonen"
    )
}
