
#' @title validate temporal patterns
#' @name sits_validate
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Splits the set of time series into training and validation and
#' perform cross-validation.  For each data partition this function
#' performs a classifiction and returns the Overall Accuracy, User's Accuracy,
#' Producer's Accuracy, error matrix (confusion matrix), and Kappa values.
#'
#' @param data.tb         a SITS tibble
#' @param method          method to create patterns ("gam", "dendogram" or "centroids")
#' @param bands           the bands used for classification
#' @param times           number of partitions to create.
#' @param perc            the percentage of data that goes to training.
#' @param from            starting date of the estimate in month-day (for "gam" method)
#' @param to              end data of the estimated in month-day (for "gam" method)
#' @param freq            int - the interval in days for the estimates to be generated
#' @param formula         the formula to be applied in the estimate (for "gam" method)
#' @param tw_alpha        (double) - the steepness of the logistic function used for temporal weighting
#' @param tw_beta         (integer) - the midpoint (in days) of the logistic function
#' @param n_clusters      the maximum number of clusters to be identified (for clustering methods)
#' @param grouping_method the agglomeration method to be used. Any `hclust` method (see `hclust`) (ignored in `kohonen` method). Default is 'ward.D2'.
#' @param min_clu_perc    the minimum percentagem of valid cluster members, with reference to the total number of samples (for clustering methods)
#' @param apply_gam       apply gam method after a clustering algorithm (ignored if method is `gam`).
#' @param koh_xgrid       x dimension of the SOM grid (used only in `kohonen` or `koho&dogram` methods). Defaul is 5.
#' @param koh_ygrid       y dimension of the SOM grid (used only in `kohonen` or `koho&dogram` methods). Defaul is 5.
#' @param koh_rlen        the number of times the complete data set will be presented to the SOM grid.
#' (used only in `kohonen` or `koho&dogram` methods). Defaul is 100.
#' @param koh_alpha       learning rate, a vector of two numbers indicating the amount of change.
#' Default is to decline linearly from 0.05 to 0.01 over rlen updates.
#' @param file            file to save the results
#' @return cm              a validation assessment
#' @export

# method = "gam", bands = NULL, from = NULL, to = NULL, freq = 8,
# formula = y ~ s(x), n_clusters = 2, grouping_method = "ward.D2", min_clu_perc = 0.10, apply_gam = FALSE,
# koh_xgrid = 5, koh_ygrid = 5, koh_rlen = 100, koh_alpha = c(0.05, 0.01)

sits_validate <- function (data.tb, method = "gam", bands = NULL, times = 100, perc = 0.1,
                           from = NULL, to = NULL, freq = 8, formula = y ~ s(x), tw_alpha = -0.1, tw_beta = 100,
                           n_clusters = 2, grouping_method = "ward.D2", min_clu_perc = 0.10,
                           apply_gam = FALSE, koh_xgrid = NULL, koh_ygrid = NULL, koh_rlen = NULL, koh_alpha = c(0.05, 0.01),
                           file = "./conf_matrix.json"){

     # does the input data exist?
     ensurer::ensure_that(data.tb, !purrr::is_null(.),
                          err_desc = "sits_validate: input data not provided")
     # are the bands to be classified part of the input data?
     ensurer::ensure_that(data.tb, !(FALSE %in% bands %in% (sits_bands(.))),
                          err_desc = "sits_validate: invalid input bands")

     # recalculate kohonen params according to perc value
     koh_xgrid = trunc(koh_xgrid * sqrt(perc))
     koh_ygrid = trunc(koh_ygrid * sqrt(perc))
     koh_rlen = trunc(koh_rlen * sqrt(perc))

     #extract the bands to be included in the patterns
     if (purrr::is_null (bands))
          bands <- sits_bands (data.tb)
     data.tb <- sits_select(data.tb, bands)

     # create partitions different splits of the input data
     partitions.lst <- .sits_create_partitions (data.tb, times, frac = perc)

     # create a vector to store the result of the predictions
     pred.vec <- c()

     # create a vector to store the references
     ref.vec  <- c()

     # for each partition, fill the prediction and reference vectors
     partitions.lst %>% purrr::map(function (p) {

          # use the extracted partition to create the patterns
          patterns.tb <- sits_patterns(p, method = method, bands = bands, from = from, to = to, freq = freq,
                                       formula = formula, n_clusters = n_clusters, grouping_method = grouping_method,
                                       min_clu_perc = min_clu_perc, apply_gam = apply_gam,
                                       koh_xgrid = koh_xgrid, koh_ygrid = koh_ygrid, koh_rlen = koh_rlen, koh_alpha = koh_alpha,
                                       show = FALSE)

          # use the rest of the data for classification
          non_p.tb <- dplyr::anti_join(data.tb, p,
                                by = c("longitude", "latitude", "start_date",
                                       "end_date", "label", "coverage"))
          # classify each row of the data
          for (i in 1:nrow(non_p.tb)) {
               # do the classification of a single time series
               results.tb  <- sits_TWDTW (non_p.tb[i,], patterns.tb, bands = bands, alpha = tw_alpha, beta = tw_beta)
               # find out the alignment associated to the minimum distance
               i <- which.min(results.tb$alignments[[1]]$distance)
               # find the predicted label
               pred <- as.character(results.tb$alignments[[1]][i,"label"])
               # remove the numeric qualifier for the labels (generated by clustering)
               # for example, "Forest.1" becomes "Forest"
               if (stringr::str_detect(pred,"[.]"))
                    pred <- stringr::str_extract(pred,"[A-Za-z]+|[^.$]")
               # find the reference label
               ref  <- as.character(results.tb$label)
               # increase the prediction and reference vectors
               pred.vec[length(pred.vec) + 1] <<- pred
               ref.vec [length(ref.vec)  + 1] <<- ref
          }

     })
     # create the confusion vector (predicted x reference)
     confusion.vec <- c(pred.vec, ref.vec)
     # save the confusion vector in  a JSON file
     sits_toJSON (confusion.vec, file)
     # Classification accuracy measures
     measures <- rfUtilities::accuracy(pred.vec, ref.vec)

     return (measures)
}
#' @title Create partitions of a data set
#' @name  sits_create_partitions
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Create a list of partitions of a SITS table, based on a percentage and
#' a number of iterations
#'
#' @param data.tb a SITS table to be partitioned
#' @param times   number of iterations
#' @param frac    fraction of original data to be extracted. Value must be between 0 and 1.
#' @export
.sits_create_partitions <- function (data.tb, times, frac) {

     # create a list to store the partitions
     partitions.lst <- tibble::lst()

     # iterate and create the partitions
     for (i in 1:times){
          partitions.lst [[i]] <- sits_label_sample (data.tb, frac)
     }
     return (partitions.lst)
}

#' @title re-label classification results
#' @name sits_relabel
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Given a confusion matrix obtained in the validation
#' procedure, and a conversion list between the original labels and
#' new labels, returns a new confusion matrix
#' where classes have been merged.
#'
#' @param  data.tb        a SITS table with the samples to be validated
#' @param  file           a JSON file contaning the result of a validation procedure
#' @param  conv           a conversion of label names for the classes (optional))
#' @return assess         an assessment of validation
#' @export
sits_relabel <- function (data.tb = NULL, file = NULL, conv = NULL){
     # do the input file exist?
     ensurer::ensure_that(data.tb, !purrr::is_null(.),
                          err_desc = "sits_relabel: SITS table not provided")
     ensurer::ensure_that(file, !purrr::is_null(.),
                          err_desc = "sits_relabel: JSON file not provided")

     # what are the labels of the samples?
     labels <- dplyr::distinct (data.tb, label)

     # if the conversion list is NULL, create an identity list
     if (purrr::is_null(conv)) {
          conv <- tibble::lst()
          for (i in 1:nrow(labels)) {
               lab <- as.character(labels[i,"label"])
               conv [lab] <- lab
          }
     }
     # if the conversion list exists, ensure that its labels exist in the data
     else
          ensurer::ensure_that(conv, !(FALSE %in% names(.) %in% unlist(labels[,1])),
                               err_desc = "conversion list does not match labels of the data")

     confusion.vec <- jsonlite::fromJSON (file)
     mid <- length(confusion.vec)/2
     pred.vec <- confusion.vec[1:mid]
     ref.vec  <- confusion.vec[(mid+1):length(confusion.vec)]

     pred.vec <- as.character(conv[pred.vec])
     ref.vec  <- as.character(conv[ref.vec])

     assess <- rfUtilities::accuracy(pred.vec, ref.vec)
     return (assess)
}
#' @title validades clusters against original labels
#' @name sits_validate_cluster
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Given a sits table with `original_label` column, computes a confusion matrix
#' between the original labels (`original_label` column) and new labels
#'
#' @param  data.tb        a SITS table with the samples to be validated
#' @export
sits_validate_cluster <- function (data.tb){
     ensurer::ensure_that(data.tb, !purrr::is_null(.),
                          err_desc = "sits_validadeCluster: SITS table not provided")
     # do the input data have the `original_label` column?
     ensurer::ensure_that(data.tb, "original_label" %in% colnames(data.tb),
                          err_desc = "sits_validadeCluster: informed SITS table has not an `original_label` column.")

     result.tb <- data.tb %>%
          dplyr::group_by(original_label, label) %>%
          dplyr::summarise(count = n()) %>%
          tidyr::spread(key = label, value = count) %>%
          dplyr::ungroup()

     return (result.tb)
}
