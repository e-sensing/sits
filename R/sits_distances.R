#' @title Calculate a set of distance measures for satellite image time series
#' @name sits_distances
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#'
#' @description This function allows the user to select different alternatives to define a set of
#' distances between a set of satellite image time series and a set of patterns.
#'  The alternatives are:
#' "TWTDTW"    - uses the TWDTW (time-weighted dynamic time warping)
#' other methods as used by the TSdist package
#'
#' @param data.tb          SITS tibble time series
#' @param pt_method        Method used for calculating the patterns
#' @param dist_method      Method for calculating distances between time series
#' @return result          Set of distance metrics
#' @export
#'
sits_distances <- function(data.tb, pt_method = sits_gam(),
                           dist_method = sits_distances_from_data()
){

    # does the input data exist?
    .sits_test_tibble (data.tb)
    # is the pattern method a function?
    ensurer::ensure_that(pt_method, class(.) == "function", err_desc = "sits_distances: pt_method is not a valid function")

    # is the train method a function?
    ensurer::ensure_that(dist_method, class(.) == "function", err_desc = "sits_distances: dist_method is not a valid function")

    # compute the training method by the given data
    result <- dist_method(data.tb, pt_method)
    return(result)

}

#' @title Calculate a set of distance measures for satellite image time series
#' @name sits_TS_distances
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function allows the user to select different alternatives to define a set of
#' distances between a set of satellite image time series and a set of patterns.
#' The following alternatives are available in the TSdist package:
#' "euclidean": Euclidean distance.
#' "manhattan": Manhattan distance.
#' "minkowski": Minkowski distance.
#' "infnorm": Infinite norm distance.
#' "ccor": Distance based on the cross-correlation.
#' "sts": Short time series distance.
#' "dtw": Dynamic Time Warping distance. Uses the dtw package (see dtw).
#' "lb.keogh": LB_Keogh lower bound for the Dynamic Time Warping distance.
#' "edr": Edit distance for real sequences.
#' "erp": Edit distance with real penalty.
#' "lcss": Longest Common Subsequence Matching.
#' "fourier": Distance based on the Fourier Discrete Transform.
#' "tquest": TQuest distance.
#' "dissim": Dissim distance.
#' "acf": Autocorrelation-based dissimilarity, Uses the TSclust package (see diss.ACF).
#' "pacf": Partial autocorrelation-based dissimilarity. Uses the TSclust package (see diss.PACF).
#' "ar.lpc.ceps": Dissimilarity based on LPC cepstral coefficients. Uses the TSclust package (see diss.AR.LPC.CEPS).
#' "ar.mah": Model-based dissimilarity proposed by Maharaj (1996, 2000). Uses the TSclust package (see diss.AR.MAH).
#' "ar.pic": Model-based dissimilarity measure proposed by Piccolo (1990). Uses the TSclust package (see diss.AR.PIC).
#' "cdm": Compression-based dissimilarity measure. Uses the TSclust package (see diss.CDM).
#' "cid": Complexity-invariant distance measure. Uses the TSclust package (see diss.CID).
#' "cor": Dissimilarities based on Pearson's correlation. Uses the TSclust package (see diss.COR).
#' "cort": Dissimilarity index which combines temporal correlation and raw value behavior. Uses the TSclust package (see diss.CORT).
#' "wav": Dissimilarity based on wavelet feature extraction. Uses the TSclust package (see diss.DWT).
#' "int.per": Integrated periodogram based dissimilarity IntPerDistance. Uses the TSclust package (see diss.INT.PER).
#' "per": Periodogram based dissimilarity PerDistance. Uses the TSclust package (see diss.PER).
#' "mindist.sax": Symbolic Aggregate Aproximation based dissimilarity. Uses the TSclust package (see diss.MINDIST.SAX).
#' "ncd": Normalized compression based distance NCDDistance. Uses the TSclust package (see diss.NCD).
#' "pred": Dissimilarity measure cased on nonparametric forecasts PredDistance. Uses the TSclust package (see diss.PRED).
#' "spec.glk": Dissimilarity based on the generalized likelihood ratio test. Uses the TSclust package (see diss.SPEC.GLK).
#' "spec.isd": Dissimilarity based on the integrated squared difference between the log-spectra. Uses the TSclust package (see diss.SPEC.ISD).
#' "spec.llr": General spectral dissimilarity measure using local-linear estimation of the logspectra. Uses the TSclust package (see diss.SPEC.LLR).
#' "pdc": Permutation Distribution Distance. Uses the pdc package (see pdcDist).
#' "frechet": Frechet distance. Uses the longitudinalData package (see distFrechet).
#'
#' @param data.tb          SITS tibble time series
#' @param pt_method        Method to calculate the patterns
#' @param distance         Method for calculating distances between time series and pattern
#' @param ...              Other parameters passed to the TSDistances function
#' @return result          a set of distance metrics
#'
#' @export
sits_TS_distances <- function (data.tb = NULL, pt_method = sits_gam(), distance = "dtw", ...) {

    # function that returns a distance tibble
    result_fun <- function(data.tb, pt_method){

        # does the input data exist?
        .sits_test_tibble (data.tb)
        # find the patterns on the data set
        patterns.tb <- pt_method (data.tb)
        # create a data table to store the distances
        distances.tb <-  sits_distance_table(patterns.tb)
        # set the original row of the distance table
        original_row <-  1
        # set the labels of the distances
        labels <- sits_labels(patterns.tb)$label
        # set the bands of the distance table
        bands  <- sits_bands(patterns.tb)

        # create a progress bar
        progress_bar <- NULL
        if (nrow (data.tb) > 10) {
            message("Finding distances from data to patterns...")
            progress_bar <- utils::txtProgressBar(min = 0, max = nrow(data.tb), style = 3)
            i <- 0
        }
        # go row by row of the input data, and calculate the distances
        data.tb %>%
            purrrlyr::by_row(function (row) {
                # retrieve the time series of the input row
                ts_data <- row$time_series[[1]]
                # create a distance table to store the results for the row
                drow.tb <- data.frame ("original_row" = original_row, "reference" = row$label)

                # loop through the bands of the patterns
                bands %>%
                    purrr::map (function (b) {
                        patterns.tb %>%
                            # loop through the labels of the patterns
                            purrrlyr::by_row(function (row_patt) {
                                # what is the label of the pattern?
                                labelp  <- row_patt$label
                                # what is the time series associated to the pattern?
                                ts_patt <- row_patt$time_series[[1]]
                                # tranform the input time series and the pattern time series to ZOO format
                                ts_x    <- sits_toZOO (ts_data, b)
                                ts_y    <- sits_toZOO (ts_patt, b)
                                # find the name of the measure
                                measure <-  paste0(labelp, ".", b)
                                # call the TSDistances function to extract the distance
                                drow.tb [measure] <<- TSdist::TSDistances(ts_x, ts_y, distance = distance, ...)
                            })
                    })
                distances.tb <<- data.table::rbindlist(list (distances.tb, drow.tb))
                # update progress bar
                if (!purrr::is_null(progress_bar)) {
                    i <<- i + 1
                    utils::setTxtProgressBar(progress_bar, i)
                }
                original_row <<- original_row + 1
            })
        if (!purrr::is_null(progress_bar)) close(progress_bar)
        return (distances.tb)
    }
    result <- .sits_factory_function2 (data.tb, pt_method, result_fun)
    return (result)
}
#' @title Find distances between a set of SITS patterns and segments of sits tibble using TWDTW
#' @name sits_TWDTW_distances
#' @author Rolf Simoes, \email{rolf.simores@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Returns a SITS table with distances to be used for training in ML methods
#' This is a front-end to the sits_TWDTW_matches whose outout is trimmed down to
#' contain just distances
#'
#' @references Maus V, Camara G, Cartaxo R, Sanchez A, Ramos FM, de Queiroz GR (2016).
#' A Time-Weighted Dynamic Time Warping Method for Land-Use and Land-Cover Mapping.
#' IEEE Journal of Selected Topics in Applied Earth Observations and Remote Sensing, 9(8):3729-3739,
#' August 2016. ISSN 1939-1404. doi:10.1109/JSTARS.2016.2517118.
#'
#' @param  data.tb       Table in SITS format with a time series to be classified using TWTDW
#' @param  pt_method     Method to calculate the patterns
#' @param  dist.method   Method to derive the local cost matrix.
#' @param  alpha         Steepness of the logistic function used for temporal weighting
#' @param  beta          Midpoint (in days) of the logistic function
#' @param  theta         Relative weight of the time distance compared to the dtw distance
#' @param  span          Minimum number of days between two matches of the same pattern in the time series (approximate)
#' @param  keep          Keep internal values for plotting matches
#' @param  multicores    Number of threads to process the validation (Linux only). Each process will run a
#'                       whole partition validation.
#' @return distances.tb       a SITS table with the information on distances for the data
#' @export
sits_TWDTW_distances <- function (data.tb = NULL, pt_method = sits_gam(),
                                  dist.method = "euclidean",
                                  alpha = -0.1, beta = 100, theta = 0.5, span  = 250,
                                  keep  = FALSE, multicores = 2) {

    # function that returns a distance tibble
    result_fun <- function(data.tb, pt_method){

        # does the input data exist?
        .sits_test_tibble (data.tb)

        # find the patterns on the data set
        patterns.tb <- pt_method (data.tb)

        # determine the bands of the data
        bands <- sits_bands (data.tb)

        # determine the labels of the patterns
        labels <- sits_labels(patterns.tb)$label

        # Define the logistic function
        log_fun <- dtwSat::logisticWeight(alpha = alpha, beta = beta)

        # compute partition vector
        part.vec <- rep.int(1, NROW(data.tb))
        if(multicores > 1)
            part.vec <- cut(seq(NROW(data.tb)), multicores, labels = FALSE)


        # compute partition list putting each set of same value of part.vec
        # inside corresponding list element
        part.lst <- 1:multicores %>%
            purrr::map(function(i) data.tb[part.vec == i,] )

        # prepare function to be passed to `parallel::mclapply`. this function returns a distance table to each partition
        dist_fun <- function(part.tb){

            # create a tibble to store the results
            distances_part.tb <-  sits_distance_table(patterns.tb)

            # add a progress bar
            n <- 0
            progress_bar <- NULL
            if (nrow (data.tb) > 10 && multicores == 1) {
                message("Matching patterns to time series...")
                progress_bar <- utils::txtProgressBar(min = 0, max = nrow(data.tb), style = 3)

            }
            # take each row of input subset
            part.tb %>%
                purrrlyr::by_row (function (row.tb) {
                    n <<- n + 1
                    # add original information to results tibble
                    new_row <- data.table::data.table(
                        original_row = n,
                        reference    = row.tb$label
                    )
                    # process each band of the input
                    bands %>%
                        purrr::map (function (b){
                            # extract band from data and from patterns
                            data_b.tb  <- sits_select_bands (row.tb, bands = b)
                            patt_b.tb  <- sits_select_bands (patterns.tb, bands = b)

                            # convert data to TWDTW format
                            twdtw_series   <- sits_toTWDTW(data_b.tb)
                            # convert patterns to TWDTW format
                            twdtw_patterns <- sits_toTWDTW(patt_b.tb)

                            #classify the data using TWDTW
                            matches.twdtw = dtwSat::twdtwApply(
                                x          = twdtw_series,
                                y          = twdtw_patterns,
                                weight.fun = log_fun,
                                theta      = theta,
                                span       = span,
                                keep       = keep,
                                dist.method = dist.method)

                            # transform the matches to a tibble
                            matches_b.tb <- tibble::as_tibble(matches.twdtw[[1]]) %>%
                                dplyr::mutate(predicted = as.character(label))


                            # Get best TWDTW alignments for each class
                            matches_b.tb <- matches_b.tb %>%
                                dplyr::group_by(predicted) %>%
                                dplyr::summarise(distance=min(distance)) %>%
                                tidyr::spread(key = predicted, value = distance)

                            # obtain the resulting distances from bands
                            colnames (matches_b.tb) <- paste0(colnames(matches_b.tb),".",b)

                            # add the results of the matching patterns/bands to new_row
                            new_row[colnames (matches_b.tb)] <<- as.data.frame(matches_b.tb)

                        })
                    distances_part.tb <<- data.table::rbindlist(list(distances_part.tb, new_row))
                    # update progress bar
                    if (!purrr::is_null(progress_bar)) {
                        utils::setTxtProgressBar(progress_bar, n)
                    }
                })
            if (!purrr::is_null(progress_bar)) close(progress_bar)
            return(distances_part.tb)
        }


        # get the matches from the sits_TWDTW_matches
        distances.lst <- parallel::mclapply(part.lst, dist_fun, mc.cores = multicores)

        # compose final result binding each partition by row
        distances.tb <- data.table::rbindlist(distances.lst)

        return (distances.tb)
    }
    result <- .sits_factory_function2 (data.tb, pt_method, result_fun)
    return (result)
}

#' @title Use time series values from a sits tibble as distances for training patterns
#' @name sits_distances_from_data
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function allows using a set of labelled time series as
#' input to the machine learning models. Instead of first estimating a set
#' of idealised patterns and then computing distances from these patterns,
#' the attributes used to train the model are the series themselves.
#' This function then extracts the time series from a SITS tibble and
#' "spreads" them in time to produce a tibble with distances.
#'
#' @param  data.tb        a SITS tibble with original data
#' @param  pt_method      Method used for calculating the patterns
#' @param  shift          Adjustment value to avoid negative pixel vales
#' @return distances.tb  a tibble where columns have the reference label and the time series values as distances
#' @export
sits_distances_from_data <- function(data.tb = NULL, pt_method = sits_gam(), shift = 3.0){

    result_fun <- function(data.tb) {
        # create a list with the time series transposed from columns to rows
        ts.lst <- data.tb$time_series %>%
            purrr::map (function (ts){
                as.data.frame(t(unlist(ts[-1])))
            })
        # bind the lists of time series together
        dist.tb <- data.table::rbindlist(ts.lst)
        # shift the values of the time series to avoid negative numbers
        dist.tb <- dist.tb + shift
        # create a data frame with the first two columns for training
        distances.tb <- data.frame("original_row" = 1:nrow(data.tb), "reference" = data.tb$label)
        # join the two references columns with the data values
        distances.tb <- cbind(distances.tb, dist.tb)

        return(distances.tb)
    }
    result <- .sits_factory_function (data.tb, result_fun)
    return (result)
}

