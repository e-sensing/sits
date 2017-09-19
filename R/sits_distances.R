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
#' @param data.tb          a SITS tibble time series
#' @param patterns.tb      a set of patterns obtained from training samples
#' @param dist_method      a method for calculating distances between time series
#' @return result          a set of distance metrics
#' @export
#'
sits_distances <- function(data.tb, patterns.tb,
                           dist_method = sits_TWDTW_distances(data.tb = NULL, patterns.tb = NULL, alpha = -0.1, beta = 100, theta = 0.5, span = 0)) {

    # does the input data exist?
    .sits_test_tibble (data.tb)
    .sits_test_tibble (patterns.tb)
    # is the train method a function?
    ensurer::ensure_that(dist_method, class(.) == "function", err_desc = "sits_distances: dist_method is not a valid function")

    # compute the training method by the given data
    result <- dist_method(data.tb, patterns.tb)
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
#' @param data.tb          a SITS tibble time series
#' @param patterns.tb      a set of patterns obtained from training samples
#' @param distance         a method for calculating distances between time series
#' @param ...              Additional parameters required by the distance method.
#' @return result          a set of distance metrics
#' @export
#'
sits_TS_distances <- function (data.tb = NULL, patterns.tb = NULL, distance = "dtw", ...) {

    # function that returns a distance tibble
    result_fun <- function(data.tb, patterns.tb){

        # does the input data exist?
        .sits_test_tibble (data.tb)
        .sits_test_tibble (patterns.tb)

        distances.tb <-  sits_tibble_distance(patterns.tb)
        original_row <-  1

        labels <- (dplyr::distinct(patterns.tb, label))$label
        bands  <- sits_bands (patterns.tb)

        progress_bar <- NULL
        if (nrow (data.tb) > 10) {
            message("Finding distances from data to patterns...")
            progress_bar <- utils::txtProgressBar(min = 0, max = nrow(data.tb), style = 3)
            i <- 0
        }

        data.tb %>%
            purrrlyr::by_row(function (row) {
                ts_data <- row$time_series[[1]]
                drow.tb <- sits_tibble_distance(patterns.tb)
                r <- dplyr::add_row(drow.tb)
                r$original_row <- original_row
                r$reference    <- row$label
                bands %>%
                    purrr::map (function (b) {
                        patterns.tb %>%
                            purrrlyr::by_row(function (row_patt) {
                                labelp  <- row_patt$label
                                ts_patt <- row_patt$time_series[[1]]
                                ts_x    <- sits_toZOO (ts_data, b)
                                ts_y    <- sits_toZOO (ts_patt, b)
                                measure <-  paste0(labelp, ".", b)
                                r [measure] <<- TSdist::TSDistances(ts_x, ts_y, distance = distance, ...)
                            })
                    })
                distances.tb <<- dplyr::bind_rows(distances.tb, r)
                # update progress bar
                if (!purrr::is_null(progress_bar)) {
                    i <<- i + 1
                    utils::setTxtProgressBar(progress_bar, i)
                }
            })
    if (!purrr::is_null(progress_bar)) close(progress_bar)
    return (distances.tb)
    }
    result <- .sits_factory_function2 (data.tb, patterns.tb, result_fun)

}
#' @title Find distances between a set of SITS patterns and segments of sits tibble using TWDTW
#' @name sits_TWDTW_distances
#' @author Rolf Simoes, \email{rolf.simores@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Returns a SITS table with distances to be used for training in ML methods
#' This is a front-end to the sits_TWDTW_matches whose outout is trimmed down to contain just distances
#'
#' @param  data.tb     a table in SITS format with a time series to be classified using TWTDW
#' @param  patterns.tb   a set of known temporal signatures for the chosen classes
#' @param  dist.method   A character. Method to derive the local cost matrix.
#' @param  alpha         (double) - the steepness of the logistic function used for temporal weighting
#' @param  beta          (integer) - the midpoint (in days) of the logistic function
#' @param  theta         (double)  - the relative weight of the time distance compared to the dtw distance
#' @param  span          minimum number of days between two matches of the same pattern in the time series (approximate)
#' @param  keep          keep internal values for plotting matches
#' @param  multicores    number of threads to process the validation (Linux only). Each process will run a
#'                       whole partition validation.
#' @return distances.tb       a SITS table with the information on distances for the data
#' @export
sits_TWDTW_distances <- function (data.tb = NULL, patterns.tb = NULL, dist.method = "euclidean",
                                  alpha = -0.1, beta = 100, theta = 0.5, span  = 250, keep  = FALSE, multicores = 1) {

    ensurer::ensure_that(data.tb, all(sits_bands(patterns.tb) == sits_bands(.)),
                         err_desc = "sits_TWDTW_distances: bands in the data do not match bands in the patterns")

    result_fun <- function (data.tb, patterns.tb) {

        # break the input data into subsets to match the partition dates
        data.tb <- sits_break_ts (data.tb, patterns.tb)

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


        # compute partition list putting each set of same value of part.vec inside corresponding list element
        part.lst <- 1:multicores %>%
            purrr::map(function(i) data.tb[part.vec == i,] )

        # prepare function to be passed to `parallel::mclapply`. this function returns a distance table to each partition
        dist_fun <- function(part.tb){

            # create a tibble to store the results
            distances_part.tb <-  sits_tibble_distance(patterns.tb)

            # add a progress bar
            progress_bar <- NULL
            if (nrow (data.tb) > 10 && multicores == 1) {
                message("Matching patterns to time series...")
                progress_bar <- utils::txtProgressBar(min = 0, max = nrow(data.tb), style = 3)
                nrow <- 0
            }
            # take each row of input subset
            part.tb %>%
                purrrlyr::by_row (function (row.tb) {

                    nrow <<- nrow + 1

                    # add original information to results tibble)
                    new_row <- tibble::tibble(
                        original_row = nrow,
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
                            new_row <<- dplyr::bind_cols(new_row, matches_b.tb)

                        })
                    distances_part.tb <<- dplyr::bind_rows(distances_part.tb, new_row)
                })
            # update progress bar
            if (!purrr::is_null(progress_bar)) {
                utils::setTxtProgressBar(progress_bar, nrow)
            }
            if (!purrr::is_null(progress_bar)) close(progress_bar)
            return(distances_part.tb)
        }


        # get the matches from the sits_TWDTW_matches
        distances.lst <- parallel::mclapply(part.lst, dist_fun, mc.cores = multicores)

        # compose final result binding each partition by row
        distances.tb <- dplyr::bind_rows(distances.lst)

        return (distances.tb)
    }

    result <- .sits_factory_function2 (data.tb, patterns.tb, result_fun)
    return (result)
}


#' @title Spread matches from a sits matches tibble
#' @name sits_spread_matches
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Given a SITS tibble with a set of TWDTW matches, returns a tibble whose columns have
#' the reference label and the TWDTW distances for each temporal pattern.
#'
#' @param  data.tb    a SITS matches tibble
#' @return result.tb  a tibble where whose columns have the reference label and the TWDTW distances for each temporal pattern
#' @export
sits_spread_matches <- function(data.tb){

    # Get best TWDTW aligniments for each class
    data.tb$matches <- data.tb$matches %>%
        purrr::map(function (data.tb){
            data.tb %>%
                dplyr::group_by(predicted) %>%
                dplyr::summarise(distance=min(distance))
        })

    # Select best match and spread pred to columns
    result.tb <- data.tb %>%
        dplyr::transmute(original_row = 1:NROW(.), reference = label, matches = matches) %>%
        tidyr::unnest(matches, .drop = FALSE) %>%
        tidyr::spread(key = predicted, value = distance)

    return(result.tb)
}

#' @title Spread time series values from a sits tibble as distances in a sits distance tibble
#' @name sits_spread_time_series
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Given a SITS tibble with a set of time series values, returns a tibble whose columns have
#' the reference label and the time series bands as distances to be used as input in Machine Learning functions.
#'
#' @param  data.tb    a SITS tibble
#' @return result.tb  a tibble where columns have the reference label and the time series bands as distances
#' @export
sits_spread_time_series <- function(data.tb = NULL){

    result_fun <- function(data.tb, ...){
        data.tb$time_series <- data.tb$time_series %>% purrr::map(function(ts) {
            ts %>% dplyr::select(-Index) %>% purrr::map(function(band) band) %>%
                unlist() %>% as.matrix() %>% t() %>% tibble::as_tibble()
        })

        data.tb <- data.tb %>% dplyr::transmute(original_row = 1:NROW(.),
                                                reference = label,
                                                time_series) %>%
            tidyr::unnest(time_series)

        return(data.tb)
    }

    result <- .sits_factory_function (data.tb, result_fun)
    return (result)
}
