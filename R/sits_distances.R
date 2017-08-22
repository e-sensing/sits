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
#' @param bands            the bands to be used for determining patterns
#' @param dist_method      a method for calculating distances between time series
#' @return result          a set of distance metrics
#' @export
#'
sits_distances <- function(data.tb, patterns.tb, bands = NULL,
                           dist_method = sits_TWDTW_distances(data.tb = NULL, patterns.tb = NULL, bands = bands, alpha = -0.1, beta = 100, theta = 0.5, span = 0)) {

    # does the input data exist?
    .sits_test_table (data.tb)
    .sits_test_table (patterns.tb)
    # is the train method a function?
    ensurer::ensure_that(dist_method, class(.) == "function", err_desc = "sits_distances: dist_method is not a valid function")

    # compute the training method by the given data
    result <- dist_method(data.tb)
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
#'cer
#' @param data.tb          a SITS tibble time series
#' @param patterns.tb      a set of patterns obtained from training samples
#' @param bands            the bands to be used for determining patterns
#' @param distance         a method for calculating distances between time series
#' @return result          a set of distance metrics
#' @export
#'
sits_TSdist_distances <- function (data.tb, patterns.tb, bands = NULL, distance = "lccs") {

    # function that returns e1071::svm model based on a sits sample tibble
    result_fun <- function(tb){

        # does the input data exist?
        .sits_test_table (data.tb)
        .sits_test_table (patterns.tb)

        class.tb <- data.tb %>%
            purrrlyr::by_row (function (row) {
                ts <- row$time_series[[1]]
                patterns.tb %>%
                    purrrlyr::by_row (function (pat) {


                    })

            })
    }
    result <- .sits_factory_function2 (data.tb, patterns.tb, result_fun)

}
