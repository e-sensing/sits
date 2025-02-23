#' @title Find temporal patterns associated to a set of time series
#' @name sits_patterns
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description This function takes a set of time series samples as input
#' estimates a set of patterns. The patterns are calculated using a GAM model.
#' The idea is to use a formula of type y ~ s(x), where x is a temporal
#' reference and y if the value of the signal. For each time, there will
#' be as many predictions as there are sample values.
#' The GAM model predicts a suitable
#' approximation that fits the assumptions of the statistical model,
#' based on a smooth function.
#'
#' This method is based on the "createPatterns" method of the dtwSat package,
#' which is also described in the reference paper.
#'
#' @references Maus V, Camara G, Cartaxo R, Sanchez A, Ramos F, Queiroz GR.
#' A Time-Weighted Dynamic Time Warping Method for Land-Use
#' and Land-Cover Mapping. IEEE Journal of Selected Topics in Applied
#' Earth Observations and Remote Sensing, 9(8):3729-3739,
#' August 2016. ISSN 1939-1404. doi:10.1109/JSTARS.2016.2517118.
#'
#' @param  data          Time series.
#' @param  freq          Interval in days for estimates.
#' @param  formula       Formula to be applied in the estimate.
#' @param  ...           Any additional parameters.
#' @return               Time series with patterns.
#'
#' @examples
#' if (sits_run_examples()) {
#'     patterns <- sits_patterns(cerrado_2classes)
#'     plot(patterns)
#' }
#'
#' @export
sits_patterns <- function(data = NULL, freq = 8, formula = y ~ s(x), ...) {
    .check_set_caller("sits_patterns")
    # verifies if mgcv package is installed
    .check_require_packages("mgcv")
    # function that is used to be called as a value from another function
    result_fun <- function(tb) {
        # does the input data exist?
        .check_samples_train(tb)
        # find the bands of the data
        bds <- .samples_bands(tb)
        # create a tibble to store the results
        patterns <- .tibble()
        # what are the variables in the formula?
        vars <- all.vars(formula)
        # align all samples to the same time series intervals
        sample_dates <- lubridate::as_date(.samples_timeline(tb))
        tb <- .tibble_align_dates(tb, sample_dates)
        # extract the start and and dates
        start_date <- lubridate::as_date(utils::head(sample_dates, n = 1))
        end_date <- lubridate::as_date(utils::tail(sample_dates, n = 1))
        # determine the sequence of prediction times
        pred_time <- seq(
            from = lubridate::as_date(start_date),
            to = lubridate::as_date(end_date),
            by = freq
        )
        # how many different labels are there?
        labels <- dplyr::distinct(tb, .data[["label"]])[["label"]]
        # traverse labels
        patterns <- labels |>
            .map_dfr(function(lb) {
                # filter only those rows with the same label
                label_rows <- dplyr::filter(tb, .data[["label"]] == lb)
                # make sure label_rows is of class sits
                class(label_rows) <- c("sits", class(label_rows))
                # create a data frame to store the time instances
                time <- data.frame(as.numeric(pred_time))
                # name the time as the second variable of the formula
                names(time) <- vars[[2]]
                # store the time series associated to the pattern
                index <- tibble::tibble(Index = lubridate::as_date(pred_time))
                # calculate the fit for each band
                fit_bands <- bds |>
                    purrr::map(function(bd) {
                        # retrieve the time series for each band
                        label_b <- .samples_select_bands(label_rows, bd)
                        ts <- dplyr::bind_rows(label_b[["time_series"]])
                        # melt the time series for each band into a long table
                        # with all values together
                        ts2 <- ts |>
                            tidyr::pivot_longer(
                                cols = -"Index",
                                names_to = "variable"
                            ) |>
                            dplyr::select(
                                "Index",
                                "value"
                            ) |>
                            dplyr::transmute(
                                x = as.numeric(.data[["Index"]]),
                                y = .data[["value"]]
                            )
                        # calculate the best fit for the data set
                        fit <- mgcv::gam(data = ts2, formula = formula)
                        # Takes a fitted gam object and produces predictions
                        # in the sequence of prediction times
                        pred_values <- mgcv::predict.gam(fit, newdata = time)
                        # include the predicted values for the band
                        patt_b <- tibble::tibble(b = pred_values)
                        # rename the column to match the band names
                        names(patt_b)[names(patt_b) == "b"] <- bd
                        # return the tibble column to the list
                        return(patt_b)
                    }) # for each band
                # join the estimates for each bands
                res_label <- dplyr::bind_cols(fit_bands)
                # Add the temporal index
                res_label <- dplyr::bind_cols(index, res_label)
                # put the pattern in a list to store in a sits tibble
                ts <- tibble::lst()
                ts[[1]] <- res_label
                # add the pattern to the results tibble
                row <- tibble::tibble(
                    longitude = 0.0,
                    latitude = 0.0,
                    start_date = as.Date(start_date),
                    end_date = as.Date(end_date),
                    label = lb,
                    cube = "patterns",
                    time_series = ts
                )
                return(row)
            })
        class(patterns) <- c("patterns", "sits", class(patterns))
        return(patterns)
    }
    result <- .factory_function(data, result_fun)
    return(result)
}
