#' @title Create temporal patterns using a generalised additive model (gam)
#' @name sits_patterns
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description This function takes a set of time series samples as input
#' estimates a set of patterns. The patterns are calculated based in a GAM model.
#' The idea is to use a formula of type y ~ s(x), where x is a temporal
#' reference and y if the value of the signal. For each time, there will be as many predictions
#' as there are sample values. The GAM model predicts a suitable
#' approximation that fits the assumptions of the statistical model.
#' By default, the GAM method produces an approximation based on a smooth function.
#'
#' This method is based on the "createPatterns" method of the dtwSat package, which is also
#' described in the reference paper.
#'
#' @references Maus V, Camara G, Cartaxo R, Sanchez A, Ramos FM, de Queiroz GR (2016).
#' A Time-Weighted Dynamic Time Warping Method for Land-Use and Land-Cover Mapping.
#' IEEE Journal of Selected Topics in Applied Earth Observations and Remote Sensing, 9(8):3729-3739,
#' August 2016. ISSN 1939-1404. doi:10.1109/JSTARS.2016.2517118.
#'
#' @param  data          A tibble in sits format with time series.
#' @param  freq          Interval in days for the estimates to be generated.
#' @param  formula       Formula to be applied in the estimate.
#' @param  ...           Any additional parameters.
#' @return A sits tibble with the patterns.
#'
#' @examples
#' \donttest{
#' # Read a set of samples for two classes
#' data(cerrado_2classes)
#' # Estimate a set of patterns (one for each label)
#' patterns <- sits_patterns(cerrado_2classes)
#' # Show the patterns
#' plot(patterns)
#'
#' # Read a set of samples for the state of Mato Grosso, Brazil, provided by EMBRAPA
#' data(samples_mt_4bands)
#' # Estimate a set of patterns (one for each label)
#' patterns <- sits_patterns(samples_mt_4bands)
#' # Show the patterns
#' plot(patterns)
#' }
#' @export
sits_patterns <- function(data = NULL, freq = 8, formula = y ~ s(x), ...){
    # backward compatibility
    if ("coverage" %in% names(data))
        data <- .sits_tibble_rename(data)
    # function that is used to be called as a value from another function
    result_fun <- function(tb){
        # does the input data exist?
        .sits_test_tibble(tb)

        # find the bands of the data
        bds <- sits_bands(tb)

        # create a tibble to store the results
        patterns <- .sits_tibble()

        # what are the variables in the formula?
        vars <- all.vars(formula)

        # align all samples to the same time series intervals
        sample_dates <- lubridate::as_date(sits_time_series_dates(tb))
        tb           <- sits_align_dates(tb, sample_dates)

        # extract the start and and dates
        start_date <- lubridate::as_date(utils::head(sample_dates, n = 1))
        end_date   <- lubridate::as_date(utils::tail(sample_dates, n = 1))


        # determine the sequence of prediction times
        pred_time = seq(from = lubridate::as_date(start_date),
                        to   = lubridate::as_date(end_date),
                        by   = freq)

        # how many different labels are there?
        labels <- dplyr::distinct(tb, label)$label

        # traverse labels
        patterns.lst <- labels %>%
            purrr::map(function(lb) {
                # filter only those rows with the same label
                label.tb <- dplyr::filter(tb, label == lb)

                # create a data frame to store the time instances
                time <- data.frame(as.numeric(pred_time))

                # name the time as the second variable of the formula (usually, this is x)
                names(time) = vars[2]

                # create a tibble to store the time series associated to the pattern
                ind.tb <- tibble::tibble(Index = lubridate::as_date(pred_time))

                # calculate the fit for each band
                fit.lst <- bds %>%
                    purrr::map(function(bd) {
                        # retrieve the time series for each band
                        label_b.tb <- .sits_select_bands_(label.tb, bd)
                        ts <- label_b.tb$time_series

                        # melt the time series for each band into a long table
                        # with all values together
                        ts2 <- ts %>%
                            reshape2::melt(id.vars = "Index") %>%
                            dplyr::select(Index, value)       %>%
                            dplyr::transmute(x = as.numeric(Index), y = value)

                        #calculate the best fit for the data set
                        fit <-  mgcv::gam(data = ts2, formula = formula)

                        # Takes a fitted gam object and produces predictions
                        # for the desired dates in the sequence of prediction times
                        pred_values <- mgcv::predict.gam(fit, newdata = time)

                        #include the predicted values for the band in the results tibble
                        res.tb <- tibble::tibble(b = pred_values)

                        # rename the column to match the band names
                        names(res.tb)[names(res.tb) == "b"] <- bd
                        # return the tibble column to the list
                        return(res.tb)
                    }) # for each band

                res.tb <- dplyr::bind_cols(fit.lst)
                res.tb <- dplyr::bind_cols(ind.tb, res.tb)

                # put the pattern in a list to store in a sits tibble
                ts <- tibble::lst()
                ts[[1]] <- res.tb

                # add the pattern to the results tibble
                row <- tibble::tibble(
                    longitude      = 0.0,
                    latitude       = 0.0,
                    start_date     = as.Date(start_date),
                    end_date       = as.Date(end_date),
                    label          = lb,
                    cube           = label.tb[1,]$cube,
                    time_series    = ts)
                return(row)
            })

        patterns <- dplyr::bind_rows(patterns.lst)
        class(patterns) <- append(class(patterns), "patterns", after = 0)
        return(patterns)
    }

    result <- .sits_factory_function(data, result_fun)
}
