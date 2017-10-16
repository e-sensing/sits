#' @title Create time series patterns for classification
#' @name sits_patterns
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#'
#' @description This function allows the user to select different alternatives to define a set of
#' patterns, given his samples. The alternatives are:
#' "gam" - uses a generalised additive model to approximate a smooth spline for each pattern
#' "dendogram" - uses a herarchical clustering method to group the patterns
#' "centroids" - uses a positional clustering method to group the patterns
#'
#' @param data.tb          a SITS tibble time series with an alignment column
#' @param timeline         the valid timeline for the data
#' @param timeline         The timeline for the coverage (all acquisition dates)
#' @param pt_method        a pattern fitting method
#' @return result          a model fitted into input data given by train_method parameter
#' @export
#'
sits_patterns <- function(data.tb, timeline, pt_method = sits_gam(data.tb = NULL, timeline = NULL,
                                                        from = NULL, to = NULL, freq = 8, formula = y ~ s(x), interval = "12 month")) {

    # does the input data exist?
    .sits_test_tibble (data.tb)
    # is the train method a function?
    ensurer::ensure_that(pt_method, class(.) == "function", err_desc = "sits_train: train_method is not a valid function")

    # compute the training method by the given data
    result <- pt_method(data.tb, timeline)
    return(result)

}
#' @title Do not create patterns for classification
#' @name sits_patterns_from_data
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#'
#' @description This function is to be used when oen does not want to create
#' a set of idealised patterns from the input data, but prefers to use the
#' input series themselves as training data from estimating a prediction model.
#' It should be used in connection with sits_distances_from_data.
#'
#' @param  data.tb           a SITS tibble time series
#' @param  timeline          timeline with the all dates for the coverage
#' @param  interval          the interval to obtain the patterns
#' @return patterns.tb       a SITS tibble time series used as reference for traning the model
#' @export
sits_patterns_from_data <- function (data.tb = NULL, timeline = NULL, interval = "12 month"){


    # function that is used to be called as a value from another function
    result_fun <- function(tb, timeline){

        # ensure timeline is not null
        ensurer::ensure_that(timeline, !purrr::is_null(.), err_desc = "sits_patterns : please provide the timeline of the coverage")

        times <- timeline

        # create a reference tibble that will align all training data to the same
        # dates as the first entry in the input data
        # the reference tibble has one sample of each label
        ref.tb  <-  sits_sample (tb, n = 1)

        # create a tibble to store the results
        patterns.tb <- sits_tibble_patterns()

        ensurer::ensure_that(tb[1,]$start_date, .sits_is_valid_start_date(., timeline),
                             err_desc = ".sits_patterns: expected start date in not inside timeline of observations")

        ensurer::ensure_that(tb[1,]$end_date, .sits_is_valid_end_date(., timeline),
                             err_desc = ".sits_patterns: expected end date in not inside timeline of observations")


        times <- timeline
        for (i in 1:NROW(ref.tb)){

            row <- ref.tb[i,]

            ref_dates.lst <- .sits_match_timelines(timeline, row$start_date, row$end_date, interval = interval)

            patterns.tb <- tibble::add_row (patterns.tb,
                                             start_date     = row$start_date,
                                             end_date       = row$end_date,
                                             label          = row$label,
                                             coverage       = row$coverage,
                                             timeline       = list(times),
                                             ref_dates      = list(ref_dates.lst),
                                             time_series    = row$time_series)

        }

        return (patterns.tb)

    }
    result <- .sits_factory_function2 (data.tb, timeline, result_fun)
}


