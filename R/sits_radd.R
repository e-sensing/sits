sits_radd <- function(data, pdf, ...,
                      stats_layer = NULL,
                      chi = 0.9,
                      start_date = NULL,
                      end_date = NULL) {

    UseMethod("sits_radd", data)
}


sits_radd.sits <- function(data,
                           pdf = "gaussian",
                           ...,
                           stats_layer = NULL,
                           chi = 0.9,
                           start_date = NULL,
                           end_date = NULL) {
    # Training function
    train_fun <- function(data) {
        # Check 'pdf' parameter
        .check_chr_parameter(pdf)
        # Check 'chi' parameter
        .check_num_min_max(chi, min = 0.1, max = 1)
        # Check 'start_date' parameter
        .check_date_parameter(start_date)
        # Check 'end_date' parameter
        .check_date_parameter(end_date)

        # Get pdf function
        pdf_fn <- .pdf_fun(pdf)
        # Create stats layer
        if (!.has(stats_layer)) {
            stats_layer <- .radd_create_stats(data)
        }
        # Calculate probability for NF
        data <- .radd_calc_pnf(
            data = data,
            pdf_fn = pdf_fn,
            stats_layer = stats_layer
        )
        predict_fun <- function() {
            # Now we need to detected the changes
            data <- .radd_detect_events(
                data = data,
                threshold = 0.5,
                start_date = start_date,
                end_date = end_date
            )
        }
        # Set model class
        predict_fun <- .set_class(
            predict_fun, "radd_model", "sits_model", class(predict_fun)
        )
        return(predict_fun)
    }
    # If samples is informed, train a model and return a predict function
    # Otherwise give back a train function to train model further
    result <- .factory_function(data, train_fun)
    return(result)
}

sits_radd.raster_cube <- function(data,
                                  pdf = "gaussian",
                                  ...,
                                  stats_layer = NULL,
                                  chi = 0.9,
                                  start_date = NULL,
                                  end_date = NULL) {
    # Training function
    train_fun <- function(data) {
        # Check 'pdf' parameter
        .check_chr_parameter(pdf)
        # Check 'chi' parameter
        .check_num_min_max(chi, min = 0.1, max = 1)
        # Check 'start_date' parameter
        .check_date_parameter(start_date)
        # Check 'end_date' parameter
        .check_date_parameter(end_date)

        # Get pdf function
        pdf_fn <- .pdf_fun(pdf)
        # Create stats layer
        if (!.has(stats_layer)) {
            stats_layer <- .radd_create_stats(data)
        }
        # Calculate probability for NF
        data <- .radd_calc_pnf(
            data = data,
            pdf_fn = pdf_fn,
            stats_layer = stats_layer
        )
        predict_fun <- function() {
            # Now we need to detected the changes
            data <- .radd_detect_events(
                data = data,
                threshold = 0.5,
                start_date = start_date,
                end_date = end_date
            )
        }
        # Set model class
        predict_fun <- .set_class(
            predict_fun, "radd_model", "sits_model", class(predict_fun)
        )
        return(predict_fun)
    }
    # If samples is informed, train a model and return a predict function
    # Otherwise give back a train function to train model further
    result <- .factory_function(data, train_fun)
    return(result)


}
