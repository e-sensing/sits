#' @title  Summarize sits
#' @method summary sits
#' @name summary.sits
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Felipe Souza, \email{felipe.souza@@inpe.br}
#' @description This is a generic function. Parameters depend on the specific
#' type of input.
#'
#' @param  object      Object of class "sits".
#' @param  ...         Further specifications for \link{summary}.
#'
#' @return A summary of the sits tibble.
#'
#' @examples
#' if (sits_run_examples()) {
#'     summary(samples_modis_ndvi)
#' }
#'
#' @export
summary.sits <- function(object, ...) {
    # get frequency table
    data_labels <- table(object[["label"]])

    # compose tibble containing labels, count and relative frequency columns
    result <- tibble::as_tibble(list(
        label = names(data_labels),
        count = as.integer(data_labels),
        prop = as.numeric(prop.table(data_labels))
    ))
    return(result)
}

#' @title  Summarize accuracy matrix for training data
#' @method summary sits_accuracy
#' @name summary.sits_accuracy
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description This is a generic function. Parameters depend on the specific
#' type of input.
#'
#' @param  object      Object of class "sits_accuracy".
#' @param  ...         Further specifications for \link{summary}.
#'
#' @return A summary of the sample accuracy
#'
#' @examples
#' if (sits_run_examples()) {
#'     data(cerrado_2classes)
#'     # split training and test data
#'     train_data <- sits_sample(cerrado_2classes, frac = 0.5)
#'     test_data  <- sits_sample(cerrado_2classes, frac = 0.5)
#'     # train a random forest model
#'     rfor_model <- sits_train(train_data, sits_rfor())
#'     # classify test data
#'     points_class <- sits_classify(
#'         data = test_data,
#'         ml_model = rfor_model
#'     )
#'     # measure accuracy
#'     acc <- sits_accuracy(points_class)
#'     summary(acc)
#' }
#'
#' @export
summary.sits_accuracy <- function(object, ...) {
    sits_accuracy_summary(object)
}
#' @title  Summarize accuracy matrix for area data
#' @method summary sits_area_accuracy
#' @name summary.sits_area_accuracy
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description This is a generic function. Parameters depend on the specific
#' type of input.
#'
#' @param  object      Object of classe "sits_accuracy".
#' @param  ...         Further specifications for \link{summary}.
#'
#' @return A summary of the sample accuracy
#'
#' @examples
#' if (sits_run_examples()) {
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'     # create a random forest model
#'     rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
#'     # classify a data cube
#'     probs_cube <- sits_classify(
#'         data = cube, ml_model = rfor_model, output_dir = tempdir()
#'     )
#'     # label the probability cube
#'     label_cube <- sits_label_classification(
#'         probs_cube,
#'         output_dir = tempdir()
#'     )
#'     # obtain the ground truth for accuracy assessment
#'     ground_truth <- system.file("extdata/samples/samples_sinop_crop.csv",
#'         package = "sits"
#'     )
#'     # make accuracy assessment
#'     as <- sits_accuracy(label_cube, validation = ground_truth)
#'     summary(as)
#' }
#'
#' @export
summary.sits_area_accuracy <- function(object, ...) {
    print.sits_area_accuracy(object)
}
#' @title  Summarize data cubes
#' @method summary raster_cube
#' @name summary.raster_cube
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Felipe Souza, \email{felipe.souza@@inpe.br}
#' @description This is a generic function. Parameters depend on the specific
#' type of input.
#'
#' @param  object      Object of classes "raster_cube".
#' @param  ...         Further specifications for \link{summary}.
#' @param  tile        Tile to be summarized
#' @param  date        Date to be summarized
#'
#' @return A summary of the data cube.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'     summary(cube)
#' }
#'
#' @export
summary.raster_cube <- function(object, ..., tile = NULL, date = NULL) {
    .check_set_caller("summary_raster_cube")
    # Pre-conditional check
    .check_date_parameter(date, allow_null = TRUE)
    .check_chr_parameter(tile, allow_null = TRUE)
    # Extract the chosen tile
    if (!is.null(tile)) {
        object <- .summary_check_tile(object, tile)
    }
    # Extract the chosen date
    if (!is.null(date)) {
        object <- .cube_filter_dates(object, dates = date)
    }
    # Display cube general metadata
    cli::cli_h1("Cube Metadata")
    cli::cli_li("Class: {.field raster_cube}")
    cube_bbox <- .bbox(object)[, c('xmin', 'xmax', 'ymin', 'ymax')]
    cli::cli_li("Bounding Box: xmin = {.field {cube_bbox[['xmin']]}},
                               xmax = {.field {cube_bbox[['xmax']]}},
                               ymin = {.field {cube_bbox[['ymin']]}},
                               ymax = {.field {cube_bbox[['ymax']]}}")
    cli::cli_li("Bands: {.field {(.cube_bands(object))}}")
    timeline <- unique(lubridate::as_date(unlist(.cube_timeline(object))))
    cli::cli_li("Timeline: {.field {timeline}}")
    is_regular <- .cube_is_complete(object)
    cli::cli_li("Regular cube: {.field {is_regular}}")
    # Display cube cloud coverage
    if ("CLOUD" %in% .cube_bands(object) &&
        .has_column(.fi(object), "cloud_cover")) {
        cube_unnest <- tidyr::unnest(
            object[, c("tile", "file_info")], "file_info"
        )
        cli::cli_h1("Cloud cover info")
        cube_unnest <- cube_unnest[, c("tile", "date", "cloud_cover")]
        cube_unnest <- unique(dplyr::arrange(cube_unnest, .data[["date"]]))
        print(cube_unnest, n = Inf)
    }
    # Display raster summary
    cli::cli_h1("Cube Summary")
    sum <- slider::slide(object, function(tile) {
        # Get the first date to not read all images
        date <- .default(date, .tile_timeline(tile)[[1]])
        tile <- .tile_filter_dates(tile, date)
        bands <- if (is_regular) .tile_bands(tile) else .tile_bands(tile)[[1]]
        tile <- .tile_filter_bands(tile, bands)
        cli::cli_h3("Tile: {.field {tile$tile}} and Date: {.field {date}}")
        rast <- .raster_open_rast(.tile_paths(tile))
        sum <- suppressWarnings(.raster_summary(rast))
        print(sum)
        return(sum)
    })
    # Return the summary from the cube
    names(sum) <- .cube_tiles(object)
    return(invisible(sum))
}
#' @title Summary of a derived cube
#' @author Felipe Souza, \email{felipe.souza@@inpe.br}
#' @noRd
#' @param object       data cube
#' @param  ...         Further specifications for \link{summary}.
#' @param sample_size  The size of samples will be extracted from the variance
#'                     cube.
#' @return Summary of a derived cube
#'
#' @examples
#' if (sits_run_examples()) {
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'     # create a random forest model
#'     rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
#'     # classify a data cube
#'     probs_cube <- sits_classify(
#'         data = cube, ml_model = rfor_model, output_dir = tempdir()
#'     )
#'     summary(probs_cube)
#'     # get the variance cube
#'     variance_cube <- sits_variance(
#'         probs_cube,
#'         output_dir = tempdir()
#'     )
#'     summary(variance_cube)
#' }
#'
#' @export
summary.derived_cube <- function(object, ..., sample_size = 10000) {
    .check_set_caller("summary_derived_cube")
    # Get cube labels
    labels <- unname(.cube_labels(object))
    # Extract variance values for each tiles using a sample size
    var_values <- slider::slide(object, function(tile) {
        # get the bands
        band <- .tile_bands(tile)
        # extract the file path
        file <- .tile_paths(tile)
        # read the files with terra
        r <- .raster_open_rast(file)
        # get the a sample of the values
        values <- r |>
            .raster_sample(size = sample_size, na.rm = TRUE)
        # scale the values
        band_conf <- .tile_band_conf(tile, band)
        scale <- .scale(band_conf)
        offset <- .offset(band_conf)
        values <- values * scale + offset
        values
    })
    # Combine variance values
    var_values <- dplyr::bind_rows(var_values)
    var_values <- summary(var_values)
    # Update columns name
    colnames(var_values) <- labels
    # Return summary values
    return(var_values)
}
#' @title  Summarise variance cubes
#' @method summary variance_cube
#' @name summary.variance_cube
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description This is a generic function. Parameters depend on the specific
#' type of input.
#' @param  object       Object of class "class_cube"
#' @param  ...          Further specifications for \link{summary}.
#' @param  sample_size  The size of samples will be extracted from the variance
#'                      cube.
#' @param  intervals    Intervals to calculate the quantiles
#' @param  quantiles    Quantiles to be shown
#'
#' @return A summary of a variance cube
#'
#' @examples
#' if (sits_run_examples()) {
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'     # create a random forest model
#'     rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
#'     # classify a data cube
#'     probs_cube <- sits_classify(
#'         data = cube, ml_model = rfor_model, output_dir = tempdir()
#'     )
#'     variance_cube <- sits_variance(
#'         data = probs_cube,
#'         output_dir = tempdir()
#'     )
#'     summary(variance_cube)
#' }
#' @export
summary.variance_cube <- function(
        object, ...,
        intervals = 0.05,
        sample_size = 10000,
        quantiles = c("75%", "80%", "85%", "90%", "95%", "100%")) {
    .check_set_caller("summary_variance_cube")
    # Get cube labels
    labels <- unname(.cube_labels(object))
    # Extract variance values for each tiles using a sample size
    var_values <- slider::slide(object, function(tile) {
        # get the bands
        band <- .tile_bands(tile)
        # extract the file path
        file <- .tile_paths(tile)
        # read the files with terra
        r <- .raster_open_rast(file)
        # get the a sample of the values
        values <- r |>
            .raster_sample(size = sample_size, na.rm = TRUE)
        # scale the values
        band_conf <- .tile_band_conf(tile, band)
        scale <- .scale(band_conf)
        offset <- .offset(band_conf)
        values <- values * scale + offset
        values
    })
    # Combine variance values
    var_values <- dplyr::bind_rows(var_values)
    # Update columns name
    colnames(var_values) <- labels
    # Extract quantile for each column
    var_values <- dplyr::reframe(
        var_values,
        dplyr::across(.cols = dplyr::all_of(labels), function(x) {
            stats::quantile(x, probs = seq(0, 1, intervals))
        })
    )
    # Update row names
    percent_intervals <- paste0(seq(from = 0, to = 1, by = intervals)*100, "%")
    rownames(var_values) <- percent_intervals
    # Return variance values filtered by quantiles
    return(var_values[quantiles, ])
}
#' @title  Summarize data cubes
#' @method summary class_cube
#' @name summary.class_cube
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description This is a generic function. Parameters depend on the specific
#' type of input.
#' @param  object    Object of class "class_cube"
#' @param ...        Further specifications for \link{summary}.
#'
#' @return A summary of a classified cube
#'
#' @examples
#' if (sits_run_examples()) {
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'     # create a random forest model
#'     rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
#'     # classify a data cube
#'     probs_cube <- sits_classify(
#'         data = cube, ml_model = rfor_model, output_dir = tempdir()
#'     )
#'     # label the probability cube
#'     label_cube <- sits_label_classification(
#'         probs_cube,
#'         output_dir = tempdir()
#'     )
#'     summary(label_cube)
#' }
#' @export
summary.class_cube <- function(object, ...) {
    .check_set_caller("summary_class_cube")
    # Get cube labels
    labels <- unname(.cube_labels(object))
    # Extract classes values for each tiles using a sample size
    classes_areas <- slider::slide(object, function(tile) {
        # get the bands
        band <- .tile_bands(tile)
        # extract the file path
        file <- .tile_paths(tile)
        # read the files with terra
        r <- .raster_open_rast(file)
        # get a frequency of values
        class_areas <- .raster_freq(r)
        # transform to km^2
        cell_size <- .tile_xres(tile) * .tile_yres(tile)
        class_areas[["area"]] <-  (class_areas[["count"]] * cell_size) / 10^6
        # change value to character
        class_areas <- dplyr::mutate(
            class_areas, value = as.character(.data[["value"]])
        )
        # create a data.frame with the labels
        labels <- .tile_labels(tile)
        df1 <- tibble::tibble(value = names(labels), class = unname(labels))
        # join the labels with the areas
        sum <- dplyr::full_join(df1, class_areas, by = "value")
        sum <- dplyr::mutate(sum,
                             area_km2 = signif(.data[["area"]], 2),
                             .keep = "unused"
        )
        # remove layer information
        sum_clean <- sum[, -3] |>
            tidyr::replace_na(list(layer = 1, count = 0, area_km2 = 0))

        sum_clean
    })
    # Combine tiles areas
    classes_areas <- dplyr::bind_rows(classes_areas) |>
        dplyr::group_by(.data[["value"]], .data[["class"]]) |>
        dplyr::summarise(
            count = sum(.data[["count"]]),
            area_km2 = sum(.data[["area_km2"]]),
            .groups = "keep") |>
        dplyr::ungroup()
    # Return classes areas
    return(classes_areas)
}
