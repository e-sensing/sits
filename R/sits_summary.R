#' @title  Summarize sits
#' @method summary sits
#' @name summary.sits
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description This is a generic function. Parameters depend on the specific
#' type of input.
#'
#' @param  object      Object of classes "sits".
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
    data_labels <- table(object$label)

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
#' @param  object      Object of classe "sits_accuracy".
#' @param  ...         Further specifications for \link{summary}.
#'
#' @return A summary of the sample accuracy
#'
#' @examples
#' if (sits_run_examples()) {
#'     data(cerrado_2classes)
#'     # split training and test data
#'     train_data <- sits_sample(cerrado_2classes, n = 200)
#'     test_data <- sits_sample(cerrado_2classes, n = 200)
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
#'         collection = "MOD13Q1-6",
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
#'         collection = "MOD13Q1-6",
#'         data_dir = data_dir
#'     )
#'     summary(cube)
#' }
#'
#' @export
summary.raster_cube <- function(object, ...,
                                tile = object$tile[[1]],
                                date = NULL) {
    # filter the tile to be processed
    tile <- .summary_check_tile(object, tile)
    if (purrr::is_null(date)) {
        date <- .tile_timeline(tile)[[1]]
    }
    # only one date at a time
    .check_that(length(date) == 1,
        msg = "only one date per summary is allowed"
    )
    # is this a valid date?
    date <- as.Date(date)
    .check_that(date %in% .tile_timeline(tile),
        msg = "date is not contained in the cube timeline"
    )

    # extract the chosen date
    tile <- .tile_filter_dates(tile, dates = date)
    # get the bands
    bands <- sits_bands(tile)
    if ("CLOUD" %in% bands) {
        tile <- .tile_filter_bands(tile, bands[bands != "CLOUD"])
    }
    # extract the file paths
    files <- .tile_paths(tile)
    # print the base information
    .summary_tile_information(tile)
    # get sample size
    sample_size <- .conf("summary_sample_size")
    # read the files with terra
    r <- terra::rast(files)
    # get the a sample of the values
    values <- r |>
        terra::spatSample(size = sample_size, na.rm = TRUE) |>
        tibble::as_tibble()
    # set names as the band names
    bands <- bands[bands != "CLOUD"]
    # extract statistics
    band_conf <- .tile_band_conf(tile, bands[[1]])
    scale <- .scale(band_conf)
    offset <- .offset(band_conf)
    # rescale values
    # get summary
    sum <- summary(values * scale + offset)
    # change names
    colnames(sum) <- bands
    # print statistics
    sum
}
#' @title  Summarize data cubes
#' @method summary probs_cube
#' @name summary.probs_cube
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description This is a generic function. Parameters depend on the specific
#' type of input.
#' @param  object    Object of class "probs_cube"
#' @param ...        Further specifications for \link{summary}.
#' @param  tile        Tile to be summarized
#'
#' @return A summary of a probability cube.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6",
#'         data_dir = data_dir
#'     )
#'     # create a random forest model
#'     rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
#'     # classify a data cube
#'     probs_cube <- sits_classify(
#'         data = cube, ml_model = rfor_model, output_dir = tempdir()
#'     )
#'     summary(probs_cube)
#' }
#'
#' @export
summary.probs_cube <- function(object, ...,
                               tile = object$tile[[1]]) {
    .summary_derived_cube(object, tile)
}
#' @title  Summarize data cubes
#' @method summary variance_cube
#' @name summary.variance_cube
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description This is a generic function. Parameters depend on the specific
#' type of input.
#' @param  object    Object of class "probs_cube"
#' @param ...        Further specifications for \link{summary}.
#' @param  tile      Tile to be summarized
#'
#' @return A summary of a probability cube
#'
#' @examples
#' if (sits_run_examples()) {
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6",
#'         data_dir = data_dir
#'     )
#'     # create a random forest model
#'     rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
#'     # classify a data cube
#'     probs_cube <- sits_classify(
#'         data = cube, ml_model = rfor_model, output_dir = tempdir()
#'     )
#'     # get the variance cube
#'     variance_cube <- sits_variance(
#'         probs_cube,
#'         output_dir = tempdir()
#'     )
#'     summary(variance_cube)
#' }
#'
#' @export
summary.variance_cube <- function(object, ...,
                                  tile = object$tile[[1]]) {
    .summary_derived_cube(object, tile)
}
#' @title  Summarize data cubes
#' @method summary class_cube
#' @name summary.class_cube
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description This is a generic function. Parameters depend on the specific
#' type of input.
#' @param  object    Object of class "class_cube"
#' @param ...        Further specifications for \link{summary}.
#' @param  tile        Tile to be summarized
#'
#' @return A summary of a classified cube
#'
#' @examples
#' if (sits_run_examples()) {
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6",
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
#'
summary.class_cube <- function(object, ...,
                               tile = object$tile[[1]]) {
    # check tile
    tile <- .summary_check_tile(object, tile)
    # get the bands
    band <- sits_bands(tile)
    .check_num(
        x = length(band),
        min = 1,
        max = 1,
        is_integer = TRUE,
        msg = "invalid cube - more than one class band"
    )
    # extract the file paths
    files <- .tile_paths(tile)
    # print the base information (if requested)
    .summary_tile_information(tile)
    # read the files with terra
    r <- terra::rast(files)
    # get the a sample of the values
    class_areas <- r |>
        terra::expanse(unit = "km", byValue = TRUE)
    # change value to character
    class_areas <- dplyr::mutate(class_areas,
        value = as.character(.data[["value"]])
    )
    # create a data.frame with the labels
    labels <- sits_labels(tile)
    df1 <- data.frame(value = names(labels), class = unname(labels))
    # join the labels with the areas
    sum <- dplyr::full_join(df1, class_areas, by = "value")
    sum <- dplyr::mutate(sum,
        area_km2 = signif(.data[["area"]], 4),
        .keep = "unused"
    )
    # remove layer information
    sum_clean <- sum[, -3] |>
        stats::na.omit()
    # are there NA's ?
    sum_NA <- dplyr::filter(sum, is.na(.data[["area_km2"]]))
    # inform missing classes
    if (nrow(sum_NA) > 0) {
        message(
            "classes ", paste0(sum_NA$class, collapse = " "),
            " have no area"
        )
    }
    # show the result
    return(sum_clean)
}
