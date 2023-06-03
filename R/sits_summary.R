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
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#' @examples
#' if (sits_run_examples()) {
#'      summary(samples_modis_ndvi)
#' }
#'
#' @export
summary.sits <- function(object, ...) {
    sits_labels_summary(object)
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
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
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
#'          data = test_data,
#'          ml_model = rfor_model
#'      )
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
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
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
#'         probs_cube, output_dir = tempdir()
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
#' @param  only_stats  Show only the statistics? (TRUE/FALSE)
#' @param  sample_size Number of sample used to build statistics
#'
#' @return A summary of the data cube.
#'
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#' @examples
#' if (sits_run_examples()) {
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6",
#'         data_dir = data_dir
#'     )
#' summary(cube)
#' }
#'
#' @export
summary.raster_cube <- function(
        object, ...,
        tile = object$tile[[1]],
        date = NULL,
        only_stats = FALSE,
        sample_size = 100000
) {
    # filter the tile to be processed
    tile <- .summary_check_tile(object, tile)
    if (purrr::is_null(date))
        date <- .tile_timeline(tile)[[1]]
    # only one date at a time
    .check_that(length(date) == 1,
                msg = "only one date per summary is allowed")
    # is this a valid date?
    date <- as.Date(date)
    .check_that(date %in% .tile_timeline(tile),
                msg = "date is not contained in the cube timeline")

    # extract the chosen date
    tile <- .tile_filter_dates(tile, dates = date)
    # get the bands
    bands <- sits_bands(tile)
    if ("CLOUD" %in% bands)
        tile <- .tile_filter_bands(tile, bands[bands != "CLOUD"])
    # extract the file paths
    files <- .tile_paths(tile)

    # print the base information (if requested)
    if (!only_stats)
        .summary_tile_information(tile)
    # read the files with terra
    r <- terra::rast(files)
    # get the a sample of the values
    values <- r %>%
        terra::spatSample(size = sample_size, na.rm = TRUE) %>%
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
#' @param  only_stats  Show only the statistics? (TRUE/FALSE)
#' @param  sample_size Number of sample used to build statistics
#'
#' @return A summary of a probability cube
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
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
summary.probs_cube <- function(
       object, ...,
       tile = object$tile[[1]],
       only_stats = FALSE,
       sample_size = 100000
) {
    .summary_derived_cube(object, tile, only_stats, sample_size)
}
#' @title  Summarize data cubes
#' @method summary variance_cube
#' @name summary.variance_cube
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description This is a generic function. Parameters depend on the specific
#' type of input.
#' @param  object    Object of class "probs_cube"
#' @param ...        Further specifications for \link{summary}.
#' @param  tile        Tile to be summarized
#' @param  only_stats  Show only the statistics? (TRUE/FALSE)
#' @param  sample_size Number of sample used to build statistics
#'
#' @return A summary of a probability cube
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
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
#'         probs_cube, output_dir = tempdir()
#'     )
#'     summary(variance_cube)
#' }
#'
#' @export
summary.variance_cube <- function(
        object, ...,
        tile = object$tile[[1]],
        only_stats = FALSE,
        sample_size = 100000) {
    .summary_derived_cube(object, tile, only_stats, sample_size)
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
#' @param  only_stats  Show only the statistics? (TRUE/FALSE)
#' @param  sample_size Number of sample used to build statistics
#'
#' @return A summary of a classified cube
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
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
#'         probs_cube, output_dir = tempdir()
#'     )
#'     summary(label_cube)
#' }
#' @export
#'
summary.class_cube <- function(
        object, ...,
        tile = object$tile[[1]],
        only_stats = FALSE,
        sample_size = 100000) {
    # check tile
    tile <- .summary_check_tile(object, tile)
    # get the bands
    band <- sits_bands(tile)
    .check_num(
        x = length(band),
        min = 1,
        max = 1,
        is_integer = TRUE,
        msg = "invalid cube - more than one class band")
    # extract the file paths
    files <- .tile_paths(tile)

    # print the base information (if requested)
    if (!only_stats)
        .summary_tile_information(tile)
    # read the files with terra
    r <- terra::rast(files)
    # get the a sample of the values
    class_areas <- r %>%
        terra::expanse(unit = "km", byValue = TRUE)
    # create a tibble
    areas <-  class_areas[, 3]
    # get the result
    data.frame(class = sits_labels(tile), area_km2  = signif(areas, 4))
}
.summary_tile_information <- function(tile) {
    # print the basic tile information
    cat("class       : ", class(tile)[1], "\n")
    cat("dimensions  : ",
        .tile_nrows(tile), ", ",
        .tile_ncols(tile), "  (nrow, ncol)\n", sep = "")
    cat("resolution  : ",
        .tile_xres(tile), ", ",
        .tile_yres(tile), "  (x, y)\n", sep = "")
    cat("extent      : ",
        .xmin(tile), ", ",
        .xmax(tile), ", ",
        .ymin(tile), ", ",
        .ymax(tile),
        "  (xmin, xmax, ymin, ymax)\n", sep = "")
    cat("coord ref   : ", .crs_wkt_to_proj4(tile$crs), "\n")
    return(invisible(NULL))
}
.summary_derived_cube <- function(object,
                                  tile = object$tile[[1]],
                                  only_stats = FALSE,
                                  sample_size = 100000) {

    # filter the tile to be processed
    tile <- .summary_check_tile(object, tile)
    # get the bands
    band <- sits_bands(tile)
    .check_num(
        x = length(band),
        min = 1,
        max = 1,
        is_integer = TRUE,
        msg = "invalid cube - more than one probs band")
    # extract the file paths
    files <- .tile_paths(tile)

    # print the base information (if requested)
    if (!only_stats)
        .summary_tile_information(tile)
    # read the files with terra
    r <- terra::rast(files)
    # get the a sample of the values
    values <- r %>%
        terra::spatSample(size = sample_size, na.rm = TRUE)
    # scale the values
    band_conf <- .tile_band_conf(tile, band)
    scale <- .scale(band_conf)
    offset <- .offset(band_conf)
    sum <- summary(values * scale + offset)
    colnames(sum) <- sits_labels(tile)
    sum
}
.summary_check_tile <- function(object, tile) {
    # only one tile at a time
    .check_chr_parameter(tile)
    # is tile inside the cube?
    .check_chr_contains(
        x = object$tile,
        contains = tile,
        case_sensitive = FALSE,
        discriminator = "one_of",
        can_repeat = FALSE,
        msg = "tile is not included in the cube"
    )
    # filter the tile to be processed
    tile <- .cube_filter_tiles(cube = object, tiles = tile)
    return(tile)
}
