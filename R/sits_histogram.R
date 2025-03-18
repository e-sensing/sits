#' @title  Histogram
#' @method hist sits
#' @name hist.sits
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description This is a generic function. Parameters depend on the specific
#' type of input.
#'
#' @param  x     Object of classes "sits".
#' @param  ...         Further specifications for \link{hist}.
#'
#' @return A summary of the sits tibble.
#'
#' @examples
#' if (sits_run_examples()) {
#'     hist(samples_modis_ndvi)
#' }
#'
#' @export
hist.sits <- function(x, ...) {
    # get frequency table
    print("histogram of time series not available")
}
#' @title  histogram of data cubes
#' @method hist raster_cube
#' @name hist.raster_cube
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description This is a generic function. Parameters depend on the specific
#' type of input.
#'
#' @param  x           Object of classes "raster_cube".
#' @param  ...         Further specifications for \link{summary}.
#' @param  tile        Tile to be shown
#' @param  date        Date to be shown
#' @param  band        Band to be shown
#' @param  size       Number of cells to be sampled
#'
#' @return A histogram of one band of  data cube.
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
#'     hist(cube)
#' }
#'
#' @export
hist.raster_cube <- function(x, ...,
                             tile = x[["tile"]][[1]],
                             date = NULL,
                             band = NULL,
                             size = 10000) {
    .check_set_caller("summary_raster_cube")
    # Pre-conditional check
    .check_date_parameter(date, allow_null = TRUE)
    .check_chr_parameter(tile, allow_null = TRUE)

    # is tile inside the cube?
    .check_chr_contains(
        x = x[["tile"]],
        contains = tile,
        case_sensitive = FALSE,
        discriminator = "one_of",
        can_repeat = FALSE,
        msg = .conf("messages", "sits_hist_tile")
    )
    # filter the tile to be processed
    tile <- .cube_filter_tiles(cube = x, tiles = tile)
    if (.has(date)) {
        # is this a valid date?
        date <- as.Date(date)
        .check_that(date %in% .tile_timeline(tile),
                    msg = .conf("messages", "sits_hist_date")
        )
    } else {
        date <- .tile_timeline(tile)[[1]]
    }
    if (.has(band)) {
        # is this a valid band?
        .check_that(band %in% .tile_bands(tile),
                    msg = .conf("messages", "sits_hist_band")
        )
    } else {
        band <- .tile_bands(tile)[[1]]
    }
    # select the file to be plotted
    band_file <- .tile_path(tile, band, date)
    # scale and offset
    band_conf <- .tile_band_conf(tile, band)
    band_scale <- .scale(band_conf)
    band_offset <- .offset(band_conf)
    #
    r <- .raster_open_rast(band_file)
    values <- .raster_sample(r, size = size)
    values <- values * band_scale + band_offset
    colnames(values) <- band

    density_plot <-
        values |>
        ggplot2::ggplot(ggplot2::aes(x = .data[[band]])) +
        ggplot2::geom_density(
            ggplot2::aes(x = .data[[band]]),
            color = "#000000",
            fill = "#94C293",
            linewidth = 0.8,
            alpha = 0.50,
            show.legend = FALSE
        ) +
        ggplot2::scale_x_continuous(limits = c(0.0, 1.0)) +
        ggplot2::xlab("Ground reflectance") +
        ggplot2::ylab("") +
        ggplot2::ggtitle(paste("Distribution of Values for band",
                               band,"date", date))

    return(suppressWarnings(density_plot))
}
#' @title  histogram of prob cubes
#' @method hist probs_cube
#' @name hist.probs_cube
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description This is a generic function. Parameters depend on the specific
#' type of input.
#'
#' @param  x           Object of classes "raster_cube".
#' @param  ...         Further specifications for \link{summary}.
#' @param  tile        Tile to be shown
#' @param  label       Label to be shown
#' @param  size        Number of cells to be sampled
#'
#' @return A histogram of one label of a probability cube.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     modis_cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'     rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
#'     probs_cube <- sits_classify(
#'         data = modis_cube,
#'         ml_model = rfor_model,
#'         output_dir = tempdir()
#'     )
#'     hist(probs_cube, label = "Forest")
#' }
#'
#' @export
hist.probs_cube <- function(x, ...,
                             tile = x[["tile"]][[1]],
                             label  = NULL,
                             size = 100000) {
    .check_set_caller("sits_hist_raster_cube")
    # Pre-conditional check
    .check_chr_parameter(tile, allow_null = TRUE)

    # is tile inside the cube?
    .check_chr_contains(
        x = x[["tile"]],
        contains = tile,
        case_sensitive = FALSE,
        discriminator = "one_of",
        can_repeat = FALSE,
        msg = .conf("messages", "sits_hist_tile")
    )
    # filter the tile to be processed
    tile <- .cube_filter_tiles(cube = x, tiles = tile)
    # check the labels
    if (.has(label)) {
        # is this a valid label?
        .check_that(label %in% .tile_labels(tile),
                    msg = .conf("messages", "sits_hist_label")
        )
    } else {
        label <- .tile_labels(tile)[[1]]
    }
    # select the file to be plotted
    probs_file <- .tile_path(tile)
    band <- .tile_bands(tile)
    # scale and offset
    band_conf <- .tile_band_conf(tile, band)
    band_scale <- .scale(band_conf)
    band_offset <- .offset(band_conf)

    # recover all labels
    all_labels <- .tile_labels(tile)
    layers <- seq_len(length(all_labels))
    names(layers) <- all_labels
    # read file
    r <- .raster_open_rast(probs_file)
    # select layer
    layers <- layers[label]
    values <- .raster_sample(r[[layers]], size = size)
    values <- values * band_scale + band_offset
    colnames(values) <- label
    color_sits <- .colors_get(label)
    # values[["color"]] <- colors_sits[values[["name"]]]
    density_plot <-
        values |>
        ggplot2::ggplot(ggplot2::aes(x = .data[[label]])) +
        ggplot2::geom_density(
            ggplot2::aes(x = .data[[label]]),
            color = color_sits,
            fill = color_sits,
            linewidth = 0.8,
            alpha = 0.50
        ) +
        ggplot2::scale_x_continuous(limits = c(0.0, 1.0)) +
        ggplot2::xlab("Probability") +
        ggplot2::ylab("") +
        ggplot2::theme(legend.title = ggplot2::element_blank()) +
        ggplot2::ggtitle(paste("Probabilities for label", label))

    return(suppressWarnings(density_plot))
}

#' @title  Histogram uncertainty cubes
#' @method hist uncertainty_cube
#' @name hist.uncertainty_cube
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description This is a generic function. Parameters depend on the specific
#' type of input.
#' @param  x         Object of class "variance_cube"
#' @param ...        Further specifications for \link{hist}.
#' @param  tile      Tile to be summarized
#' @param  size      Sample size
#'
#' @return A histogram of a uncertainty cube
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
#'     uncert_cube <- sits_uncertainty(
#'         cube = probs_cube,
#'         output_dir = tempdir()
#'     )
#'     hist(uncert_cube)
#' }
#' @export
hist.uncertainty_cube <- function(
        x, ...,
        tile = x[["tile"]][[1]],
        size = 100000) {
    .check_set_caller("sits_hist_uncertainty_cube")
    # Pre-conditional check
    .check_chr_parameter(tile, allow_null = TRUE)
    # Extract the chosen tile
    .check_chr_contains(
        x = x[["tile"]],
        contains = tile,
        case_sensitive = FALSE,
        discriminator = "one_of",
        can_repeat = FALSE,
        msg = .conf("messages", "sits_hist_tile")
    )
    # filter the tile to be processed
    tile <- .cube_filter_tiles(cube = x, tiles = tile)

    # select the file to be plotted
    uncert_file <- .tile_path(tile)
    band <- .tile_bands(tile)
    # scale and offset
    band_conf <- .tile_band_conf(tile, band)
    band_scale <- .scale(band_conf)
    band_offset <- .offset(band_conf)
    # read file
    r <- .raster_open_rast(uncert_file)
    values <- .raster_sample(r, size = size)
    values <- values * band_scale + band_offset
    max <- max(values)
    colnames(values) <- band
    density_plot <-
        values |>
        ggplot2::ggplot(ggplot2::aes(x = .data[[band]])) +
        ggplot2::geom_density(
            ggplot2::aes(x = .data[[band]]),
            color = "#000000",
            fill = "#94C293",
            linewidth = 0.8,
            alpha = 0.50,
            show.legend = FALSE
        ) +
        ggplot2::scale_x_continuous(limits = c(0.0, max)) +
        ggplot2::xlab("Uncertainty") +
        ggplot2::ylab("") +
        ggplot2::ggtitle(paste("Distribution of uncertainty for band", band))

    return(suppressWarnings(density_plot))

}

