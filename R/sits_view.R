#' @title  View data cubes and samples in leaflet
#' @name sits_view
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Uses leaflet to visualize time series, raster cube and
#' classified images.
#'
#' @note
#' To show a false color image, use "band" to chose one
#' of the bands, "tiles" to select tiles,
#' "first_quantile" and "last_quantile" to set the cutoff points. Choose
#' only one date in the "dates" parameter. The color
#' scheme is defined by either "palette" (use an available color scheme) or
#' legend (user-defined color scheme). To see which palettes are pre-defined,
#' use \code{cols4all::g4a_gui} or select any ColorBrewer name. The "rev"
#' parameter reverts the order of colors in the palette.
#'
#' To show an RGB composite, select "red", "green" and "blue" bands, "tiles",
#' "dates", "opacity", "first_quantile" and "last_quantile". One can also get
#' an RGB composite, by selecting one band and three dates. In this case,
#' the first date will be shown in red, the second in green and third in blue.
#'
#' Probability cubes are shown in false color. The parameter "labels" controls
#' which labels are shown. If left blank, only the first map is shown. For
#' color control, use "palette", "legend", and "rev" (as described above).
#'
#' Vector cubes have both a vector and a raster component. The vector part
#' are the segments produced by \code{\link{sits_segment}}. Their
#' visual output is controlled by "seg_color" and "line_width" parameters.
#' The raster output works in the same way as the false color and RGB views
#' described above.
#'
#' Classified cubes need information on how to render each class. There are
#' three options: (a) the classes are part of an existing color scheme;
#' (b) the user provides a legend which associates each class to a color;
#' (c) use a generic palette (such as "Spectral") and allocate colors
#' based on this palette. To find out how to create a customized color
#' scheme, read the chapter "Data Visualisation in sits" in the sits book.
#'
#' To compare different classifications, use the "version" parameter to
#' distinguish between the different maps that are shown.
#'
#' Vector classified cubes are displayed as classified cubes, with the
#' segments overlaid on top of the class map, controlled by "seg_color"
#' and "line_width".
#'
#' Samples are shown on the map based on their geographical locations and
#' on the color of their classes assigned in their color scheme. Users can
#' also assign a legend or a palette to choose colors. See information above
#' on the display of classified cubes.
#'
#' For all types of data cubes, the following parameters apply:
#' \itemize{
#' \item opacity: controls the transparency of the map.
#' \item max_cog_size: For COG data, controls the level of aggregation
#' to be used for display, measured in pixels, e.g., a value of 512 will
#' select a 512 x 512 aggregated image. Small values are faster to
#' show, at a loss of visual quality.
#' \item leaflet_megabytes: maximum size of leaflet to be shown associated
#' to the map (in megabytes). Bigger values use more memory.
#' \item add: controls whether a new visualisation will be overlaid on
#' top of an existing one. Default is FALSE.
#' }
#'
#' @param  x             Object of class "sits", "data.frame", "som_map",
#'                       "raster_cube", "probs_cube", "vector_cube",
#'                       or "class cube".
#' @param  ...           Further specifications for \link{sits_view}.
#' @param  band          Single band for viewing false color images.
#' @param  red           Band for red color.
#' @param  green         Band for green color.
#' @param  blue          Band for blue color.
#' @param  dates         Dates to be plotted.
#' @param  tiles         Tiles to be plotted (in case of a multi-tile cube).
#' @param  label         Label to be plotted (in case of probs cube)
#' @param  legend        Named vector that associates labels to colors.
#' @param  palette       Color palette from RColorBrewer
#' @param  rev           Revert color palette?
#' @param  version       Version name (to compare different classifications)
#' @param  opacity       Opacity of segment fill or class cube
#' @param  seg_color     Color for segment boundaries
#' @param  line_width    Line width for segments (in pixels)
#' @param  max_cog_size  Maximum size of COG overviews (lines or columns)
#' @param  first_quantile First quantile for stretching images
#' @param  last_quantile  Last quantile for stretching images
#' @param  leaflet_megabytes Maximum size for leaflet (in MB)
#' @param  id_neurons    Neurons from the SOM map to be shown.
#' @param  radius        Radius of circle markers
#' @param  add           Add image to current leaflet
#'
#' @return               A leaflet object containing either samples or
#'                       data cubes embedded in a global map that can
#'                       be visualized directly in an RStudio viewer.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # view samples
#'     sits_view(cerrado_2classes)
#'     # create a local data cube
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     modis_cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'     # view the data cube
#'     sits_view(modis_cube,
#'         band = "NDVI"
#'     )
#'     # train a model
#'     rf_model <- sits_train(samples_modis_ndvi, sits_rfor())
#'     # classify the cube
#'     modis_probs <- sits_classify(
#'         data = modis_cube,
#'         ml_model = rf_model,
#'         output_dir = tempdir()
#'     )
#'     # generate a map
#'     modis_label <- sits_label_classification(
#'         modis_probs,
#'         output_dir = tempdir()
#'     )
#'     # view the classified map
#'     sits_view(modis_label)
#'     # view the classified map with the B/W image
#'     sits_view(modis_cube,
#'         band = "NDVI",
#'         class_cube = modis_label,
#'         dates = sits_timeline(modis_cube)[[1]]
#'     )
#'     # view the classified map with the RGB image
#'     sits_view(modis_cube,
#'         red = "NDVI", green = "NDVI", blue = "NDVI",
#'         class_cube = modis_label,
#'         dates = sits_timeline(modis_cube)[[1]]
#'     )
#'     # create an uncertainty cube
#'     modis_uncert <- sits_uncertainty(
#'         cube = modis_probs,
#'         output_dir = tempdir()
#'     )
#'     # view the uncertainty cube
#'     sits_view(modis_uncert, rev = TRUE)
#' }
#' @export
sits_view <- function(x, ...) {
    UseMethod("sits_view", x)
}
#' @rdname   sits_view
#'
#' @export
sits_view.sits <- function(x, ...,
                           legend = NULL,
                           palette = "Set3",
                           radius = 10,
                           add = FALSE) {
    .check_set_caller("sits_view_sits")
    # precondition
    .check_require_packages("leaflet")
    # check samples contains the expected columns
    .check_that(all(c("longitude", "latitude", "label") %in% colnames(x)))
    # check palette
    .check_palette(palette)
    # check logical control
    .check_lgl_parameter(add)

    # if not ADD, create a new sits leaflet
    if (!add)
        .conf_clean_leaflet()

    # recover global leaflet objects
    overlay_groups <- sits_env[["leaflet"]][["overlay_groups"]]
    leaf_map       <- sits_env[["leaflet"]][["leaf_map"]]

    # create a leaflet for samples
    leaf_map <- leaf_map |>
        .view_samples(
            samples = x,
            group = "samples",
            legend = legend,
            palette = palette,
            radius = radius
    )
    # append samples to overlay groups
    overlay_groups <- append(overlay_groups, "samples")
    # add layers control and update global leaflet-related variables
    leaf_map <- leaf_map |>
        .view_add_layers_control(overlay_groups) |>
        .view_update_global_leaflet(overlay_groups)

    # return the leaflet
    return(leaf_map)
}
#' @rdname   sits_view
#'
#' @export
sits_view.data.frame <- function(x, ...,
                                 legend = NULL,
                                 palette = "Harmonic",
                                 add = FALSE) {
    leaf_map <- sits_view.sits(x, legend, palette, add)
    return(leaf_map)
}
#' @rdname   sits_view
#'
#' @export
sits_view.som_map <- function(x, ...,
                              id_neurons,
                              legend = NULL,
                              palette = "Harmonic",
                              radius = 10,
                              add = FALSE) {
    .check_set_caller("sits_view_som_map")
    # check id_neuron
    .check_int_parameter(
        id_neurons,
        min = 1,
        max = max(unique(x[["labelled_neurons"]][["id_neuron"]])),
        len_min = 1,
        len_max = length(unique(x[["labelled_neurons"]][["id_neuron"]]))
    )
    # if not ADD, create a new sits leaflet
    if (!add)
        .conf_clean_leaflet()

    # recover global leaflet info
    overlay_groups <- sits_env[["leaflet"]][["overlay_groups"]]
    leaf_map <- sits_env[["leaflet"]][["leaf_map"]]

    # get the samples
    samples <- x[["data"]]
    labels <- sort(unique(samples[["label"]]))

    for (id in id_neurons) {
        # assign group name (one neuron per)
        group <- paste("neuron", id)

        # first select unique locations
        samples_neuron <- dplyr::filter(
            samples, .data[["id_neuron"]] == id
        )
        leaf_map <- leaf_map |>
            .view_neurons(
                samples = samples_neuron,
                labels = labels,
                group = group,
                legend = legend,
                palette = palette,
                radius = radius
            )
        # append samples to overlay groups
        overlay_groups <- append(overlay_groups, group)
    }
    # add layers control and update global leaflet-related variables
    leaf_map <- leaf_map |>
        .view_add_layers_control(overlay_groups) |>
        .view_update_global_leaflet(overlay_groups)

    # return the leaflet
    return(leaf_map)

}
#' @rdname   sits_view
#'
#' @export
sits_view.raster_cube <- function(x, ...,
                                  band = NULL,
                                  red = NULL,
                                  green = NULL,
                                  blue = NULL,
                                  tiles = x[["tile"]][[1]],
                                  dates = NULL,
                                  palette = "RdYlGn",
                                  rev = FALSE,
                                  opacity = 0.85,
                                  max_cog_size = 2048,
                                  first_quantile = 0.02,
                                  last_quantile = 0.98,
                                  leaflet_megabytes = 64,
                                  add = FALSE) {
    # set caller for errors
    .check_set_caller("sits_view_raster_cube")
    # preconditions
    # verifies if leaflet package is installed
    .check_require_packages("leaflet")
    # precondition for tiles
    .check_cube_tiles(x, tiles)
    # check palette
    .check_palette(palette)
    # check rev
    .check_lgl_parameter(rev)
    # check opacity
    .check_num_parameter(opacity, min = 0.2, max = 1.0)
    # check COG size
    .check_int_parameter(max_cog_size, min = 512)
    # check quantiles
    .check_num_parameter(first_quantile, min = 0.0, max = 1.0)
    .check_num_parameter(last_quantile, min = 0.0, max = 1.0)
    # check leaflet megabytes
    .check_int_parameter(leaflet_megabytes, min = 16)
    # check logical control
    .check_lgl_parameter(add)
    # pre-condition for bands
    bands <- .check_bw_rgb_bands(x, band, red, green, blue)
    if (length(bands) == 1)
        band_name <- bands[[1]]
    else
        band_name <- stringr::str_flatten(bands, collapse = " ")
    # retrieve dots
    dots <- list(...)
    # deal with wrong parameter "date"
    if ("date" %in% names(dots) && missing(dates)) {
        dates <- as.Date(dots[["date"]])
    }

    # if not ADD, create a new sits leaflet
    if (!add)
        .conf_clean_leaflet()

    # recover global leaflet info
    overlay_groups <- sits_env[["leaflet"]][["overlay_groups"]]
    leaf_map <- sits_env[["leaflet"]][["leaf_map"]]
    # convert tiles names to tile objects
    cube <- dplyr::filter(x, .data[["tile"]] %in% tiles)
    # create a new layer in the leaflet
    for (i in seq_len(nrow(cube))) {
        row <- cube[i, ]
        tile_name <- row[["tile"]]
        # check dates
        if (.has(dates))
            .check_dates_timeline(dates, row)
        else
            dates <- .fi_date_least_cloud_cover(.fi(row))
        for (date in dates) {
            # convert to proper date
            date <- lubridate::as_date(date)
            # add group
            group <- paste(tile_name, date, band_name)
            # recover global leaflet and include group
            overlay_groups <- append(overlay_groups, group)
            # view image raster
            leaf_map <- leaf_map |>
                .view_image_raster(
                    group = group,
                    tile = row,
                    date = as.Date(date),
                    bands = bands,
                    palette = palette,
                    rev = rev,
                    opacity = opacity,
                    max_cog_size = max_cog_size,
                    first_quantile = first_quantile,
                    last_quantile = last_quantile,
                    leaflet_megabytes = leaflet_megabytes
            )
        }
    }
    # add layers control and update global leaflet-related variables
    leaf_map <- leaf_map |>
        .view_add_layers_control(overlay_groups) |>
        .view_update_global_leaflet(overlay_groups)

    return(leaf_map)
}
#' @rdname   sits_view
#'
#' @export
sits_view.uncertainty_cube <- function(x, ...,
                                       tiles = x[["tile"]][[1]],
                                       legend = NULL,
                                       palette = "RdYlGn",
                                       rev = FALSE,
                                       opacity = 0.85,
                                       max_cog_size = 2048,
                                       first_quantile = 0.02,
                                       last_quantile = 0.98,
                                       leaflet_megabytes = 64,
                                       add = FALSE) {
    # set caller for errors
    .check_set_caller("sits_view_uncertainty_cube")
    # preconditions
    # verifies if leaflet package is installed
    .check_require_packages("leaflet")
    # precondition for tiles
    .check_cube_tiles(x, tiles)
    # check palette
    .check_palette(palette)
    # check rev
    .check_lgl_parameter(rev)
    # check opacity
    .check_num_parameter(opacity, min = 0.2, max = 1.0)
    # check COG size
    .check_int_parameter(max_cog_size, min = 512)
    # check quantiles
    .check_num_parameter(first_quantile, min = 0.0, max = 1.0)
    .check_num_parameter(last_quantile, min = 0.0, max = 1.0)
    # check leaflet megabytes
    .check_int_parameter(leaflet_megabytes, min = 16)
    # check logical control
    .check_lgl_parameter(add)

    # if not ADD, create a new sits leaflet
    if (!add)
        .conf_clean_leaflet()

    # recover global leaflet info
    overlay_groups <- sits_env[["leaflet"]][["overlay_groups"]]
    leaf_map <- sits_env[["leaflet"]][["leaf_map"]]

    # convert tiles names to tile objects
    cube <- dplyr::filter(x, .data[["tile"]] %in% tiles)

    # create a new layer in the leaflet
    for (i in seq_len(nrow(cube))) {
        row <- cube[i, ]
        tile_name <- row[["tile"]]
        band <- .tile_bands(row)
        # add group
        group <- paste(tile_name, band)
        # recover global leaflet and include group
        overlay_groups <- append(overlay_groups, group)
        # get image file associated to band
        band_file <- .tile_path(row, band)
        # scale and offset
        band_conf <- .tile_band_conf(row, band)
        # view image raster
        leaf_map <- leaf_map |>
            .view_bw_band(
                group = group,
                tile = row,
                band_file = band_file,
                band_conf = band_conf,
                palette = palette,
                rev = rev,
                opacity = opacity,
                max_cog_size = max_cog_size,
                first_quantile = first_quantile,
                last_quantile = last_quantile,
                leaflet_megabytes = leaflet_megabytes
            )
    }
    # add layers control and update global leaflet-related variables
    leaf_map <- leaf_map |>
        .view_add_layers_control(overlay_groups) |>
        .view_update_global_leaflet(overlay_groups)

    return(leaf_map)
}
#' @rdname sits_view
#'
#' @export
#'
sits_view.class_cube <- function(x, ...,
                                 tiles = x[["tile"]],
                                 legend = NULL,
                                 palette = "Set3",
                                 version = NULL,
                                 opacity = 0.85,
                                 max_cog_size = 2048,
                                 leaflet_megabytes = 32,
                                 add = FALSE){
    # set caller for errors
    .check_set_caller("sits_view_class_cube")
    # preconditions
    .check_require_packages("leaflet")
    # precondition for tiles
    .check_cube_tiles(x, tiles)
    # check palette
    .check_palette(palette)
    # check version
    .check_chr_parameter(version, len_max = 1, allow_null = TRUE)
    # check opacity
    .check_num_parameter(opacity, min = 0.2, max = 1.0)
    # check COG size
    .check_int_parameter(max_cog_size, min = 512)
    # check leaflet megabytes
    .check_int_parameter(leaflet_megabytes, min = 16)
    # check logical control
    .check_lgl_parameter(add)

    # if not ADD, create a new sits leaflet
    if (!add)
        .conf_clean_leaflet()

    # recover global leaflet info
    overlay_groups <- sits_env[["leaflet"]][["overlay_groups"]]
    leaf_map <- sits_env[["leaflet"]][["leaf_map"]]

    # filter the tiles to be processed
    cube <- .view_filter_tiles(x, tiles)

    # go through the tiles
    for (row in nrow(cube)) {
        tile <- cube[row, ]
        tile_name <- tile[["tile"]]
        # add group
        group <- paste(tile_name, "class")
        # add version if available
        if (.has(version))
            group <- paste(group, version)
        # add a leaflet for class cube
        leaf_map <- leaf_map |>
            .view_class_cube(
                class_cube = cube,
                tile = tile,
                overlay_groups = overlay_groups,
                group = group,
                legend = legend,
                palette = palette,
                opacity = opacity,
                max_cog_size = max_cog_size,
                leaflet_megabytes = leaflet_megabytes
            )
        # include group in global control
        overlay_groups <- append(overlay_groups, group)
    }
    # add layers control and update global leaflet-related variables
    leaf_map <- leaf_map |>
        .view_add_layers_control(overlay_groups) |>
        .view_update_global_leaflet(overlay_groups)

    return(leaf_map)
}
#' @rdname sits_view
#'
#' @export
#'
sits_view.probs_cube <- function(x, ...,
                                 tiles = x[["tile"]][[1]],
                                 label = x[["labels"]][[1]][[1]],
                                 legend = NULL,
                                 palette = "YlGn",
                                 rev = FALSE,
                                 opacity = 0.85,
                                 max_cog_size = 2048,
                                 first_quantile = 0.02,
                                 last_quantile = 0.98,
                                 leaflet_megabytes = 64,
                                 add = FALSE) {

    # set caller for errors
    .check_set_caller("sits_view_probs_cube")
    # verifies if leaflet package is installed
    .check_require_packages("leaflet")
    # precondition for tiles
    .check_cube_tiles(x, tiles)
    # check if label is unique
    .check_chr_parameter(label, len_max = 1,
                         msg = .conf("messages", "sits_view_probs_label"))
    # check that label is part of the probs cube
    .check_labels_probs_cube(x, label)
    # check palette
    .check_palette(palette)
    # check opacity
    .check_num_parameter(opacity, min = 0.2, max = 1.0)
    # check COG size
    .check_int_parameter(max_cog_size, min = 512)
    # check quantiles
    .check_num_parameter(first_quantile, min = 0.0, max = 1.0)
    .check_num_parameter(last_quantile, min = 0.0, max = 1.0)
    # check leaflet megabytes
    .check_int_parameter(leaflet_megabytes, min = 16)
    # check logical control
    .check_lgl_parameter(add)

    # if not ADD, create a new sits leaflet
    if (!add)
        .conf_clean_leaflet()

    # recover global leaflet info
    overlay_groups <- sits_env[["leaflet"]][["overlay_groups"]]
    leaf_map <- sits_env[["leaflet"]][["leaf_map"]]

    # convert tiles names to tile objects
    cube <- dplyr::filter(x, .data[["tile"]] %in% tiles)

    # get all labels to be plotted
    labels <- .tile_labels(cube)
    names(labels) <- seq_along(labels)

    # create a new layer in the leaflet
    for (i in seq_len(nrow(cube))) {
        row <- cube[i, ]
        tile_name <- row[["tile"]]
        # add group
        group <- paste(tile_name, "probs", label)
        # recover global leaflet and include group
        overlay_groups <- append(overlay_groups, group)
        # view image raster
        leaf_map <- leaf_map |>
            .view_probs_label(
                group = group,
                tile = row,
                date = as.Date(date),
                labels = labels,
                label = label,
                palette = palette,
                rev = rev,
                opacity = opacity,
                max_cog_size = max_cog_size,
                first_quantile = first_quantile,
                last_quantile = last_quantile,
                leaflet_megabytes = leaflet_megabytes
            )
    }
    # add layers control and update global leaflet-related variables
    leaf_map <- leaf_map |>
        .view_add_layers_control(overlay_groups) |>
        .view_update_global_leaflet(overlay_groups)

    return(leaf_map)
}
#' @rdname   sits_view
#'
#' @export
sits_view.vector_cube <- function(x, ...,
                                  tiles = x[["tile"]][[1]],
                                  seg_color = "yellow",
                                  line_width = 0.5,
                                  add = FALSE) {
    # set caller for errors
    .check_set_caller("sits_view_vector_cube")
    # preconditions
    # verifies if leaflet package is installed
    .check_require_packages("leaflet")
    # precondition for tiles
    .check_cube_tiles(x, tiles)
    # check opacity
    .check_num_parameter(line_width, min = 0.1, max = 3.0)

    # if not ADD, create a new sits leaflet
    if (!add)
        .conf_clean_leaflet()

    # recover global leaflet info
    overlay_groups <- sits_env[["leaflet"]][["overlay_groups"]]
    leaf_map <- sits_env[["leaflet"]][["leaf_map"]]

    # convert tiles names to tile objects
    cube <- dplyr::filter(x, .data[["tile"]] %in% tiles)
    # create a new layer in the leaflet
    for (i in seq_len(nrow(cube))) {
        row <- cube[i, ]
        tile_name <- row[["tile"]]
            group <- paste(tile_name, "segments")
            # recover global leaflet and include group
            overlay_groups <- append(overlay_groups, group)
            # view image raster
            leaf_map <- leaf_map |>
                .view_segments(
                    group = group,
                    tile = row,
                    seg_color = seg_color,
                    line_width = line_width
                )
    }
    # add layers control and update global leaflet-related variables
    leaf_map <- leaf_map |>
        .view_add_layers_control(overlay_groups) |>
        .view_update_global_leaflet(overlay_groups)

    return(leaf_map)
}
#' @rdname   sits_view
#'
#' @export
sits_view.class_vector_cube <- function(x, ...,
                                  tiles = x[["tile"]][[1]],
                                  seg_color = "yellow",
                                  line_width = 0.2,
                                  version = NULL,
                                  legend = NULL,
                                  palette = "Set3",
                                  opacity = 0.85,
                                  add = FALSE) {
    # set caller for errors
    .check_set_caller("sits_view_class_vector_cube")
    # preconditions
    # verifies if leaflet package is installed
    .check_require_packages("leaflet")
    # precondition for tiles
    .check_cube_tiles(x, tiles)
    # check opacity
    .check_num_parameter(line_width, min = 0.1, max = 3.0)
    # check palette
    .check_palette(palette)
    # check version
    .check_chr_parameter(version, len_max = 1, allow_null = TRUE)
    # check opacity
    .check_num_parameter(opacity, min = 0.2, max = 1.0)
    # check logical control
    .check_lgl_parameter(add)

    # if not ADD, create a new sits leaflet
    if (!add)
        .conf_clean_leaflet()
    # recover global leaflet info
    overlay_groups <- sits_env[["leaflet"]][["overlay_groups"]]
    leaf_map <- sits_env[["leaflet"]][["leaf_map"]]

    # convert tiles names to tile objects
    cube <- dplyr::filter(x, .data[["tile"]] %in% tiles)
    # create a new layer in the leaflet
    for (i in seq_len(nrow(cube))) {
        row <- cube[i, ]
        tile_name <- row[["tile"]]
        # add group
        group <- paste(tile_name, "class_segments")
        # add version if available
        if (.has(version))
            group <- paste(group, version)
        # include in overlay groups
        overlay_groups <- append(overlay_groups, group)
        # view image raster
        leaf_map <- leaf_map |>
            .view_vector_class_cube(
                group = group,
                tile = row,
                seg_color = seg_color,
                line_width = line_width,
                opacity = opacity,
                legend = legend,
                palette = palette
            )
    }
    # add layers control and update global leaflet-related variables
    leaf_map <- leaf_map |>
        .view_add_layers_control(overlay_groups) |>
        .view_update_global_leaflet(overlay_groups)
    return(leaf_map)
}

#' @rdname sits_view
#'
#' @export
#'
sits_view.default <- function(x, ...) {
    stop(.conf("messages", "sits_view_default"))
}
