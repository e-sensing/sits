#' @title  Add layers control to leaflet
#' @name .view_add_layers_control
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  leaf_map      Leaflet map
#' @param  overlay_groups Overlay groups for leaflet
#'
#' @return               A leaflet object
#'
.view_add_layers_control <- function(leaf_map, overlay_groups) {

    # recover base groups
    base_groups <- sits_env[["leaflet"]][["base_groups"]]

    # add layers control
    leaf_map <- leaf_map |>
        leaflet::addLayersControl(
            baseGroups = base_groups,
            overlayGroups = overlay_groups,
            options = leaflet::layersControlOptions(collapsed = FALSE)
        )
    return(leaf_map)

}
#' @title  Update global leaflet
#' @name .view_update_global_leaflet
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  leaf_map      Leaflet map
#' @param  overlay_groups Overlay groups for leaflet
#'
#' @return                A leaflet object
#'
.view_update_global_leaflet <- function(leaf_map, overlay_groups){
    # update global leaflet control
    sits_env[["leaflet"]][["overlay_groups"]] <- overlay_groups
    sits_env[["leaflet"]][["leaf_map"]] <- leaf_map

    return(leaf_map)
}

#' @title  Visualize a set of samples
#' @name .view_samples
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  leaf_map      Leaflet map
#' @param  samples       Data.frame with columns "longitude", "latitude"
#'                       and "label"
#' @param  group         Leaflet group to be added
#' @param  legend        Named vector that associates labels to colors.
#' @param  palette       Palette provided in the configuration file.
#' @param  radius        Radius of circle markers
#' @return               A leaflet object
#'
.view_samples <- function(leaf_map, samples, group,
                          legend, palette, radius) {
    .check_set_caller(".view_samples")
    # first select unique locations
    samples <- dplyr::distinct(
        samples,
        .data[["longitude"]],
        .data[["latitude"]],
        .data[["label"]]
    )
    # convert tibble to sf
    samples <- sf::st_as_sf(
        samples[c("longitude", "latitude", "label")],
        coords = c("longitude", "latitude"),
        crs = "EPSG:4326"
    )
    # get the bounding box
    samples_bbox <- sf::st_bbox(samples)
    # get the labels
    labels <- sort(unique(samples[["label"]]))
    # get colors
    colors <- .colors_get(
        labels = labels,
        legend = legend,
        palette = palette,
        rev = TRUE
    )
    # create a palette of colors
    factpal <- leaflet::colorFactor(
        palette = colors,
        domain = labels
    )
    # add samples to leaflet
    leaf_map <- leaf_map |>
        leaflet::flyToBounds(
            lng1 = samples_bbox[["xmin"]],
            lat1 = samples_bbox[["ymin"]],
            lng2 = samples_bbox[["xmax"]],
            lat2 = samples_bbox[["ymax"]]
        ) |>
        leafgl::addGlPoints(
            data = samples,
            fillColor = ~ factpal(label),
            radius = radius,
            stroke = FALSE,
            fillOpacity = 1,
            group = group
        )
    # recover overlay groups
    overlay_groups <- sits_env[["leaflet"]][["overlay_groups"]]
    # add legend if it does not exist already
    if (!any(grepl("samples", overlay_groups)) &&
        !any(grepl("class", overlay_groups))) {
        leaf_map <- leaf_map |>
            leaflet::addLegend(
                position = "topright",
                pal = factpal,
                values = labels,
                title = "Classes",
                opacity = 1
            )
    }
    return(leaf_map)
}
#' @title  Visualize a set of neurons
#' @name .view_neurons
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  leaf_map      Leaflet map
#' @param  samples       Data.frame with columns "longitude", "latitude"
#'                       and "label"
#' @param  labels        Labels to display
#' @param  group         Leaflet group to be added
#' @param  legend        Named vector that associates labels to colors.
#' @param  palette       Palette provided in the configuration file.
#' @param  radius        Radius of circle markers
#' @return               A leaflet object
#'
.view_neurons <- function(leaf_map, samples, labels, group,
                          legend, palette, radius) {
    .check_set_caller(".view_neurons")
    # first select unique locations
    samples <- dplyr::distinct(
        samples,
        .data[["longitude"]],
        .data[["latitude"]],
        .data[["label"]]
    )
    # convert tibble to sf
    samples <- sf::st_as_sf(
        samples[c("longitude", "latitude", "label")],
        coords = c("longitude", "latitude"),
        crs = "EPSG:4326"
    )
    # get the bounding box
    samples_bbox <- sf::st_bbox(samples)
    # get colors
    colors <- .colors_get(
        labels = labels,
        legend = legend,
        palette = palette,
        rev = TRUE
    )
    # create a palette of colors
    factpal <- leaflet::colorFactor(
        palette = colors,
        domain = labels
    )
    # add samples to leaflet
    leaf_map <- leaf_map |>
        leaflet::flyToBounds(
            lng1 = samples_bbox[["xmin"]],
            lat1 = samples_bbox[["ymin"]],
            lng2 = samples_bbox[["xmax"]],
            lat2 = samples_bbox[["ymax"]]
        ) |>
        leafgl::addGlPoints(
            data = samples,
            fillColor = ~ factpal(label),
            radius = radius,
            stroke = FALSE,
            fillOpacity = 1,
            group = group
        )
    # recover overlay groups
    overlay_groups <- sits_env[["leaflet"]][["overlay_groups"]]
    # add legend if it does not exist already
    if (!any(grepl("samples", overlay_groups)) &&
        !any(grepl("class", overlay_groups)) &&
        !sits_env[["leaflet_som_colors"]]) {
        leaf_map <- leaf_map |>
            leaflet::addLegend(
                position = "topright",
                pal = factpal,
                values = labels,
                title = "Classes",
                opacity = 1
            )
        sits_env[["leaflet_som_colors"]] <- TRUE
    }
    return(leaf_map)
}
#' @title  Include leaflet to view segments
#' @name .view_segments
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  leafmap       Leaflet map
#' @param  group         Group associated to the leaflet map
#' @param  tile          Vector tile
#' @param  seg_color     Color for segments boundaries
#' @param  line_width    Line width for segments (in pixels)
#' @return               A leaflet object
#
.view_segments <- function(leaf_map,
                           group,
                           tile,
                           seg_color,
                           line_width) {
    # retrieve the segments for this tile
    sf_seg <- .segments_read_vec(tile)
    # transform the segments
    sf_seg <- sf::st_transform(
        sf_seg,
        crs = sf::st_crs("EPSG:4326")
    )
    # create a layer with the segment borders
    leaf_map <- leaf_map |>
        leafgl::addGlPolygons(
            data = sf_seg,
            color = seg_color,
            opacity = 1,
            fillOpacity = 0,
            weight = line_width,
            group = group
        )

    return(leaf_map)
}
#' @title  Include leaflet to view classified regions
#' @name .view_vector_class_cube
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  leafmap       Leaflet map
#' @param  group         Group associated to the leaflet map
#' @param  tile          Vector tile
#' @param  seg_color     Color for segments boundaries
#' @param  line_width    Line width for segments (in pixels)
#' @param  opacity       Opacity of segment fill
#' @param  legend        Named vector that associates labels to colors.
#' @param  palette       Palette provided in the configuration file
#' @return               A leaflet object
#
.view_vector_class_cube <- function(leaf_map,
                                    group,
                                    tile,
                                    seg_color,
                                    line_width,
                                    opacity,
                                    legend,
                                    palette) {
    # retrieve segments on a tile basis
    sf_seg <- .segments_read_vec(tile)
    # transform the segments
    sf_seg <- sf::st_transform(
        sf_seg,
        crs = sf::st_crs("EPSG:4326")
    )

    # dissolve sf_seg
    sf_seg <- sf_seg |>
        dplyr::group_by(.data[["class"]]) |>
        dplyr::summarise()
    labels_seg <- sf_seg |>
        sf::st_drop_geometry() |>
        dplyr::select("class") |>
        dplyr::pull()
    # get the names of the labels
    names(labels_seg) <- seq_along(labels_seg)
    # obtain the colors
    colors <- .colors_get(
        labels = labels_seg,
        legend = legend,
        palette = palette,
        rev = TRUE
    )
    # add a new leafmap to show polygons of segments
    leaf_map <- leaf_map |>
        leaflet::addPolygons(
            data = sf_seg,
            label = labels_seg,
            color = seg_color,
            stroke = TRUE,
            weight = line_width,
            opacity = 1,
            fillColor = unname(colors),
            fillOpacity = opacity,
            group = group
        )
    return(leaf_map)
}
#' @title  Include leaflet to view images (BW or RGB)
#' @name .view_image_raster
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  leaf_map      Leaflet map to be added to
#' @param  group         Group to which map will be assigned
#' @param  tile          Tile to be plotted.
#' @param  date          Date to be plotted.
#' @param  bands         Bands to be plotted..
#' @param  legend        Named vector that associates labels to colors.
#' @param  palette       Palette provided in the configuration file
#' @param  rev           Reverse the color palette?
#' @param  opacity       Opacity to be applied to map layer
#' @param  max_cog_size  Maximum size of COG overviews (lines or columns)
#' @param  first_quantile First quantile for stretching images
#' @param  last_quantile  Last quantile for stretching images
#' @param  leaflet_megabytes Maximum size for leaflet (in MB)
#'
#' @return               A leaflet object.
#'
 .view_image_raster <- function(leaf_map,
                               group,
                               tile,
                               date,
                               bands,
                               palette,
                               rev,
                               opacity,
                               max_cog_size,
                               first_quantile,
                               last_quantile,
                               leaflet_megabytes) {
    #
    # obtain the raster objects for the dates chosen
    # check if date is inside the timeline
    tile_dates <- .tile_timeline(tile)
    if (!date %in% tile_dates) {
        idx_date <- which.min(abs(date - tile_dates))
        date <- tile_dates[idx_date]
    }
    # define which method is used
    if (length(bands) == 3)
        class(bands) <- c("rgb", class(bands))
    else
        class(bands) <- c("bw", class(bands))

    UseMethod(".view_image_raster", bands)
}
#' View RGB image
#' @title  Include leaflet to view RGB images
#' @name .view_image_raster.rgb
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  leaf_map      Leaflet map to be added to
#' @param  group         Group to which map will be assigned
#' @param  tile          Tile to be plotted.
#' @param  date          Date to be plotted.
#' @param  bands         Bands to be plotted
#' @param  legend        Named vector that associates labels to colors.
#' @param  palette       Palette provided in the configuration file
#' @param  rev           Reverse the color palette?
#' @param  opacity       Opacity to be applied to map layer
#' @param  max_cog_size  Maximum size of COG overviews (lines or columns)
#' @param  first_quantile First quantile for stretching images
#' @param  last_quantile  Last quantile for stretching images
#' @param  leaflet_megabytes Maximum size for leaflet (in MB)
#'
#' @return               A leaflet object.
#' @export
.view_image_raster.rgb <- function(leaf_map,
                                   group,
                                   tile,
                                   date,
                                   bands,
                                   palette,
                                   rev,
                                   opacity,
                                   max_cog_size,
                                   first_quantile,
                                   last_quantile,
                                   leaflet_megabytes) {
    # scale and offset
    band_conf <- .tile_band_conf(tile, bands[[1]])

    # filter by date and band
    # if there is only one band, RGB files will be the same
    red_file <- .tile_path(tile, bands[[1]], date)
    green_file <- .tile_path(tile, bands[[2]], date)
    blue_file <- .tile_path(tile, bands[[3]], date)

    # create a leaflet for RGB bands
    leaf_map <- leaf_map |>
        .view_rgb_bands(
            group = group,
            tile = tile,
            red_file = red_file,
            green_file = green_file,
            blue_file = blue_file,
            band_conf = band_conf,
            opacity = opacity,
            max_cog_size = max_cog_size,
            first_quantile = first_quantile,
            last_quantile = last_quantile,
            leaflet_megabytes = leaflet_megabytes
        )
}
#' View BW image
#' @title  Include leaflet to view BW images
#' @name .view_image_raster.bw
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  leaf_map      Leaflet map to be added to
#' @param  group         Group to which map will be assigned
#' @param  tile          Tile to be plotted.
#' @param  date          Date to be plotted.
#' @param  bands         For plotting grey images.
#' @param  legend        Named vector that associates labels to colors.
#' @param  palette       Palette provided in the configuration file
#' @param  rev           Reverse the color palette?
#' @param  opacity       Opacity to be applied to map layer
#' @param  max_cog_size  Maximum size of COG overviews (lines or columns)
#' @param  first_quantile First quantile for stretching images
#' @param  last_quantile  Last quantile for stretching images
#' @param  leaflet_megabytes Maximum size for leaflet (in MB)
#'
#' @return               A leaflet object.
#' @export
.view_image_raster.bw <- function(leaf_map,
                                  group,
                                  tile,
                                  date,
                                  bands,
                                  palette,
                                  rev,
                                  opacity,
                                  max_cog_size,
                                  first_quantile,
                                  last_quantile,
                                  leaflet_megabytes) {
    # filter by date and band
    band_file <- .tile_path(tile, bands[[1]], date)
    # scale and offset
    band_conf <- .tile_band_conf(tile, bands[[1]])
    leaf_map <- leaf_map |>
        .view_bw_band(
            group = group,
            tile = tile,
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
#' @title  Include leaflet to view B/W band
#' @name .view_bw_band
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  leaf_map      Leaflet map to be added to
#' @param  group         Group to which map will be assigned
#' @param  tile          Tile to be plotted.
#' @param  band_file     For plotting grey images.
#' @param  band_conf     Band configuration file
#' @param  palette       Palette to show false colors
#' @param  rev           Revert the color palette?
#' @param  opacity       Opacity to be used to cover the base map
#' @param  max_cog_size  Maximum size of COG overviews (lines or columns)
#' @param  first_quantile First quantile for stretching images
#' @param  last_quantile  Last quantile for stretching images
#' @param  leaflet_megabytes Maximum size for leaflet (in MB)
#' @return               A leaflet object
#
.view_bw_band <- function(leaf_map,
                          group,
                          tile,
                          band_file,
                          band_conf,
                          palette,
                          rev,
                          opacity,
                          max_cog_size,
                          first_quantile,
                          last_quantile,
                          leaflet_megabytes) {

    # find if file supports COG overviews
    sizes <- .tile_overview_size(tile = tile, max_cog_size)
    # warp the file to produce a temporary overview (except for derived cube)
    band_file <- .gdal_warp_file(
        raster_file = band_file,
        sizes = sizes
    )
    # scale and offset
    band_scale <- .scale(band_conf)
    band_offset <- .offset(band_conf)

    # read spatial raster file
    rast <- .raster_open_rast(band_file)
    # resample and warp the image
    rast <- .raster_project(rast, "EPSG:3857")
    # scale the data
    rast <- rast * band_scale + band_offset
    # extract the values
    vals <- .raster_get_values(rast)
    # obtain the quantiles
    quantiles <- stats::quantile(
        vals,
        probs = c(0, 0.05, 0.95, 1),
        na.rm = TRUE
    )
    # get quantile values
    minv <- quantiles[[1]]
    minq <- quantiles[[2]]
    maxq <- quantiles[[3]]
    maxv <- quantiles[[4]]

    # set limits to raster
    vals <- ifelse(vals > minq, vals, minq)
    vals <- ifelse(vals < maxq, vals, maxq)
    rast <- .raster_set_values(rast, vals)
    domain <- c(minq, maxq)

    # produce color map
    colors_leaf <- leaflet::colorNumeric(
        palette = palette,
        domain = domain,
        reverse = rev
    )
    # calculate maximum size in MB
    max_bytes <- leaflet_megabytes * 1024^2

    # add SpatRaster to leaflet
    leaf_map <- leaf_map |>
        leaflet::addRasterImage(
            x = rast,
            colors = colors_leaf,
            project = FALSE,
            group = group,
            maxBytes = max_bytes,
            opacity = opacity
    )
    if (!sits_env[["leaflet_false_color_legend"]]) {
        leaf_map <- leaf_map |>
            leaflet::addLegend(
                position = "bottomleft",
                pal = colors_leaf,
                values = vals,
                title = "scale",
                opacity = 1
            )
        sits_env[["leaflet_false_color_legend"]] <- TRUE
    }
    return(leaf_map)
}
#' @title  Include leaflet to view RGB bands
#' @name .view_rgb_bands
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  leaf_map      Leaflet map to be added to
#' @param  group         Group to which map will be assigned
#' @param  tile          Tile to be plotted.
#' @param  red_file      Image file to be shown in red color
#' @param  green_file    Image file to be shown in green color
#' @param  blue_file     Image file to be shown in blue color
#' @param  band_conf     Band configuration file
#' @param  opacity       Opacity to be applied
#' @param  max_cog_size  Maximum size of COG overviews (lines or columns)
#' @param  first_quantile First quantile for stretching images
#' @param  last_quantile  Last quantile for stretching images
#' @param  leaflet_megabytes Maximum size for leaflet (in MB)
#' @return               A leaflet object
#
.view_rgb_bands <- function(leaf_map,
                            group,
                            tile,
                            red_file,
                            green_file,
                            blue_file,
                            band_conf,
                            opacity,
                            max_cog_size,
                            first_quantile,
                            last_quantile,
                            leaflet_megabytes) {

    # find if file supports COG overviews
    sizes <- .tile_overview_size(tile = tile, max_cog_size)
    # warp the image
    red_file <- .gdal_warp_file(red_file, sizes)
    green_file <- .gdal_warp_file(green_file, sizes)
    blue_file <- .gdal_warp_file(blue_file, sizes)

    # prepare a SpatRaster object for visualization
    rast <- .raster_view_rgb_object(red_file, green_file, blue_file, band_conf)

    # calculate maximum size in MB
    max_bytes <- leaflet_megabytes * 1024^2

    leaf_map <- leaf_map |>
        leaflet::addRasterImage(
            x = rast,
            project = FALSE,
            group = group,
            maxBytes = max_bytes,
            opacity = opacity
        )
    return(leaf_map)
}

#' @title  Include leaflet to view classified cube
#' @name .view_class_cube
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  leafmap       Leaflet map
#' @param  class_cube    Classified cube to be overlayed on top on image
#' @param  tile          Tile to be plotted
#' @param  overlay_groups Overlay groups in the leaflet
#' @param  group         Leaflet group
#' @param  legend        Named vector that associates labels to colors.
#' @param  palette       Palette provided as alternative legend.
#' @param  opacity       Fill opacity
#' @param  max_cog_size  Maximum size of COG overviews (lines or columns)
#' @param  leaflet_megabytes Maximum size for leaflet (in MB)
#'
.view_class_cube <- function(leaf_map,
                             class_cube,
                             tile,
                             overlay_groups,
                             group,
                             legend,
                             palette,
                             opacity,
                             max_cog_size,
                             leaflet_megabytes) {
    # set caller to show in errors
    .check_set_caller(".view_class_cube")
    # check that class_cube is valid
    .check_that(inherits(class_cube, "class_cube"))
    # get the labels
    labels <- .cube_labels(class_cube)
    if (.has_not(names(labels))) {
        names(labels) <- seq_along(labels)
    }
    # find if file supports COG overviews
    sizes <- .tile_overview_size(tile = class_cube,
                                 max_size = max_cog_size)
    # warp the file to produce a temporary overview
    class_file <- .gdal_warp_file(
        raster_file = .tile_path(tile),
        sizes = sizes,
        t_srs = list("-r" = "near")
    )
    # read spatial raster file
    rast <- .raster_open_rast(class_file)

    # resample and warp the image
    rast <- .raster_project(rast, "EPSG:3857", method = "near")
    # If available, use labels to define which colors must be presented.
    # This is useful as some datasets (e.g., World Cover) represent
    # classified data with values that are not the same as the positions
    # of the color array (e.g., 10, 20), causing a misrepresentation of
    # the classes
    values_available <- as.character(sort(unique(.raster_values_mem(rast),
                                                 na.omit = TRUE)))
    labels <- labels[values_available]
    # set levels for raster
    rast_levels <- data.frame(
        id = as.numeric(names(labels)),
        cover = unname(labels)
    )
    # get colors only for the available labels
    colors <- .colors_get(
        labels = labels,
        legend = legend,
        palette = palette,
        rev = TRUE
    )
    # set the levels and the palette for terra
    levels(rast) <- rast_levels
    options(terra.pal = unname(colors))
    leaflet_colors <- leaflet::colorFactor(
        palette = unname(colors),
        domain = as.character(names(labels))
    )
    # calculate maximum size in MB
    max_bytes <- leaflet_megabytes * 1024^2
    # add the classified image object
    leaf_map <- leaf_map |>
        leaflet::addRasterImage(
            x = rast,
            colors = leaflet_colors,
            opacity = opacity,
            method = "ngb",
            group = group,
            project = FALSE,
            maxBytes = max_bytes
        )
    # add legend if it does not exist already
    if (!any(grepl("samples", overlay_groups)) &&
        !any(grepl("class", overlay_groups))) {
        leaf_map <- leaf_map |>
            .view_add_legend(
                labels = labels,
                legend = legend,
                palette = palette
            )
    }

    return(leaf_map)
}
#' @title  Include leaflet to view probs label
#' @name .view_probs_label
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  leaf_map      Leaflet map to be added to
#' @param  group         Group to which map will be assigned
#' @param  tile          Tile to be plotted.
#' @param  labels        Labels associated with the probs cube
#' @param  label         Probs label to be plotted
#' @param  palette       Palette to show false colors
#' @param  rev           Revert the color palette?
#' @param  opacity       Opacity to be used to cover the base map
#' @param  max_cog_size  Maximum size of COG overviews (lines or columns)
#' @param  first_quantile First quantile for stretching images
#' @param  last_quantile  Last quantile for stretching images
#' @param  leaflet_megabytes Maximum size for leaflet (in MB)
#' @return               A leaflet object
#
.view_probs_label <- function(leaf_map,
                          group,
                          tile,
                          labels,
                          label,
                          date,
                          palette,
                          rev,
                          opacity,
                          max_cog_size,
                          first_quantile,
                          last_quantile,
                          leaflet_megabytes) {

    # calculate maximum size in MB
    max_bytes <- leaflet_megabytes * 1024^2
    # obtain the raster objects
    probs_file <- .tile_path(tile)
    # find if file supports COG overviews
    sizes <- .tile_overview_size(tile = tile, max_cog_size)
    # warp the file to produce a temporary overview
    probs_file <- .gdal_warp_file(
        raster_file = probs_file,
        sizes = sizes
    )
    # scale and offset
    probs_conf <- .tile_band_conf(tile, "probs")
    probs_scale <- .scale(probs_conf)
    probs_offset <- .offset(probs_conf)
    max_value <- .max_value(probs_conf)

    # select SpatRaster band to be plotted
    layer_rast <- which(labels == label)

    # read spatial raster file
    rast <- .raster_open_rast(probs_file)
    # extract only selected label
    rast <- rast[[layer_rast]]

    # resample and warp the image
    rast <- .raster_project(rast, "EPSG:3857")
    # scale the data
    rast <- rast * probs_scale + probs_offset

    # extract the values
    vals <- .raster_get_values(rast)

    # obtain the quantiles
    quantiles <- stats::quantile(
        vals,
        probs = c(0, 0.05, 0.95, 1),
        na.rm = TRUE
    )
    # get quantile values
    minv <- quantiles[[1]]
    minq <- quantiles[[2]]
    maxq <- quantiles[[3]]
    maxv <- quantiles[[4]]

    # set limits to raster
    vals <- ifelse(vals > minq, vals, minq)
    vals <- ifelse(vals < maxq, vals, maxq)
    rast <- .raster_set_values(rast, vals)
    domain <- c(minq, maxq)

    # produce color map
    colors_leaf <- leaflet::colorNumeric(
        palette = palette,
        domain = domain,
        reverse = rev
    )
    # add Spatial Raster to leaflet
    leaf_map <- leaf_map |>
        leaflet::addRasterImage(
            x = rast,
            colors = colors_leaf,
            project = FALSE,
            group = group,
            maxBytes = max_bytes,
            opacity = opacity
        )
    if (!sits_env[["leaflet_false_color_legend"]]) {
        leaf_map <- leaf_map |>
            leaflet::addLegend(
                position = "bottomleft",
                pal = colors_leaf,
                values = vals,
                title = "scale",
                opacity = 1
            )
        sits_env[["leaflet_false_color_legend"]] <- TRUE
    }
    return(leaf_map)
}
#' @title  Set the dates for visualisation
#' @name .view_set_dates
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  cube          Cube.
#' @param  dates         Dates to be viewed.
#' @return               Valid dates
#'
#'
.view_set_dates <- function(cube, dates) {
    # get the timeline
    timeline <- .cube_timeline(cube)[[1]]

    if (.has_not(dates)) {
        dates <- timeline[[1]]
    }
    # make sure dates are valid
    dates <- lubridate::as_date(dates)
    return(dates)
}
#' @title  Select the tiles to be visualised
#' @name .view_filter_tiles
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  cube          Cube
#' @param  tiles         Tiles to be viewed.
#' @return               Cube subset to be viewed
#'
#'
.view_filter_tiles <- function(cube, tiles) {
    .check_set_caller(".view_filter_tiles")
    # try to find tiles in the list of tiles of the cube
    .check_that(all(tiles %in% cube[["tile"]]))
    # filter the tiles to be processed
    cube <- .cube_filter_tiles(cube, tiles)
    return(cube)
}
#' @title  Add a legend to the leafmap
#' @name .view_add_legend
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  leaf_map      Leaflet map
#' @param  labels        Class labels
#' @param  legend        Class legend
#' @param  palette       Color palette
#' @return               Leaflet map with legend
#'
#'
.view_add_legend <- function(leaf_map,
                             labels,
                             legend,
                             palette) {
    # obtain labels from vector class cube
    labels <- sort(unname(labels))
    colors <- .colors_get(
        labels = labels,
        legend = legend,
        palette = palette,
        rev = TRUE
    )
    # create a palette of colors
    fact_pal <- leaflet::colorFactor(
        palette = colors,
        domain = labels
    )
    leaf_map <- leaflet::addLegend(
        map = leaf_map,
        position = "topright",
        pal = fact_pal,
        values = labels,
        title = "Classes",
        opacity = 1
    )
    return(leaf_map)
}
