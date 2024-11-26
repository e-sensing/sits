
#' @title  Visualize a set of samples
#' @name .view_samples
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  samples       Data.frame with columns "longitude", "latitude"
#'                       and "label"
#' @param  legend        Named vector that associates labels to colors.
#' @param  palette       Palette provided in the configuration file.
#' @return               A leaflet object
#'
.view_samples <- function(samples, legend, palette) {
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

    # if colors are not specified, get them from the configuration file
    if (.has_not(legend)) {
        colors <- .colors_get(
            labels = labels,
            legend = NULL,
            palette = palette,
            rev = TRUE
        )
    } else {
        .check_chr_within(
            labels,
            within = names(legend)
        )
        colors <- unname(legend[labels])
    }
    # create a pallete of colors
    factpal <- leaflet::colorFactor(
        palette = colors,
        domain = labels
    )
    # create a leaflet and add providers
    leaf_map <- .view_add_base_maps()
    leaf_map <- leaf_map |>
        leaflet::flyToBounds(
            lng1 = samples_bbox[["xmin"]],
            lat1 = samples_bbox[["ymin"]],
            lng2 = samples_bbox[["xmax"]],
            lat2 = samples_bbox[["ymax"]]
        ) |>
        leaflet::addCircleMarkers(
            data = samples,
            color = ~ factpal(label),
            radius = 4,
            stroke = FALSE,
            fillOpacity = 1,
            group = "Samples"
        ) |>
        leaflet::addLayersControl(
            baseGroups = c("ESRI", "GeoPortalFrance",
                           "Sentinel-2-2020", "OSM"),
            overlayGroups = "Samples",
            options = leaflet::layersControlOptions(collapsed = FALSE)
        ) |>
        leaflet::addLegend("topright",
            pal     = factpal,
            values  = samples[["label"]],
            title   = "Training Samples",
            opacity = 1
        )
    return(leaf_map)
}
#' @title  Create a leafmap to view base background maps
#' @name .view_add_base_maps
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @return          Leafmap with maps from providers
#'
.view_add_base_maps <- function() {
    # create a leaflet and add providers
    leaf_map <- leaflet::leaflet() |>
        leaflet::addProviderTiles(
            provider = leaflet::providers[["GeoportailFrance.orthos"]],
            group = "GeoPortalFrance"
        ) |>
        leaflet::addProviderTiles(
            provider = leaflet::providers[["Esri.WorldImagery"]],
            group = "ESRI"
        ) |>
        leaflet::addProviderTiles(
            provider = leaflet::providers[["OpenStreetMap"]],
            group = "OSM"
        ) |>
        leaflet::addWMSTiles(
            baseUrl = "https://tiles.maps.eox.at/wms/",
            layers = "s2cloudless-2020_3857_512",
            group = "Sentinel-2-2020"
        ) |>
        leafem::addMouseCoordinates()
    return(leaf_map)
}
#' @title  Include leaflet to view segments
#' @name .view_segments
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  leafmap       Leaflet map
#' @param  tile          Vector tile
#' @param  seg_color     Color for segments boundaries
#' @param  line_width    Line width for segments (in pixels)
#' @param  opacity       Opacity of segment fill
#' @param  legend        Named vector that associates labels to colors.
#' @param  palette       Palette provided in the configuration file
#' @return               A leaflet object
#
.view_segments <- function(leaf_map,
                           tile,
                           seg_color,
                           line_width,
                           opacity,
                           legend,
                           palette) {
    # retrieve segments on a tile basis
    if (inherits(tile, "vector_cube")) {
        # retrieve the segments for this tile
        sf_seg <- .segments_read_vec(tile)
        # transform the segments
        sf_seg <- sf::st_transform(
            sf_seg,
            crs = sf::st_crs("EPSG:4326")
        )
        # create a layer with the segment borders
        leaf_map <- leafem::addFeatures(
            leaf_map,
            data = sf_seg,
            color = seg_color,
            opacity = 1,
            fillOpacity = 0,
            weight = line_width,
            group = "segments"
        )
        # have the segments been classified?
        if ("class" %in% colnames(sf_seg)) {
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
            leaf_map <- leafem::addFeatures(
                leaf_map,
                data = sf_seg,
                label = labels_seg,
                color = seg_color,
                stroke = FALSE,
                weight = line_width,
                opacity = 1,
                fillColor = unname(colors),
                fillOpacity = opacity,
                group = "class_segments"
            )
        }
    }
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
#' @param  band          For plotting grey images.
#' @param  red           Band for red color.
#' @param  green         Band for green color.
#' @param  blue          Band for blue color.
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
                               band,
                               red,
                               green,
                               blue,
                               legend,
                               palette,
                               rev,
                               opacity,
                               max_cog_size,
                               first_quantile,
                               last_quantile,
                               leaflet_megabytes) {
    # add B/W band if required
    # create a leaflet for B/W bands
    if (.has(band)) {
        leaf_map <- leaf_map |>
            .view_bw_band(
                group = group,
                tile = tile,
                band = band,
                date = date,
                palette = palette,
                rev = rev,
                opacity = opacity,
                max_cog_size = max_cog_size,
                first_quantile = first_quantile,
                last_quantile = last_quantile,
                leaflet_megabytes = leaflet_megabytes
            )
    } else {
        # add RGB bands if required
        # create a leaflet for RGB bands
        leaf_map <- leaf_map |>
            .view_rgb_bands(
                group = group,
                tile = tile,
                red = red,
                green = green,
                blue = blue,
                date = date,
                opacity = opacity,
                max_cog_size = max_cog_size,
                first_quantile = first_quantile,
                last_quantile = last_quantile,
                leaflet_megabytes = leaflet_megabytes
            )
    }
    return(leaf_map)
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
#' @param  band          For plotting grey images.
#' @param  date          Date to be plotted.
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
                          band,
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
    # obtain the raster objects for the dates chosen
    # check if date is inside the timeline
    tile_dates <- .tile_timeline(tile)
    if (!date %in% tile_dates) {
        idx_date <- which.min(abs(date - tile_dates))
        date <- tile_dates[idx_date]
    }
    # filter by date and band
    band_file <- .tile_path(tile, band, date)
    # plot a single file
    # find if file supports COG overviews
    sizes <- .tile_overview_size(tile = tile, max_cog_size)
    # warp the file to produce a temporary overview (except for derived cube)
    if (!inherits(tile, "derived_cube"))
        band_file <- .gdal_warp_file(
            raster_file = band_file,
            sizes = sizes)
    # create a stars object
    st_obj <- stars::read_stars(
        band_file,
        along = "band",
        RasterIO = list(
            nBufXSize = sizes[["xsize"]],
            nBufYSize = sizes[["ysize"]]
        ),
        proxy = FALSE
    )
    # get scale and offset
    band_conf   <- .tile_band_conf(tile, band)
    band_scale  <- .scale(band_conf)
    band_offset <- .offset(band_conf)
    max_value   <- .max_value(band_conf)
    # scale the image
    st_obj <-  st_obj * band_scale + band_offset
    # get the values
    vals <- as.vector(st_obj[[1]])
    # obtain the quantiles
    quantiles <- stats::quantile(
        vals,
        probs = c(0, first_quantile, last_quantile, 1),
        na.rm = TRUE
    )
    # determine minmax
    minv <- quantiles[[1]]
    minq <- quantiles[[2]]
    maxq <- quantiles[[3]]
    maxv <- quantiles[[4]]
    # resample and warp the image
    st_obj <- stars::st_warp(
        src = st_obj,
        crs = sf::st_crs("EPSG:3857")
    )
    if (inherits(tile, "sar_cube"))
        domain <-  c(minq, maxq)
    else
        domain <-  c(minv, maxv)
    # produce color map
    colors_leaf <- leaflet::colorNumeric(
        palette = palette,
        domain = domain,
        reverse = FALSE
    )
    # add stars to leaflet
    leaf_map <- leafem::addStarsImage(
        leaf_map,
        x = st_obj,
        band = 1,
        colors = colors_leaf,
        project = FALSE,
        group = group,
        maxBytes = max_bytes,
        opacity = opacity
    )
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
#' @param  red           Band to be shown in red color
#' @param  green         Band to be shown in green color
#' @param  blue          Band to be shown in blue color
#' @param  date          Date to be plotted
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
                            red,
                            green,
                            blue,
                            date,
                            opacity,
                            max_cog_size,
                            first_quantile,
                            last_quantile,
                            leaflet_megabytes) {
    # calculate maximum size in MB
    max_bytes <- leaflet_megabytes * 1024^2
    # obtain the raster objects for the dates chosen
    # check if date is inside the timeline
    tile_dates <- .tile_timeline(tile)
    if (!date %in% tile_dates) {
        idx_date <- which.min(abs(date - tile_dates))
        date <- tile_dates[idx_date]
    }
    # filter by date and band
    # if there is only one band, RGB files will be the same
    red_file <- .tile_path(tile, red, date)
    green_file <- .tile_path(tile, green, date)
    blue_file <- .tile_path(tile, blue, date)

    # find if file supports COG overviews
    sizes <- .tile_overview_size(tile = tile, max_cog_size)
    # warp the image
    red_file <- .gdal_warp_file(red_file, sizes)
    green_file <- .gdal_warp_file(green_file, sizes)
    blue_file <- .gdal_warp_file(blue_file, sizes)

    # compose RGB files
    rgb_files <- c(r = red_file, g = green_file, b = blue_file)
    st_obj <- stars::read_stars(
        rgb_files,
        along = "band",
        RasterIO = list(
            nBufXSize = sizes[["xsize"]],
            nBufYSize = sizes[["ysize"]]
        ),
        proxy = FALSE
    )
    # resample and warp the image
    st_obj <- stars::st_warp(
        src = st_obj,
        crs = sf::st_crs("EPSG:3857")
    )
    # obtain the quantiles
    leaf_map <- leafem::addRasterRGB(
        leaf_map,
        x = st_obj,
        r = 1,
        g = 2,
        b = 3,
        quantiles = c(first_quantile, last_quantile),
        project = FALSE,
        group = group,
        opacity = opacity,
        maxBytes = max_bytes
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
#' @param  legend        Named vector that associates labels to colors.
#' @param  palette       Palette provided as alternative legend.
#' @param  opacity       Fill opacity
#' @param  max_cog_size  Maximum size of COG overviews (lines or columns)
#' @param  leaflet_megabytes Maximum size for leaflet (in MB)
#'
.view_class_cube <- function(leaf_map,
                             class_cube,
                             tile,
                             legend,
                             palette,
                             opacity,
                             max_cog_size,
                             leaflet_megabytes) {
    # set caller to show in errors
    .check_set_caller(".view_class_cube")
    # should we overlay a classified image?
    if (.has(class_cube)) {
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
        # create the stars objects that correspond to the tiles
        st_objs <- slider::slide(class_cube, function(tile) {
            # obtain the raster stars object
            st_obj <- stars::read_stars(
                .tile_path(tile),
                RAT = labels,
                RasterIO = list(
                    nBufXSize = sizes[["xsize"]],
                    nBufYSize = sizes[["ysize"]]
                ),
                proxy = FALSE
            )
            return(st_obj)
        })
        # keep the first object
        st_merge <- st_objs[[1]]
        # if there is more than one stars object, merge them
        if (length(st_objs) > 1) {
            st_merge <- do.call(
                stars::st_mosaic, st_objs
            )
        }
        # resample and warp the image
        st_obj_new <- stars::st_warp(
            src = st_merge,
            crs = sf::st_crs("EPSG:3857")
        )
        # rename dimension
        st_obj_new <- stats::setNames(st_obj_new, "labels")
        # If available, use labels to define which colors must be presented.
        # This is useful as some datasets (e.g., World Cover) represent
        # classified data with values that are not the same as the positions
        # of the color array (e.g., 10, 20), causing a misrepresentation of
        # the classes
        labels_available <- levels(st_obj_new[["labels"]])
        if (.has(labels_available)) {
            labels <- labels[labels_available]
        }
        # get colors only for the available labels
        colors <- .colors_get(
            labels = labels,
            legend = legend,
            palette = palette,
            rev = TRUE
        )
        # calculate maximum size in MB
        max_bytes <- leaflet_megabytes * 1024^2
        # add the classified image object
        leaf_map <- leaf_map |>
            leafem::addStarsImage(
                x = st_obj_new,
                opacity = opacity,
                colors = colors,
                method = "auto",
                group = "classification",
                project = FALSE,
                maxBytes = max_bytes
            )
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
#' @title  Get the labels for a classified vector cube
#' @name .view_get_labels_raster
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  class_cube    Classified raster cube
#' @return               Labels
#'
#'
.view_get_labels_raster <- function(class_cube) {
    labels <- .cube_labels(class_cube)
    return(labels)
}
#' @title  Get the labels for a classified vector cube
#' @name .view_get_labels_vector
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  cube          Classified vector cube
#' @param  legend        Class legend
#' @param  palette       Color palette
#' @return               Leaflet map with legend
#'
#'
.view_get_labels_vector <- function(cube,
                                    legend = NULL,
                                    palette = NULL) {
    # get segments from cube
    labels <- slider::slide(cube, function(tile) {
        # retrieve the segments for this tile
        segments <- .segments_read_vec(tile)
        # dissolve segments
        segments <- segments |>
            dplyr::group_by(.data[["class"]]) |>
            dplyr::summarise()
        # get the labels
        labels_tile <- segments |>
            sf::st_drop_geometry() |>
            dplyr::select("class") |>
            dplyr::pull()
        return(labels_tile)
    })
    labels <- unique(unlist(labels))
    return(labels)
}
#' @title  Add a legend to the leafmap
#' @name .view_add_legend
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  leaf_map      Leaflet map
#' @param  cube          Vector or raster cube
#' @param  legend        Class legend
#' @param  palette       Color palette
#' @return               Leaflet map with legend
#'
#'
.view_add_legend <- function(leaf_map,
                             cube,
                             legend,
                             palette) {
    # initialize labels
    labels <- NULL

    # obtain labels from class cube
    if (.has(cube)) {
        if (inherits(cube, "class_cube")) {
            labels <- .view_get_labels_raster(cube)
        }
        if (inherits(cube, "class_vector_cube")) {
            labels <- .view_get_labels_vector(cube)
        }
    }
    if (.has(labels)) {
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
    }
    return(leaf_map)
}
#' @title  Add overlay groups to leaflet map
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  overlay_groups  Vector with overlay groups
#' @param  cube            Vector cube (if available)
#' @param  class_cube      Classified cube (if available)
#' @return                 Updated overlay groups
.view_add_overlay_grps <- function(overlay_groups,
                                   cube,
                                   class_cube = NULL) {
    if (inherits(cube, "vector_cube"))
        overlay_groups <- append(overlay_groups, "segments")
    if (.has(class_cube))
        overlay_groups <- append(overlay_groups, "classification")
    return(overlay_groups)
}
#' @title  Add overlay group to leaflet map
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  tile          tile
#' @param  dates         Dates
#' @param  class_cube    Classified cube
#' @return               Leaflet map with with overlay groups
.view_add_overlay_group <- function(tile, date, band = NULL) {
    UseMethod(".view_add_overlay_group", tile)
}
#' @noRd
#' @export
.view_add_overlay_group.raster_cube <- function(tile, date, band) {
    group <- paste(tile[["tile"]], date)
}
#' @noRd
#' @export
.view_add_overlay_group.vector_cube <- function(tile, date, band = NULL) {
    group <- paste(tile[["tile"]],  as.Date(date))
}
#' @noRd
#' @export
.view_add_overlay_group.derived_cube <- function(tile, date = NULL, band) {
    group <- paste(tile[["tile"]], band)
}

