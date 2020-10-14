#' @title Get information from collection
#' @name .sits_stac_collection
#' @keywords internal
#'
#' @param url         a \code{character} representing a URL for the BDC catalog.
#' @param collection  a \code{character} with the collection to be searched.
#' @param bands       a \code{character} with the bands names to be filtered.
#'
#' @return            a \code{STACCollection} object returned by rstac.
.sits_stac_collection <- function(url         = NULL,
                                  collection  = NULL,
                                  bands       = NULL) {

    assertthat::assert_that(!purrr::is_null(url),
                            msg = paste("sits_cube: for STAC_CUBE url must be",
                                        "provided"))

    assertthat::assert_that(!purrr::is_null(collection),
                            msg = paste("sits_cube: for STAC_CUBE collections",
                                        "must be provided"))

    # creating a rstac object and making the requisition
    collection_info <- rstac::stac(url) %>%
        rstac::collections(collection_id = collection) %>%
        rstac::get_request()

    # get the name of the bands
    collection_bands <- sapply(collection_info$properties[["eo:bands"]],
                               `[[`, c("name"))
    # checks if the supplied bands match the product bands
    if (!is.null(bands)) {
        assertthat::assert_that(!all(collection_bands %in% bands),
                                msg = paste("The supplied bands do not match",
                                            "the data cube bands."))

        collection_bands <- collection_bands[collection_bands %in% bands]
    }

    # Add bands information as an attribute
    collection_info$bands <- collection_bands

    return(collection_info)
}
#' @title Get information from item
#' @name .sits_stac_items
#' @keywords internal
#'
#' @param url        a \code{character} representing a URL for the BDC catalog.
#' @param collection a \code{character} with the collection to be searched.
#' @param tiles      a \code{character} with the names of the tiles.
#' @param ids        a \code{character} vector with the items features ids.
#' @param bbox       a \code{numeric} vector with only features that have a
#' geometry that intersects the bounding box are selected. The bounding box is
#' provided as four or six numbers, depending on whether the coordinate
#' reference system includes a vertical axis (elevation or depth):
#' \itemize{ \item Lower left corner, coordinate axis 1
#'           \item Lower left corner, coordinate axis 2
#'           \item Lower left corner, coordinate axis 3 (optional)
#'           \item Upper right corner, coordinate axis 1
#'           \item Upper right corner, coordinate axis 2
#'           \item Upper right corner, coordinate axis 3 (optional) }
#'
#' The coordinate reference system of the values is WGS84 longitude/latitude
#' (\url{http://www.opengis.net/def/crs/OGC/1.3/CRS84}). The values are in
#' most cases the sequence of minimum longitude, minimum latitude, maximum
#' longitude and maximum latitude. However, in cases where the box spans the
#' antimeridian the first value (west-most box edge) is larger than the third
#' value (east-most box edge).
#' @param datetime   a \code{character} with a date-time or an interval. Date
#'  and time strings needs to conform RFC 3339. Intervals are expressed by
#'  separating two date-time strings by \code{'/'} character. Open intervals are
#'  expressed by using \code{'..'} in place of date-time.
#' @param intersects a \code{character} value expressing GeoJSON geometries
#' objects as specified in RFC 7946. Only returns items that intersect with
#' the provided polygon.
#' @param limit      an \code{integer} defining the maximum number of results
#' to return. If not informed it defaults to the service implementation.
#'
#' @return           a \code{STACItemCollection} object representing the search
#'  by rstac.
.sits_stac_items <- function(url             = NULL,
                             collection      = NULL,
                             tiles           = NULL,
                             ids             = NULL,
                             bbox            = NULL,
                             datetime        = NULL,
                             intersects      = NULL,
                             limit           = NULL) {

    # creating a rstac object
    rstac_query <- rstac::stac(url) %>%
        rstac::stac_search(collection = collection,
                           ids        = ids,
                           bbox       = bbox,
                           datetime   = datetime,
                           intersects = intersects,
                           limit      = limit)

    # if specified, a filter per tile is added to the query
    if (!is.null(tiles))
        rstac_query <- rstac_query %>%
            rstac::ext_query(keys = "bdc:tile", ops = "%in%", values = tiles)

    # making the request and fetching all the metadata
    items_info <- rstac_query %>% rstac::post_request() %>%
        rstac::items_fetch(progress = FALSE)

    return(items_info)
}
#' @title Create a group of items
#' @name .sits_stac_group
#' @keywords internal
#'
#' @param items  a \code{STACItemCollection} object returned by rstac package.
#' @param fields a \code{character} vector with the names of fields to be
#'  grouped.
#'
#' @return       a \code{list} in which each index corresponds to a group with
#'  its corresponding \code{STACItemCollection} objects.
.sits_stac_group <- function(items, fields) {

    # grouping the items according to fields provided
    items_grouped <- rstac::items_group(items  = items,
                                        fields = fields)

    # adding a tile attribute in the root
    items_grouped <- purrr::map(items_grouped, function(x) {
        x$tile <- rstac::items_reap(x, fields = fields)[[1]]

        return(x)
    })

    return(items_grouped)
}
#' @title Format assets
#' @name .sits_stac_items_info
#' @keywords internal
#'
#' @param items a \code{STACItemCollection} object returned by rstac package.
#' @param bands a \code{character} with the bands names to be filtered.
#'
#' @return      a \code{tibble} with date, band and path information, arranged
#'  by the date.
.sits_stac_items_info <- function(items, bands) {

    assets_info <- rstac::assets_list(items, assets_names = bands) %>%
        tibble::as_tibble() %>% dplyr::arrange(date)

    return(assets_info)
}
#' @title Get the metadata values from STAC.
#' @name .sits_config_stac_values
#' @keywords internal
#'
#' @param collection_info a \code{STACCollection} object returned by rstac.
#'  package.
#' @param bands           a \code{character} with the bands names to be
#'  filtered.
#'
#' @return                a \code{list} with the information of scale factors,
#'  missing, minimum, and maximum values.
.sits_config_stac_values <- function(collection_info, bands) {

    # filters by the index of the bands that correspond to the collection
    index_bands <-
        which(lapply(collection_info$properties$`eo:bands`,`[[`, c("name"))
              %in% bands)

    vect_values <- vector()
    list_values <- list()

    # creating a named list of the metadata values
    purrr::map(c("min", "max", "nodata", "scale"), function(field) {
        purrr::map(index_bands, function(index) {
            vect_values[collection_info$properties$`eo:bands`[[index]]$name] <<-
                as.numeric(collection_info$properties$`eo:bands`[[index]][[field]])
        })
        list_values[[field]] <<- vect_values
    })
    list_values
}
#' @title Get the STAC information corresponding to a tile.
#' @name .sits_stac_tile_cube
#' @keywords internal
#'
#' @param url             a \code{character} representing URL for the BDC STAC.
#' @param name            a \code{character} representing the output data cube.
#' @param collection_info a \code{STACCollection} object returned by rstac.
#' @param items_info      a \code{STACItemCollection} object returned by rstac.
#' @param cube            a \code{character} with name input data cube in BDC.
#' @param file_info       a \code{tbl_df} with the information from STAC.
#'
#' @return                a \code{tibble} with metadata information about a
#'  raster data set.
.sits_stac_tile_cube <- function(url,
                                 name,
                                 collection_info,
                                 items_info,
                                 cube,
                                 file_info){

    # obtain the timeline
    timeline <- unique(lubridate::as_date(file_info$date))

    # set the labels
    labels <- c("NoClass")

    # get the first image
    full_path_1 <- .sits_raster_check_webfiles(file_info[1,]$path)
    # obtain the parameters
    params <- .sits_raster_params(terra::rast(full_path_1))

    # get the bands
    bands <- unique(file_info$band)

    # get scale factors, missing, minimum, and maximum values
    metadata_values  <- .sits_config_stac_values(collection_info, bands)

    # create a tibble to store the metadata
    cube <- .sits_cube_create(type           = "BDC_STAC",
                              URL            = url,
                              satellite      = collection_info$properties$platform,
                              sensor         = collection_info$properties$instruments,
                              name           = name,
                              cube           = cube,
                              tile           = items_info$tile,
                              bands          = collection_info$bands,
                              labels         = labels,
                              scale_factors  = metadata_values$scale,
                              missing_values = metadata_values$nodata,
                              minimum_values = metadata_values$min,
                              maximum_values = metadata_values$max,
                              timelines      = list(timeline),
                              nrows          = params$nrows,
                              ncols          = params$ncols,
                              xmin           = params$xmin,
                              xmax           = params$xmax,
                              ymin           = params$ymin,
                              ymax           = params$ymax,
                              xres           = params$xres,
                              yres           = params$yres,
                              crs            = params$crs,
                              file_info      = file_info)

    return(cube)
}
