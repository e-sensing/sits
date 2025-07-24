# ---- source api ----
#' @title Transform an items object in an TerraScope (World Cover) cube
#' @keywords internal
#' @noRd
#' @description \code{.source_items_new()} this function is called to create
#' an items object. In case of Web services, this function is responsible for
#' making the Web requests to the server.
#' @param source     Name of the STAC provider.
#' @param ...        Other parameters to be passed for specific types.
#' @param collection Collection to be searched in the data source.
#' @param stac_query Query that follows the STAC protocol
#' @param tiles      Selected tiles (optional)
#' @param platform   Satellite platform (optional).
#' @return An object referring the images of a sits cube.
#' @export
`.source_items_new.terrascope_cube_world-cover-2021` <- function(source, ...,
                                                                 collection,
                                                                 stac_query,
                                                                 tiles = NULL,
                                                                 platform = NULL) {
    # set caller to show in errors
    .check_set_caller(".source_items_new_terrascope_cube")
    # convert roi to bbox
    roi <- .stac_intersects_as_bbox(stac_query)
    # update stac query with the new spatial reference
    stac_query[["params"]][["intersects"]] <- NULL
    stac_query[["params"]][["bbox"]] <- roi[["bbox"]]
    # world cover product has all data available for a single date. So, fix the
    # temporal interval from the query
    stac_query[["params"]][["datetime"]] <- "2022-06-01T00:00:00Z"
    # making the request
    items_info <- rstac::post_request(q = stac_query, ...)
    .check_stac_items(items_info)
    # if more than 2 times items pagination are found the progress bar
    # fetching all the metadata and updating to upper case instruments
    items_info <- rstac::items_fetch(items = items_info, progress = FALSE)
    # checks if the items returned any items
    .check_stac_items(items_info)
    return(items_info)
}
# ---- source api ----
#' @title Transform an items object in an TerraScope (World Cover) cube
#' @keywords internal
#' @noRd
#' @description \code{.source_items_new()} this function is called to create
#' an items object. In case of Web services, this function is responsible for
#' making the Web requests to the server.
#' @param source     Name of the STAC provider.
#' @param ...        Other parameters to be passed for specific types.
#' @param collection Collection to be searched in the data source.
#' @param stac_query Query that follows the STAC protocol
#' @param tiles      Selected tiles (optional)
#' @param platform   Satellite platform (optional).
#' @return An object referring the images of a sits cube.
#' @export
`.source_items_new.terrascope_cube_world-cereal-2021` <- function(source, ...,
                                                                  collection,
                                                                  stac_query,
                                                                  tiles = NULL,
                                                                  platform = NULL) {
    # set caller to show in errors
    .check_set_caller(".source_items_new_terrascope_cube")
    # force token generation
    .source_terrascope_persist_token()
    # convert roi to bbox
    roi <- .stac_intersects_as_bbox(stac_query)
    # update stac query with the new spatial reference
    stac_query[["params"]][["intersects"]] <- NULL
    stac_query[["params"]][["bbox"]] <- roi[["bbox"]]
    # world cover product has all data available for a single date. So, fix the
    # temporal interval from the query
    stac_query[["params"]][["datetime"]] <- "2021-01-01T00:00:00Z/2021-12-31T00:00:00Z"
    # making the request
    items_info <- rstac::post_request(q = stac_query, ...)
    .check_stac_items(items_info)
    # if more than 2 times items pagination are found the progress bar
    # fetching all the metadata and updating to upper case instruments
    items_info <- rstac::items_fetch(items = items_info, progress = FALSE)
    # checks if the items returned any items
    .check_stac_items(items_info)
    return(items_info)
}


#' @keywords internal
#' @noRd
#' @export
`.source_items_tile.terrascope_cube_world-cereal-2021` <- function(source, ...,
                                                                   items,
                                                                   collection = NULL) {
    rstac::items_reap(items, field = "id") |>
        purrr::map_chr(function(property) {
            # extract tile from asset id
            stringr::str_split(property, "_") |>
                (\(x) x[[1]][4])() |>
                stringr::str_split(":") |>
                (\(x) x[[1]][2])()
        })
}

#' @keywords internal
#' @noRd
#' @export
`.source_items_tile.terrascope_cube_world-cover-2021` <- function(source, ...,
                                                                  items,
                                                                  collection = NULL) {
    rstac::items_reap(items, field = c("properties", "title")) |>
        purrr::map_chr(function(property) {
            # extract date from the filename
            stringr::str_split(property, "_")[[1L]][[6L]]
        })
}

# ---- token-related functions ----
#' @title Get new Terrascope authentication token.
#' @keywords internal
#' @noRd
#' @description \code{.source_terrascope_get_token()} this function can be
#' used to authenticate gdal calls to Terrascope services.
#' @return A list object with the token details.
.source_terrascope_get_token <- function() {
    # define caller
    .check_set_caller(".terrascope_get_token")
    # get authentication env variables
    terrascope_user <- Sys.getenv("TERRASCOPE_USER")
    terrascope_pass <- Sys.getenv("TERRASCOPE_PASSWORD")
    # auth variables must be available
    .check_that(
        terrascope_user != "" || terrascope_pass != ""
    )
    # get terrascope auth endpoint
    terrascope_auth_endpoint <- .conf("sources", "TERRASCOPE", "auth")
    # generate token
    res <- httr2::request(terrascope_auth_endpoint) |>
        httr2::req_body_form(
            grant_type = "password",
            client_id = "public",
            username = terrascope_user,
            password = terrascope_pass
        ) |>
        httr2::req_headers(
            `Content-Type` = "application/x-www-form-urlencoded"
        ) |>
        httr2::req_perform()
    # check status
    httr2::resp_check_status(res)
    # get request token and return!
    httr2::resp_body_json(res)
}
#' @title Get and persist Terrascope authentication token.
#' @keywords internal
#' @noRd
#' @description \code{.source_terrascope_get_token()} this function can be
#' used to authenticate gdal calls to Terrascope services. To persist the
#' token, this function defines the \code{GDAL_HTTP_HEADER_FILE} env variable.
#' @return No value is returned.
.source_terrascope_persist_token <- function() {
    # get auth token
    terrascope_token <- .source_terrascope_get_token()
    terrascope_token <- terrascope_token[["access_token"]]
    # create gdal header file to persist token
    gdal_header_file <- tempfile()
    # format token as http authorization code
    gdal_header_content <- paste("Authorization: Bearer", terrascope_token)
    # save gdal header content
    writeLines(gdal_header_content, gdal_header_file)
    # gdal header
    Sys.setenv("GDAL_HTTP_HEADER_FILE" = gdal_header_file)
}
#' @title Flush terrascope authentication token.
#' @keywords internal
#' @noRd
#' @description \code{.source_terrascope_flush_token()} this function flushes
#' the terrascope authentication token. To flush, the function removes the
#' content of the \code{GDAL_HTTP_HEADER_FILE} env variable.
#' @return No value is returned.
.source_terrascope_flush_token <- function() {
    # flush header file
    Sys.unsetenv("GDAL_HTTP_HEADER_FILE")
}
