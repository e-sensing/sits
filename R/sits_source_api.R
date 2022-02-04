#' @title Source functions
#' @name source_functions
#' @keywords internal
#'
#' @description
#' These functions provide an API to handle/retrieve data from sources.
#'
#' @param source     A \code{character} value referring to a valid data source.
#'
#' @return
#' The values returned by each function are described as follows.
NULL

#' @rdname source_functions
#'
#' @description \code{.sources()} lists all sources available in sits.
#'
#' @return \code{.sources()} returns a \code{character} vector
#' with all sources names available in sits.
.sources <- function() {

    src <- .config_names(key = c("sources"))

    # source names are upper case
    src <- toupper(src)

    # post-condition
    .check_chr(src, allow_empty = FALSE, len_min = 1,
               msg = "invalid 'sources' in config file")

    return(src)
}

#' @rdname source_functions
#'
#' @description \code{.source_check()} checks if a source is available in sits.
#'
#' @return \code{.source_check()} returns \code{NULL} if no error occurs.
.source_check <- function(source) {

    # source is upper case
    source <- toupper(source)

    # check source
    .check_chr(source, len_min = 1, len_max = 1,
               msg = "invalid 'source' parameter")
    .check_chr_within(source, within = .sources(),
                      msg = paste0("invalid 'source' parameter.", "\n",
                                   "please check valid sources with sits_list_sources()"))

    return(invisible(NULL))
}

#' @rdname source_functions
#'
#' @description \code{.source_new()} creates an object with a corresponding
#' S3 class defined in a given source and collection.
#'
#' @return \code{.source_new()} returns a \code{character} vector with the
#' S3 class defined in source's \code{S3class} attribute.
#'
.source_new <- function(source, collection = NULL, is_local = FALSE) {

    # if local, return local cube
    if (is_local) {
        class(source) <- c("local_cube", class(source))
        return(source)
    }
    # source name is upper case
    classes <- .source_s3class(source = toupper(source))

    class(source) <- c(classes, class(source))

    if (!is.null(collection)) {
        classes <- c(paste(classes, tolower(collection), sep = "_"), classes)
        class(source) <- c(classes, class(source))
    }

    return(source)
}

#' @rdname source_functions
#'
#' @description \code{.source_service()} returns the service associated
#' with a given source.
#'
#' @return \code{.source_service()} returns a \code{character} value or
#' \code{NA} if no service is associated with a given source.
.source_service <- function(source) {

    # source is upper case
    source <- toupper(source)

    # pre-condition
    .source_check(source = source)

    service <- .config_get(key = c("sources", source, "service"),
                           default = NA_character_)

    # post-condition
    .check_chr(service, allow_na = TRUE, allow_empty = FALSE,
               len_min = 1, len_max = 1,
               msg = sprintf("invalid 'service' for source %s in config file",
                             source))

    return(service)
}

#' @rdname source_functions
#'
#' @description \code{.source_s3class()} returns the s3 class associated
#' with a given source.
#'
#' @return \code{.source_s3class()} returns a \code{character} vector.
#' sits uses these classes to run source functions.
.source_s3class <- function(source) {

    # source is upper case
    source <- toupper(source)

    # pre-condition
    .source_check(source = source)

    s3_class <- .config_get(key = c("sources", source, "s3_class"))

    # post-condition
    .check_chr(s3_class, allow_empty = FALSE, len_min = 1,
               msg = sprintf("invalid 's3_class' for source %s in config file",
                             source))

    return(s3_class)
}

#' @rdname source_functions
#'
#' @description \code{.source_url()} get an URL associated with a source. Not
#' all sources have an URL. In these cases, an \code{NA} is returned.
#'
#' @return \code{.source_url()} returns a \code{character} value or \code{NA}
#' if no URL is associated with a given source.
.source_url <- function(source) {

    # source is upper case
    source <- toupper(source)

    # pre-condition
    .source_check(source = source)

    url <- .config_get(key = c("sources", source, "url"),
                       default = NA_character_)

    # post-condition
    .check_chr(url, allow_na = TRUE, allow_empty = FALSE,
               len_min = 1, len_max = 1,
               msg = sprintf("invalid 'url' for source %s in config file",
                             source))

    return(url)
}

#' @title Source bands functions
#' @name source_bands
#' @keywords internal
#'
#' @description
#' These functions provide an API to handle/retrieve data from bands.
#'
#' @param source     A \code{character} value referring to a valid data source.
#' @param collection A \code{character} value referring to a collection of the
#' source.
#' @param fn_filter  A \code{function} that will be applied in each band
#' to filter selection. The provided function must have an input parameter to
#' receive band object and return a \code{logical} value.
#' @param add_cloud  A \code{logical} value indicating if cloud band
#' must be returned.
#' @param key        A \code{character} containing a valid key of a
#' band object. The corresponding band attribute will be returned as result.
#' @param bands      A \code{character} vector containing all bands to
#' be considered. If a \code{NULL} value is provided (default) all bands are
#' considered.
#' @param default    Any value to be returned if an attribute or key is not
#' found.
#'
#' @return
#' The values returned by each function are described as follows.
NULL

#' @rdname source_bands
#'
#' @description \code{.source_bands()} lists all bands defined in a collection
#' that matches the criteria defined by its parameters. If no filter is
#' provided, all bands are returned.
#'
#' @return \code{.source_bands()} returns a \code{character} vector with bands
#' names
.source_bands <- function(source,
                          collection, ...,
                          fn_filter = NULL,
                          add_cloud = TRUE) {

    # source is upper case
    source <- toupper(source)

    # collection is upper case
    collection <- toupper(collection)

    # pre-condition
    .source_collection_check(source = source, collection = collection)

    bands <- .config_names(key = c("sources", source, "collections",
                                   collection, "bands"))
    # bands names are upper case
    bands <- toupper(bands)

    if (!add_cloud)
        bands <- bands[bands != .source_cloud()]

    if (!is.null(fn_filter)) {
        select <- vapply(bands, function(band) {
            fn_filter(.config_get(key = c("sources", source, "collections",
                                          collection, "bands", band)))
        }, logical(1))

        bands <- bands[select]
    }

    # post-condition
    # check bands are non-NA character
    .check_chr(bands, allow_empty = FALSE,
               msg = "invalid selected bands")

    return(bands)
}

#' @rdname source_bands
#'
#' @description \code{.source_bands_reap()} reaps the attributes' values
#' indicated by \code{key} argument for all bands filtered by its parameters.
#'
#' @return \code{.source_bands_reap()} returns any object stored in the
#' band attribute indicated by \code{key} parameter. If attribute is not
#' found, \code{default} value is returned.
.source_bands_reap <- function(source,
                               collection,
                               key, ...,
                               bands = NULL,
                               fn_filter = NULL,
                               add_cloud = TRUE,
                               default = NULL) {

    # source is upper case
    source <- toupper(source)

    # collection is upper case
    collection <- toupper(collection)

    # pre-condition
    .source_collection_check(source = source, collection = collection)

    if (is.null(bands))
        bands <- .source_bands(source = source,
                               collection = collection,
                               fn_filter = fn_filter,
                               add_cloud = add_cloud)

    # pre-condition
    .check_chr(bands, allow_na = FALSE, allow_empty = FALSE, len_min = 1,
               msg = "invalid bands")

    # bands names are upper case
    bands <- toupper(bands)

    # always returns a list!
    result <- lapply(bands, function(band) {
        .config_get(key = c("sources", source, "collections",
                            collection, "bands", band, key),
                    default = default)
    })

    names(result) <- bands

    return(result)
}

#' @rdname source_bands
#'
#' @description \code{.source_bands_band_name()} returns the \code{band_name}
#' attribute of all bands filtered by its parameters.
#'
#' @return \code{.source_bands_band_name()} returns a \code{character} vector.
.source_bands_band_name <- function(source,
                                    collection, ...,
                                    bands = NULL) {

    # source is upper case
    source <- toupper(source)

    # collection is upper case
    collection <- toupper(collection)

    # pre-condition
    .source_collection_check(source = source, collection = collection)

    bands <- .source_bands_reap(source = source,
                                collection = collection,
                                key = "band_name",
                                bands = bands)

    # simplify to a unnamed character vector
    bands <- unlist(bands, recursive = FALSE, use.names = FALSE)

    # post-conditions
    .check_chr(bands, allow_na = FALSE, allow_empty = FALSE,
               len_min = length(bands), len_max = length(bands),
               msg = "inconsistent 'band_name' values")

    return(bands)
}

#' @rdname source_bands
#'
#' @description \code{.source_bands_resolution()} returns the
#' \code{resolution} attribute of all bands filtered by its parameters.
#'
#' @return \code{.source_bands_resolution()} returns a named \code{list}
#' containing \code{numeric} vectors with the spatial resolution of a band.
.source_bands_resolution <- function(source,
                                     collection, ...,
                                     bands = NULL,
                                     fn_filter = NULL,
                                     add_cloud = TRUE) {

    # source is upper case
    source <- toupper(source)

    # collection is upper case
    collection <- toupper(collection)

    # pre-condition
    .source_collection_check(source = source, collection = collection)

    resolution <- .source_bands_reap(source = source,
                                     collection = collection,
                                     key = "resolution",
                                     bands = bands,
                                     fn_filter = fn_filter,
                                     add_cloud = add_cloud)

    # cannot simplify as each element can have length greater than one
    # post-condition
    .check_lst(resolution, fn_check = .check_num, min = 0,
               allow_zero = FALSE, len_min = 1,
               msg = "invalid 'resolution' in config file")

    return(resolution)
}

#' @rdname source_bands
#'
#' @description \code{.source_bands_to_sits()} converts any bands to its
#' sits name indicated in band entry.
#'
#' @return \code{.source_bands_to_sits()} returns a \code{character} vector
#' with all converted bands name.
.source_bands_to_sits <- function(source,
                                  collection,
                                  bands) {

    # bands name are upper case
    bands <- toupper(bands)

    # bands sits
    bands_sits <- .source_bands(source, collection)
    names(bands_sits) <- toupper(bands_sits)

    # bands source
    bands_to_sits <- bands_sits
    names(bands_to_sits) <-  toupper(
        .source_bands_band_name(source = source,
                                collection = collection))

    unknown_bands <- setdiff(unique(bands), names(bands_sits))
    names(unknown_bands) <- unknown_bands

    bands_converter <- c(bands_to_sits, bands_sits, unknown_bands)

    # post-condition
    .check_chr_within(bands, within = names(bands_converter),
                      msg = "invalid 'bands' parameter")

    return(unname(bands_converter[bands]))
}

#' @rdname source_bands
#'
#' @description \code{.source_bands_to_source()} converts any bands to its
#' corresponding names indicated in \code{band_name} attribute.
#'
#' @return \code{.source_bands_to_source()} returns a \code{character} vector
#' with all converted bands name.
.source_bands_to_source <- function(source, collection, bands) {

    # bands are upper case
    bands <- toupper(bands)

    # bands sits
    bands_source <- .source_bands_band_name(source = source,
                                            collection = collection)
    names(bands_source) <- toupper(bands_source)

    # bands source
    bands_to_source <- bands_source
    names(bands_to_source) <- toupper(.source_bands(source, collection))

    bands_converter <- c(bands_to_source, bands_source)

    # post-condition
    .check_chr_within(bands, within = names(bands_converter),
                      msg = "invalid 'bands' parameter")

    return(unname(bands_converter[bands]))
}

#' @rdname source_bands
#'
#' @description \code{.source_cloud()} lists cloud band for a collection.
#'
#' @return \code{.source_cloud()} returns a \code{character} vector with cloud
#' band name.
.source_cloud <- function() {

    return("CLOUD")
}

#' @rdname source_bands
#'
#' @description \code{.source_cloud_bit_mask()} returns the \code{bit_mask}
#' attribute of a cloud band, indicating if the cloud band is a bit mask.
#'
#' @return \code{.source_cloud_bit_mask()} returns a \code{logical} value.
.source_cloud_bit_mask <- function(source,
                                   collection) {

    # source is upper case
    source <- toupper(source)

    # collection is upper case
    collection <- toupper(collection)

    # pre-condition
    .source_collection_check(source = source, collection = collection)

    bit_mask <- .config_get(key = c("sources", source, "collections", collection,
                                    "bands", .source_cloud(),
                                    "bit_mask"))

    # post-condition
    .check_lgl(bit_mask, len_min = 1, len_max = 1,
               msg = "invalid 'bit_mask' value in config file")

    return(bit_mask)
}

#' @rdname source_bands
#'
#' @description \code{.source_cloud_values()} returns the \code{values}
#' attribute of a cloud band.
#'
#' @return \code{.source_cloud_values()} returns a named \code{list} containing
#' all values/or bits description of a cloud band.
.source_cloud_values <- function(source,
                                 collection) {

    # source is upper case
    source <- toupper(source)

    # collection is upper case
    collection <- toupper(collection)

    # pre-condition
    .source_collection_check(source = source, collection = collection)

    vls <- .config_get(key = c("sources", source, "collections", collection,
                               "bands", .source_cloud(),
                               "values"))

    # post-condition
    .check_lst(vls, msg = "invalid cloud 'values' in config file")

    return(vls)
}

#' @rdname source_bands
#'
#' @description \code{.source_cloud_interp_values()} returns the
#' \code{interp_values} attribute of a cloud band, indicating which value/bit
#' must be interpolated (e.g. shadows, clouds).
#'
#' @return \code{.source_cloud_interp_values()} returns a \code{numeric}
#' vector with all values/or bits to be interpolated if found in the cloud band.
.source_cloud_interp_values <- function(source,
                                        collection) {

    # source is upper case
    source <- toupper(source)

    # collection is upper case
    collection <- toupper(collection)

    # pre-condition
    .source_collection_check(source = source, collection = collection)

    vls <- .config_get(key = c("sources", source, "collections", collection,
                               "bands", .source_cloud(),
                               "interp_values"))

    # post-condition
    .check_num(vls, msg = "invalid 'interp_values' in config file")

    return(vls)
}

#' @title Source collection functions
#' @name source_collection
#' @keywords internal
#'
#' @description
#' These functions provide an API to handle/retrieve data from source's
#' collections.
#'
#' @param source     A \code{string} value referring to a valid data source.
#' @param collection A \code{string} value referring to a collection of the
#' source.
#' @param tiles      A \code{vector} with the tile names
#'
#' @return
#' The values returned by each function are described as follows.
NULL

#' @rdname source_collection
#'
#' @description \code{.source_collections()} lists all collections of a source.
#'
#' @return \code{.source_collections()} returns a \code{character} vector
#' with all collection names of a given source.
.source_collections <- function(source, ...) {

    # source is upper case
    source <- toupper(source)

    # check source
    .source_check(source = source)

    collections <- .config_names(c("sources", source, "collections"))

    return(collections)
}

#' @rdname source_collection
.source_collection_access_test <- function(source, collection, ...) {

    source <- .source_new(source)

    UseMethod(".source_collection_access_test", source)
}

#' @rdname source_collection
#'
#' @description \code{.source_collection_access_vars_set} sets
#' \code{access_vars} environment variables.
#'
#' @return \code{.source_collection_access_vars_set } returns \code{NULL} if
#' no error occurs.
.source_collection_access_vars_set <- function(source,
                                               collection) {

    # source is upper case
    source <- toupper(source)

    # collection is upper case
    collection <- toupper(collection)
    # get access variables for this source/collection
    vars <- .config_get(key = c("sources", source, "collections", collection,
                                "access_vars"),
                        default = list())
    # post-condition
    .check_lst(vars, msg = paste0("invalid access vars for collection ", collection,
                                  " in source ", source))
    if (length(vars) > 0)
        do.call(Sys.setenv, args = vars)

    return(invisible(vars))
}

#' @rdname source_collection
#'
#' @description \code{.source_collection_check()} checks if a collection
#' is from a source.
#'
#' @return \code{.source_collection_check()} returns \code{NULL} if
#' no error occurs.
.source_collection_check <- function(source,
                                     collection) {

    # check collection
    .check_chr(collection, len_min = 1, len_max = 1,
               msg = "invalid 'collection' parameter")

    .check_chr_within(collection,
                      within = .source_collections(source = source),
                      msg = "invalid 'collection' parameter")

    return(invisible(NULL))
}

#' @rdname source_collection
#'
#' @description \code{.source_collection_gdalcubes_type()} checks if a collection
#' has a gdalcubes type for writing files.
#'
#' @return \code{.source_collection_gdalcubes_type()} returns the gdal type.
.source_collection_gdalcubes_type <- function(source, collection){

    # try to find the gdalcubes configuration type for this collection
    gdal_format <- .config_get(key = c("sources", source, "collections",
                                       collection, "gdalcubes_type_format"),
                               default = NA)

    # if the format does not exist, report to the user
    .check_na(gdal_format,
              msg = paste("no type was found for collection ", collection,
                          "and source", source,
                          ". Please raise an issue in github"))

    # return the gdal format file path
    return(gdal_format)
}

#' @rdname source_collection
#'
#' @description \code{.source_collection_gdalcubes_support()} checks if a collection
#' can be regularized using gdalcubes.
#'
#' @return true/false
.source_collection_gdalcubes_support <- function(source, collection){

    # try to find the gdalcubes configuration
    gdal_support <- .config_get(key = c("sources", source, "collections",
                                        collection, "gdalcubes_support"),
                                default = NA)

    # if the collection cant be supported the user is reported
    .check_na(gdal_support,
              msg = paste("no type was found for collection", collection,
                          "and source", source))

    return(invisible(gdal_support))
}

#' @rdname source_collection
#'
#' @description \code{.source_collection_gdalcubes_config()} checks if a collection
#' has a gdalcubes format description.
#'
#' @return \code{.source_collection_gdal_config()} returns the gdal format file path.
.source_collection_gdalcubes_config <- function(source, collection){
    # try to find the gdalcubes configuration format for this collection
    gdal_config <- .config_get(key = c("sources", source, "collections",
                                       collection, "gdalcubes_format_col"),
                               default = NA)
    # if the format does not exist, report to the user
    .check_that(!(is.na(gdal_config)),
                msg = paste0("collection ", collection, " in source ", source,
                             " not supported yet\n",
                             "Please raise an issue in github"))

    # return the gdal format file path
    system.file(paste0("extdata/gdalcubes/", gdal_config), package = "sits")
}
#' @rdname source_collection
#'
#' @description \code{.source_collection_metadata_search()} retrieves the
#' metadadata search strategy for a given source and collection.
#'
#' @return \code{.source_collection_metadata_search()} returns a character
#' value with the metadata search strategy.
.source_collection_metadata_search <- function(source, collection){

    # try to find the gdalcubes configuration
    metadata_search <- .config_get(key = c("sources", source, "collections",
                                           collection, "metadata_search"),
                                   default = NA)

    # if the collection cant be supported the user is reported
    .check_na(metadata_search,
              msg = paste("no type was found for collection", collection,
                          "and source", source))

    return(invisible(metadata_search))
}

#' @rdname source_collection
#'
#' @description \code{.source_collection_name()} returns the name of a
#' collection in its original source.
#'
#' @return \code{.source_collection_name()} returns a \code{character}.
#'
.source_collection_name <- function(source,
                                    collection) {

    # source is upper case
    source <- toupper(source)

    # collection is upper case
    collection <- toupper(collection)

    # pre-condition
    .source_collection_check(source = source,
                             collection = collection)

    res <- .config_get(key = c("sources", source, "collections", collection,
                               "collection_name"))

    # post-condition
    .check_chr(res, allow_empty = FALSE, len_min = 1, len_max = 1,
               msg = "invalid 'collection_name' value")

    return(res)
}

#' @rdname source_collection
#'
#' @description \code{.source_collection_open_data()} informs if a
#' collection is open data or not.
#'
#' @return \code{.source_collection_open_data()} returns a \code{logical}.
#'
.source_collection_open_data <- function(source,
                                         collection) {

    # source is upper case
    source <- toupper(source)

    # collection is upper case
    collection <- toupper(collection)

    # pre-condition
    .source_collection_check(source = source,
                             collection = collection)

    res <- .config_get(key = c("sources", source, "collections", collection,
                               "open_data"), default = FALSE)

    # post-condition
    .check_lgl(res, len_min = 1, len_max = 1,
               msg = "invalid 'open_data' value")

    return(res)
}
#' @rdname source_collection
#'
#' @description \code{.source_collection_open_data_token()} informs if a
#' collection requires a token to access.
#'
#' @return \code{.source_collection_open_data_token()} returns a \code{logical}.
#'
.source_collection_open_data_token <- function(source,
                                               collection) {

    # source is upper case
    source <- toupper(source)

    # collection is upper case
    collection <- toupper(collection)

    # pre-condition
    .source_collection_check(source = source,
                             collection = collection)

    res <- .config_get(key = c("sources", source, "collections", collection,
                               "open_data_token"), default = FALSE)

    # post-condition
    .check_lgl(res, len_min = 1, len_max = 1,
               msg = "invalid 'open_data_token' value")

    return(res)
}

#' @rdname source_collection
#'
#' @description \code{.source_collection_token_check()} checks if a collection
#' needs environmental variables.
#'
#' @return \code{.source_collection_token_check()} returns \code{NULL} if
#' no error occurs.
#'
.source_collection_token_check <- function(source, collection){

    res <- .config_get(key = c("sources", source, "collections", collection,
                               "token_vars"),
                       default = character(0))
    # post-condition
    .check_chr(res, allow_empty = FALSE,
               msg = paste0("Missing access token for collection ", collection,
                            " in source ", source))
    if (length(res) > 0) {
        # Pre-condition - try to find the access key as an environment variable
        .check_env_var(res,
                       msg = paste0("Missing access token for source ", source))
    }
}

#' @rdname source_collection
#'
#' @description \code{.source_collection_tile_check()} checks if a collection
#' requires tiles to be defined
#'
#' @return \code{.source_collection_tile_check()} returns \code{NULL} if
#' no error occurs.
#'
.source_collection_tile_check <- function(source, collection, tiles){

    res <- .config_get(key = c("sources", source, "collections", collection,
                               "tile_required"),
                       default = "false")
    if (res) {
        # Are the tiles provided?
        .check_chr(x = tiles,
                   allow_empty = FALSE,
                   len_min = 1,
                   msg = paste("for ", source, " collection ", collection,
                               "please inform the tiles of the region of interest"))
    }
    return(invisible(NULL))
}

#' @title Functions to instantiate a new cube from a source
#' @name source_cube
#' @keywords internal
#'
#' @description
#' These functions provide an API to instantiate a new cube object and
#' access/retrieve information from services or local files to fill
#' cube attributes.
#'
#' A cube is formed by images (items) organized in tiles. To create a sits
#' cube object (a \code{tibble}), a set of functions are called in order
#' to retrieve metadata.
#'
#' @param source     A \code{character} value referring to a valid data source.
#' @param ...        Additional parameters.
#' @param items      Any object referring to the images bands (scenes) that
#' compose a cube.
#' @param asset      Any supported \code{raster} object to retrieve information.
#' @param collection A \code{character} value referring to a collection of the
#' source.
#' @param data_dir   Directory where local files are stored
#' @param file_info  A \code{tibble} that organizes the metadata about each
#' file in the tile: date, band, resolution, and path (or URL).
#' @param bands      Bands to be selected in the collection.
#'
#' @return
#' The values returned by each function are described as follows.
NULL

#' @rdname source_cube
#'
#' @description \code{.source_cube()} is called to start the cube creation
#' from a source.
#'
#' @return \code{.source_cube()} returns a sits \code{tibble} with cube
#' metadata.
#'
.source_cube <- function(source, collection, ...) {
    source <- .source_new(source = source, collection = collection)
    UseMethod(".source_cube", source)
}

#' @rdname source_cube
#'
#' @description \code{.source_item_get_date()} retrieves the date of an item
#' (a set of images from different bands that forms a scene).
#'
#' @return \code{.source_item_get_date()} returns a \code{Date} value.
#'
.source_item_get_date <- function(source, item, ..., collection = NULL) {
    source <- .source_new(source)
    UseMethod(".source_item_get_date", source)
}

#' @rdname source_cube
#'
#' @description \code{.source_item_get_hrefs()} retrieves the paths or URLs of
#' each file bands of an item.
#'
#' @return \code{.source_item_get_hrefs()} returns a \code{character} vector
#' containing paths to each image band of an item.
#'
.source_item_get_hrefs <- function(source, item, ..., collection = NULL) {
    source <- .source_new(source)
    UseMethod(".source_item_get_hrefs", source)
}

#' @rdname source_cube
#'
#' @description \code{.source_item_get_cloud_cover()} retrieves the percentage of cloud
#' cover of an image.
#' @return \code{.source_item_get_cloud_cover()} returns a \code{numeric} vector
#' containing the percentage of cloud cover to each image band of an item.
#'
.source_item_get_cloud_cover <- function(source, ..., item, collection = NULL) {
    source <- .source_new(source)
    UseMethod(".source_item_get_cloud_cover", source)
}

#' @rdname source_cube
#'
#' @description \code{.source_item_get_bands()} retrieves the bands present
#' in an item.
#'
#' @return \code{.source_item_get_bands()} returns a \code{character} vector
#' containing bands name of an item.
#'
.source_item_get_bands <- function(source, item, ..., collection = NULL) {
    source <- .source_new(source)
    UseMethod(".source_item_get_bands", source)
}

#' @rdname source_cube
#'
#' @description \code{.source_item_get_fid()} retrieves the feature id of
#' one item.
#'
#' @return \code{.source_item_get_fid()} returns a \code{character}.
#'
.source_item_get_fid <- function(source, item, ..., collection = NULL) {
    source <- .source_new(source)
    UseMethod(".source_item_get_fid", source)
}

#' @rdname source_cube
#'
#' @description \code{.source_item_get_resolution()} retrieves the feature
#' resolution.
#'
#' @return \code{.source_item_get_resolution()} returns a \code{numeric}.
#'
.source_item_get_resolution <- function(source, item, ..., collection = NULL) {
    source <- .source_new(source)
    UseMethod(".source_item_get_resolution", source)
}

#' @rdname source_cube
#'
#' @description \code{.source_item_get_tile()} retrieves the feature
#' tile.
#'
#' @return \code{.source_item_get_tile()} returns a \code{character}.
#'
.source_item_get_tile <- function(source, item, ..., collection = NULL) {
    source <- .source_new(source)
    UseMethod(".source_item_get_tile", source)
}

#' @rdname source_cube
#'
#' @description \code{.source_item_get_bbox()} retrieves the feature
#' bounding box.
#'
#' @return \code{.source_item_get_bbox()} returns a \code{numeric}.
#'
.source_item_get_bbox <- function(source, item, ..., collection = NULL) {
    source <- .source_new(source)
    UseMethod(".source_item_get_bbox", source)
}

#' @rdname source_cube
#'
#' @description \code{.source_item_get_crs()} retrieves the feature
#' crs.
#'
#' @return \code{.source_item_get_crs()} returns a \code{character}.
#'
.source_item_get_crs <- function(source, item, ..., collection = NULL) {
    source <- .source_new(source)
    UseMethod(".source_item_get_crs", source)
}

#' @rdname source_cube
#'
#' @description \code{.source_item_get_size()} retrieves the feature
#' raster sizes.
#'
#' @return \code{.source_item_get_size()} returns a \code{numeric}.
#'
.source_item_get_size <- function(source, item, ..., collection = NULL) {
    source <- .source_new(source)
    UseMethod(".source_item_get_size", source)
}

#' @rdname source_cube
#'
#' @description \code{.source_items_new()} this function is called to create
#' an items object. In case of Web services, this function is responsible for
#' making the Web requests to the server.
#'
#' @return \code{.source_items_new()} returns any object referring the images
#' of a sits cube.
#'
.source_items_new <- function(source, ..., collection = NULL) {
    source <- .source_new(source = source, collection = collection)
    UseMethod(".source_items_new", source)
}

#' @rdname source_cube
#'
#' @title Item selection from Bands
#' @name .source_items_bands_select
#' @keywords internal
#'
#'
#' @return \code{.source_items_bands_select()} returns the same object as
#' \code{items} with selected bands.
#'
.source_items_bands_select <- function(source, items, bands, ...,
                                       collection = NULL) {
    source <- .source_new(source = source, collection = collection)
    UseMethod(".source_items_bands_select", source)
}

#' @rdname source_cube
#'
#' @description \code{.source_items_fid()} retrieves the feature id of
#' all items.
#'
#' @return \code{.source_items_fid()} returns a \code{character} vector.
#'
.source_items_fid <- function(source, items, ..., collection = NULL) {
    source <- .source_new(source)
    UseMethod(".source_items_fid", source)
}

#' @rdname source_cube
#'
#' @description \code{.source_items_file_info()} creates the \code{fileinfo}
#' specification from items object.
#'
#' @return \code{.source_items_file_info()} returns a \code{tibble} containing
#' sits cube.
#'
.source_items_file_info <- function(source, items, ..., collection = NULL) {
    source <- .source_new(source = source, collection = collection)
    UseMethod(".source_items_file_info", source)
}

#' @rdname source_cube
#'
#' @description \code{.source_items_tile()} organizes items by tiles
#' and arrange items in each tile by date.
#'
#' @return \code{.source_items_tile()} returns a \code{list} of
#' items.
#'
.source_items_tile <- function(source, items, ..., collection = NULL) {
    source <- .source_new(source = source, collection = collection)
    UseMethod(".source_items_tile", source)
}

#' @rdname source_cube
#'
#' @description \code{.source_items_get_sensor()} retrieves the sensor from
#' items object.
#'
#' @return \code{.source_items_get_sensor()} returns a \code{character} value.
#'
.source_collection_sensor <- function(source, collection) {

    res <- .config_get(key = c("sources", source, "collections",
                               collection, "sensor"))

    .check_chr(res, allow_null = TRUE,
               msg = "invalid 'sensor' value")

    return(res)
}

#' @rdname source_cube
#'
#' @description \code{.source_items_get_satellite()} retrieves the satellite
#' name (platform) from items object.
#'
#' @return \code{.source_items_get_satellite()} returns a \code{character}
#' value.
#'
.source_collection_satellite <- function(source, collection) {

    res <- .config_get(key = c("sources", source, "collections",
                               collection, "satellite"))

    .check_chr(res, allow_null = TRUE,
               msg = "invalid 'satellite' value")

    return(res)
}

#' @rdname source_cube
#'
#' @description \code{.source_tile_get_bbox()} retrieves the bounding
#' box from items of a tile.
#'
#' @return \code{.source_tile_get_bbox()} returns a \code{numeric}
#' vector with 4 elements (xmin, ymin, xmax, ymax).
#'
.source_tile_get_bbox <- function(source, ...,
                                  file_info,
                                  collection = NULL) {

    source <- .source_new(source = source, collection = collection)
    UseMethod(".source_tile_get_bbox", source)
}

#' @rdname source_cube
#'
#' @description \code{.source_items_cube()} is called to create a data cubes tile,
#' that is, a row in sits data cube.
#'
#' @return \code{.source_items_cube()} returns a \code{tibble} containing a sits
#' cube tile (one row).
.source_items_cube <- function(source,
                               collection,
                               items, ...) {
    source <- .source_new(source = source, collection = collection)
    UseMethod(".source_items_cube", source)
}
