#' @title Source functions
#' @name source_functions
#' @keywords internal
#' @noRd
#' @description
#' These functions provide an API to handle/retrieve data from sources.
#'
#' @param source     A \code{character} value referring to a valid data source.
#'
#' @return
#' The values returned by each function are described as follows.
NULL

#' @rdname source_functions
#' @noRd
#' @description lists all sources available in sits.
#'
#' @return   all source names available in sits.
.sources <- function() {
    .check_set_caller(".sources")
    src <- names(.conf("sources"))
    # source names are upper case
    src <- toupper(src)
    # post-condition
    .check_chr(src, allow_empty = FALSE, len_min = 1)
    return(src)
}

#' @rdname source_functions
#' @noRd
#' @description Is a source is available in sits?
#' @return  code{NULL} if no error occurs.
#'
.source_check <- function(source) {
    .check_set_caller(".source_check")
    # source is upper case
    source <- toupper(source)
    # check source
    .check_chr(source, len_min = 1, len_max = 1)
    .check_chr_within(source, within = .sources())
    return(invisible(NULL))
}

#' @rdname source_functions
#' @noRd
#' @description creates an object with a corresponding
#' S3 class defined in a given source and collection.
#'
#' @return returns the S3 class for the source
#'
.source_new <- function(source, collection = NULL, is_local = FALSE) {
    # if local, return local cube
    if (is_local) {
        class(source) <- c("local_cube", class(source))
        return(source)
    }
    # source name is upper case
    classes <- .source_s3class(source = toupper(source))
    class(source) <- unique(c(classes, class(source)))

    if (!is.null(collection)) {
        # is this a collection of SAR data?
        sar_cube <- .try({
            .conf("sources", source, "collections", collection, "sar_cube")
            },
            .default = FALSE
        )
        # is this a collection of DEM data ?
        dem_cube <- .try({
            .conf("sources", source, "collections", collection, "dem_cube")
            },
            .default = FALSE
        )
        # if this is a SAR collection, add "sar_cube" to the class
        if (sar_cube)
            class(source) <- c("sar_cube", class(source))
        # if this is a DEM collection, add "dem_cube" to the class
        if (dem_cube)
            class(source) <- c("dem_cube", class(source))
        # add a class combining source and collection
        class_source_col <- paste(classes[[1]], tolower(collection), sep = "_")
        class(source) <- unique(c(class_source_col, class(source)))
    }
    return(source)
}

#' @rdname source_functions
#' @noRd
#' @description  Returns the service associated with a given source.
#' @return service name or
#' \code{NA} if no service is associated with a given source.
.source_service <- function(source) {
    .check_set_caller(".source_service")
    # source is upper case
    source <- toupper(source)
    # pre-condition
    .source_check(source = source)
    # get service name
    service <- .conf("sources", source, "service")
    # post-condition
    .check_chr_parameter(service,
        allow_na = TRUE, allow_empty = FALSE,
        len_min = 1, len_max = 1
    )
    return(service)
}

#' @rdname source_functions
#' @noRd
#' @description Returns the s3 class for a given source.
#' @return a vector of classes.
.source_s3class <- function(source) {
    .check_set_caller(".source_s3class")
    # source is upper case
    source <- toupper(source)
    # pre-condition
    .source_check(source = source)
    # set class
    s3_class <- .conf("sources", source, "s3_class")
    # post-condition
    .check_chr_parameter(s3_class,
        allow_empty = FALSE,
        len_min = 1
    )
    return(s3_class)
}

#' @rdname source_functions
#' @noRd
#' @description get the URL associated with a source.
#' @return a valid URL or  \code{NA}
.source_url <- function(source) {
    .check_set_caller(".source_url")
    # source is upper case
    source <- toupper(source)
    # pre-condition
    .source_check(source = source)
    # get URL
    url <- .conf("sources", source, "url")
    # post-condition
    .check_chr_parameter(url,
        allow_na = FALSE, allow_empty = FALSE,
        len_min = 1, len_max = 1
    )
    return(url)
}

#' @title Source bands functions
#' @name .source_bands
#' @keywords internal
#' @noRd
#' @description
#' These functions provide an API to handle/retrieve data from bands.
#'
#' @param source            Valid data source.
#' @param collection        Image collection.
#' source.
#' @param fn_filter        A \code{function} that will be applied in each band
#' to filter selection. The provided function must have an input parameter to
#' receive band object and return a \code{logical} value.
#' @param add_cloud        Should the cloud band be returned?
#' @param key              Key of a band object.
#' @param bands            Bands to be retrieved
#' @param default          Value to be returned if an attribute or key is not
#'                         found.
#'
#' @return
#' The values returned by each function are described as follows.
NULL

#' @rdname .source_bands
#' @noRd
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
    .check_set_caller(".source_bands")
    # source is upper case
    source <- toupper(source)
    # collection is upper case
    collection <- toupper(collection)
    # pre-condition
    .source_collection_check(source = source, collection = collection)
    # find the bands available in the collection
    bands <- names(.conf("sources", source, "collections", collection, "bands"))
    # bands names are upper case
    bands <- toupper(bands)
    # add the cloud band?
    if (!add_cloud) {
        bands <- bands[bands != .source_cloud()]
    }
    # filter the data?
    if (!is.null(fn_filter)) {
        select <- vapply(bands, function(band) {
            fn_filter(.conf(
                "sources", source,
                "collections", collection,
                "bands", band
            ))
        }, logical(1))
        bands <- bands[select]
    }
    # post-condition
    # check bands are non-NA character
    .check_chr_parameter(bands, allow_empty = FALSE)
    return(bands)
}

#' @rdname .source_bands
#' @noRd
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
    .check_set_caller(".source_bands_reap")
    # source is upper case
    source <- toupper(source)
    # collection is upper case
    collection <- toupper(collection)
    # pre-condition
    .source_collection_check(source = source, collection = collection)
    # get the bands
    if (is.null(bands)) {
        bands <- .source_bands(
            source = source,
            collection = collection,
            fn_filter = fn_filter,
            add_cloud = add_cloud
        )
    }
    # pre-condition
    .check_chr_parameter(bands,
        allow_na = FALSE, allow_empty = FALSE, len_min = 1
    )
    # bands names are upper case
    bands <- toupper(bands)
    # always returns a list!
    result <- lapply(bands, function(band) {
        .try(
            .conf(
                "sources", source,
                "collections", collection,
                "bands", band,
                key
            ),
            .default = default
        )
    })
    names(result) <- bands
    return(result)
}

#' @rdname .source_bands
#' @noRd
#' @description \code{.source_bands_band_name()} returns the \code{band_name}
#' attribute of all bands filtered by its parameters.
#'
#' @return \code{.source_bands_band_name()} returns a \code{character} vector.
.source_bands_band_name <- function(source,
                                    collection, ...,
                                    bands = NULL) {
    .check_set_caller(".source_bands_band_name")
    # source is upper case
    source <- toupper(source)
    # collection is upper case
    collection <- toupper(collection)
    # pre-condition
    .source_collection_check(source = source, collection = collection)
    # get the bands
    bands <- .source_bands_reap(
        source = source,
        collection = collection,
        key = "band_name",
        bands = bands
    )
    # simplify to a unnamed character vector
    bands <- unlist(bands, recursive = FALSE, use.names = FALSE)
    # post-conditions
    .check_chr(bands,
        allow_na = FALSE, allow_empty = FALSE,
        len_min = length(bands), len_max = length(bands)
    )
    return(bands)
}

#' @rdname .source_bands
#' @noRd
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
    .check_set_caller(".source_bands_resolution")
    # source is upper case
    source <- toupper(source)
    # collection is upper case
    collection <- toupper(collection)
    # pre-condition
    .source_collection_check(source = source, collection = collection)
    # get the resolution
    resolution <- .source_bands_reap(
        source = source,
        collection = collection,
        key = "resolution",
        bands = bands,
        fn_filter = fn_filter,
        add_cloud = add_cloud
    )
    # post-condition
    .check_lst_parameter(
        resolution,
        fn_check = .check_num,
        exclusive_min = 0,
        len_min = 1
    )
    return(resolution)
}

#' @rdname .source_bands
#' @noRd
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
    names(bands_to_sits) <- toupper(
        .source_bands_band_name(
            source = source,
            collection = collection
        )
    )
    # are there unknown bands?
    unknown_bands <- setdiff(unique(bands), names(bands_sits))
    names(unknown_bands) <- unknown_bands
    # create a vector with all bands
    bands_converter <- c(bands_to_sits, bands_sits, unknown_bands)
    # post-condition
    .check_chr_within(bands, within = names(bands_converter))
    return(unname(bands_converter[bands]))
}

#' @rdname .source_bands
#' @noRd
#' @description \code{.source_bands_to_source()} converts any bands to its
#' corresponding names indicated in \code{band_name} attribute.
#'
#' @return \code{.source_bands_to_source()} returns a \code{character} vector
#' with all converted bands name.
.source_bands_to_source <- function(source, collection, bands) {
    .check_set_caller(".source_bands_to_source")
    # bands are upper case
    bands <- toupper(bands)
    # bands sits
    bands_source <- .source_bands_band_name(
        source = source,
        collection = collection
    )
    names(bands_source) <- toupper(bands_source)
    # bands source
    bands_to_source <- bands_source
    names(bands_to_source) <- toupper(.source_bands(source, collection))
    # bands converter
    bands_converter <- c(bands_to_source, bands_source)
    # post-condition
    .check_chr_within(bands,
        within = names(bands_converter)
    )
    return(unname(bands_converter[bands]))
}

#' @rdname .source_bands
#' @noRd
#' @description \code{.source_cloud()} lists cloud band for a collection.
#'
#' @return \code{.source_cloud()} returns a \code{character} vector with cloud
#' band name.
.source_cloud <- function() {
    return("CLOUD")
}

#' @rdname .source_bands
#' @noRd
#' @description \code{.source_cloud_bit_mask()} returns the \code{bit_mask}
#' attribute of a cloud band, indicating if the cloud band is a bit mask.
#'
#' @return \code{.source_cloud_bit_mask()} returns a \code{logical} value.
.source_cloud_bit_mask <- function(source,
                                   collection) {
    .check_set_caller(".source_cloud_bit_mask")
    # source is upper case
    source <- toupper(source)
    # collection is upper case
    collection <- toupper(collection)
    # pre-condition
    .source_collection_check(source = source, collection = collection)
    # get the bit mask
    bit_mask <- .conf(
        "sources", source,
        "collections", collection,
        "bands", .source_cloud(),
        "bit_mask"
    )
    # post-condition
    .check_lgl_parameter(bit_mask)
    return(bit_mask)
}

#' @rdname .source_bands
#' @noRd
#' @description \code{.source_cloud_values()} returns the \code{values}
#' attribute of a cloud band.
#'
#' @return \code{.source_cloud_values()} returns a named \code{list} containing
#' all values/or bits description of a cloud band.
.source_cloud_values <- function(source,
                                 collection) {
    .check_set_caller(".source_cloud_values")
    # source is upper case
    source <- toupper(source)
    # collection is upper case
    collection <- toupper(collection)
    # pre-condition
    .source_collection_check(source = source, collection = collection)
    # get values
    cloud_values <- .conf(
        "sources", source,
        "collections", collection,
        "bands", .source_cloud(),
        "values"
    )
    # post-condition
    .check_lst_parameter(cloud_values)
    return(cloud_values)
}

#' @rdname .source_bands
#' @noRd
#' @description \code{.source_cloud_interp_values()} returns the
#' \code{interp_values} attribute of a cloud band, indicating which value/bit
#' must be interpolated (e.g. shadows, clouds).
#'
#' @return \code{.source_cloud_interp_values()} returns a \code{numeric}
#' vector with all values/or bits to be interpolated if found in the cloud band.
.source_cloud_interp_values <- function(source, collection) {
    .check_set_caller(".source_cloud_interp_vales")
    # source is upper case
    source <- toupper(source)
    # collection is upper case
    collection <- toupper(collection)
    # pre-condition
    .source_collection_check(source = source, collection = collection)
    # get values
    cloud_interp_values <- .conf(
        "sources", source,
        "collections", collection,
        "bands", .source_cloud(),
        "interp_values"
    )
    # post-condition
    .check_num_parameter(cloud_interp_values, len_max = Inf)

    return(cloud_interp_values)
}

#' @title Source collection functions
#' @name .source_collection
#' @keywords internal
#' @noRd
#' @description
#' These functions provide an API to handle/retrieve data from source's
#' collections.
#'
#' @param source     Data source.
#' @param collection Image collection.
#' @param tiles      Tile names
#' @param start_date Start date.
#' @param end_date   End date.
#' @param token      Requires token (TRUE/FALSE)
#'
#' @return
#' The values returned by each function are described as follows.
NULL

#' @rdname .source_collection
#' @noRd
#' @description \code{.source_collections()} lists all collections of a source.
#'
#' @return \code{.source_collections()} returns a \code{character} vector
#' with all collection names of a given source.
.source_collections <- function(source, ...) {
    # source is upper case
    source <- toupper(source)
    # check source
    .source_check(source = source)
    # get collections from source
    collections <- .conf_names("sources", source, "collections")
    return(collections)
}

#' @rdname .source_collection
#' @noRd
.source_collection_access_test <- function(source, collection, ...) {
    .check_set_caller(".source_collection_access_test")
    source <- .source_new(source, collection = collection)

    UseMethod(".source_collection_access_test", source)
}

#' @rdname .source_collection
#' @noRd
#' @description \code{.source_collection_access_vars_set} sets
#' \code{access_vars} environment variables.
#'
#' @return \code{.source_collection_access_vars_set } returns \code{NULL} if
#' no error occurs.
.source_collection_access_vars_set <- function(source,
                                               collection) {
    .check_set_caller(".source_collection_access_var_set")
    # source is upper case
    source <- toupper(source)
    # collection is upper case
    collection <- toupper(collection)
    # get access variables for this source/collection
    vars <- .try(
        .conf(
            "sources", source,
            "collections", collection,
            "access_vars"
        ),
        .default = list()
    )
    # post-condition
    .check_lst(vars)
    if (length(vars) > 0) {
        do.call(Sys.setenv, args = vars)
    }
    return(invisible(vars))
}

#' @rdname .source_collection
#' @noRd
#' @description \code{.source_collection_check()} checks if a collection
#' is from a source.
#'
#' @return \code{.source_collection_check()} returns \code{NULL} if
#' no error occurs.
.source_collection_check <- function(source,
                                     collection) {
    # set calller for error msg
    .check_set_caller(".source_collection_check")
    # check collection
    .check_chr_parameter(collection, len_min = 1, len_max = 1)
    .check_chr_within(collection,
        within = .source_collections(source = source)
    )
    return(invisible(NULL))
}

#' @rdname source_collection
#' @noRd
#' @description \code{.source_collection_metadata_search()} retrieves the
#' metadata search strategy for a given source and collection.
#'
#' @return \code{.source_collection_metadata_search()} returns a character
#' value with the metadata search strategy.
.source_collection_metadata_search <- function(source, collection) {
    .check_set_caller(".source_collection_metadata_search")
    # try to find the gdalcubes configuration
    metadata_search <- .try(
        .conf(
            "sources", source,
            "collections", collection,
            "metadata_search"
        ),
        .default = NA
    )
    # if the collection cant be supported report error
    .check_that(!is.na(metadata_search))
    return(invisible(metadata_search))
}

#' @rdname .source_collection
#' @noRd
#' @description \code{.source_collection_name()} returns the name of a
#' collection in its original source.
#'
#' @return \code{.source_collection_name()} returns a \code{character}.
#'
.source_collection_name <- function(source,
                                    collection) {
    .check_set_caller(".source_collection_name")
    # source is upper case
    source <- toupper(source)
    # collection is upper case
    collection <- toupper(collection)
    # pre-condition
    .source_collection_check(
        source = source,
        collection = collection
    )
    collection_name <- .conf(
        "sources", source,
        "collections", collection,
        "collection_name"
    )
    # post-condition
    .check_chr_parameter(collection_name,
        allow_empty = FALSE, len_min = 1, len_max = 1
    )
    return(collection_name)
}

#' @rdname .source_collection
#' @noRd
#' @description \code{.source_collection_open_data()} informs if a
#' collection is open data or not. If token is TRUE, informs if a
#' collection requires a token to access.
#'
#' @return \code{.source_collection_open_data()} returns a \code{logical}.
#'
.source_collection_open_data <- function(source,
                                         collection,
                                         token = FALSE) {
    .check_set_caller(".source_collection_open_data")
    # source is upper case
    source <- toupper(source)
    # collection is upper case
    collection <- toupper(collection)
    # pre-condition
    .source_collection_check(
        source = source,
        collection = collection
    )
    if (token) {
        res <- .try(
            .conf(
                "sources", source,
                "collections", collection,
                "open_data_token"
            ),
            .default = FALSE
        )
    } else {
        res <- .try(
            .conf(
                "sources", source,
                "collections", collection,
                "open_data"
            ),
            .default = FALSE
        )
    }
    # post-condition
    .check_lgl_parameter(res)
    return(res)
}
#' @rdname .source_collection
#' @noRd
#' @description \code{.source_collection_token_check()} checks if a collection
#' needs environmental variables.
#'
#' @return \code{.source_collection_token_check()} returns \code{NULL} if
#' no error occurs.
#'
.source_collection_token_check <- function(source, collection) {
    .check_set_caller(".source_collection_token_check")
    token <- .try(
        .conf(
            "sources", source,
            "collections", collection,
            "token_vars"
        ),
        .default = "NO_TOKEN"
    )
    # Pre-condition - try to find the access key as an environment variable
    if (token != "NO_TOKEN")
        .check_env_var(token)

    return(invisible(TRUE))
}

#' @rdname .source_collection
#' @noRd
#' @description \code{.source_collection_tile_check()} checks if a collection
#' requires tiles to be defined
#'
#' @return \code{.source_collection_tile_check()} returns \code{NULL} if
#' no error occurs.
#'
.source_collection_tile_check <- function(source, collection, tiles) {
    .check_set_caller(".source_collection_tile_check")
    res <- .try(
        .conf(
            "sources", source,
            "collections", collection,
            "tile_required"
        ),
        .default = "false"
    )
    if (res) {
        # Are the tiles provided?
        .check_chr_parameter(
            x = tiles,
            allow_empty = FALSE,
            len_min = 1
        )
    }
    return(invisible(NULL))
}

#' @title Functions to instantiate a new cube from a source
#' @name .source_cube
#' @keywords internal
#' @noRd
#' @description
#' These functions provide an API to instantiate a new cube object and
#' access/retrieve information from services or local files to fill
#' cube attributes.
#'
#' A cube is formed by images (items) organized in tiles. To create a sits
#' cube object (a \code{tibble}), a set of functions are called in order
#' to retrieve metadata.
#'
#' @param source     Data source.
#' @param ...        Additional parameters.
#' @param items      Images that compose a cube.
#' @param asset      A \code{raster} object to retrieve information.
#' @param collection Image collection.
#' @param data_dir   Directory where local files are stored
#' @param file_info  A \code{tibble} that organizes the metadata about each
#' file in the tile: date, band, resolution, and path (or URL).
#' @param bands      Bands to be selected in the collection.
#' @param progress   Show a progress bar?
#'
#' @return
#' The values returned by each function are described as follows.
NULL

#' @rdname .source_cube
#' @noRd
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

#' @rdname .source_cube
#' @noRd
#' @description \code{.source_item_get_date()} retrieves the date of an item
#' (a set of images from different bands that forms a scene).
#'
#' @return \code{.source_item_get_date()} returns a \code{Date} value.
#'
.source_item_get_date <- function(source, item, ..., collection = NULL) {
    source <- .source_new(source, collection)
    UseMethod(".source_item_get_date", source)
}

#' @rdname .source_cube
#' @noRd
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

#' @rdname .source_cube
#' @noRd
#' @description \code{.source_item_get_cloud_cover()} retrieves the percentage
#' of cloud cover of an image.
#' @return \code{.source_item_get_cloud_cover()} returns a \code{numeric} vector
#' containing the percentage of cloud cover to each image band of an item.
#'
.source_item_get_cloud_cover <- function(source, ..., item, collection = NULL) {
    source <- .source_new(source)
    UseMethod(".source_item_get_cloud_cover", source)
}

#' @rdname .source_cube
#' @noRd
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

#' @rdname .source_cube
#' @noRd
#' @description \code{.source_items_new()} this function is called to create
#' an items object. In case of Web services, this function is responsible for
#' making the Web requests to the server.
#'
#' @return \code{.source_items_new()} returns any object referring the images
#' of a sits cube.
#'
.source_items_new <- function(source, ..., collection = NULL) {
    .check_set_caller(".source_items_new")
    source <- .source_new(source = source, collection = collection)
    UseMethod(".source_items_new", source)
}

#' @rdname .source_cube
#' @noRd
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
    .check_set_caller(".source_items_bands_select")
    source <- .source_new(source = source, collection = collection)
    UseMethod(".source_items_bands_select", source)
}

#' @rdname .source_cube
#' @noRd
#' @description \code{.source_items_fid()} retrieves the feature id of
#' all items.
#'
#' @return \code{.source_items_fid()} returns a \code{character} vector.
#'
.source_items_fid <- function(source, items, ..., collection = NULL) {
    source <- .source_new(source)
    UseMethod(".source_items_fid", source)
}

#' @rdname .source_cube
#' @noRd
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

#' @rdname .source_cube
#' @noRd
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

#' @rdname .source_cube
#' @noRd
#' @description \code{.source_items_get_sensor()} retrieves the sensor from
#' items object.
#'
#' @return \code{.source_items_get_sensor()} returns a \code{character} value.
#'
.source_collection_sensor <- function(source, collection) {
    .check_set_caller(".source_collection_sensor")
    sensor <- .conf(
        "sources", source,
        "collections", collection,
        "sensor"
    )
    .check_chr_parameter(sensor, allow_null = TRUE)
    return(sensor)
}

#' @rdname .source_cube
#' @noRd
#' @description \code{.source_items_get_satellite()} retrieves the satellite
#' name (platform) from items object.
#'
#' @return \code{.source_items_get_satellite()} returns a \code{character}
#' value.
#'
.source_collection_satellite <- function(source, collection) {
    .check_set_caller(".source_collection_satellite")
    satellite <- .conf(
        "sources", source,
        "collections", collection,
        "satellite"
    )
    .check_chr_parameter(satellite, allow_null = TRUE)
    return(satellite)
}
#' @rdname .source_cube
#' @noRd
#' @description \code{.source_collection_grid_system()} retrieves the
#' cartographical grid system for the collection.
#'
#' @return \code{character}
#'
.source_collection_grid_system <- function(source, collection) {
    .check_set_caller(".source_collection_grid_system")
    grid_system <- .conf(
        "sources", source,
        "collections", collection,
        "grid_system"
    )
    .check_chr(grid_system, allow_null = TRUE)
    return(grid_system)
}

#' @rdname .source_cube
#' @noRd
#' @description \code{.source_tile_get_bbox()} retrieves the bounding
#' box from items of a tile.
#'
#' @return \code{.source_tile_get_bbox()} returns a \code{numeric}
#' vector with 4 elements (xmin, ymin, xmax, ymax).
#'
.source_tile_get_bbox <- function(source, ...,
                                  file_info,
                                  collection = NULL) {
    .check_set_caller(".source_tile_get_bbox")
    source <- .source_new(source = source, collection = collection)
    UseMethod(".source_tile_get_bbox", source)
}

#' @rdname source_cube
#' @noRd
#' @description \code{.source_items_cube()} is called to create a data cubes
#' tile, that is, a row in sits data cube.
#'
#' @return \code{.source_items_cube()} returns a \code{tibble} containing a sits
#' cube tile (one row).
.source_items_cube <- function(source,
                               collection,
                               items, ...) {
    .check_set_caller(".source_items_cube")
    source <- .source_new(source = source, collection = collection)
    UseMethod(".source_items_cube", source)
}
#' @rdname source_cube
#' @noRd
#' @description Configure access.
#' @param source  Data source
#' @param collection Image collection
#' @return Called for side effects
.source_configure_access <- function(source, collection = NULL) {
    source <- .source_new(source = source)
    UseMethod(".source_configure_access", source)
}
#' @title Adjusts date-time if required by source
#' @noRd
#' @param source  Data source
#' @param date    Date to be adjusted
#' @return Adjusted date
.source_adjust_date <- function(source, date) {
    source <- .source_new(source = source)
    UseMethod(".source_adjust_date", source)
}
#' @title Filter tiles if required by source
#' @noRd
#' @param source  Data source
#' @param cube    Cube to be filtered
#' @param tiles   Tiles to be selected
#' @return Filtered cube
.source_filter_tiles <- function(source, collection, cube, tiles) {
    source <- .source_new(source = source, collection = collection)
    UseMethod(".source_filter_tiles", source)
}
#' @title Check if roi or tiles are provided
#' @name .source_roi_tiles
#' @param source        Data source
#' @param roi           Region of interest
#' @param tiles         Tiles to be included in cube
#' @return Called for side effects.
#' @keywords internal
#' @noRd
.source_roi_tiles <- function(source, roi, tiles) {
    source <- .source_new(source = source)
    .check_roi_tiles(roi, tiles)
    UseMethod(".source_roi_tiles", source)
}
