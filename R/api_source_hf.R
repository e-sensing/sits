# ---- hf utilities ----
#' @title Verify if a given source refers to HuggingFace.
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description This functions verifies if a source is HuggingFace. This is
#' required as there is no HF source registered. This is done by checking
#' if the prefix we defined for HF (e.g., `HF`) is available in the source name.
#'
#' @param source  Data source name.
#'
#' @return TRUE if the source refers to a HuggingFace dataset and FALSE for
#' invalid sources.
.hf_is_source <- function(source) {
    # source name is upper case
    source <- toupper(source)
    # is there any HuggingFace prefix in the source user is exploring ?
    isTRUE(startsWith(source, .conf("hf", "source_prefix")))
}

#' @title Convert a HuggingFace name to a common valid name.
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description Users definining their \code{"sits.yaml"}, can use any name
#' for satellite / sensor. As we doesn't have control over all config files
#' created out there, we assume a conservative position: we convert the names
#' of the elements from HuggingFace by replacing any invalid character
#' with a common and accepted separator (e.g., \code{"-"}) and making them
#' uppercase.
#'
#' @param name HuggingFace name.
#'
#' @return Converted names
.hf_name <- function(name) {
    gsub(
        pattern = .conf("hf", "name_invalid_chars"),
        replacement = .conf("hf", "name_separator"),
        x = toupper(name)
    )
}

#' @title Get the HuggingFace user of a source
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description To identify sources from HuggingFace, we add for all them a
#' prefix. To be able to use them, this function process the source name by
#' removing this prefix.
#'
#' @param source  Data source (\code{"HF:<user>"}).
#'
#' @return HuggingFace user name.
.hf_user <- function(source) {
    sub(.conf("hf", "source_prefix"), "", toupper(source), fixed = TRUE)
}

#' @title Find the HuggingFace repository of a collection
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description Users are able to define any user / repository as source in HF
#' implementation. To avoid us get any weird error when. this repository is not
#' available, this function search in HF API for the given user / repository
#' specified. If it is not available, we raise a proper error. Otherwise, we
#' just continue.
#'
#' Also, as we are using the HF Rest API as the source of truth, we are able
#' to manage any source name (e.g., case variations). This is helpful for users.
#'
#' @param source Data source (\code{"HF:<user>"}).
#' @param collection Image collection.
#'
#' @return HuggingFace repository name (\code{"<user>/<repository>"}).
.hf_repo <- function(source, collection) {
    .check_set_caller(".hf_repo")
    # get the user name
    user <- .hf_user(source)
    # search the datasets of the user
    # > please not that, this search is case insensitive, which is not
    # > the case of the user name in the repository
    datasets <- .try(
        {
            .response_content(
                .get_request(
                    url = .conf("hf", "api_url"),
                    query = list(
                        search = paste0(tolower(user), "/"),
                        limit = .conf("hf", "search_limit")
                    )
                )
            )
        },
        .default = NULL
    )
    # get repositories IDs
    repos <- purrr::map_chr(datasets, "id")
    # keep the datasets of the user (the search matches any part of names)
    repos <- repos[toupper(dirname(repos)) == user]
    # select the dataset named as the collection
    repo <- repos[toupper(basename(repos)) == toupper(collection)]
    # post-condition: exactly one dataset must match
    .check_that(length(repo) == 1L)
    # return!
    repo
}

#' @title Build the URL of a file stored in a HuggingFace repository
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description Prepare URL for the "resolve" endpoint, which allows access with
#' yaml/terra/gdal/qgis or any other tools depending on the file type.
#'
#' @param repo HuggingFace repository name.
#' @param file File name in the repository.
#'
#' @return URL of the file.
.hf_file_url <- function(repo, file) {
    paste(.conf("hf", "url"), repo, .conf("hf", "file_path"), file, sep = "/")
}

#' @title Retrieve the metadata of a HuggingFace dataset
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description Fetch dataset metadata from the HuggingFace API. The API
#' returns a complete description of the dataset, including their files.
#'
#' @param repo HuggingFace repository name.
#'
#' @return A list with the dataset metadata.
.hf_dataset <- function(repo) {
    # set caller
    .check_set_caller(".hf_dataset")
    # request the dataset metadata
    dataset <- .try(
        {
            .response_content(
                .get_request(paste(.conf("hf", "api_url"), repo, sep = "/"))
            )
        },
        .default = NULL
    )
    # is the dataset available?
    .check_that(.has(dataset[["id"]]))
    # return metadata!
    dataset
}

#' @title Read the collection definition of a HuggingFace dataset
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description As a convention adopted in \code{sits} to facilitate the usage
#' of HF, it is expected that the provider of the dataset shares also a config
#' file (.e.g, \code{"sits.yaml"}) with the specifications of the collection
#' available in the repository. This avoid users to manually create config
#' files. This function tries to read a config file from a given repositroy.
#'
#' @param repo HuggingFace repository name.
#'
#' @return A list with the collection definition.
.hf_collection_conf <- function(repo) {
    # set caller
    .check_set_caller(".hf_collection_conf")
    # read the collection definition from the repositroy
    collection <- .try(
        {
            suppressWarnings(
                yaml::yaml.load_file(
                    input = .hf_file_url(repo, .conf("hf", "config_file")),
                    readLines.warn = FALSE
                )
            )
        },
        .default = NULL
    )
    # check if the dataset config exists
    .check_that(.has(collection))
    # return!
    collection
}

#' @title Register a HuggingFace dataset as a sits collection
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description Reads the collection definition of a HF dataset and registers it
#' in the current \code{sits} session. This allow users to consume the dataset
#' as any other data source in \code{sits}.
#'
#' This operation is intended to be executed by \code{sits_cube()}, and also
#' when the source of a cube is first used (e.g., \code{.tile_source}) for
#' cases when user save the HF cube in a RDS.
#'
#' @param source     Data source (\code{"HF:<user>"}).
#' @param collection Image collection (repository name).
#'
#' @return Called for side effects.
.hf_source_register <- function(source, collection) {
    # sits names are upper case
    source <- toupper(source)
    collection <- toupper(collection)
    # if the collection is already registered, skip it
    if (.conf_exists("sources", source, "collections", collection)) {
        return(invisible(source))
    }
    # get the repository of the collection
    repo <- .hf_repo(source, collection)
    # read the collection definition of the dataset
    collection_conf <- .hf_collection_conf(repo)
    # get the collection defaults
    collection_defaults <- .conf("hf", "collection_defaults")
    # if it is a results cube, we assume the default sits parse info
    if (.hf_collection_is_results(collection_conf)) {
        collection_defaults[["parse_info"]] <- .conf("results_parse_info_def")
    }
    # complete the collection definition with the sits defaults
    collection_conf <- utils::modifyList(collection_defaults, collection_conf)
    # the collection name is the repository defined by user
    collection_conf[["collection_name"]] <- repo
    # for all cube types, we require `bands` as we can't guess them. the only
    # exception we are assuming for this first version is the embeddings cube.
    # As we are generating them, and they may produce many bands, for user
    # convenience, we derive embeddings bands. We assume we are dealing with
    # embeddings cube when no band is specified
    if (.has_not(collection_conf[["bands"]])) {
        collection_conf <- .hf_collection_embeddings(collection_conf)
    }
    # of course, to avoid any mismatch of definition, with the strong assumption
    # above, we check the type of collection. For this first version, we are
    # using a very strict definition and only accepting cubes that are aligned
    # with our definitions cubes (i.e., raster cube, results cube and friends)
    .hf_collection_check(collection_conf)
    # if collection is a results cube
    if (.hf_collection_is_results(collection_conf)) {
        # we ensure the bands definition are aligned with
        # what we expect in sits
        collection_conf <- .hf_collection_results(collection_conf)
    }
    # convert satellite and sensor names and use them as part of the
    # collection configuration
    collection_conf[["satellite"]] <- .hf_name(collection_conf[["satellite"]])
    collection_conf[["sensor"]] <- .hf_name(collection_conf[["sensor"]])
    # we are ready to go! so, let's add the new collection config we built
    # in the sits collections list
    .conf_add_source(
        source = source,
        source_conf = list(
            s3_class = .conf("hf", "s3_class"),
            url = .conf("hf", "url"),
            collections = stats::setNames(list(collection_conf), collection)
        )
    )
    # return!
    invisible(source)
}

#' @title Check a HuggingFace collection definition
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description Datasets can be shared as data cubes of images (e.g., optical,
#' SAR), embeddings, classified maps, or results produced by sits (e.g.,
#' probabilities). This function ensures that the collection definition will
#' allows sits to identify the right type of the cube:
#'
#' \itemize{
#'   \item{classified maps not produced by sits are declared as
#'   \code{class_cube: true}, with a single band named "class";}
#'
#'   \item{results produced by sits (bands named as sits results, e.g.,
#'   "probs", "class", "entropy") can't be mixed with other bands, and must
#'   be results that sits defines;}
#'
#'   \item{embeddings (bands named "EMB<n>") are read using the sits
#'   definition of embeddings, so a dataset that describes them must follow it.}
#' }
#'
#' @param collection_conf  Collection definition.
#'
#' @return Called for side effects.
.hf_collection_check <- function(collection_conf) {
    # set caller
    .check_set_caller(".hf_collection_check")
    # a collection must have a `satellite` and `sensor` defined
    .check_that(
        .has(
            collection_conf[["satellite"]]
        ) &&
        .has(
            collection_conf[["sensor"]]
        ),
        msg = .conf("messages", ".hf_collection_check_names")
    )
    # get band names
    bands <- tolower(names(collection_conf[["bands"]]))
    # classified maps that are not produced by sits
    if (isTRUE(collection_conf[["class_cube"]])) {
        .check_that(
            all(bands == "class"),
            msg = .conf("messages", ".hf_collection_check_class")
        )
        # nothing more is needed, just return!
        return(invisible(collection_conf))
    }
    # check if bands are somehow associated with results from sits
    if (any(bands %in% .conf("sits_results_bands"))) {
        # check results bands
        .check_that(
            .hf_collection_is_results(collection_conf),
            msg = .conf("messages", ".hf_collection_check_results")
        )
        # verify if configuration exists for every band
        are_all_valid <- purrr::every(bands, function(band) {
            .conf_exists(
                "derived_cube", .conf("sits_results_s3_class")[[band]],
                "bands", band
            )
        })
        # check if all are true
        .check_that(are_all_valid,
            msg = .conf("messages", ".hf_collection_check_derived")
        )
        # return!
        return(invisible(collection_conf))
    }
    # embeddings must follow the sits definition
    if (.hf_collection_is_embeddings(collection_conf)) {
        # get embedding values type
        emb_conf <- .conf("embedding_values", "INT2S")
        # settings that decode the values stored in the images
        emb_keys <- c(
            "data_type", "missing_value", "scale_factor", "offset_value"
        )
        # verify if bands definitions are aligned with sits
        all_valid <- purrr::every(collection_conf[["bands"]], function(band) {
            identical(
                purrr::map(band[emb_keys], as.character),
                purrr::map(emb_conf[emb_keys], as.character)
            )
        })
        # check if all are valid
        .check_that(
            all_valid,
            msg = .conf("messages", ".hf_collection_check_embeddings")
        )
    }
    # return!
    invisible(collection_conf)
}

#' @title Verify if a collection is an embeddings cube
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description This function verifies if a given collection from HF is storing
#' an embeddings cube.
#'
#' @param collection_conf Collection definition.
#'
#' @return TRUE if the collection holds embeddings.
.hf_collection_is_embeddings <- function(collection_conf) {
    # get collection bands
    bands <- toupper(names(collection_conf[["bands"]]))
    # get embeddings bands regex
    emb_regex <- paste0("^", .conf("embedding_band_prefix"), "[0-9]+$")
    # verification - has bands available
    is_valid <- .has(bands)
    # verification - is not a class cube
    is_valid <- is_valid && !isTRUE(collection_conf[["class_cube"]])
    # verification - all bands follows the embeddings regex
    is_valid <- is_valid && all(grepl(emb_regex, bands))
    # return!
    is_valid
}

#' @title Prepare embeddings collection using a HuggingFace dataset config
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description sits defined how embeddings are stored and reads them that way,
#' so a dataset of embeddings doesn't have to describe its bands. This function
#'
#' @param collection_conf Collection definition.
#'
#' @return Collection definition with the bands of the dataset.
.hf_collection_embeddings <- function(collection_conf) {
    # set caller
    .check_set_caller(".hf_collection_embeddings")
    # get collection files
    files <- .hf_files(collection_conf[["collection_name"]])
    # update files URLs
    files <- .stac_add_gdal_fs(files)
    # find the cube images of the dataset
    items <- .hf_items_parse(
        files = files,
        parse_info = collection_conf[["parse_info"]],
        delim = collection_conf[["delim"]]
    )
    # bands are named in upper case
    bands <- sort(unique(toupper(items[["band"]])))
    # as we are going to prepare embeddings bands, all files must follow
    # the embeddings regex from sits
    emb_regex <- paste0("^", .conf("embedding_band_prefix"), "[0-9]+$")
    emb_regex_valid <- grepl(emb_regex, bands)
    # check all regex are valid embeddings
    .check_that(all(emb_regex_valid))
    # every embedding is stored the same way, at the resolution of the images
    emb_conf <- c(
        .conf("embedding_values", "INT2S"),
        resolution = .raster_xres(.raster_open_rast(items[["path"]][[1L]]))
    )
    # update collection with the band configuration for embeddings
    collection_conf[["bands"]] <- purrr::map(
        stats::setNames(bands, bands), function(band) {
            c(emb_conf, band_name = band)
        }
    )
    # return!
    collection_conf
}

#' @title Verify if a collection is an embeddings cube
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description This function verifies if a given collection from HF is storing
#' any results files from \code{sits}.
#'
#' @param collection_conf  Collection definition.
#'
#' @return TRUE if the collection holds results produced by sits.
.hf_collection_is_results <- function(collection_conf) {
    # bands are named in upper case
    bands <- tolower(names(collection_conf[["bands"]]))
    # verification - has bands available
    is_valid <- .has(bands)
    # verification - is not a class cube
    # > please not here there is a nuance. I'm assuming that, a collection
    # > defining a `class_cube: true` is providing a product that were not
    # > produced by \code{sits}. The idea was divide what we produce and
    # > what user can load from other sources. We don't want to assume
    # > any behavior in such important case.
    is_valid <- is_valid && !isTRUE(collection_conf[["class_cube"]])
    # verification - all bands are results bands
    is_valid <- is_valid && all(bands %in% .conf("sits_results_bands"))
    # return!
    is_valid
}

#' @title Prepare collection using sits results from HuggingFace dataset config
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description This functions ensures a given collection assumed as results
#' cube is valid and ready to be used in \code{sits}.
#'
#' @param collection_conf Collection definition.
#'
#' @return Collection definition with the bands completed.
.hf_collection_results <- function(collection_conf) {
    # update band names
    collection_conf[["bands"]] <- purrr::imap(
        collection_conf[["bands"]], function(band_conf, band) {
            # update band name
            band <- tolower(band)
            # get collection class
            derived_class <- .conf("sits_results_s3_class")[[band]]
            derived_band <- .conf_derived_band(derived_class, band)
            # define band config
            band_config <- .default(band_conf, list())
            # update band data class
            utils::modifyList(
                band_config,
                c(derived_band, band_name = band)
            )
        }
    )
    # return!
    collection_conf
}

#' @title List the image files of a HuggingFace dataset
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description The HuggingFace API lists all files of the repository in a
#' single request. Hidden files and files that are not images are ignored.
#'
#' @param repo  HuggingFace repository name.
#'
#' @return URLs of the image files.
.hf_files <- function(repo) {
    # set caller
    .check_set_caller(".hf_files")
    # get repository metadata
    repo_metadata <- .hf_dataset(repo)
    # list the files of the dataset
    files <- purrr::map_chr(repo_metadata[["siblings"]], "rfilename")
    # select the image files (hidden files are not considered)
    file_ext <- paste(.conf("local_file_extensions"), collapse = "|")
    # select only non-hidden files in the valid extension
    # > this fixes the issue of files with `.` produced by some
    # > disks drivers
    files <- files[
        grepl(paste0("^[^.].*[.](", file_ext, ")$"), basename(files))
    ]
    # post-condition - files must exist
    .check_that(.has(files))
    # resolve HF file URLs
    .hf_file_url(repo, files)
}

#' @title Parse the names of the files of a HuggingFace dataset
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description HuggingFace has no image search service: as done for local
#' data cubes, the metadata of an image (e.g., tile, band, date) is taken
#' from its name, as described by the dataset (\code{parse_info}).
#'
#' @param files URLs of the image files.
#' @param parse_info Parsing information of the collection.
#' @param delim Delimiter of the file names.
#'
#' @return A tibble with the fields parsed from the name of each file.
.hf_items_parse <- function(files, parse_info, delim) {
    # set caller
    .check_set_caller(".hf_items_parse")
    # split the names of the files
    files_fields <- strsplit(.file_sans_ext(files), split = delim, fixed = TRUE)
    # map files that are valid
    # (i.e., the parse info got the right number of entities from the file name)
    files_valid <- purrr::map_lgl(files_fields, function(file_fields) {
        length(file_fields) == length(parse_info)
    })
    # post-condition - we must have at least one valid file
    .check_that(any(files_valid))
    # prepare file items - they have one column per field + file itself
    items <- do.call(rbind, files_fields[files_valid])
    # update column names
    colnames(items) <- parse_info
    # transform the resulting items in a valid tibble
    items <- suppressMessages(
        tibble::as_tibble(items, .name_repair = "universal")
    )
    # include files path as last column
    items[["path"]] <- files[files_valid]
    # return!
    items
}

#' @title Select the tiles requested by users
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description HuggingFace has no tile search service: tiles are selected
#' after the images of the dataset are found by their names.
#'
#' @param items Images of the dataset.
#' @param tiles Selected tiles.
#' @param check_tiles Must all tiles be in the dataset?
#'
#' @return Images of the selected tiles.
.hf_items_tiles_select <- function(items, tiles, check_tiles) {
    # set caller
    .check_set_caller(".hf_items_tiles_select")
    # tiles informed by users must be in the dataset
    if (check_tiles) {
        .check_chr_within(tiles, within = unique(items[["tile"]]))
    }
    # get only files in the tile indicated by user
    dplyr::filter(items, .data[["tile"]] %in% !!tiles)
}

#' @title Select the tiles of a HuggingFace dataset that intersect a roi
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description When the collection uses a tiling system known in \code{sits}
#' (e.g., "MGRS"), the roi is converted to tiles, so that only the images of
#' these tiles are opened. Otherwise, all tiles are used, and the cube is
#' filtered by the roi once created.
#'
#' @param source     Data source.
#' @param collection Image collection.
#' @param roi        Region of interest.
#'
#' @return Tiles that intersect the roi, or NULL to use all tiles.
.hf_roi_tiles <- function(source, collection, roi) {
    # if there is no roi available, we just skip this operation
    if (.has_not(roi)) {
        return(NULL)
    }
    # get the tiling system of the collection
    grid_system <- .source_collection_grid_system(source, collection)
    # verify if grid system if known in sits
    is_grid_system_valid <- .conf_exists("grid_systems", grid_system)
    # if grid system is not known in sits, skip operation
    if (!is_grid_system_valid) {
        # return
        return(NULL)
    }
    # otherwise, select the tiles that intersect the roi
    tiles <- .grid_filter_tiles(
        grid_system = grid_system,
        roi = roi,
        tiles = NULL
    )
    # return tile names!
    tiles[["tile_id"]]
}

#' @title Retrieve the images of a HuggingFace dataset
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description HuggingFace has no image search service: the images of the
#' repository are selected by name, as done for local data cubes. The result
#' mimics the items object of a STAC service, so that the standard cube
#' creation can be reused.
#'
#' @param source      Data source.
#' @param collection  Image collection.
#' @param tiles       Selected tiles (optional).
#' @param check_tiles Must all tiles be in the dataset?
#' @param start_date  Start date.
#' @param end_date    End date.
#'
#' @return An items object referring to the images of a sits cube.
.hf_items <- function(source, collection, tiles, check_tiles,
                      start_date, end_date) {
    # set caller
    .check_set_caller(".hf_items")
    # get the definition of the collection
    collection_conf <- .conf("sources", source, "collections", collection)
    # get source/collection files
    files <- .hf_files(.source_collection_name(source, collection))
    # parse files as items
    items <- .hf_items_parse(
        files = files,
        parse_info = collection_conf[["parse_info"]],
        delim = collection_conf[["delim"]]
    )
    # prepare extra columns and remove duplicates
    items <- items |>
        # bands are case insensitive (converted to upper case)
        dplyr::mutate(band = toupper(.data[["band"]])) |>
        # transform the date format
        dplyr::mutate(date = .timeline_format(.data[["date"]])) |>
        # select the relevant parts
        dplyr::select("tile", "date", "band", "path") |>
        # filter to remove duplicate combinations of file and band
        dplyr::distinct(
            .data[["tile"]],
            .data[["date"]],
            .data[["band"]],
            .keep_all = TRUE
        ) |>
        # order by dates
        dplyr::arrange(.data[["date"]], .data[["band"]])
    # filter start date
    if (.has(start_date)) {
        items <- dplyr::filter(items, .data[["date"]] >= !!start_date)
    }
    # filter end date
    if (.has(end_date)) {
        items <- dplyr::filter(items, .data[["date"]] <= !!end_date)
    }
    # select the tiles
    if (.has(tiles)) {
        items <- .hf_items_tiles_select(items, tiles, check_tiles)
    }
    # post-condition - we must have at least on item
    .check_that(nrow(items) > 0L)
    # build one item per tile and date, with one asset per band
    features <- items |>
        dplyr::group_by(.data[["tile"]], .data[["date"]]) |>
        dplyr::group_map(function(item, tile_date) {
            .hf_item(
                tile = tile_date[["tile"]],
                date = tile_date[["date"]],
                bands = item[["band"]],
                hrefs = item[["path"]]
            )
        })
    # transform features and return!
    .hf_items_new(features)
}

#' @title Create an item from the images of a tile in a date
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description Mimics an item of a STAC service.
#'
#' @param tile Tile name.
#' @param date Date of the images.
#' @param bands Band names.
#' @param hrefs URL of the image of each band.
#'
#' @return An item object.
.hf_item <- function(tile, date, bands, hrefs) {
    structure(
        list(
            type = "Feature",
            id = paste(tile, date, sep = "_"),
            geometry = NULL,
            properties = list(
                datetime = as.character(date),
                tile = tile
            ),
            assets = purrr::map(stats::setNames(hrefs, bands), function(href) {
                list(href = href)
            })
        ),
        class = c(
            "doc_item",
            "rstac_doc",
            "list"
        )
    )
}

#' @title Create an items object from a set of items
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description Mimics the items object of a STAC service.
#'
#' @param features List of items.
#'
#' @return An items object.
.hf_items_new <- function(features) {
    structure(
        list(
            type = "FeatureCollection",
            features = features
        ),
        class = c(
            "doc_items",
            "rstac_doc",
            "list"
        )
    )
}

# ---- source api ----
#' @title Retrieve results files available in HuggingFace dataset
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description This function gets results file available in a given HuggingFace
#' dataset and transform it in a valid file info object.
#'
#' @param source Data source.
#' @param collection Image collection.
#' @param band Band of the results.
#' @param version Version of the results.
#' @param tiles Selected tiles (optional).
#' @param check_tiles Must all tiles be in the dataset?
#'
#' @return A tibble with the results of the dataset.
.hf_source_results_items <- function(source, collection, band, version,
                              tiles, check_tiles) {
    # set caller
    .check_set_caller(".hf_results_items")
    # get the definition of the collection
    collection_conf <- .conf("sources", source, "collections", collection)
    # get collection files
    source_files <- .hf_files(.source_collection_name(source, collection))
    # update files URLs
    source_files <- .stac_add_gdal_fs(source_files)
    # get parse info
    source_parse_info <- .conf_parse_info(
        collection_conf[["parse_info"]],
        results_cube = TRUE
    )
    # get delim
    source_delim <- collection_conf[["delim"]]
    # select the images by their names
    items <- .hf_items_parse(
        files = source_files,
        parse_info = source_parse_info,
        delim = source_delim
    )
    # check required version exists
    .check_chr_within(
        x = version,
        within = items[["version"]],
        discriminator = "any_of",
        msg = .conf("messages", ".hf_results_version")
    )
    # prepare extra columns and remove duplicates
    items <- items |>
        # bands are case insensitive (converted to lower case)
        dplyr::mutate(band = tolower(.data[["band"]])) |>
        # filter by the band and by the version
        dplyr::filter(
            .data[["band"]] == !!band,
            .data[["version"]] == !!version
        ) |>
        # transform the format of the dates
        dplyr::mutate(
            start_date = .timeline_format(.data[["start_date"]]),
            end_date = .timeline_format(.data[["end_date"]])
        ) |>
        # select the relevant parts
        dplyr::select(dplyr::all_of(c(
            "tile", "start_date", "end_date", "band", "path"
        ))) |>
        # filter to remove duplicates (tile + dates + band)
        dplyr::distinct(
            .data[["tile"]],
            .data[["start_date"]],
            .data[["end_date"]],
            .data[["band"]],
            .keep_all = TRUE
        ) |>
        # order by dates
        dplyr::arrange(.data[["start_date"]])
    # select the tiles
    if (.has(tiles)) {
        items <- .hf_items_tiles_select(items, tiles, check_tiles)
    }
    # post-condition - we must have at least 1 item
    .check_that(nrow(items) > 0L)
    # return!
    items
}
#' @title Assemble results files of a HuggingFace dataset in a cube
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description This function read the files available in a dataset, prepares
#' them and create a cube to assemble them.
#'
#' @param source Data source.
#' @param collection Image collection.
#' @param items Results of the dataset.
#' @param labels Labels of the results.
#' @param multicores Number of cores.
#' @param progress Show a progress bar?
#'
#' @return A sits cube (one row per tile).
.hf_source_results_tiles <- function(source, collection, items, labels,
                              multicores, progress) {
    # as the source can have many files, we can read the items in parallel
    started <- .parallel_start(workers = multicores)
    on.exit(.parallel_stop(started), add = TRUE)
    # prepare file info
    file_info <- .parallel_map(seq_len(nrow(items)), function(i) {
        # get item
        item <- items[i, ]
        # the tile and its crs are not part of the file info
        dplyr::bind_cols(
            tile = item[["tile"]],
            crs = .raster_crs(.raster_open_rast(item[["path"]])),
            .fi_derived_from_file(
                file = item[["path"]],
                band = item[["band"]],
                start_date = item[["start_date"]],
                end_date = item[["end_date"]]
            )
        )
    }, progress = progress)
    # bind all file info rows!
    file_info <- dplyr::bind_rows(file_info)
    # make a cube for each tile
    .map_dfr(unique(file_info[["tile"]]), function(tile) {
        # get tile files
        fi_tile <- dplyr::filter(file_info, .data[["tile"]] == !!tile)
        # filter file details
        fi <- dplyr::select(fi_tile, -dplyr::all_of(c("tile", "crs")))
        # create cube
        .cube_create(
            source = source,
            collection = collection,
            satellite = .source_collection_satellite(source, collection),
            sensor = .source_collection_sensor(source, collection),
            tile = tile,
            xmin = max(fi[["xmin"]]),
            xmax = min(fi[["xmax"]]),
            ymin = max(fi[["ymin"]]),
            ymax = min(fi[["ymax"]]),
            crs = unique(fi_tile[["crs"]]),
            labels = labels,
            file_info = fi
        )
    })
}

#' @title Create a cube of result files from a HuggingFace dataset
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description This function creates a data cube from result files shared in
#' a HuggingFace dataset
#'
#' @param source Data source.
#' @param collection Image collection.
#' @param bands Band to be selected in the collection.
#' @param tiles A set of tiles in the collection reference system.
#' @param check_tiles Must all tiles be in the dataset?
#' @param labels Labels of the results.
#' @param version Version of the results.
#' @param multicores Number of cores.
#' @param memsize Memory available (in GB).
#' @param progress Show a progress bar?
#'
#' @return A sits cube.
.hf_source_results_cube <- function(source, collection, bands, tiles, check_tiles,
                             labels, version, multicores, memsize, progress) {
    # (if available) get cube labels
    labels <- .conf("sources", source, "collections", collection, "labels")
    labels <- .default(
        labels, unlist(labels)
    )
    # check if cube is a results cube
    .check_is_results_cube(bands, labels)
    # bands of results are in lower case
    band <- .band_set_case(bands)
    # select the result files of the dataset
    items <- .hf_source_results_items(
        source = source,
        collection = collection,
        band = band,
        version = version,
        tiles = tiles,
        check_tiles = check_tiles
    )
    # assemble file info as a cube
    cube <- .hf_source_results_tiles(
        source = source,
        collection = collection,
        items = items,
        labels = labels,
        multicores = multicores,
        progress = progress
    )
    # set the class of the results cube
    cube <- .cube_set_class(
        cube, .conf_derived_s3class(.conf("sits_results_s3_class")[[band]])
    )
    # in the case of class cubes
    if (inherits(cube, "class_cube")) {
        # we must check if the labels defined in the cube
        # are real and represent pixels available there
        .check_labels_class_cube(cube, multicores, memsize)
    }
    # return!
    cube
}

#' @title Create a cube of raster images from a HuggingFace dataset
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description This function creates a data cube from image files like
#' raster cubes, embeddings, shared in a HuggingFace dataset
#'
#' @param source Data source.
#' @param collection Image collection.
#' @param bands Bands to be selected in the collection.
#' @param tiles A set of tiles in the collection reference system.
#' @param check_tiles Must all tiles be in the dataset?
#' @param start_date Start date.
#' @param end_date End date.
#' @param multicores Number of cores.
#' @param progress Show a progress bar?
#' @param ... Additional parameters.
#'
#' @return A sits cube.
.hf_source_images_cube <- function(source, collection, bands, tiles, check_tiles,
                            start_date, end_date, multicores, progress, ...) {
    # retrieve the images of the dataset
    items <- .hf_items(
        source = source,
        collection = collection,
        tiles = tiles,
        check_tiles = check_tiles,
        start_date = start_date,
        end_date = end_date
    )
    # filter bands in items
    items <- .source_items_bands_select(
        source = source,
        items = items,
        bands = bands,
        collection = collection, ...
    )
    # create cube!
    cube <- .source_items_cube(
        source = source,
        items = items,
        collection = collection,
        multicores = multicores,
        progress = progress, ...
    )
    # set cube classes
    class(cube) <- .cube_s3class(cube)
    # return!
    cube
}

#' @title Test access to a HuggingFace collection
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description HuggingFace has no catalogue service to probe: the dataset was
#' already requested when its collection was registered, so the standard
#' dry-run is a no-op.
#'
#' @param source     Data source.
#' @param collection Image collection.
#' @param bands      Band names.
#' @param ...        Other parameters to be passed for specific types.
#'
#' @return Called for side effects
#' @export
.source_collection_access_test.hf_cube <- function(source, collection,
                                                   bands, ...) {
    return(invisible(source))
}

#' @title Create a data cube from a HuggingFace dataset
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description This function retries files and config from a given HuggingFace
#' dataset, validates them and using them produces a valid \code{sits} data cube
#'
#' @param source Data source.
#' @param collection Image collection.
#' @param bands Bands to be selected in the collection.
#' @param tiles A set of tiles in the collection reference system.
#' @param roi Region of interest.
#' @param start_date Start date.
#' @param end_date End date.
#' @param platform Satellite platform (not used: datasets describe a
#'                 single collection).
#' @param multicores Number of cores.
#' @param progress Show a progress bar?
#' @param ... Additional parameters.
#' @param labels Labels of results (by default, those of the dataset).
#' @param version Version of results.
#' @param memsize Memory available for checking classified maps (in GB).
#'
#' @return A sits cube.
#' @export
.source_cube.hf_cube <- function(source,
                                 collection,
                                 bands,
                                 tiles,
                                 roi,
                                 start_date,
                                 end_date,
                                 platform,
                                 multicores,
                                 progress, ...,
                                 labels = NULL,
                                 version = .conf("results_version_def"),
                                 memsize = 2L) {
    # if tiles are specified by the user, we assume they must be available
    # in the dataset that will be loaded
    check_tiles <- .has(tiles)
    # a roi is converted to tiles when the collection uses a
    # grid system that sits known
    if (.has(roi)) {
        tiles <- .hf_roi_tiles(source, collection, roi)
    }
    # get collection definition
    collection_config <- .conf("sources", source, "collections", collection)
    # results produced by sits are read as results cubes
    is_results <- .hf_collection_is_results(collection_config)
    # if dataset files can be represented as a results cube
    if (is_results) {
        # generates it
        cube <- .hf_source_results_cube(
            source = source,
            collection = collection,
            bands = bands,
            tiles = tiles,
            check_tiles = check_tiles,
            labels = labels,
            version = version,
            multicores = multicores,
            memsize = memsize,
            progress = progress
        )
    } else {
        # otherwise, we try to load the dataset files as an "image" cube, which
        # includes surface reflectance cube, embeddings cube and friends
        cube <- .hf_source_images_cube(
            source = source,
            collection = collection,
            bands = bands,
            tiles = tiles,
            check_tiles = check_tiles,
            start_date = start_date,
            end_date = end_date,
            multicores = multicores,
            progress = progress, ...
        )
    }
    # if roi is available, filter cubes using it
    if (.has(roi)) {
        cube <- .cube_filter_spatial(cube, roi)
    }
    # return!
    cube
}

#' @title Organize items of a HuggingFace dataset by tile
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @param source     Data source.
#' @param items      Items object.
#' @param ...        Additional parameters.
#' @param collection Image collection.
#'
#' @return Tile of each item.
#' @export
.source_items_tile.hf_cube <- function(source, items, ..., collection = NULL) {
    rstac::items_reap(items, field = c("properties", "tile"))
}
