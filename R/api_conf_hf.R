# ---- hf collection definition ----
#' @title Collection definition in the HuggingFace dataset format
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description This function prepares a collection definition in the format
#' used in the HuggingFace configuration file (\code{"sits.yml"}).
#'
#' @param collection_conf Collection definition.
#'
#' @return Collection definition.
.hf_conf_clean <- function(collection_conf) {
    # get default hf collection
    collection_hf <- .conf("hf", "collection_conf_source_keys")
    # get collection configuration and remove provider details
    collection_clean <- setdiff(names(collection_conf), collection_hf)
    # return!
    collection_conf[collection_clean]
}

#' @title Order the keys of a collection definition
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description This function sort the keys of a collection definition in a way
#' to facilitate file usage. The names of the dataset come first, and its bands
#' definition last. We use this strategy to show base informations first and the
#' (possible) long list of band metadata after.
#'
#' @param collection_conf Collection definition.
#'
#' @return Collection definition, ordered.
.hf_conf_order <- function(collection_conf) {
    # get the order of the keys
    keys <- .conf("hf", "collection_conf_key_order")
    # define order of keys
    keys_order <- c(
        intersect(keys, names(collection_conf)),
        setdiff(names(collection_conf), keys)
    )
    # return collection definition ordered
    # > extra config keys are always send to the end
    # > of the description object
    collection_conf[keys_order]
}

#' @title Name the bands of a collection as sits names them
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description This function prepares the name of the bands as they are used
#' by users in \code{sits} (e.g., \code{"NDVI"}) and not as the assets of its
#' provider (which sometimes write bands in a "low-level" way, like
#' \code{"NDVI_sr"} or others)
#'
#' @param bands_conf Definition of the bands of a collection.
#'
#' @return Definition of the bands
.hf_conf_bands_name <- function(bands_conf) {
    purrr::imap(bands_conf, function(band_conf, band) {
        # update band config to use sits name
        band_conf[["band_name"]] <- band
        band_conf
    })
}

#' @title Describe a collection registered in sits
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description This function takes a definition of a collection available in
#' \code{sits} and prepares it to be shared as a HuggingFace dataset.
#'
#' @param source Data source.
#' @param collection Image collection.
#' @param bands Bands shared in the dataset.
#'
#' @return Collection definition.
.hf_conf_source <- function(source, collection, bands) {
    # get collection configuration
    collection_conf <- .conf("sources", source, "collections", collection)
    # get bands
    bands_conf <- collection_conf[["bands"]]
    # prepare bands
    bands <- toupper(.default(bands, names(bands_conf)))
    # check if bands requested by user are available in the collection
    .check_chr_within(x = bands, within = names(bands_conf))
    # use only selected bands
    collection_conf[["bands"]] <- .hf_conf_bands_name(bands_conf[bands])
    # reorder and return!
    .hf_conf_order(.hf_conf_clean(collection_conf))
}

#' @title Describe a cube of a dataset
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description This function describes the properties available in all
#  \code{sits} cubes shared in a HuggingFace dataset.
#'
#' @param cube Data cube.
#'
#' @return Collection definition, without bands.
.hf_conf_cube <- function(cube) {
    # set caller
    .check_set_caller(".hf_conf_cube")
    # get satellite and sensor from the cube
    satellite <- .cube_satellite(cube)
    sensor <- .cube_sensor(cube)
    # satellite and sensor must be defined
    .check_that(length(satellite) == 1L && !anyNA(satellite))
    .check_that(length(sensor) == 1L && !anyNA(sensor))
    # get grid system
    grid_system <- .try(
        .cube_grid_system(cube),
        .default = .conf("hf", "collection_defaults", "grid_system")
    )
    # return collection definition
    list(
        satellite = satellite,
        sensor = sensor,
        grid_system = grid_system,
        dates = .hf_conf_dates(cube)
    )
}

#' @title Describe the period of a data cube
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description Gets the period of a collection.
#'
#' @param cube Data cube.
#'
#' @return Period of the cube.
.hf_conf_dates <- function(cube) {
    # get min-max dates to define the period
    dates <- c(min(.cube_start_date(cube)), max(.cube_end_date(cube)))
    # we are assuming years for the period definition
    # so we get unique dates to form (start-year ; end-year)
    dates <- unique(format(dates, "%Y"))
    # merge dates with a proper separator
    dates <- paste(dates, collapse = .conf("hf", "collection_conf_dates_sep"))
    # return!
    dates
}

#' @title Describe the labels of a data cube
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description Returns labels of a data cube.
#'
#' @param cube Data cube.
#'
#' @return Labels of the cube.
.hf_conf_labels <- function(cube) {
    # get cube labels
    labels <- .cube_labels(cube)
    # create label mapping
    labels <- stats::setNames(
        unname(labels), .default(names(labels), .as_chr(seq_along(labels)))
    )
    # return!
    as.list(labels)
}

#' @title Describe the resolution of a band of a data cube
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description Returns band resolution.
#'
#' @param tile Tile of a data cube.
#' @param band Band of the tile.
#'
#' @return Resolution of the band.
.hf_conf_res <- function(tile, band) {
    # set caller
    .check_set_caller(".hf_conf_res")
    # get band resolution
    res <- unique(.xres(.fi(.tile_filter_bands(tile, band))))
    # band must have one valid resolution
    # > this is always the case in sits, but it is better
    # > to double check and avoid issues
    .check_num_parameter(res, exclusive_min = 0.0, len_max = 1L)
    # return!
    res
}

#' @title Describe the bands of a data cube
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description Returns bands of a data cube
#'
#' @param cube Data cube.
#'
#' @return Definition of the bands from a cube.
.hf_conf_cube_bands <- function(cube) {
    # get tile
    tile <- .tile(cube)
    # process bands
    purrr::map(stats::setNames(nm = .tile_bands(tile)), function(band) {
        # get band config
        band_conf <- .tile_band_conf(tile, band)
        # save band name as used in sits
        band_conf[["band_name"]] <- band
        # save band resolution
        band_conf[["resolution"]] <- .hf_conf_res(tile, band)
        # return!
        band_conf
    })
}

#' @title Describe the bands of a data cube of results
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description This function returns the configuration of results bands.
#'
#' @param cube Data cube.
#'
#' @return Definition of the bands of the cube.
.hf_conf_derived_bands <- function(cube) {
    # get tile
    tile <- .tile(cube)
    # get bands
    bands <- .tile_bands(tile)
    bands <- stats::setNames(bands, toupper(bands))
    # process bands
    purrr::map(bands, function(band) {
        # save only resolution
        # > by doing this, we are assuming users loading a
        # > results cube will be selecting the band type during load.
        # > For instance, definiting `bands = class` or `bands = probs`
        list(
            resolution = .hf_conf_res(tile, band)
        )
    })
}

#' @title Describe a results collection
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description This function harmonize the description of a results collection
#' with multiple definitions on it (i.e., probabilities, uncertainties and a
#' classified map).
#'
#' @param collection_confs Collection definition.
#'
#' @return Collection definition.
.hf_conf_merge <- function(collection_confs) {
    # set caller
    .check_set_caller(".hf_conf_merge")
    # get collection bands (assuming multiple compatible bands)
    bands <- purrr::flatten(purrr::map(collection_confs, "bands"))
    # get remaining configuration (that is not "bands" config)
    names_conf <- purrr::map(collection_confs, function(collection_conf) {
        collection_conf[names(collection_conf) != "bands"]
    })
    # we are assuming here a results cube, which contains multiple different
    # bands, but all describing the same tiles / properties. To ensure we
    # are describing the same properties (e.g., satellite, sensor), we do a
    # validation
    are_equal <- all(purrr::map_lgl(names_conf, identical, names_conf[[1L]]))
    are_not_duplicated <- !anyDuplicated(names(bands))
    # must have same properties and not repeat bands
    .check_that(are_equal && are_not_duplicated)
    # if all good, return harmonized properties with multiple bands
    # e.g., same properties + definition of bands like probs, uncert, others
    .hf_conf_order(c(names_conf[[1L]], list(bands = bands)))
}

#' @title Check a collection definition as sits reads it
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description This function checks if a given configuration is compatible
#' with the \code{sits} configuration registry. If so, the config can be used
#' in \code{sits} with no errors.
#'
#' @param collection_conf Collection definition.
#'
#' @return Collection definition.
.hf_conf_check <- function(collection_conf) {
    # get default config
    collection_default <- .conf("hf", "collection_defaults")
    # following the approach we use in the registration, the default values
    # can be used to complete missing values from the current collection config
    collection_conf <- utils::modifyList(collection_default, collection_conf)
    # then, the definition must identify the type of the cube
    .hf_collection_check(collection_conf)
    # verify if is a results collection
    is_results <- .hf_collection_is_results(collection_conf)
    # if so, we process the collection to ensure it is in the
    # expected format for results data
    if (is_results) {
        # prepare results config
        collection_conf <- .hf_collection_results(collection_conf)
        # update parse info to match what we use in sits
        collection_conf[["parse_info"]] <- .conf("results_parse_info_def")
    }
    # if everything is prepared, we try to build the collection
    if (.has(collection_conf[["bands"]])) {
        # build collection
        # (with a series of validation before. If we miss something before, this
        # function will get it)
        do.call(.conf_new_collection, collection_conf)
    }
    # return!
    invisible(collection_conf)
}

#' @title Write a collection definition to be shared in a HuggingFace dataset
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description Writes the definition as the file read by \code{sits}.
#'
#' @param collection_conf Collection definition.
#' @param output_dir Directory where the files of the dataset 
#'                   are written (optional).
#' @param origin Description of where the definition came from.
#'
#' @return Collection definition.
.hf_conf_write <- function(collection_conf, output_dir, origin) {
    # check if the collection description to be saved can be
    # used by sits in a second moment
    # the idea here is, we write what we can read
    .hf_conf_check(collection_conf)
    # if a directory was informed, we write it
    if (.has(output_dir)) {
        .hf_conf_file(collection_conf, output_dir, origin)
    }
    # done
    collection_conf
}

#' @title Write a collection definition into a file
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description Writes a collection definition in a valid file \code{sits} can
#' read back as a collection
#'
#' @param collection_conf Collection definition.
#' @param output_dir Directory where the files of the dataset are written.
#' @param origin Description of where the definition came from.
#'
#' @return Called for side effects.
.hf_conf_file <- function(collection_conf, output_dir, origin) {
    # define file (named as sits reads it in the dataset)
    file <- .hf_conf_path(output_dir, .conf("hf", "config_file"))
    # write file (header + content)
    file_content <- c(
        # header
        .conf("hf", "collection_conf_header"),
        # tool signature
        sprintf(.conf("hf", "collection_conf_origin"), origin),
        # white space
        "",
        # content
        .hf_conf_yaml(collection_conf)
    )
    # write content
    writeLines(file_content, con = file)
    # return written file
    invisible(file)
}

#' @title Build the path of a file of sits in a directory
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description Files written by \code{sits} to describe a dataset are kept in
#' a \code{"sits"} directory, as they are read in the repository. So, users
#' upload the content of \code{output_dir} as it is. The directory is created
#' when it doesn't exist.
#'
#' @param output_dir Directory where the files of the dataset are written.
#' @param file File name.
#'
#' @return Path of the file (\code{"<output_dir>/sits/<file>"}).
.hf_conf_path <- function(output_dir, file) {
    .file_path(
        file,
        output_dir = file.path(output_dir, .conf("hf", "sits_dir")),
        create_dir = TRUE
    )
}

#' @title Convert a collection definition to YAML
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description This function convert a \code{sits} collection description
#' to a valid YAML file.
#'
#' @param collection_conf Collection definition.
#'
#' @return Collection definition in YAML.
.hf_conf_yaml <- function(collection_conf) {
    yaml::as.yaml(
        collection_conf,
        # try to keep indent similar to the sits
        # source files
        indent = 4L,
        handlers = list(
            # logicals are written as sits uses them
            logical = yaml::verbatim_logical
        )
    )
}

# ---- hf cube cache ----
#' @title Name the repository a data cube is shared in
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description This function gets the repository in which a cube is saved in.
#'
#' @param cube Data cube shared in the dataset.
#' @param repo HuggingFace repository name.
#'
#' @return HuggingFace repository name.
.hf_conf_cache_repo <- function(cube, repo) {
    # set caller
    .check_set_caller(".hf_conf_cache_repo")
    # if there is no repo defined and we are dealing with a cube from
    # HuggingFace, we can use the source from the cube itself
    if (.has_not(repo) && .hf_is_source(.cube_source(cube))) {
        repo <- .try(
            .source_collection_name(.cube_source(cube), .cube_collection(cube)),
            .default = NULL
        )
    }
    # post-condition - we must have a repository defined
    .check_chr_parameter(repo, len_max = 1L)
    # return!
    repo
}

#' @title Prepares a data cube as a HuggingFace dataset
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description Data cubes from a HuggingFace dataset reads files from the
#' platform. This function prepares a given cube as it is read from a remote
#' repository: its images are the files of the repository, and its source and
#' collection are those of the dataset (\code{"HF:<USER>"}, \code{"<REPO>"}),
#' so it is described by the collection definition of the dataset, and not
#' by the provider the images came from.
#'
#' @param cube Data cube shared in the dataset.
#' @param repo HuggingFace repository name.
#'
#' @return Data cube with file addresses refering to HuggingFace repository.
.hf_conf_cache_cube <- function(cube, repo) {
    # let's go tile by tile
    .cube_foreach_tile(cube, function(tile) {
        # get images
        file_info <- .fi(tile)
        # remove VSI prefix from file paths
        file_paths <- .file_remove_vsi(.fi_paths(file_info))
        # remove repository URL from file paths
        repo_url <- .hf_file_url(repo, "")
        is_repo <- startsWith(file_paths, repo_url)
        file_paths[is_repo] <- substring(file_paths[is_repo], nchar(repo_url) + 1L)
        file_paths[!is_repo] <- basename(file_paths[!is_repo])
        # update file paths to point to the HuggingFace platform
        file_paths <- .hf_file_url(repo, file_paths)
        # update file reference
        file_info[["path"]] <- .stac_add_gdal_fs(file_paths)
        # save changes into the tile
        tile[["file_info"]] <- list(file_info)
        # the tile is described by the dataset
        tile[["source"]] <- .hf_repo_source(repo)
        tile[["collection"]] <- .hf_repo_collection(repo)
        # return!
        tile
    })
}

#' @title Writes a cache cube for a HuggingFace dataset repository
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description This function writes a data cube object to be used as a cache
#' file in a HuggingFace dataset repository.
#'
#' @param cube Data cube shared in the dataset.
#' @param output_dir Directory where the files of the cache are written.
#' @param repo HuggingFace repository name.
#'
#' @return Cache cube to be shared in HuggingFace dataset repository.
.hf_conf_cache <- function(cube, output_dir, repo) {
    # prepare cache cube object
    cache <- list(
        sits_version = utils::packageDescription("sits")[["Version"]],
        created = .as_chr(Sys.Date()),
        source = .hf_repo_source(repo),
        collection = .hf_repo_collection(repo),
        cube = .hf_conf_cache_cube(cube, repo)
    )
    # save cache file (named as sits reads it in the dataset)
    saveRDS(cache, .hf_conf_path(output_dir, .conf("hf", "cache_file")))
    # return!
    invisible(cache)
}

# ---- hf dataset description ----
#' @title Writes the files describing a data cube in a HuggingFace dataset
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description A data cube is shared with its collection definition and its
#' cache cube. The cache refers to images in the repository, so the repository
#' is required. It is verified before anything is written, so no partial
#' description of a dataset is written.
#'
#' @param cube Data cube shared in the dataset.
#' @param collection_conf Collection definition of the cube.
#' @param output_dir Directory where the files of the dataset 
#'                   are written (optional).
#' @param repo HuggingFace repository name.
#'
#' @return Collection definition.
.hf_conf_cube_write <- function(cube, collection_conf, output_dir, repo) {
    # get origin of the definition
    origin <- .cube_collection(cube)
    # without a directory, the definition is only checked and returned
    if (.has_not(output_dir)) {
        return(.hf_conf_write(collection_conf, output_dir, origin))
    }
    # the cache refers to the images in the repository
    repo <- .hf_conf_cache_repo(cube, repo)
    # write collection definition
    collection_conf <- .hf_conf_write(collection_conf, output_dir, origin)
    # write cache cube
    .hf_conf_cache(cube, output_dir, repo)
    # return!
    collection_conf
}

#' @title Describe a data cube as a HuggingFace dataset
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description This function creates a valid collection description from a
#' given data cube. It converts the properties of a cube (e.g., raster,
#' embeddings, results) in configurations that can be read by sits as a valid
#' data collection.
#'
#' @param cube Data cube.
#' @param output_dir Directory where the files of the dataset 
#'                   are written (optional).
#' @param repo HuggingFace repository name.
#'
#' @return Collection definition of the dataset.
.hf_conf_dataset <- function(cube, output_dir, repo) {
    UseMethod(".hf_conf_dataset", cube)
}

#' @export
.hf_conf_dataset.embeddings_cube <- function(cube, output_dir, repo) {
    # write collection definition and cache cube
    .hf_conf_cube_write(
        cube = cube,
        collection_conf = .hf_conf_cube(cube),
        output_dir = output_dir,
        repo = repo
    )
}

#' @export
.hf_conf_dataset.derived_cube <- function(cube, output_dir, repo) {
    # get collection config
    collection_conf <- .hf_conf_cube(cube)
    # prepare collection config
    collection_conf[["labels"]] <- .hf_conf_labels(cube)
    collection_conf[["bands"]] <- .hf_conf_derived_bands(cube)
    # write collection definition and cache cube
    .hf_conf_cube_write(
        cube = cube,
        collection_conf = collection_conf,
        output_dir = output_dir,
        repo = repo
    )
}

#' @export
.hf_conf_dataset.raster_cube <- function(cube, output_dir, repo) {
    # get collection config
    collection_conf <- .hf_conf_cube(cube)
    # prepare collection config
    collection_conf[["bands"]] <- .hf_conf_cube_bands(cube)
    # write collection definition and cache cube
    .hf_conf_cube_write(
        cube = cube,
        collection_conf = collection_conf,
        output_dir = output_dir,
        repo = repo
    )
}

#' @export
.hf_conf_dataset.list <- function(cube, output_dir, repo) {
    # set caller
    .check_set_caller("sits_to_hf_list")
    # we assume a list as a group of cubes to be saved
    # pre-condition - the dataset is shared with at least one cube
    .check_lst(cube, len_min = 1L, is_named = FALSE)
    # pre-condition - if we have a list of cubes, to be strict and avoid
    # unexpected results, we assume all of them must describe the same
    # dataset with different bands (e.g., list of results cubes). So,
    # properties must be the same in all cubes
    collection_confs <- purrr::map(cube, function(cube) {
        .hf_conf_dataset(cube, output_dir = NULL, repo = repo)
    })
    # write collection definition
    # > a cache describes a single cube, so it is not written for many cubes
    .hf_conf_write(
        collection_conf = .hf_conf_merge(collection_confs),
        output_dir = output_dir,
        origin = .cube_collection(cube[[1L]])
    )
}

#' @export
.hf_conf_dataset.default <- function(cube, output_dir, repo) {
    # cube as tibble
    cube <- tibble::as_tibble(cube)
    # if cube object is a valid cube tibble, try to find its class
    if (all(.conf("sits_cube_cols") %in% colnames(cube))) {
        cube <- .cube_find_class(cube)
    }
    # otherwise, we need to stop the operation
    else {
        stop(.conf("messages", "sits_to_hf_default"))
    }
    # return!
    .hf_conf_dataset(cube, output_dir, repo)
}
