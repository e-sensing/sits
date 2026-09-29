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
#' @param source Data source (\code{"HF:<user>"}).
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
                    ),
                    headers = .hf_headers()
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
#' @param type Type of the repository (\code{"dataset"} or \code{"model"}).
#'
#' @return URL of the file.
.hf_file_url <- function(repo, file, type = "dataset") {
    paste(
        .conf("hf", "repo_types", type, "url"),
        repo,
        .conf("hf", "file_path"),
        file,
        sep = "/"
    )
}

#' @title Build the path of a file of sits in a HuggingFace repository
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description This function builds the path of a file of sits in a HuggingFace
#' repository.Files written by \code{sits} to describe a dataset (e.g.,
#' \code{"sits.yml"}, \code{"cache.rds"}) are kept in their own directory
#' of the repository, so they are not mixed with the other files it shares.
#'
#' @param file File name.
#'
#' @return Path of the file in the repository (\code{"sits/<file>"}).
.hf_sits_file <- function(file) {
    paste(.conf("hf", "sits_dir"), file, sep = "/")
}

# ---- hf authentication ----
#' @title Get the HuggingFace access token of the user
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description To access private repositories or to increase rate limits, it is
#' required to be authenticated in HuggingFace. In \code{sits}, it is possible
#' to define a proper token, so requests to the platform are signed. We use the
#' token in API and GDAL raster requests.
#'
#' @return Access token of the user, or NULL when there is none.
.hf_token <- function() {
    # read user token from env var
    user_token <- Sys.getenv(.conf("hf", "token_env"))
    # clean loaded token
    user_token <- user_token[nzchar(user_token)]
    # if token is not available, skip it
    if (.has_not(user_token)) {
        return(NULL)
    }
    # return token!
    unname(user_token[[1L]])
}

#' @title Verify the HuggingFace access token of the user
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description HuggingFace ignores an invalid token when a dataset is public,
#' answering as it does to anonymous requests: a token that is not valid gives
#' no error and no benefit. This function verifies the token, so users are told
#' when the token they informed is not being used.
#'
#' @param token Access token of the user.
#'
#' @return Called for side effects.
.hf_token_validate <- function(token) {
    # set caller
    .check_set_caller(".hf_token_validate")
    # verify if token was already validated
    is_validated <- identical(sits_env[["hf_token_verified"]], token)
    # if already validated, just reuse it
    if (is_validated) {
        return(invisible(token))
    }
    # verify in the platform, who is the user associated to the token.
    # > this is also used to validate the token. We assume that, a request
    # > rejected by HuggingFace is caused by an invalid token.
    response <- tryCatch(
        .get_request(
            url = .conf("hf", "token_url"),
            headers = .hf_token_header(token)
        ),
        error = function(e) {
            e[["resp"]]
        }
    )
    # the service must be reachable
    .check_that(.has(response))
    # the token must be recognized by HuggingFace
    .check_that(
        !.response_is_error(response), msg = .conf("messages", ".hf_token")
    )
    # the token is valid: verify it only once
    sits_env[["hf_token_verified"]] <- token
    # return!
    invisible(token)
}

#' @title Build the authentication header of a HuggingFace request
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description This function creates a proper HTTP authentication header using
#' the \code{token} specified. If no token is specified, return is \code{NULL}.
#'
#' @param token Access token of the user.
#'
#' @return HTTP Authentication header of the requests, or NULL when there is no
#' token.
.hf_token_header <- function(token) {
    # requests are signed only when the user has a token
    if (.has_not(token)) {
        return(NULL)
    }
    # prepare token
    token_http <- list(paste(.conf("hf", "token_type"), token))
    token_http <- stats::setNames(token_http, .conf("hf", "token_header"))
    # return!
    token_http
}

#' @title Build the headers of a HuggingFace request
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description This function creates a proper HTTP authentication for
#' HuggingFace. It uses available settings in the environment, like the
#' user token, if any is specified.
#'
#' @return Headers of the requests, or NULL when there is no token.
.hf_headers <- function() {
    # get the token of the user
    token <- .hf_token()
    # a token informed must be a token HuggingFace recognizes
    if (.has(token)) {
        .hf_token_validate(token)
    }
    # return!
    .hf_token_header(token)
}

#' @title Build the headers of a request to a HuggingFace URL
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description This functions builds the auth headers for a request
# to a HuggingFace URL.
#'
#' @param url URL of the request.
#'
#' @return Headers of the request, or NULL when the URL is not in HuggingFace
#' or there is no token.
.hf_url_headers <- function(url) {
    # only requests to HuggingFace are signed
    is_hf <- startsWith(url, .conf("hf", "base_url"))
    # if not in HuggingFace, we return NULL
    if (!isTRUE(is_hf)) {
        return(NULL)
    }
    # otherwise, we return the headers
    # (it can be NULL, if no token is available)
    .hf_headers()
}

#' @title Persist the HuggingFace access token for GDAL
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description Images of a dataset are read by GDAL, which signs its requests
#' with the headers specified in the \code{GDAL_HTTP_HEADER_FILE} env variable.
#'
#' @return Called for side effects.
.hf_token_persist <- function() {
    # get token
    token <- .hf_token()
    # if token is available, validate it
    if (.has(token)) {
        .hf_token_validate(token)
    }
    # otherwise, we can just skip the operation as there
    # is nothing for us here
    else {
        return(invisible(NULL))
    }
    # the token is persisted once in a session
    is_in_session <- identical(sits_env[["hf_token_gdal"]], token)
    # if already persisted, we just return it assuming the GDAL elements were
    # already defined
    if (is_in_session) {
        return(invisible(token))
    }
    # as sits users can handle multiple sources, here we assume a conservative
    # position and save any value previously defined in the GDAL header file
    has_gdal_token <- .has(sits_env[["hf_token_gdal"]])
    # if any value is available, save it
    if (!has_gdal_token) {
        # get current value
        current_value <- Sys.getenv(
            .conf("hf", "token_gdal_env"), unset = NA
        )
        # save value
        sits_env[["hf_token_gdal_old"]] <- current_value
    }
    # get gdal header file
    gdal_header_file <- sits_env[["hf_token_gdal_file"]]
    # we need to verify if the token was already defined
    is_written <- identical(sits_env[["hf_token_gdal_token"]], token)
    is_written <- is_written && .has(gdal_header_file)
    is_written <- is_written && file.exists(gdal_header_file)
    # if written, we just skip the file creation
    if (!is_written) {
        # otherwise, we create gdal header file to persist token
        gdal_header_file <- tempfile()
        # define token in http authorization format
        http_header <- paste0(
            .conf("hf", "token_header"), ": ",
            .conf("hf", "token_type"), " ", token
        )
        # save token
        writeLines(http_header, gdal_header_file)
        # update local variables indicating the token was identified and
        # gdal file was created
        sits_env[["hf_token_gdal_token"]] <- token
        sits_env[["hf_token_gdal_file"]] <- gdal_header_file
    }
    # define gdal header
    do.call(
        Sys.setenv,
        stats::setNames(
            list(gdal_header_file), .conf("hf", "token_gdal_env")
        )
    )
    # save token used in gdal header
    sits_env[["hf_token_gdal"]] <- token
    # return!
    invisible(token)
}

#' @title Flush the HuggingFace access token of gdal
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description This function flushes the user token from the GDAL session
#'
#' @return Called for side effects.
.hf_token_flush <- function() {
    # verify if token is empty
    is_token_empty <- .has_not(sits_env[["hf_token_gdal"]])
    # if token is empty, just skip the operation
    if (is_token_empty) {
        return(invisible(NULL))
    }
    # get the gdal configuration
    gdal_header_file <- sits_env[["hf_token_gdal_old"]]
    # restore the configuration of the session, or remove the one of sits
    if (.has(gdal_header_file) && !is.na(gdal_header_file)) {
        do.call(
            Sys.setenv,
            stats::setNames(
                list(gdal_header_file), .conf("hf", "token_gdal_env")
            )
        )
    } else {
        Sys.unsetenv(.conf("hf", "token_gdal_env"))
    }
    # flush local env
    sits_env[["hf_token_gdal"]] <- NULL
    sits_env[["hf_token_gdal_old"]] <- NULL
    # return!
    invisible(NULL)
}

# ---- hf gdal ----
#' @title Configure GDAL to open the images of a HuggingFace dataset
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description GDAL opens a remote image searching for auxiliary files that
#' datasets don't share. Each search is a request counted in the limits of
#' HuggingFace. This function disables the searches using env variables, 
#' which are also read by the workers of parallel operations.
#'
#' @return The previous values of the env variables (NA when not defined), to
#' be restored with \code{.hf_gdal_flush()}.
.hf_gdal_persist <- function() {
    # get hf gdal configuration
    gdal_env <- .conf("hf", "gdal_env")
    # save the current configuration of the session
    gdal_env_old <- Sys.getenv(names(gdal_env), unset = NA, names = TRUE)
    # define sits configuration
    do.call(Sys.setenv, gdal_env)
    # return!
    invisible(gdal_env_old)
}

#' @title Restore the GDAL configuration of the session
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description Restores the env variables changed by \code{.hf_gdal_persist()},
#' so images of other providers are opened as before.
#'
#' @param gdal_env_old Previous values of the env variables.
#'
#' @return Called for side effects.
.hf_gdal_flush <- function(gdal_env_old) {
    # define variables that are not defined
    is_undefined <- is.na(gdal_env_old)
    # remove the variables that are not defined
    Sys.unsetenv(names(gdal_env_old)[is_undefined])
    # restore the variables that are defined
    # the others are restored
    if (!all(is_undefined)) {
        do.call(Sys.setenv, as.list(gdal_env_old[!is_undefined]))
    }
    # return!
    invisible(NULL)
}

# ---- hf dataset ----
#' @title Download a file stored in a HuggingFace repository
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description This function downloads a file stored in HuggingFace
#'
#' @param repo HuggingFace repository name.
#' @param file File name in the repository.
#' @param type Type of the repository (\code{"dataset"} or \code{"model"}).
#'
#' @return Path of the file in the local file system.
.hf_file_download <- function(repo, file, type = "dataset") {
    # here, we are assuming only auxiliary files (e.g., config, samples,
    # models) will be downloaded from HuggingFace, so, we "hard coded" the
    # target as one temporary file. This was done consciously, as we want to
    # fail if users of this function wants to save a huge file using it.
    # the extension is kept, as readers of sits (e.g., parquet) check it.
    file_local <- tempfile(fileext = paste0(".", .file_ext(file)))
    # get file (using any token if available)
    response <- .get_request(
        url = .hf_file_url(repo, file, type),
        headers = .hf_headers(),
        path = file_local
    )
    # check response to ensure download went well
    .response_check_status(response)
    # return!
    file_local
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
#' @param type Type of the repository (\code{"dataset"} or \code{"model"}).
#'
#' @return A list with the dataset metadata.
.hf_dataset <- function(repo, type = "dataset") {
    # set caller
    .check_set_caller(".hf_dataset")
    # request the dataset metadata
    dataset <- .try(
        {
            .response_content(
                .get_request(
                    url = paste(
                        .conf("hf", "repo_types", type, "api_url"),
                        repo,
                        sep = "/"
                    ),
                    headers = .hf_headers()
                )
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
    # > the definition is requested as any other file of the dataset, so a
    # > private dataset is read with the token of the user
    collection <- .try(
        {
            suppressWarnings(
                yaml::yaml.load_file(
                    input = .hf_file_download(
                        repo = repo, 
                        file = .hf_sits_file(.conf("hf", "config_file"))
                    ),
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
#' This operation is intended to be executed by \code{sits_from_hf()}, and also
#' when the source of a cube is first used (e.g., \code{.tile_source}) for
#' cases when user save the HF cube in a RDS.
#'
#' @param source     Data source (\code{"HF:<user>"}).
#' @param collection Image collection (repository name).
#'
#' @return Called for side effects.
.hf_source_register <- function(source, collection) {
    # images of a dataset are read by gdal, which signs its requests
    # with the token of the user (if available)
    .hf_token_persist()
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
#' @description This function ensures a given collection assumed as results
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

# ---- hf cube cache ----
#' @title Read cached cube from a HuggingFace dataset
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description This function tries to read a cache file from a HuggingFace
#' dataset repository.
#'
#' @param repo HuggingFace repository name.
#'
#' @return Cached cube shared by the dataset, or NULL when it shares none.
.hf_cache_load <- function(repo) {
    # try to read it and get its value. In case of error, return NULL
    .try({
            readRDS(
                .hf_file_download(
                    repo = repo,
                    file = .hf_sits_file(.conf("hf", "cache_file"))
                )
            )
        },
        .default = NULL
    )
}

#' @title Verify cached cube from a HuggingFace dataset
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description This function validates if a cached cube still aligned with
#' the content available in a HuggingFace dataset repository. This is done
#' to help users consume the right version of files from HuggingFace.
#'
#' @param cache Cache cube from a HuggingFace dataset repository.
#'
#' @return TRUE when the cube can be used.
.hf_cache_check <- function(cache) {
    # cache object must be a list
    is_valid <- is.list(cache)
    is_valid <- is_valid && all(.conf("hf", "cache_keys") %in% names(cache))
    # if cache is not in a valid shape, inform user and refuse validation
    if (!is_valid) {
        warning(.conf("messages", ".hf_cache_check"), call. = FALSE)
        return(FALSE)
    }
    # if sits version is different from the one used to produce the cache
    # inform user, as behavior can change.
    sits_version <- utils::packageDescription("sits")[["Version"]]
    if (!identical(cache[["sits_version"]], sits_version)) {
        warning(.conf("messages", ".hf_cache_version"), call. = FALSE)
    }
    # cube object in the cache, must be a valid cube tibble
    is_valid <- all(
        .conf("sits_cube_cols") %in% colnames(cache[["cube"]])
    )
    # if cube is not valid, inform user
    if (!is_valid) {
        warning(.conf("messages", ".hf_cache_check"), call. = FALSE)
    }
    # return!
    is_valid
}

#' @title Verify files of a cached cube from a HuggingFace dataset
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description This function verifies if files specified in the cache cube
#' are all available in the same repository of the cache. Files not available
#' are reported and files from another repository are rejected.
#'
#' @param cube Cache cube from a HuggingFace dataset repository.
#' @param repo HuggingFace repository name.
#'
#' @return TRUE when the files described are those of the dataset.
.hf_cache_files <- function(cube, repo) {
    # get the images described by the cube, as they are requested
    # get files from the cached cube
    paths <- .file_remove_vsi(unlist(.cube_paths(cube)))
    # all files in a cube must be associated with its repository itself
    is_repo <- startsWith(paths, .hf_file_url(repo, ""))
    # if some files are not in the repository, report to user and reject cache
    if (!all(is_repo)) {
        warning(.conf("messages", ".hf_cache_repo"), call. = FALSE)
        return(FALSE)
    }
    # to confirm cache is synced with the repo files, we must check one-by-one
    # so, first we get the files available in the repo
    files <- .hf_files(repo)
    # every image in the cache cube must be an image of the dataset
    files_missing <- setdiff(paths, files)
    # if any file is missing
    if (.has(files_missing)) {
        # inform user
        warning(
            paste(
                .conf("messages", ".hf_cache_files"),
                toString(basename(files_missing))
            ),
            call. = FALSE
        )
        # and reject cache
        return(FALSE)
    }
    # otherwise, accept the cache cube!
    TRUE
}

#' @title Create a datacube from a cache available in a HuggingFace dataset
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description This function reads a cache cube from a HuggingFace dataset
#' validate it, and use it as cube object of the repository. This speedup the
#' cube load operation as here we are just loading a small rds file.
#'
#' @param source Data source.
#' @param collection Image collection.
#' @param bands Bands to be selected in the collection.
#' @param tiles A set of tiles in the collection reference system.
#' @param start_date Start date.
#' @param end_date End date.
#'
#' @return A sits cube, or NULL when the dataset shares no cube to be used.
.hf_cache_cube <- function(source, collection, bands, tiles,
                           start_date, end_date) {
    # set caller
    .check_set_caller(".hf_cache_cube")
    # get the repository of the collection
    repo <- .source_collection_name(source, collection)
    # get cached cube
    cache <- .hf_cache_load(repo)
    # if there is no cache, we skip the rest of the operation
    if (.has_not(cache)) {
        return(NULL)
    }
    # the cache cube must be valid for the given source / collection
    is_valid_cache <- .hf_cache_check(cache)
    # if not valid, skip operation
    if (!is_valid_cache) {
        return(NULL)
    }
    # get the cube object
    cube <- tibble::as_tibble(cache[["cube"]])
    # we ensure the classes of the cube loaded
    cube <- .cube_find_class(cube)
    # results have only the classes of the results (e.g., class_cube), as
    # when they are read from the images of the dataset
    if (!inherits(cube, "derived_cube")) {
        class(cube) <- .cube_s3class(cube)
    }
    # files from the cache cube must be valid
    has_valid_files <- .hf_cache_files(cube, repo)
    # if files are not valid, finish operation
    if (!has_valid_files) {
        return(NULL)
    }
    # the cache cube shared must contains what users select from the dataset
    has_user_request <- all(bands %in% .cube_bands(cube))
    has_user_request <- has_user_request && all(tiles %in% .cube_tiles(cube))
    # if not possible to accommodate user request, skip cache load to force
    # a reload operation
    if (!has_user_request) {
        warning(.conf("messages", ".hf_cache_select"), call. = FALSE)
        return(NULL)
    }
    # otherwise, select the bands of the cube
    if (.has(bands)) {
        cube <- .cube_filter_bands(cube, bands)
    }
    # select the tiles of the cube
    if (.has(tiles)) {
        cube <- dplyr::filter(cube, .data[["tile"]] %in% !!tiles)
    }
    # select the period of the cube (each date informed is a limit of it)
    if (.has(start_date) || .has(end_date)) {
        # get timeline
        timeline <- .as_date(unlist(.cube_timeline(cube)))
        # select interval
        cube <- .cube_filter_interval(
            cube,
            start_date = .default(start_date, min(timeline)),
            end_date = .default(end_date, max(timeline))
        )
    }
    # post-condition - we must have at least one row
    .check_that(nrow(cube) > 0L)
    # return!
    cube
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
    # (if available) get the labels described by the dataset
    labels_conf <- .try(
        .conf("sources", source, "collections", collection, "labels"),
        .default = NULL
    )
    # labels informed by the user have precedence over the dataset
    labels <- .default(labels, unlist(labels_conf))
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

#' @title Persist the access token of a HuggingFace dataset
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description Images of a dataset are read by gdal, which is configured to
#' sign its requests with the token of the user every time the data of a cube
#' is read.
#'
#' @param cube Data cube.
#'
#' @return A sits cube.
#' @export
.cube_token_generator.hf_cube <- function(cube) {
    # set caller
    .check_set_caller(".cube_token_generator_hf")
    # persist token for gdal
    .hf_token_persist()
    # return!
    cube
}

#' @title Flush the access token of a HuggingFace dataset
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description The gdal configuration of the session is restored when the data
#' of a cube is not being read anymore, so the token of the user is not
#' informed to other services.
#'
#' @param cube Data cube.
#'
#' @return A sits cube.
#' @export
.cube_token_flush.hf_cube <- function(cube) {
    # set caller
    .check_set_caller(".cube_token_flush_hf")
    # flush token of gdal
    .hf_token_flush()
    # return!
    cube
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
                                 memsize = 2L,
                                 cache = TRUE) {
    # if tiles are specified by the user, we assume they must be available
    # in the dataset that will be loaded
    check_tiles <- .has(tiles)
    # a roi is converted to tiles when the collection uses a
    # grid system that sits known
    if (.has(roi)) {
        tiles <- .hf_roi_tiles(source, collection, roi)
    }
    # a dataset can share the cube of its images, prepared by its provider,
    # which is read instead of describing every image of the dataset
    cube <- NULL
    if (cache) {
        cube <- .hf_cache_cube(
            source = source,
            collection = collection,
            bands = bands,
            tiles = tiles,
            start_date = start_date,
            end_date = end_date
        )
    }
    # get collection definition
    collection_config <- .conf("sources", source, "collections", collection)
    # results produced by sits are read as results cubes
    is_results <- .hf_collection_is_results(collection_config)
    # the images of a dataset are described only when it shares no cube
    if (.has_not(cube)) {
        if (is_results) {
            # if dataset files can be represented as a results cube
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
            # otherwise, we try to load the dataset files as an "image" cube,
            # which includes surface reflectance, embeddings cube and friends
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

# ---- hf repository ----
#' @title Select the file read from a HuggingFace repository
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description A repository can share a data cube (described by a
#' \code{"sits/sits.yml"} file), samples or models (\code{".rds"} or
#' \code{".parquet"} files). This function selects the file read:
#'
#' \itemize{
#'   \item{when \code{file} is informed, it must be a file of the repository;}
#'
#'   \item{or, when a repository describing a cube is read as a cube;}
#'
#'   \item{otherwise, the repository must have a single file sits reads.
#'   When there are many, users must choose one - we never guess.}
#' }
#'
#' @param repo HuggingFace repository name (\code{"<user>/<repository>"}).
#' @param file File to be read (optional).
#' @param type Type of the repository (\code{"dataset"} or \code{"model"}).
#'
#' @return File name in the repository.
.hf_repo_file <- function(repo, file, type) {
    # set caller
    .check_set_caller(".hf_repo_file")
    # list the files of the repository
    files <- .hf_dataset(repo, type)
    files <- files[["siblings"]]
    files <- purrr::map_chr(files, "rfilename")
    # the cube description of the repository
    config_file <- .hf_sits_file(.conf("hf", "config_file"))
    # get the extensions of the files
    files_ext <- tolower(.file_ext(files))
    # validation - the files must have a valid extension
    is_readable <- files_ext %in% .conf("hf", "files_extensions")
    # validation - we can't read files in the directory `sits`
    is_sits <- startsWith(files, .hf_sits_file(""))
    # define the valid files from the repository
    candidates <- files[is_readable & !is_sits]
    # file informed must be a file sits reads in the repository
    if (.has(file)) {
        # the user specified file, must be read from the repository
        is_valid <- file %in% c(intersect(config_file, files), candidates)
        .check_that(
            is_valid, msg = .conf("messages", ".hf_repo_file_missing")
        )
        # return already validated file
        return(file)
    }
    # repository describing a cube is read as a cube
    if (config_file %in% files) {
        return(config_file)
    }
    # otherwise, the repository must have a file sits reads
    .check_that(
        .has(candidates), msg = .conf("messages", ".hf_repo_file_empty")
    )
    # and only a single file can be selected
    .check_that(
        length(candidates) == 1L,
        msg = paste(.conf("messages", ".hf_repo_file"), toString(candidates))
    )
    # return!
    candidates
}

#' @title Identify the content of a HuggingFace repository
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description The content of a repository is defined by the file read: its
#' cube description (\code{"sits/sits.yml"}), or a \code{".rds"} or
#' \code{".parquet"} file. This function sets the class of the repository,
#' so the file is read as the content it holds.
#'
#' @param repo HuggingFace repository name (\code{"<user>/<repository>"}).
#' @param file File read from the repository (see \code{.hf_repo_file()}).
#'
#' @return Repository name, with the class of its content.
.hf_repo_new <- function(repo, file) {
    # by default, we assume the content is a cube
    # a repository describing a cube is read as a cube
    content <- "cube"
    # otherwise, the content is defined by the type of the file
    is_cube <- identical(file, .hf_sits_file(.conf("hf", "config_file")))
    if (!is_cube) {
        content <- tolower(.file_ext(file))
    }
    # return!
    .set_class(repo, paste0("hf_repo_", content), "hf_repo", class(repo))
}

#' @title Get the source of a HuggingFace repository
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description A dataset shared by a HuggingFace user is registered in
#' \code{sits} as a collection of the source \code{"HF:<user>"}. This
#' function returns the source of the repository.
#'
#' @param repo HuggingFace repository name (\code{"<user>/<repository>"}).
#'
#' @return Data source (\code{"HF:<USER>"}).
.hf_repo_source <- function(repo) {
    toupper(paste0(.conf("hf", "source_prefix"), dirname(repo)))
}

#' @title Get the collection of a HuggingFace repository
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description A dataset shared by a HuggingFace user is registered in
#' \code{sits} as a collection named after the repository.
#'
#' @param repo HuggingFace repository name (\code{"<user>/<repository>"}).
#'
#' @return Image collection.
.hf_repo_collection <- function(repo) {
    toupper(basename(repo))
}

# ---- hf download ----
#' @title Get the requests available to download files from HuggingFace
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description HuggingFace limits the files requested by a user (or by an IP,
#' for anonymous users) in fixed windows of time (usually 5 minutes). Every response 
#' from HuggingFace informs how many requests remain in the window and when it resets.
#' This function reads these values with a single request to a file of the repository.
#'
#' @param repo HuggingFace repository name.
#' @param file File of the repository used in the request.
#' @param type Type of the repository (\code{"dataset"} or \code{"model"}).
#'
#' @return A list with the \code{remaining} requests and the seconds to
#' \code{reset} the window, or NULL when HuggingFace does not inform them.
.hf_download_limit <- function(repo, file, type = "dataset") {
    # request the file headers
    # (using any token, as limits are per user)
    response <- .try(
        {
            .head_request(
                url = .hf_file_url(repo, file, type),
                headers = .hf_headers()
            )
        },
        .default = NULL
    )
    # get the limits from the response
    limit <- NULL
    if (.has(response)) {
        limit <- .response_header(response, .conf("hf", "rate_limit_header"))
    }
    # if limits are not informed, there is nothing to control
    if (.has_not(limit)) {
        return(NULL)
    }
    # parse remaining requests and seconds to reset
    values <- regexec(.conf("hf", "rate_limit_regex"), limit)
    values <- regmatches(limit, values)[[1L]]
    # if limits are not in the expected format, there is nothing to control
    if (.has_not(values)) {
        return(NULL)
    }
    # return!
    list(
        remaining = as.integer(values[[2L]]),
        reset = as.integer(values[[3L]])
    )
}

#' @title Get the number of files that can be downloaded from HuggingFace
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description Each file downloaded is a request (in HuggingFace resolver).
#' This function defines how many files can be downloaded in the current window 
#' of HuggingFace, keeping a margin of the remaining requests to retry downloads 
#' that fail. When no request is available, it waits until the window resets.
#'
#' The limit is per user, not per core: \code{multicores} only defines how many
#' of these files are downloaded at the same time.
#'
#' @param repo HuggingFace repository name.
#' @param file File of the repository used to request the limits.
#' @param n_files Number of files to be downloaded.
#'
#' @return Number of files to be downloaded in the current window.
.hf_download_budget <- function(repo, file, n_files) {
    # get the limits of the user
    limit <- .hf_download_limit(repo, file)
    # when limits are unknown, we download everything and let the
    # retries of the download handle any rejected request
    if (.has_not(limit)) {
        return(n_files)
    }
    # requests available, keeping a margin for retries
    margin <- .conf("hf", "rate_limit_margin")
    # budget means here how many requests I have available to download files
    # so here we use a simple idea of give the budget but remove a margin
    budget <- floor(limit[["remaining"]] * (1 - margin))
    # if no request is available, wait for the next window
    if (budget < 1L) {
        # inform user
        message(paste(.conf("messages", ".hf_download_wait"), limit[["reset"]]))
        # wait the window to reset
        Sys.sleep(limit[["reset"]])
        # and try again!
        return(.hf_download_budget(repo, file, n_files))
    }
    # return!
    min(budget, n_files)
}

#' @title Download assets of a HuggingFace data cube
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description Downloads the assets that fit the requests available in the
#' current window of HuggingFace, and the remaining ones in the next windows.
#'
#' @param assets Assets of the cube (one image per row).
#' @param repo HuggingFace repository name.
#' @param output_dir Directory where images will be saved.
#' @param n_tries Number of attempts to download the same image.
#' @param progress Show progress bar?
#'
#' @return List with one local asset per asset (NULL when the download of the
#' asset failed).
.hf_download_assets <- function(assets, repo, output_dir, n_tries, progress) {
    # nothing left to download
    if (nrow(assets) == 0L) {
        return(list())
    }
    # assets fitting the current window
    # (limits are requested using the cube description of the repository)
    cfg_file <- .hf_sits_file(.conf("hf", "config_file"))
    n_batch <- .hf_download_budget(
        repo = repo,
        file = cfg_file,
        n_files = nrow(assets)
    )
    # define batches for each group of assets
    batch <- seq_len(n_batch)
    # download batch
    local_assets <- .jobs_map_parallel(assets[batch, ], function(asset) {
        .download_asset(
            asset = asset,
            roi = NULL,
            res = NULL,
            n_tries = n_tries,
            output_dir = output_dir
        )
    }, progress = progress)
    # download remaining assets in the next windows
    c(
        local_assets,
        .hf_download_assets(
            assets = assets[-batch, ],
            repo = repo,
            output_dir = output_dir,
            n_tries = n_tries,
            progress = progress
        )
    )
}

#' @title Download the images of a HuggingFace data cube
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description Reading images of a remote cube (e.g., by GDAL) makes many
#' requests for each image, which quickly reaches the limits of HuggingFace.
#' So, for \code{sits}, downloading an image is a single request. This function 
#' downloads the images of a cube, as \code{sits_cube_copy()} does, in batches 
#' that fit the requests available to the user in each window of HuggingFace.
#'
#' Images already downloaded in \code{output_dir} are not requested again, so
#' an interrupted download is resumed when the function is called again.
#'
#' @param cube Data cube from a HuggingFace dataset.
#' @param repo HuggingFace repository name.
#' @param output_dir Directory where images will be saved.
#' @param n_tries Number of attempts to download the same image.
#' @param multicores Number of cores for parallel downloading.
#' @param progress Show progress bar?
#'
#' @return Data cube with the images in \code{output_dir}.
.hf_download_cube <- function(cube, repo, output_dir, n_tries,
                              multicores, progress) {
    # prepare parallel processing
    started <- .parallel_start(workers = multicores)
    on.exit(.parallel_stop(started), add = TRUE)
    # each image of the cube is a request
    cube_assets <- .cube_split_assets(cube)
    # download images in batches fitting the limits of HuggingFace
    local_assets <- .hf_download_assets(
        assets = cube_assets,
        repo = repo,
        output_dir = output_dir,
        n_tries = n_tries,
        progress = progress
    )
    # assets that exhausted their download attempts come back as `NULL`. Report
    # them so users at least stay aware about the issues
    .message_warnings_cube_copy_missing(
        cube_assets[purrr::map_lgl(local_assets, is.null), ]
    )
    # bind all assets
    cube_assets <- dplyr::bind_rows(local_assets)
    # check assets
    .check_empty_data_frame(cube_assets)
    # merge tiles
    cube_assets <- .cube_merge_tiles(cube_assets)
    # update assets class
    class(cube_assets) <- class(cube)
    # return!
    cube_assets
}

# ---- hf repository content ----
#' @title Describe the data cube of a HuggingFace repository
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description Registers the collection described in a HuggingFace dataset
#' and creates its data cube through the source API.
#'
#' @param repo       Repository identified.
#' @param ...        Other parameters of the source API (e.g., \code{labels}).
#' @param bands      Bands to be selected (NULL for all).
#' @param tiles      Tiles to be selected (optional).
#' @param roi        Region of interest (optional).
#' @param crs        The Coordinate Reference System (CRS) of the roi.
#' @param start_date Start date (optional).
#' @param end_date   End date (optional).
#' @param multicores Number of cores.
#' @param progress   Show progress bar?
#'
#' @return A data cube with images in the HuggingFace dataset.
.hf_repo_cube <- function(repo, ...,
                          bands,
                          tiles,
                          roi,
                          crs,
                          start_date,
                          end_date,
                          multicores,
                          progress) {
    # the repository is a collection of the HuggingFace user
    source <- .hf_repo_source(repo)
    collection <- .hf_repo_collection(repo)
    # register the collection described in the dataset
    .hf_source_register(source, collection)
    # ensures that there are no duplicate tiles
    if (.has(tiles)) {
        tiles <- unique(tiles)
    }
    # converts provided roi to sf
    if (.has(roi)) {
        roi <- .roi_as_sf(roi, default_crs = crs)
    }
    # by default, all bands of the collection are selected
    bands <- .default(bands, .source_bands(source, collection))
    # bands of results produced by sits are in lower case (classified maps
    # not produced by sits, with `class_cube: true`, have a "CLASS" band)
    collection_conf <- .conf("sources", source, "collections", collection)
    if (.hf_collection_is_results(collection_conf)) {
        bands <- tolower(bands)
    }
    # pre-condition - checks if the bands are supported by the collection
    .check_bands_collection(
        source = source,
        collection = collection,
        bands = bands
    )
    # builds a sits data cube
    cube <- .source_cube(
        source = source,
        collection = collection,
        bands = bands,
        tiles = tiles,
        roi = roi,
        start_date = start_date,
        end_date = end_date,
        platform = NULL,
        multicores = multicores,
        progress = progress, ...
    )
    # flush any defined token
    .cube_token_flush(cube)
}

#' @title Read the content of a HuggingFace repository
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description Reads the content of a repository identified by
#' \code{.hf_repo_new()}: a data cube (downloaded to \code{output_dir}),
#' samples or models.
#'
#' @param repo Repository identified by \code{.hf_repo_new()}.
#' @param file File read from the repository (see \code{.hf_repo_file()}).
#' @param type Type of the repository (\code{"dataset"} or \code{"model"}).
#' @param ...  Parameters of the content read (see \code{sits_from_hf()}).
#'
#' @return A data cube, a set of samples, or a model.
.hf_repo_read <- function(repo, file, type, ...) {
    UseMethod(".hf_repo_read", repo)
}

#' @export
.hf_repo_read.hf_repo_cube <- function(repo, file, type, ...,
                                       bands,
                                       tiles,
                                       roi,
                                       crs,
                                       start_date,
                                       end_date,
                                       output_dir,
                                       n_tries,
                                       multicores,
                                       progress) {
    # set caller
    .check_set_caller("sits_from_hf")
    # pre-condition - in case of multiple cubes, the type must be a cube
    .check_that(
        identical(type, .conf("hf", "cube_repo_type")),
        msg = .conf("messages", ".hf_repo_read_cube")
    )
    # define progress bar
    progress <- .message_progress(progress)
    # pre-condition - parameters must work
    .check_num_min_max(x = n_tries, min = 1L, max = 50L)
    .check_int_parameter(multicores, min = 1L, max = 2048L)
    .check_chr_parameter(output_dir, len_max = 1L)
    # pre-condition - output directory must be a valid path
    output_dir <- .file_path_expand(output_dir)
    .check_output_dir(output_dir)
    # images are opened to describe the cube without searching auxiliary
    # files, which would be counted in the limits of HuggingFace
    gdal_env_old <- .hf_gdal_persist()
    on.exit(.hf_gdal_flush(gdal_env_old), add = TRUE)
    # describe the cube, using the cache shared in the dataset when available
    cube <- .hf_repo_cube(
        repo = repo, ...,
        bands = bands,
        tiles = tiles,
        roi = roi,
        crs = crs,
        start_date = start_date,
        end_date = end_date,
        multicores = multicores,
        progress = progress
    )
    # download the images of the cube
    .hf_download_cube(
        cube = cube,
        repo = .source_collection_name(
            .hf_repo_source(repo),
            .hf_repo_collection(repo)
        ),
        output_dir = output_dir,
        n_tries = n_tries,
        multicores = multicores,
        progress = progress
    )
}

#' @export
.hf_repo_read.hf_repo_rds <- function(repo, file, type, ...) {
    # set caller
    .check_set_caller(".hf_repo_read_rds")
    # download file
    file_local <- .hf_file_download(repo, file, type)
    on.exit(unlink(file_local), add = TRUE)
    # read file
    data <- readRDS(file_local)
    # only models and samples of sits are read
    .check_that(inherits(data, c("sits_model", "sits")))
    # return!
    data
}

#' @export
.hf_repo_read.hf_repo_parquet <- function(repo, file, type, ...) {
    # download file
    file_local <- .hf_file_download(repo, file, type)
    on.exit(unlink(file_local), add = TRUE)
    # read samples!
    .parquet_from_file(file_local)
}

#' @export
.hf_repo_read.default <- function(repo, file, type, ...) {
    stop(.conf("messages", ".hf_repo_read_default"))
}
