#' Cube API
#'
#' A \code{cube} is a \code{tibble} containing information on how to access
#' some data cube. Each row of a \code{cube} is a \code{tile}, which represents
#' a rectangular spatial region of space in some projection.
#' For more details, see tiles API.
#'
#' @param fn     A function.
#' @param roi    A region of interest (ROI).
#' @param start_date,end_date Date of start and end.
#' @param bands  A set of band names.
#' @param tiles  A set of tile names.
#' @param ...    Additional arguments (see details).
#'
#' @returns See description of each function.
#' @family cube and tile functions
#' @keywords internal
#' @name cube_api
#' @noRd
NULL

# ---- cube class utilities ----
#' @title Strategy function to define a default data cube classes
#' @name .cube_class_strategy_default
#' @keywords internal
#' @noRd
#' @param  base_class   Base cube class.
#' @param  source       Cube source.
#' @param  collection   Cube collection.
#' @param  s3_classs    S3 class defined for the cube.
#' @param  cube_class   Current cube class.
#' @return cube classes
.cube_class_strategy_default <- function(
        base_class, source, collection, s3_class, cube_class, ...
) {
    unique(c(base_class, s3_class, cube_class))
}
#' @title Strategy function to define `SAR (GRD)` data cube classes
#' @name .cube_class_strategy_sar-grd
#' @keywords internal
#' @noRd
#' @param  base_class   Base cube class.
#' @param  source       Cube source.
#' @param  collection   Cube collection.
#' @param  s3_classs    S3 class defined for the cube.
#' @param  cube_class   Current cube class.
#' @return cube classes
`.cube_class_strategy_sar-grd` <- function(
        base_class, source, collection, s3_class, cube_class, ...
) {
    is_sar <- .try({
        .conf("sources", source, "collections", collection, "sar_cube")
    },
        .default = FALSE
    )
    is_sar <- is_sar && !grepl("rtc", base_class, fixed = TRUE)
    if (is_sar) {
        return(unique(
            c(base_class, "grd_cube", "sar_cube", s3_class, cube_class)
        ))
    }
}
#' @title Strategy function to define `SAR (RTC)` data cube classes
#' @name `.cube_class_strategy_sar-rtc`
#' @keywords internal
#' @noRd
#' @param  base_class   Base cube class.
#' @param  source       Cube source.
#' @param  collection   Cube collection.
#' @param  s3_classs    S3 class defined for the cube.
#' @param  cube_class   Current cube class.
#' @return cube classes
`.cube_class_strategy_sar-rtc` <- function(
        base_class, source, collection, s3_class, cube_class, ...) {
    is_sar <- .try({
        .conf("sources", source, "collections", collection, "sar_cube")
    },
        .default = FALSE
    )
    is_sar <- is_sar && grepl("rtc", base_class, fixed = TRUE)

    if (is_sar) {
        return(unique(
            c(base_class, "rtc_cube", "sar_cube", s3_class, cube_class)
        ))
    }
}
#' @title Strategy function to define a `DEM` data cube class
#' @name .cube_class_strategy_dem
#' @keywords internal
#' @noRd
#' @param  base_class   Base cube class.
#' @param  source       Cube source.
#' @param  collection   Cube collection.
#' @param  s3_classs    S3 class defined for the cube.
#' @param  cube_class   Current cube class.
#' @return cube classes
.cube_class_strategy_dem  <- function(
        base_class, source, collection, s3_class, cube_class, ...
) {
    is_dem <- .try({
        .conf("sources", source, "collections", collection, "dem_cube")
    },
        .default = FALSE
    )

    if (is_dem) {
        return(unique(
            c(base_class, "dem_cube", s3_class, cube_class)
        ))
    }
}
#' @title Strategy function to define a `Rainfall` data cube class
#' @name .cube_class_strategy_rainfall
#' @keywords internal
#' @noRd
#' @param  base_class   Base cube class.
#' @param  source       Cube source.
#' @param  collection   Cube collection.
#' @param  s3_classs    S3 class defined for the cube.
#' @param  cube_class   Current cube class.
#' @return cube classes
.cube_class_strategy_rainfall  <- function(
        base_class, source, collection, s3_class, cube_class, ...
) {
    is_rainfall <- grepl("rainfall", base_class, fixed = TRUE)
    if (is_rainfall) {
        return(unique(
            c(base_class, "rainfall_cube", s3_class, cube_class)
        ))
    }
}
#' @title Strategy function to define a `Class` data cube class
#' @name .cube_class_strategy_class
#' @keywords internal
#' @noRd
#' @param  base_class   Base cube class.
#' @param  source       Cube source.
#' @param  collection   Cube collection.
#' @param  s3_classs    S3 class defined for the cube.
#' @param  cube_class   Current cube class.
#' @return cube classes
.cube_class_strategy_class <- function(
    base_class, source, collection, s3_class, cube_class, ...
) {
    is_class <- .try({
        .conf("sources", source, "collections", collection, "class_cube")
    },
        .default = FALSE
    )

    if (is_class) {
        # explicitly defining a `class_cube` following the definition from the
        # `sits_label_classification` function.
        return(
            c(
                "class_cube", "derived_cube", "raster_cube",
                base_class, "tbl_df", "tbl", "data.frame"
            )
        )
    }
}
#' @title Registry of class definition strategies
#' @name .cube_class_rules
#' @keywords internal
#' @noRd
#' @return class strategies
.cube_define_class_strategies <- function() {
    c(
        # SAR cube
        `.cube_class_strategy_sar-grd`,
        # SAR-RTC cube
        `.cube_class_strategy_sar-rtc`,
        # DEM cube
        .cube_class_strategy_dem,
        # Rainfall cube
        .cube_class_strategy_rainfall,
        # Class cube
        .cube_class_strategy_class
    )
}
#' @title Define data cube class based on a set of rules
#' @name .cube_define_class
#' @keywords internal
#' @noRd
#' @param  base_class   Base cube class.
#' @param  source       Cube source.
#' @param  collection   Cube collection.
#' @param  s3_classs    S3 class defined for the cube.
#' @param  cube_class   Current cube class.
#' @return cube classes
.cube_define_class <- function(
        base_class, source, collection, s3_class, cube_class, ...
) {
    # guess the class cube using the rules from the registry
    cube_class_new <- purrr::map(.cube_define_class_strategies(), function(fn) {
        fn(
            base_class,
            source,
            collection,
            s3_class,
            cube_class,
            ...
        )
    })
    # remove invalid values
    cube_class_new <- purrr::flatten_chr(cube_class_new)
    # use the default cube if any class was found
    if (length(cube_class_new) == 0) {
        cube_class_new <- .cube_class_strategy_default(
            base_class,
            source,
            collection,
            s3_class,
            cube_class,
            ...
        )
    }
    # return!
    cube_class_new
}

#' @title Sets the class of a data cube
#' @noRd
#' @param cube  A data cube.
#' @param ...  Provide additional class names.
#' @return   An updated data cube.
.cube_set_class <- function(cube, ...) {
    .set_class(cube, ..., c("raster_cube", "tbl_df", "tbl", "data.frame"))
}
#' @title Finds the class of a data cube
#' @name .cube_find_class
#' @noRd
#' @param cube  A data cube.
#' @return     The class of the data cube (if existing)
.cube_find_class <- function(cube) {
    .check_set_caller(".cube_find_class")
    .check_na_null_parameter(cube)
    UseMethod(".cube_find_class", cube)
}
#' @export
#'
.cube_find_class.raster_cube <- function(cube) {
    return(cube)
}
#' @export
#'
.cube_find_class.tbl_df <- function(cube) {
    cube <- tibble::as_tibble(cube)
    if (all(.conf("sits_cube_cols") %in% colnames(cube))) {
        class(cube) <- c("raster_cube", class(cube))
    } else {
        stop(.conf("messages", ".cube_find_class"))
    }
    if (all(.cube_bands(cube) %in% .conf("sits_probs_bands"))) {
        class(cube) <- c("probs_cube", "derived_cube", class(cube))
    } else if (all(.cube_bands(cube) == "class")) {
        class(cube) <- c("class_cube", "derived_cube", class(cube))
    } else if (all(.cube_bands(cube) == "variance")) {
        class(cube) <- c("variance_cube", "derived_cube", class(cube))
    } else if (all(.cube_bands(cube) %in% .conf("sits_uncert_bands"))) {
        class(cube) <- c("uncert_cube", "derived_cube", class(cube))
    } else {
        class(cube) <- c("eo_cube", class(cube))
    }
    return(cube)
}
#' @export
.cube_find_class.default <- function(cube) {
    if (is.list(cube)) {
        class(cube) <- c("list", class(cube))
        cube <- tibble::as_tibble(cube)
        cube <- .cube_find_class(cube)
    } else {
        stop(.conf("messages", ".cube_find_class"))
    }
    return(cube)
}

# ---- cube manipulation ----
#' @title Creates the description of a data cube
#' @name .cube_create
#' @keywords internal
#' @noRd
#'
#' @description Print information and save metadata about a data cube.
#'
#' @param source      Source of data
#' @param collection  Image collection
#' @param satellite   Name of satellite
#' @param sensor      Name of sensor
#' @param tile        Tile of the image collection
#' @param xmin        Spatial extent (xmin).
#' @param ymin        Spatial extent (ymin).
#' @param xmax        Spatial extent (xmax).
#' @param ymax        Spatial extent (ymin).
#' @param crs         CRS for cube (EPSG code or PROJ4 string).
#' @param file_info   Tibble with information about files
#'
#' @return  A tibble containing a data cube
#'
.cube_create <- function(source,
                         collection,
                         satellite,
                         sensor,
                         tile,
                         xmin,
                         xmax,
                         ymin,
                         ymax,
                         crs,
                         labels = NULL,
                         file_info = NULL) {
    # create a tibble to store the metadata (mandatory parameters)
    cube <- .common_size(
        source = source,
        collection = collection,
        satellite = satellite,
        sensor = sensor,
        tile = tile,
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax,
        crs = crs
    )
    # if there are labels, include them
    if (.has(labels)) {
        cube <- tibble::add_column(cube, labels = list(labels))
    }
    # if there are file_info, include it
    if (.has(file_info)) {
        cube <- tibble::add_column(cube, file_info = list(file_info))
    }
    .cube_set_class(cube)
}
#' @title Identity function for data cubes
#' @keywords internal
#' @noRd
#' @name .cube
#' @param x  cube
#'
#' @return data cube object.
.cube <- function(x) {
    # return the cube
    x
}
#' @title Get base info from a data cube
#' @keywords internal
#' @noRd
#' @name .cube
#' @param x  cube
#'
#' @return data cube from base_info
.cube_base_info <- function(x) {
    # return base info data cube
    dplyr::bind_rows(x[["base_info"]])
}
#' @title Return areas of classes of a class_cube
#' @keywords internal
#' @noRd
#' @name .cube_class_areas
#' @param cube       class cube
#'
#' @return A \code{vector} with the areas of the cube labels.
.cube_class_areas <- function(cube) {
    # Get area for each class for each row of the cube
    freq_lst <- slider::slide(cube, function(tile) {
        # Get the frequency count and value for each labelled image
        freq <- .tile_area_freq(tile)
        return(freq)
    })
    # Get a tibble by binding the row (duplicated labels with different counts)
    freq <- do.call(rbind, freq_lst)
    # summarize the counts for each label
    freq <- freq |>
        dplyr::filter(!is.na(class)) |>
        dplyr::group_by(class) |>
        dplyr::summarise(area = sum(.data[["area"]]))

    # Area is taken as the sum of pixels
    class_areas <- freq[["area"]]
    # Names of area are the classes
    names(class_areas) <- freq[["class"]]
    # NAs are set to 0
    class_areas[is.na(class_areas)] <- 0
    return(class_areas)
}

#' @title Return bands of a data cube
#' @keywords internal
#' @noRd
#' @name .cube_bands
#' @param cube       Data cube
#' @param add_cloud  Include the cloud band?
#'
#' @return A \code{vector} with the cube bands.
.cube_bands <- function(cube, add_cloud = TRUE, dissolve = TRUE) {
    UseMethod(".cube_bands", cube)
}
#' @export
.cube_bands.raster_cube <- function(cube,
                                    add_cloud = TRUE,
                                    dissolve = TRUE) {
    bands <- .compact(slider::slide(cube, .tile_bands, add_cloud = add_cloud))
    if (dissolve) {
        return(.dissolve(bands))
    }
    bands
}
#' @export
.cube_bands.tbl_df <- function(cube,
                               add_cloud = TRUE,
                               dissolve = TRUE) {
    cube <- tibble::as_tibble(cube)
    if (all(.conf("sits_cube_cols") %in% colnames(cube))) {
        class(cube) <- c("raster_cube", class(cube))
        bands <- .cube_bands(cube)
    } else {
        stop(.conf("messages", ".cube_bands"))
    }
    return(bands)
}
#' @export
.cube_bands.default <- function(cube,
                                add_cloud = TRUE,
                                dissolve = TRUE) {
    if (is.list(cube)) {
        class(cube) <- c("list", class(cube))
        cube <- tibble::as_tibble(cube)
        bands <- .cube_bands(cube, add_cloud, dissolve)
    } else {
        stop(.conf("messages", ".cube_bands"))
    }
    return(bands)
}
#' @title Return labels of a data cube
#' @keywords internal
#' @noRd
#' @name .cube_labels
#' @param cube       Data cube
#' @param dissolve   Combine repeated labels?
#'
#' @return A \code{vector} with the cube bands.
.cube_labels <- function(cube, dissolve = TRUE) {
    UseMethod(".cube_labels", cube)
}
#' @export
.cube_labels.derived_cube <- function(cube, dissolve = FALSE) {
    return(cube[["labels"]][[1]])
}
#' @export
.cube_labels.raster_cube <- function(cube, dissolve = TRUE) {
    labels <- .compact(slider::slide(cube, .tile_labels))
    if (dissolve) {
        return(.dissolve(labels))
    }
    return(labels)
}
#' @export
.cube_labels.tbl_df <- function(cube, dissolve = TRUE) {
    cube <- tibble::as_tibble(cube)
    if (all(.conf("sits_cube_cols") %in% colnames(cube))) {
        class(cube) <- c("raster_cube", class(cube))
        labels <- .cube_labels(cube)
    } else {
        stop(.conf("messages", "cube_labels"))
    }
    return(labels)
}
#' @export
.cube_labels.default <- function(cube, dissolve = TRUE) {
    if (is.list(cube)) {
        class(cube) <- c("list", class(cube))
        cube <- tibble::as_tibble(cube)
        labels <- .cube_labels(cube, dissolve)
        return(labels)
    } else {
        stop(.conf("messages", "cube_labels"))
    }
}
#' @title Return collection of a data cube
#' @keywords internal
#' @noRd
#' @name .cube_collection
#' @param cube  data cube
#' @return collection associated to the cube
.cube_collection <- function(cube) {
    UseMethod(".cube_collection", cube)
}
#' @export
.cube_collection.raster_cube <- function(cube) {
    .compact(slider::slide_chr(cube, .tile_collection))
}
#' @export
.cube_collection.default <- function(cube) {
    if (is.list(cube)) {
        cube <- tibble::as_tibble(cube)
        cube <- .cube_find_class(cube)
        collection <- .cube_collection(cube)
        return(collection)
    } else {
        stop(.conf("messages", "cube_collection"))
    }
}
#' @title Return crs of a data cube
#' @keywords internal
#' @noRd
#' @name .cube_crs
#' @param cube  data cube
#' @return crs associated to the cube
.cube_crs <- function(cube) {
    UseMethod(".cube_crs", cube)
}
#' @export
.cube_crs.raster_cube <- function(cube) {
    .compact(slider::slide_chr(cube, .tile_crs))
}
#' @export
.cube_crs.default <- function(cube) {
    cube <- tibble::as_tibble(cube)
    cube <- .cube_find_class(cube)
    crs <- .cube_crs(cube)
    return(crs)
}
#' @title Return period of a data cube
#' @keywords internal
#' @noRd
#' @name .cube_period
#' @param cube  data cube
#' @return period in days associated to the cube
.cube_period <- function(cube) {
    UseMethod(".cube_period", cube)
}
#' @export
.cube_period.raster_cube <- function(cube) {
    .compact(slider::slide_int(cube, .tile_period))
}
#' @export
.cube_period.default <- function(cube) {
    cube <- tibble::as_tibble(cube)
    cube <- .cube_find_class(cube)
    period <- .cube_period(cube)
    return(period)
}
#' @title Adjust crs of a data cube
#' @keywords internal
#' @noRd
#' @name .cube_adjust_crs
#' @param cube  data cube
#' @return data cube with adjusted crs
.cube_adjust_crs <- function(cube) {
    UseMethod(".cube_adjust_crs", cube)
}
#' @export
.cube_adjust_crs.grd_cube <- function(cube) {
    cube[["crs"]] <- "EPSG:4326"
    return(cube)
}
#' @export
.cube_adjust_crs.default <- function(cube) {
    return(cube)
}
#' @title Adjust cube tile name
#' @keywords internal
#' @noRd
#' @name .cube_convert_tile_name
#' @param cube  data cube
#' @return data cube with adjusted tile name
.cube_convert_tile_name <- function(cube) {
    dplyr::mutate(
        cube,
        tile = ifelse(
            .data[["tile"]] == "NoTilingSystem",
            paste0(.data[["tile"]], "-", dplyr::row_number()),
            .data[["tile"]])
    )
}
#' @title Adjust cube tile name
#' @keywords internal
#' @noRd
#' @name .cube_revert_tile_name
#' @param cube  data cube
#' @return data cube with adjusted tile name
.cube_revert_tile_name <- function(cube) {
    dplyr::mutate(
        cube,
        tile = ifelse(
            grepl("NoTilingSystem", .data[["tile"]]),
            "NoTilingSystem",
            .data[["tile"]]
        )
    )
}
#' @title Return the S3 class of the cube
#' @name .cube_s3class
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param  cube         input data cube
#' @return class of the cube
.cube_s3class <- function(cube) {
    UseMethod(".cube_s3class", cube)
}
#' @export
.cube_s3class.raster_cube <- function(cube) {
    # extract cube metadata
    source <-  .cube_source(cube = cube)
    collection <- .tile_collection(cube)
    s3_class <- .source_s3class(source = source)
    col_class <- paste(
        s3_class[[1]],
        tolower(collection),
        sep = "_"
    )
    # define cube class
    .cube_define_class(
        base_class = col_class,
        source = source,
        collection = collection,
        s3_class = s3_class,
        cube_class = class(cube)
    )
}
#' @export
.cube_s3class.default <- function(cube) {
    cube <- tibble::as_tibble(cube)
    cube <- .cube_find_class(cube)
    class <- .cube_s3class(cube)
    return(class)
}
#' @title Return the X resolution
#' @name .cube_xres
#' @keywords internal
#' @noRd
#'
#' @param  cube  input data cube
#' @return integer
.cube_xres <- function(cube) {
    .dissolve(slider::slide(cube, .tile_xres))
}
#' @title Return the Y resolution
#' @name .cube_yres
#' @keywords internal
#' @noRd
#'
#' @param  cube  input data cube
#' @return integer
.cube_yres <- function(cube) {
    .dissolve(slider::slide(cube, .tile_yres))
}
#' @title Return the column size of each tile
#' @name .cube_ncols
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param  cube  input data cube
#' @return integer
.cube_ncols <- function(cube) {
    UseMethod(".cube_ncols", cube)
}
#' @export
.cube_ncols.raster_cube <- function(cube) {
    .compact(slider::slide_int(cube, .tile_ncols))
}
#' @export
.cube_ncols.default <- function(cube) {
    cube <- tibble::as_tibble(cube)
    cube <- .cube_find_class(cube)
    ncols <- .cube_ncols(cube)
    return(ncols)
}
#' @title Return the row size of each tile
#' @name .cube_nrows
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param  cube  input data cube
#' @return integer
.cube_nrows <- function(cube) {
    UseMethod(".cube_nrows", cube)
}
#' @export
.cube_nrows.raster_cube <- function(cube) {
    .compact(slider::slide_int(cube, .tile_nrows))
}
#' @export
.cube_nrows.default <- function(cube) {
    cube <- tibble::as_tibble(cube)
    cube <- .cube_find_class(cube)
    nrows <- .cube_nrows(cube)
    return(nrows)
}
#' @title Get cube source
#' @name .cube_source
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#'@param  cube input data cube
#'
#'@return A character string
.cube_source <- function(cube) {
    UseMethod(".cube_source", cube)
}
#'@export
.cube_source.raster_cube <- function(cube) {
    # set caller to show in errors
    .check_set_caller(".cube_source")
    source <- .compact(slider::slide_chr(cube, .tile_source))
    .check_that(length(source) == 1)
    source
}
#'@export
.cube_source.default <- function(cube) {
    cube <- tibble::as_tibble(cube)
    cube <- .cube_find_class(cube)
    source <- .cube_source(cube)
    return(source)
}
#' @title Get start date from each tile in a cube
#' @noRd
#' @param cube  A data cube.
#' @return  A vector of dates.
.cube_start_date <- function(cube) {
    UseMethod(".cube_start_date", cube)
}
#' @export
.cube_start_date.raster_cube <- function(cube) {
    .as_date(unlist(.compact(slider::slide(cube, .tile_start_date))))
}
#' @export
.cube_start_date.default <- function(cube) {
    cube <- tibble::as_tibble(cube)
    cube <- .cube_find_class(cube)
    start_date <- .cube_start_date(cube)
    return(start_date)
}
#' @title Get end date from each tile in a cube
#' @noRd
#' @param cube  A data cube.
#' @return  A vector of dates.
.cube_end_date <- function(cube) {
    UseMethod(".cube_end_date", cube)
}
#' @export
.cube_end_date.raster_cube <- function(cube) {
    .as_date(unlist(.compact(slider::slide(cube, .tile_end_date))))
}
#' @export
.cube_end_date.default <- function(cube) {
    cube <- tibble::as_tibble(cube)
    cube <- .cube_find_class(cube)
    end_date <- .cube_end_date(cube)
    return(end_date)
}
#' @title Get timeline from each tile in a cube
#' @noRd
#' @param cube  A cube.
#' @details
#' Returns a unique timeline if there are a unique value. If there are at
#' least two different timelines, all timelines will be returned in a list.
#' @return A vector or list of dates.
.cube_timeline <- function(cube) {
    UseMethod(".cube_timeline", cube)
}
#' @export
.cube_timeline.raster_cube <- function(cube) {
    .compact(slider::slide(cube, .tile_timeline))
}
#' @export
.cube_timeline.default <- function(cube) {
    cube <- tibble::as_tibble(cube)
    cube <- .cube_find_class(cube)
    timeline <- .cube_timeline(cube)
    return(timeline)
}

#' @title Check if cube is complete
#' @noRd
#' @param cube  A cube.
#' @return      TRUE/FALSE
#' @details
#' Return
.cube_is_complete <- function(cube) {
    UseMethod(".cube_is_complete", cube)
}
#' @export
.cube_is_complete.raster_cube <- function(cube) {
    if (length(.cube_bands(cube, dissolve = FALSE)) > 1) {
        return(FALSE)
    }
    all(slider::slide_lgl(cube, .tile_is_complete))
}
#' @export
.cube_is_complete.default <- function(cube) {
    cube <- tibble::as_tibble(cube)
    cube <- .cube_find_class(cube)
    is_complete <- .cube_is_complete(cube)
    return(is_complete)
}
#' @title Check that cube is regular
#' @name .cube_is_regular
#' @keywords internal
#' @noRd
#' @param cube  datacube
#' @return Called for side effects.
.cube_is_regular <- function(cube) {
    .check_set_caller(".cube_is_regular")
    is_regular <- TRUE
    if (!.cube_is_complete(cube)) {
        is_regular <- FALSE
    }
    if (!.cube_has_unique_bbox(cube)) {
        is_regular <- FALSE
    }
    if (!.cube_has_unique_tile_size(cube)) {
        is_regular <- FALSE
    }
    if (length(.cube_timeline(cube)) > 1) {
        is_regular <- FALSE
    }
    return(is_regular)
}

#' @title Check that cube is a base cube
#' @name .cube_is_base
#' @keywords internal
#' @noRd
#' @param cube  datacube
#' @return Called for side effects.
.cube_is_base <- function(cube) {
    inherits(cube, "base_raster_cube")
}

#' @title Find out how many images are in cube during a period
#' @noRd
#' @param cube  A data cube.
#' @param period  Period character vector in ISO format.
#' @param origin  The first date to start count.
#' @details
#' Compute how many images were acquired in different periods
#' and different tiles.
#' @returns A tibble
.cube_timeline_acquisition <- function(cube, period, origin) {
    UseMethod(".cube_timeline_acquisition", cube)
}
#' @export
.cube_timeline_acquisition.raster_cube <- function(cube,
                                                   period = "P1D",
                                                   origin = NULL) {
    if (.has_not(origin)) {
        origin <- .cube_start_date(cube)
    }
    # get tiles and dates
    values <- .cube_foreach_tile(cube, function(tile) {
        tibble::tibble(
            tile = tile[["tile"]], dates = .tile_timeline(!!tile)
        )
    })
    # filter for starting date
    values <- dplyr::filter(values, !!origin <= .data[["dates"]])
    # organize by dates
    values <- dplyr::arrange(values, .data[["dates"]])
    # join tile/dates per period
    values <- slider::slide_period_dfr(
        values, values[["dates"]], .period_unit(period),
        function(x) {
            x[["from_date"]] <- min(x[["dates"]])
            x[["to_date"]] <- max(x[["dates"]])
            dplyr::count(
                x, .data[["from_date"]], .data[["to_date"]],
                .data[["tile"]]
            )
        },
        .every = .period_val(period), .origin = origin, .complete = TRUE
    )
    id_cols <- c("from_date", "to_date")
    if (all(values[["from_date"]] == values[["to_date"]])) {
        values[["date"]] <- values[["from_date"]]
        id_cols <- "date"
    }
    tidyr::pivot_wider(
        values,
        id_cols = dplyr::all_of(id_cols),
        names_from = "tile",
        values_from = "n"
    )
}
#' @export
.cube_timeline_acquisition.default <- function(cube,
                                               period = "P1D",
                                               origin = NULL) {
    cube <- tibble::as_tibble(cube)
    cube <- .cube_find_class(cube)
    values <- .cube_timeline_acquisition(cube, period, origin)
    return(values)
}
# ---- iteration ----
#' @title Tile iteration
#' @noRd
#' @param cube  A data cube.
#' @param fn  A function that receives and return a tile.
#' @param ...  Additional arguments to be passed to `fn`.
#' @details
#' Iterates over each cube tile, passing tile to function's first argument.
#' @returns  A processed data cube.
.cube_foreach_tile <- function(cube, fn, ...) {
    slider::slide_dfr(cube, fn, ...)
}
# ---- spatial ----
.cube_bbox <- function(cube, as_crs = NULL) {
    UseMethod(".cube_bbox", cube)
}
#' @export
.cube_bbox.raster_cube <- function(cube, as_crs = NULL) {
    .bbox(cube, as_crs = NULL, by_feature = TRUE)
}
#' @export
.cube_bbox.default <- function(cube, as_crs = NULL) {
    cube <- tibble::as_tibble(cube)
    cube <- .cube_find_class(cube)
    bbox <- .cube_bbox(cube, as_crs = as_crs)
    return(bbox)
}
.cube_as_sf <- function(cube, as_crs = NULL) {
    UseMethod(".cube_as_sf", cube)
}
#' @export
.cube_as_sf.raster_cube <- function(cube, as_crs = NULL) {
    .bbox_as_sf(.cube_bbox(cube), as_crs = as_crs)
}
#' @export
.cube_as_sf.default <- function(cube, as_crs = NULL) {
    cube <- tibble::as_tibble(cube)
    cube <- .cube_find_class(cube)
    sf_obj <- .cube_as_sf(cube, as_crs = as_crs)
    return(sf_obj)
}
#' @title What tiles intersect \code{roi} parameter?
#' @noRd
#' @param cube  A data cube.
#' @param roi  A region of interest (ROI).
#' @return A logical vector.
.cube_intersects <- function(cube, roi) {
    UseMethod(".cube_intersects", cube)
}
#' @export
.cube_intersects.raster_cube <- function(cube, roi) {
    .compact(slider::slide_lgl(cube, .tile_intersects, roi = .roi_as_sf(roi)))
}
#' @export
.cube_intersects.default <- function(cube, roi) {
    cube <- tibble::as_tibble(cube)
    cube <- .cube_find_class(cube)
    intersects <- .cube_intersects(cube, roi)
    return(intersects)
}
#' @title Filter tiles that intersect \code{roi} parameter.
#' @noRd
#' @param cube  A data cube.
#' @param roi  A region of interest (ROI).
#' @return  A filtered data cube.
.cube_filter_spatial <- function(cube, roi) {
    UseMethod(".cube_filter_spatial", cube)
}
#' @export
.cube_filter_spatial.raster_cube <- function(cube, roi) {
    # set caller to show in errors
    .check_set_caller(".cube_filter_spatial")
    intersecting <- .cube_intersects(cube, roi)
    .check_that(any(intersecting))
    cube[intersecting, ]
}
#' @export
.cube_filter_spatial.default <- function(cube, roi) {
    cube <- tibble::as_tibble(cube)
    cube <- .cube_find_class(cube)
    result <- .cube_filter_spatial(cube, roi)
    return(result)
}
#' @title Test tiles with images during an interval
#' @noRd
#' @param cube  A data cube.
#' @param start_date,end_date  Dates of interval.
#' @return A logical vector
.cube_during <- function(cube, start_date, end_date) {
    UseMethod(".cube_during", cube)
}
#' @export
.cube_during.raster_cube <- function(cube, start_date, end_date) {
    .compact(slider::slide_lgl(
        cube, .tile_during,
        start_date = start_date, end_date = end_date
    ))
}
#' @export
.cube_during.default <- function(cube, start_date, end_date) {
    cube <- tibble::as_tibble(cube)
    cube <- .cube_find_class(cube)
    result <- .cube_during(cube, start_date, end_date)
    return(result)
}
#' @title Filter tiles inside a temporal interval
#' @noRd
#' @param cube  A data cube.
#' @param start_date,end_date  Dates of interval.
#' @return  A filtered data cube.
.cube_filter_interval <- function(cube, start_date, end_date) {
    UseMethod(".cube_filter_interval", cube)
}
#' @export
.cube_filter_interval.raster_cube <- function(cube, start_date, end_date) {
    # set caller to show in errors
    .check_set_caller(".cube_filter_interval")
    during <- .cube_during(cube, start_date, end_date)
    .check_that(any(during))

    .cube_foreach_tile(cube[during, ], function(tile) {
        .tile_filter_interval(tile, start_date, end_date)
    })
}
#' @export
.cube_filter_interval.default <- function(cube, start_date, end_date) {
    cube <- tibble::as_tibble(cube)
    cube <- .cube_find_class(cube)
    cube <- .cube_filter_interval(cube, start_date, end_date)
    return(cube)
}

#' @title Filter tiles by sparse dates
#' @noRd
#' @param cube  A data cube.
#' @param dates A character vector with dates.
#' @return  A filtered data cube.
.cube_filter_dates <- function(cube, dates) {
    UseMethod(".cube_filter_dates", cube)
}
#' @export
.cube_filter_dates.raster_cube <- function(cube, dates) {
    # set caller to show in errors
    .check_set_caller(".cube_filter_dates")
    # Filter dates for each tile
    cube <- .cube_foreach_tile(cube, function(tile) {
        dates_in_tile <- dates %in% .tile_timeline(tile)
        if (!any(dates_in_tile)) {
            return(NULL)
        }
        .tile_filter_dates(tile, dates[dates_in_tile])
    })
    # Post-condition
    .check_that(nrow(cube) >= 1)
    # Return cube
    return(cube)
}
#' @export
.cube_filter_dates.default <- function(cube, dates) {
    cube <- tibble::as_tibble(cube)
    cube <- .cube_find_class(cube)
    cube <- .cube_filter_dates(cube = cube, dates = dates)
    return(cube)
}
#' @title Filter cube based on a set of bands
#' @noRd
#' @param cube  A data cube.
#' @param bands  Band names.
#' @return  Filtered data cube.
.cube_filter_bands <- function(cube, bands) {
    UseMethod(".cube_filter_bands", cube)
}
#' @export
.cube_filter_bands.raster_cube <- function(cube, bands) {
    .cube_foreach_tile(cube, function(tile) {
        .tile_filter_bands(tile = tile, bands = bands)
    })
}
#' @export
.cube_filter_bands.default <- function(cube, bands) {
    cube <- tibble::as_tibble(cube)
    cube <- .cube_find_class(cube)
    cube <- .cube_filter_bands(cube, bands)
    return(cube)
}
#' @title Filter tiles that are non-empty.
#' @noRd
#' @param cube  A data cube.
#' @return  A filtered data cube.
.cube_filter_nonempty <- function(cube) {
    not_empty <- slider::slide_lgl(cube, .tile_is_nonempty)
    cube[not_empty, ]
}
#' @title Returns the tile names of a data cube
#' @noRd
#' @param cube  A data cube.
#' @return  Names of tiles.
.cube_tiles <- function(cube) {
    UseMethod(".cube_tiles", cube)
}
#' @export
.cube_tiles.raster_cube <- function(cube) {
    .as_chr(cube[["tile"]])
}
#' @export
.cube_tiles.default <- function(cube) {
    cube <- tibble::as_tibble(cube)
    cube <- .cube_find_class(cube)
    tiles <- .cube_tiles(cube)
    return(tiles)
}
#' @title Returns the paths of a data cube
#' @noRd
#' @param cube  A data cube.
#' @return  Paths of images in the cube
.cube_paths <- function(cube, bands = NULL) {
    UseMethod(".cube_paths", cube)
}
#' @export
.cube_paths.raster_cube <- function(cube, bands = NULL) {
    slider::slide(cube, .tile_paths, bands = bands)
}
#' @export
.cube_paths.default <- function(cube, bands = NULL) {
    cube <- tibble::as_tibble(cube)
    cube <- .cube_find_class(cube)
    paths <- .cube_paths(cube, bands)
    return(paths)
}
#' @title Find if the cube is local
#' @noRd
#' @param cube  A data cube
#' @return  TRUE/FALSE
.cube_is_local <- function(cube) {
    UseMethod(".cube_is_local", cube)
}
#' @export
.cube_is_local.raster_cube <- function(cube) {
    all(.file_is_local(.file_remove_vsi(unlist(.cube_paths(cube)))))
}
#' @export
.cube_is_local.default <- function(cube) {
    cube <- tibble::as_tibble(cube)
    cube <- .cube_find_class(cube)
    result <- .cube_is_local(cube)
    return(result)
}
#' @title Filter the cube using tile names
#' @noRd
#' @param cube  A data cube.
#' @param tiles  Tile names.
#' @return  Filtered data cube.
.cube_filter_tiles <- function(cube, tiles) {
    UseMethod(".cube_filter_tiles", cube)
}
#' @export
.cube_filter_tiles.raster_cube <- function(cube, tiles) {
    cube[.cube_tiles(cube) %in% tiles, ]
}
#' @export
.cube_filter_tiles.default <- function(cube, tiles) {
    cube <- tibble::as_tibble(cube)
    cube <- .cube_find_class(cube)
    cube <- .cube_filter_tiles(cube, tiles)
    return(cube)
}

#' @title Create internal cube features with ID
#' @noRd
#' @param cube  data cube
#' @return cube with feature ID in file info
.cube_split_features <- function(cube) {
    UseMethod(".cube_split_features", cube)
}
#' @export
.cube_split_features.raster_cube <- function(cube) {
    # Process for each tile and return a cube
    .cube_foreach_tile(cube, function(tile) {
        features <- tile[, c("tile", "file_info")]
        features <- tidyr::unnest(features, "file_info")
        features[["feature"]] <- features[["fid"]]
        features <- tidyr::nest(features, file_info = -c("tile", "feature"))
        # Replicate each tile so that we can copy file_info to cube
        tile <- tile[rep(1, nrow(features)), ]
        tile[["file_info"]] <- features[["file_info"]]
        tile
    })
}
#' @export
.cube_split_features.default <- function(cube) {
    cube <- tibble::as_tibble(cube)
    cube <- .cube_find_class(cube)
    cube <- .cube_split_features(cube)
    return(cube)
}
#' @title Split assets for a data cube by assigning a unique ID
#' @noRd
#' @param  cube  datacube
#' @return a data cube with assets (file ID)
#'
.cube_split_assets <- function(cube) {
    UseMethod(".cube_split_assets", cube)
}
#' @export
.cube_split_assets.raster_cube <- function(cube) {
    # Process for each tile and return a cube
    .cube_foreach_tile(cube, function(tile) {
        assets <- tile[, c("tile", "file_info")]
        assets <- tidyr::unnest(assets, "file_info")
        assets[["asset"]] <- assets[["band"]]
        assets[["feature"]] <- .default(assets[["fid"]], "1")
        assets <- tidyr::nest(
            assets,
            file_info = -c("tile", "feature", "asset")
        )
        # Replicate each tile so that we can copy file_info to cube
        tile <- tile[rep(1, nrow(assets)), ]
        tile[["file_info"]] <- assets[["file_info"]]
        tile
    })
}
#' @export
.cube_split_assets.derived_cube <- function(cube) {
    # Process for each tile and return a cube
    .cube_foreach_tile(cube, function(tile) {
        assets <- tile[, c("tile", "file_info")]
        assets <- tidyr::unnest(assets, "file_info")
        assets[["asset"]] <- assets[["band"]]
        assets <- tidyr::nest(
            assets,
            file_info = -c("tile", "asset")
        )
        # Replicate each tile so that we can copy file_info to cube
        tile <- tile[rep(1, nrow(assets)), ]
        tile[["file_info"]] <- assets[["file_info"]]
        tile
    })
}
#' @export
.cube_split_assets.default <- function(cube) {
    cube <- tibble::as_tibble(cube)
    cube <- .cube_find_class(cube)
    cube <- .cube_split_assets(cube)
    return(cube)
}
#' @title Merge tiles in a data cube
#' @noRd
#' @param features  cube features
#' @return merged data cube
.cube_merge_tiles <- function(cube) {
    UseMethod(".cube_merge_tiles", cube)
}
#' @export
.cube_merge_tiles.raster_cube <- function(cube) {
    class_orig <- class(cube)
    derived_cube <- inherits(cube, "derived_cube")
    cube <- tidyr::unnest(cube, "file_info", names_sep = ".")
    if (!derived_cube) {
        cube <- dplyr::distinct(cube)
    }
    cube <- dplyr::arrange(
        cube,
        .data[["file_info.date"]],
        .data[["file_info.band"]]
    )
    cube <- tidyr::nest(
        cube,
        file_info = tidyr::starts_with("file_info"),
        .names_sep = "."
    )
    # Set class features for the cube
    class(cube) <- class_orig
    # Return cube
    cube
}
#' @export
.cube_merge_tiles.derived_cube <- function(cube) {
    class_orig <- class(cube)
    cube <- tidyr::unnest(cube, "file_info", names_sep = ".")
    cube <- dplyr::arrange(
        cube, .data[["file_info.start_date"]], .data[["file_info.band"]]
    )
    cube <- tidyr::nest(
        cube,
        file_info = tidyr::starts_with("file_info"),
        .names_sep = "."
    )
    # Set class features for the cube
    class(cube) <- class_orig
    # Return cube
    cube
}
#' @export
.cube_merge_tiles.default <- function(cube) {
    cube <- tibble::as_tibble(cube)
    cube <- .cube_find_class(cube)
    cube <- .cube_merge_tiles(cube)
    return(cube)
}
#' @title Cube contains CLOUD band
#' @noRd
#' @param features  cube features
#' @return merged data cube
.cube_contains_cloud <- function(cube) {
    UseMethod(".cube_contains_cloud", cube)
}
#' @export
.cube_contains_cloud.raster_cube <- function(cube) {
    .compact(slider::slide_lgl(cube, .tile_contains_cloud))
}
#' @export
.cube_contains_cloud.default <- function(cube) {
    cube <- tibble::as_tibble(cube)
    cube <- .cube_find_class(cube)
    cube <- .cube_contains_cloud(cube)
    return(cube)
}
#' @title Check if bboxes of all tiles of the cube are the same
#' @name .cube_has_unique_bbox
#' @keywords internal
#' @noRd
#' @param  cube         input data cube
#' @return TRUE/FALSE
.cube_has_unique_bbox <- function(cube) {
    tolerance <- .conf(
        "sources", .cube_source(cube),
        "collections", .cube_collection(cube),
        "ext_tolerance"
    )

    # check if the resolutions are unique
    equal_bbox <- slider::slide_lgl(cube, function(tile) {
        file_info <- .fi(tile)
        max_xmax <- max(file_info[["xmax"]])
        min_xmax <- min(file_info[["xmax"]])
        max_xmin <- max(file_info[["xmin"]])
        min_xmin <- min(file_info[["xmin"]])
        max_ymax <- max(file_info[["ymax"]])
        min_ymax <- min(file_info[["ymax"]])
        max_ymin <- max(file_info[["ymin"]])
        min_ymin <- min(file_info[["ymin"]])

        test <- .is_eq(max_xmax, min_xmax, tolerance = tolerance) &&
                .is_eq(max_xmin, min_xmin, tolerance = tolerance) &&
                .is_eq(max_ymin, min_ymin, tolerance = tolerance) &&
                .is_eq(max_ymax, min_ymax, tolerance = tolerance)
        return(test)
    })
    if (all(equal_bbox))
        return(TRUE)
    else
        return(FALSE)
}
#' @title Check if sizes of all tiles of the cube are the same
#' @name .cube_has_unique_tile_size
#' @keywords internal
#' @noRd
#' @param  cube         input data cube
#' @return TRUE/FALSE
.cube_has_unique_tile_size <- function(cube) {
    # check if the sizes of all tiles are the same
    test_cube_size <- slider::slide_lgl(
        cube,
        function(tile) {
            if (length(unique(.tile_nrows(tile))) > 1 ||
                length(unique(.tile_ncols(tile))) > 1)
                return(FALSE)
            else
                return(TRUE)
    })
    if (all(test_cube_size))
        return(TRUE)
    else
        return(FALSE)
}

#' @title Check if resolutions of all tiles of the cube are the same
#' @name .cube_has_unique_resolution
#' @keywords internal
#' @noRd
#' @param  cube         input data cube
#' @return TRUE/FALSE
.cube_has_unique_resolution <- function(cube) {
    return(length(c(.cube_xres(cube), .cube_yres(cube))) == 2)
}
# ---- derived_cube ----
#' @title Get derived class of a cube
#' @name .cube_derived_class
#' @keywords internal
#' @noRd
#' @param cube A cube
#'
#' @return derived class
.cube_derived_class <- function(cube) {
    unique(slider::slide_chr(cube, .tile_derived_class))
}
# ---- mpc_cube ----
#' @title Generate token to cube
#' @name .cube_token_generator
#' @keywords internal
#' @noRd
#' @param  cube input data cube
#' @param  ...  additional parameters for httr package
#'
#' @return A sits cube
.cube_token_generator <- function(cube) {
    UseMethod(".cube_token_generator", cube)
}
#' @export
.cube_token_generator.mpc_cube <- function(cube) {
    # set caller to show in errors
    .check_set_caller(".cube_token_generator")
    file_info <- cube[["file_info"]][[1]]
    fi_paths <- file_info[["path"]]

    are_local_paths <- !startsWith(fi_paths, prefix = "/vsi")
    # ignore in case of regularized and local cubes
    if (all(are_local_paths)) {
        return(cube)
    }

    # we consider token is expired when the remaining time is
    # less than 5 minutes
    if ("token_expires" %in% colnames(file_info) &&
        !.cube_is_token_expired(cube)) {
        return(cube)
    }
    token_endpoint <- .conf("sources", .cube_source(cube), "token_url")
    url <- paste0(token_endpoint, "/", tolower(.cube_collection(cube)))
    res_content <- NULL
    n_tries <- .conf("cube_token_generator_n_tries")
    sleep_time <- .conf("cube_token_generator_sleep_time")
    # Generate a random time to make a new request
    sleep_time <- sample(x = seq_len(sleep_time), size = 1)
    access_key <- Sys.getenv("MPC_TOKEN")
    if (!nzchar(access_key)) {
        access_key <- NULL
    }
    while (is.null(res_content) && n_tries > 0) {
        res_content <- tryCatch(
            {
                res <- .get_request(
                    url = url,
                    headers = list("Ocp-Apim-Subscription-Key" = access_key)
                )
                res <- .response_check_status(res)
                .response_content(res)
            },
            error = function(e) {
                return(NULL)
            }
        )

        if (is.null(res_content)) {
            Sys.sleep(sleep_time)
        }
        n_tries <- n_tries - 1
    }
    # check that token is valid
    .check_that(.has(res_content))
    # parse token
    token_parsed <- .url_parse_query(res_content[["token"]])
    file_info[["path"]] <- purrr::map_chr(seq_along(fi_paths), function(i) {
        path <- fi_paths[[i]]
        if (are_local_paths[[i]]) {
            return(path)
        }

        path_prefix <- "/vsicurl/"
        path <- stringr::str_replace(path, path_prefix, "")

        url_parsed <- .url_parse(path)
        url_parsed[["query"]] <- utils::modifyList(
            url_parsed[["query"]], token_parsed
        )
        # remove the additional chars added by httr
        new_path <- gsub("^://", "", .url_build(url_parsed))
        new_path <- paste0(path_prefix, new_path)
        new_path
    })
    file_info[["token_expires"]] <- strptime(
        x = res_content[["msft:expiry"]],
        format = "%Y-%m-%dT%H:%M:%SZ"
    )
    cube[["file_info"]][[1]] <- file_info
    return(cube)
}
#' @export
.cube_token_generator.default <- function(cube) {
    return(cube)
}

#' @title Check if a cube token was expired
#' @name .cube_is_token_expires
#' @keywords internal
#' @noRd
#' @param cube input data cube
#'
#' @return a boolean value.
.cube_is_token_expired <- function(cube) {
    UseMethod(".cube_is_token_expired", cube)
}
#' @export
.cube_is_token_expired.mpc_cube <- function(cube) {
    file_info <- cube[["file_info"]][[1]]
    fi_paths <- file_info[["path"]]

    min_remaining_time <- .conf(
        "cube_token_generator_min_remaining_time"
    )
    are_local_paths <- !startsWith(fi_paths, prefix = "/vsi")
    # ignore in case of regularized and local cubes
    if (all(are_local_paths)) {
        return(FALSE)
    }
    if ("token_expires" %in% colnames(file_info)) {
        difftime_token <- difftime(
            time1 = file_info[["token_expires"]][[1]],
            time2 = as.POSIXct(format(Sys.time(), tz = "UTC", usetz = TRUE)),
            units = "mins"
        )

        return(difftime_token < min_remaining_time)
    }
    return(FALSE)
}
#' @export
.cube_is_token_expired.default <- function(cube) {
    return(FALSE)
}
#' @title Split the cube by tiles and bands
#' @name .cube_split_tiles_bands
#' @keywords internal
#' @noRd
#' @param cube input data cube
#' @param bands vector of bands
#'
#' @return  a list of tile-band combinations
.cube_split_tiles_bands <- function(cube, bands) {
    # All combinations between tiles and bands
    tiles_bands <- tidyr::expand_grid(
        tile = .cube_tiles(cube),
        band = bands
    )
    # Generate a list combined by tiles and bands
    tiles_bands <- purrr::pmap(tiles_bands, function(tile, band) {
        return(list(tile, band))
    })
    # Return a list of combinations
    return(tiles_bands)
}
#' @title Split the cube by samples
#' @name .cube_split_chunks_samples
#' @keywords internal
#' @noRd
#' @param cube input data cube
#' @param samples_sf samples in sf format
#'
#' @return  a data.frame with cube chunks
.cube_split_chunks_samples <- function(cube, samples_sf) {
    # Hold s2 status
    s2_status <- sf::sf_use_s2()
    suppressMessages(sf::sf_use_s2(FALSE))
    # Back to original status on exit
    on.exit(suppressMessages(sf::sf_use_s2(s2_status)))
    # Get block size of raster file
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(cube)))
    # Terra requires at least two pixels to recognize an extent as valid
    # polygon and not a line or point
    block <- .block_regulate_size(block)
    cube_chunks <- slider::slide(cube, function(tile) {
        chunks <- .tile_chunks_create(
            tile = tile,
            overlap = 0,
            block = block
        )
        chunks_sf <- .bbox_as_sf(
            .bbox(chunks, by_feature = TRUE), as_crs = sf::st_crs(samples_sf)
        )
        chunks_sf <- dplyr::bind_cols(chunks_sf, chunks)
        chunks_sf <- chunks_sf[.intersects(chunks_sf, samples_sf), ]
        if (nrow(chunks_sf) == 0)
            return(NULL)
        chunks_sf[["tile"]] <- tile[["tile"]]
        chunks_sf <- dplyr::group_by(chunks_sf, .data[["row"]], .data[["tile"]])
        chunks_sf <- dplyr::summarise(chunks_sf)
        chunks_sf <- slider::slide(chunks_sf, function(chunk_sf) {
            chunk_sf[["samples"]] <- list(samples_sf[
                .within(samples_sf, chunk_sf), ])
            return(chunk_sf)
        })
        return(chunks_sf)
    })
    return(unlist(cube_chunks, recursive = FALSE))
}
#' @title  Return base info
#' @name .cube_has_base_info
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  cube       Raster cube
#' @return            TRUE/FALSE
#'
#'
.cube_has_base_info <- function(cube) {
    return(.has(cube[["base_info"]]))
}

.cube_sensor <- function(cube) {
    .dissolve(slider::slide(cube, .tile_sensor))
}

.cube_satellite <- function(cube) {
    .dissolve(slider::slide(cube, .tile_satellite))
}

#' @title  Return cube grid system
#' @name .cube_grid_system
#' @keywords internal
#' @noRd
#'
#' @param  cube       Raster cube
#' @return            Cube grid system
.cube_grid_system <- function(cube) {
    .conf_grid_system(
        source = .cube_source(cube),
        collection = .cube_collection(cube)
    )
}
