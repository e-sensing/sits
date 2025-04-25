#' @name .message_invalid_param
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param param parameter name
#' @return A valid message
#' @keywords internal
#' @noRd
.message_invalid_param <- function(param) {
    # make default message
    paste0("invalid ", param, " parameter")
}
#' @title Checks if warnings should be displayed
#' @name .message_warnings
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @return TRUE/FALSE
#' @keywords internal
#' @noRd
.message_warnings <- function() {
    !(Sys.getenv("SITS_DOCUMENTATION_MODE") == "TRUE")
}
#' @title Warning when converting a bbox into a sf object
#' @name .message_warnings_bbox_as_sf
#' @noRd
#' @returns Called for side effects
.message_warnings_bbox_as_sf <- function() {
    if (.message_warnings())
        warning(.conf("messages", ".bbox_as_sf"), call. = FALSE)
}
#' @title Warning when labels have no colors preset
#' @name .message_warnings_colors_get
#' @noRd
#' @returns Called for side effects
.message_warnings_colors_get <- function(missing, palette) {
    if (.message_warnings()) {
        warning(.conf("messages", ".colors_get_missing"), toString(missing))
        warning(.conf("messages", ".colors_get_missing_palette"), palette)
        # grDevices does not work with one color missing
    }
}
#' @title Warning when cube has no CLOUD band
#' @name .message_warnings_regularize_cloud
#' @noRd
#' @returns Called for side effects
.message_warnings_regularize_cloud <- function(cube) {
    if (!all(.cube_contains_cloud(cube))) {
        if (.message_warnings())
            warning(.conf("messages", "sits_regularize_cloud"),
                    call. = FALSE,
                    immediate. = TRUE
            )
    }
}
#' @title Warning when cube is being regularized directly from STAC files
#' @name .message_warnings_regularize_local
#' @noRd
#' @returns Called for side effects
.message_warnings_regularize_local <- function(cube) {
    if (!.cube_is_local(cube) && .message_warnings()) {
        warning(.conf("messages", "sits_regularize_local"),
                call. = FALSE, immediate. = TRUE
        )
    }
}
#' @title Warning when cube has multiple values of CRS
#' @name .message_warnings_regularize_crs
#' @noRd
#' @returns Called for side effects
.message_warnings_regularize_crs <- function() {
    if (.message_warnings())
        warning(.conf("messages", "sits_regularize_crs"),
                call. = FALSE,
                immediate. = TRUE
        )
}

#' @title Warning when cube has more than one timeline
#' @name .message_warnings_timeline_cube
#' @noRd
#' @returns Called for side effects
.message_warnings_timeline_cube <- function() {
    if (.message_warnings())
        warning(.conf("messages", "sits_timeline_raster_cube"),
                call. = FALSE
        )
}
.message_progress <- function(progress) {
    .check_lgl_parameter(progress)
    if (progress)
        progress <- Sys.getenv("SITS_DOCUMENTATION_MODE") != "TRUE"
    progress
}
.message_verbose <- function(verbose) {
    .check_lgl_parameter(verbose)
    if (verbose)
        verbose <- Sys.getenv("SITS_DOCUMENTATION_MODE") != "TRUE"
    verbose
}
#' @title Check is version parameter is valid using reasonable defaults
#' @name .message_version
#' @keywords internal
#' @noRd
#' @param  version  character vector
#' @return version adjusted to remove underscores
.message_version <- function(version) {
    .check_set_caller(".check_version")
    .check_chr(
        x = version,
        allow_na = FALSE,
        allow_null = FALSE,
        allow_empty = FALSE,
        len_min = 1L,
        len_max = 1L
    )
    # avoids use of underscores
    tolower(gsub("_", "-", version))
}
