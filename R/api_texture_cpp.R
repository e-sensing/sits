#' @title C++ GLCM texture function environment factory
#' @name .texture_cpp_functions
#' @keywords internal
#' @noRd
#' @description Builds the environment with the nine `glcm_*` functions of the
#'   C++ engine. Each function closes over the window, the angles, the block
#'   size and the grey levels, so the expression of the texture calls it with
#'   the band values only.
#' @param window_size size of local window
#' @param angles      direction angles in radians
#' @param img_nrow    image size in rows
#' @param img_ncol    image size in cols
#' @param n_grey      number of grey levels
#' @return Environment with the `glcm_*` functions
.texture_cpp_functions <- function(window_size, angles, img_nrow, img_ncol,
                                  n_grey) {
    result_env <- list2env(list(
        glcm_contrast = function(m) {
            C_glcm_contrast(
                x = .as_int(unlist(m)), angles = angles, nrows = img_nrow,
                ncols = img_ncol, window_size = window_size, n_grey = n_grey
            )
        },
        glcm_dissimilarity = function(m) {
            C_glcm_dissimilarity(
                x = .as_int(unlist(m)), angles = angles, nrows = img_nrow,
                ncols = img_ncol, window_size = window_size, n_grey = n_grey
            )
        },
        glcm_homogeneity = function(m) {
            C_glcm_homogeneity(
                x = .as_int(unlist(m)), angles = angles, nrows = img_nrow,
                ncols = img_ncol, window_size = window_size, n_grey = n_grey
            )
        },
        glcm_energy = function(m) {
            C_glcm_energy(
                x = .as_int(unlist(m)), angles = angles, nrows = img_nrow,
                ncols = img_ncol, window_size = window_size, n_grey = n_grey
            )
        },
        glcm_asm = function(m) {
            C_glcm_asm(
                x = .as_int(unlist(m)), angles = angles, nrows = img_nrow,
                ncols = img_ncol, window_size = window_size, n_grey = n_grey
            )
        },
        glcm_mean = function(m) {
            C_glcm_mean(
                x = .as_int(unlist(m)), angles = angles, nrows = img_nrow,
                ncols = img_ncol, window_size = window_size, n_grey = n_grey
            )
        },
        glcm_variance = function(m) {
            C_glcm_variance(
                x = .as_int(unlist(m)), angles = angles, nrows = img_nrow,
                ncols = img_ncol, window_size = window_size, n_grey = n_grey
            )
        },
        glcm_std = function(m) {
            C_glcm_std(
                x = .as_int(unlist(m)), angles = angles, nrows = img_nrow,
                ncols = img_ncol, window_size = window_size, n_grey = n_grey
            )
        },
        glcm_correlation = function(m) {
            C_glcm_correlation(
                x = .as_int(unlist(m)), angles = angles, nrows = img_nrow,
                ncols = img_ncol, window_size = window_size, n_grey = n_grey
            )
        }
    ), parent = parent.env(environment()), hash = TRUE)

    result_env
}
