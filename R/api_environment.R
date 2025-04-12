
# ---- Environment operations ----
#' @title Function to patch environment variables (Developer only).
#' @name .environment_patch
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @keywords internal
#' @noRd
#'
#' @description
#'  This function patches environment variables, swapping them from a `source`
#'  to a `target` variable. You can rollback the swap operation using the
#'  `.environment_rollback.`
#'
#' @note
#'  This function is suitable only for internal `sits` tests and should not be
#'  used for any other purpose.
#' @param env_config List with the configuration of the environment.
#' @return           Called for side effects.
.environment_patch <- function(env_config) {
    env_prefix <- env_config[["name"]]
    env_variables <- env_config[["variables"]]

    purrr::map(seq_along(env_variables), function(var_idx) {
        var_source <- names(env_variables)[[var_idx]]
        var_target <- unname(env_variables)[[var_idx]]
        # Get current value of the target variable
        var_target_value <- Sys.getenv(var_target)
        # Save current value of the target variable in the "swap area"
        var_target_swap <- paste(env_prefix, var_target, sep = "_SWAP_")
        var_target_swap_value <- list(tmp = var_target_value)
        var_target_swap_value <- stats::setNames(
            var_target_swap_value,
            var_target_swap
        )
        # Save variable
        do.call(Sys.setenv, var_target_swap_value)
        # Get the new value of the target variable
        var_source_value <- Sys.getenv(var_source)
        # Save new value in the target variable
        var_target_new_value <- list(tmp = var_source_value)
        var_target_new_value <- stats::setNames(
            var_target_new_value,
            var_target
        )
        # Save variable
        do.call(Sys.setenv, var_target_new_value)
    })
    invisible(NULL)
}

#' @title Function to rollback patch in environment variables (Developer only).
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @description
#'  This function rollback patches in environment variables created with the
#'  function `.environment_patch`.
#' @note
#'  This function is suitable only for internal `sits` tests and should not be
#'  used for any other purpose.
#' @param env_config List with the configuration of the environment.
#' @return           Called for side effects.
.environment_rollback <- function(env_config) {
    env_prefix <- env_config[["name"]]
    env_variables <- env_config[["variables"]]

    purrr::map(seq_along(env_variables), function(var_idx) {
        var_source <- names(env_variables)[[var_idx]]
        var_target <- unname(env_variables)[[var_idx]]
        # Get current value of the target variable
        var_target_value <- Sys.getenv(var_target)
        # Save current value in the source variable
        var_source_new_value <- list(tmp = var_target_value)
        var_source_new_value <- stats::setNames(
            var_source_new_value,
            var_source
        )
        do.call(Sys.setenv, var_source_new_value)
        # Get current value of the target variable in the "swap area"
        var_target_swap <- paste(env_prefix, var_target, sep = "_SWAP_")
        var_target_swap_value <- Sys.getenv(var_target_swap)
        # Prepare current target
        var_target_swap_value <- list(tmp = var_target_swap_value)
        var_target_swap_value <- stats::setNames(
            var_target_swap_value,
            var_target
        )
        do.call(Sys.setenv, var_target_swap_value)
    })
    invisible(NULL)
}

# ---- Environment configurations ----
#' @title Function to create patch configuration for the CDSE source.
#' @name .environment_cdse
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @keywords internal
#' @noRd
#'
#' @description
#'  This function creates a configuration for the `patch` and `rollback`
#'  specialized for the CDSE requirements.
#' @note
#'  This function is suitable only for internal `sits` tests and should not be
#'  used for any other purpose.
#' @return           List with the configuration of the CDSE environment.
.environment_cdse <- function() {
    # Define environment name. This name is used as a prefix for the
    # "swap area" which is used to keep values saved
    env_name <- "CDSE"
    # Create the environment variables
    env_variables <- list(
        "CDSE_ACCESS_KEY_ID"     = "AWS_ACCESS_KEY_ID",
        "CDSE_SECRET_ACCESS_KEY" = "AWS_SECRET_ACCESS_KEY",
        "CDSE_S3_ENDPOINT"       = "AWS_S3_ENDPOINT",
        "CDSE_VIRTUAL_HOSTING"   = "AWS_VIRTUAL_HOSTING"
    )
    # Create a base environment definition
    env_definition <-
        list(name = env_name, variables = env_variables)
    # Define a class, based on the name
    class(env_definition) <- c(env_name, "list")
    # Done!
    env_definition
}
