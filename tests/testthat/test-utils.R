suppress_keras <- function(expression) {
    tmp_dir <- gsub("^(.+/).*", "\\1", tempdir())

    # to filter transpiled and python code in pre-exec
    pre_files <- c(list.files(tmp_dir,
                              pattern = "tmp.*\\.py$",
                              full.names = TRUE),
                   list.files(paste0(tmp_dir, "__pycache__"),
                              pattern = "tmp.*\\.pyc$",
                              full.names = TRUE))

    model <- suppressMessages(suppressWarnings((expression)))

    # to filter transpiled and python code in post-exec
    post_files <- c(list.files(tmp_dir,
                               pattern = "tmp.*\\.py$",
                               full.names = TRUE),
                    list.files(paste0(tmp_dir, "__pycache__"),
                               pattern = "tmp.*\\.pyc$",
                               full.names = TRUE))

    # delete py files
    unlink(setdiff(post_files, pre_files))

    if (length(list.files(paste0(tmp_dir, "__pycache__"),
                          pattern = "tmp.*\\.pyc$",
                          full.names = TRUE)) == 0)
        unlink(paste0(tmp_dir, "__pycache__"), recursive = TRUE)

    model
}
