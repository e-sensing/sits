
.file_block_name <- function(pattern, block, output_dir) {
    # Get output_dir
    .file_path(
        pattern, "block", block[["first_row"]],
        block[["first_col"]],
        ext = "tif",
        output_dir = file.path(output_dir, ".sits"),
        create_dir = TRUE
    )
}

.file_probs_name <- function(tile, version, output_dir) {
    .file_path(
        tile[["satellite"]],
        tile[["sensor"]],
        tile[["tile"]],
        .tile_start_date(tile),
        .tile_end_date(tile),
        .config_get("probs_cube_band"),
        version,
        ext = "tif",
        output_dir = output_dir
    )
}

.file_path <- function(...,
                       ext = NULL,
                       output_dir = NULL,
                       create_dir = FALSE) {
    filenames <- paste(..., sep = "_")
    if (!is.null(ext)) {
        # remove extension final point
        ext <- gsub("^[.]*", "", ext)
        filenames <- paste(filenames, ext, sep = ".")
    }
    if (!is.null(output_dir)) {
        output_dir <- gsub("[/]*$", "", output_dir)
        if (!dir.exists(output_dir) && create_dir) {
            dir.create(output_dir, recursive = TRUE)
        }
        filenames <- file.path(output_dir, filenames)
    }
    return(filenames)
}

.file_sans_ext <- function(file) {
    gsub("(.*)\\..+$", "\\1", file)
}

.file_base <- function(file) {
    gsub("[?].*$", "", gsub("^.*/", "", file))
}
