
.file_sans_ext <- function(file) {
    gsub("(.*)\\..+$", "\\1", file)
}

.file_base <- function(file) {
    gsub("[?].*$", "", gsub("^.*/", "", file))
}

.file_pattern <- function(file, suffix = "") {
    paste0(.file_sans_ext(.file_base(file)), suffix)
}

.file_path <- function(..., ext = NULL, output_dir = NULL,
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

.file_block_name <- function(pattern, block, output_dir) {
    # Get output_dir
    .file_path(
        pattern, "block", block[["row"]], block[["col"]],
        ext = "tif", output_dir = file.path(output_dir, ".sits"),
        create_dir = TRUE
    )
}

.file_derived_name <- function(tile, band, version, output_dir) {
    .file_path(
        tile[["satellite"]], tile[["sensor"]], .tile_name(tile),
        .tile_start_date(tile), .tile_end_date(tile), band, version,
        ext = "tif", output_dir = output_dir
    )
}
