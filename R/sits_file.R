
.file_block_name <- function(output_file, block) {
    # Get output_dir
    output_dir <- dirname(output_file)
    # Get output file name as template
    output_file <- tools::file_path_sans_ext(
        basename(output_file)
    )
    .file_path(output_file, "block",
               block[["first_row"]], block[["first_col"]],
               ext = ".tif",
               output_dir = output_dir)
}

.file_tile_name <- function(tile) {
    # output filename
    file_name <- paste0(
        tile[["satellite"]], "_",
        tile[["sensor"]], "_",
        tile[["tile"]], "_",
        start_date, "_",
        end_date, "_",
        band_name, "_",
        version, ".tif"
    )

    file_name <- file.path(output_dir, file_name)

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
