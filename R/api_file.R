
.file_base <- function(file) {
    gsub("[?].*$", "", gsub("^.*/", "", file))
}

.file_sans_ext <- function(file) {
    gsub("(.*)\\..+$", "\\1", .file_base(file))
}

.file_ext <- function(file) {
    gsub(".*\\.(.+)$", "\\1", .file_base(file))
}

.file_pattern <- function(file, suffix = "") {
    paste0(.file_sans_ext(.file_base(file)), suffix)
}

.file_normalize <- function(file) {
    path.expand(file)
}

.file_path <- function(..., ext = NULL, output_dir = NULL,
                       create_dir = FALSE, sep = "_") {
    filenames <- paste(..., sep = sep)
    if (.has(ext)) {
        # remove extension final point
        ext <- gsub("^[.]*", "", ext)
        filenames <- paste(filenames, ext, sep = ".")
    }
    if (.has(output_dir)) {
        output_dir <- gsub("[/]*$", "", output_dir)
        output_dir <- .file_normalize(output_dir)
        if (!dir.exists(output_dir) && create_dir) {
            dir.create(output_dir, recursive = TRUE)
        }
        filenames <- if (length(filenames) == 0) "" else filenames
        filenames <- file.path(output_dir, filenames)
    }
    filenames
}

.file_is_local <- function(file) {
    all(!grepl(pattern = "^(http[s]?|s3)://", x = file))
}

.file_remove_vsi <- function(file) {
    gsub(pattern = "^(/vsicurl/|/vsis3/|/vsigs/)", replacement = "", x = file)
}

.file_block_name <- function(pattern, block, output_dir) {
    .file_path(
        pattern, "block", block[["row"]], block[["col"]],
        ext = "tif", output_dir = file.path(output_dir, ".sits"),
        create_dir = TRUE
    )
}

.file_log_name <- function(output_dir) {
    .file_path(
        basename(tempdir()), ext = "log",
        output_dir = file.path(output_dir, ".sits"), create_dir = TRUE
    )
}

.file_derived_name <- function(tile, band, version, output_dir) {
    .file_path(
        tile[["satellite"]], tile[["sensor"]], .tile_name(tile),
        .tile_start_date(tile), .tile_end_date(tile), band, version,
        ext = "tif", output_dir = output_dir
    )
}

.file_mosaic_name <- function(tile, band, version, output_dir) {
    .file_path(
        tile[["satellite"]], tile[["sensor"]], "MOSAIC",
        .tile_start_date(tile), .tile_end_date(tile), band, version,
        ext = "tif", output_dir = output_dir
    )
}

.file_crop_name <- function(tile, band, version, output_dir) {
    .file_path(
        tile[["satellite"]], tile[["sensor"]], .tile_name(tile),
        .tile_start_date(tile), band, "crop",
        version, ext = "tif", output_dir = file.path(output_dir, ".sits"),
        create_dir = TRUE
    )
}

.file_eo_name <- function(tile, band, date, output_dir) {
    .file_path(
        "cube", .tile_name(tile), band, date,
        ext = ".tif", output_dir = output_dir
    )
}
