#' @title Get file base name
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @param file   File name
#' @returns      File base name
.file_base <- function(file) {
    gsub("[?].*$", "", gsub("^.*/", "", file))
}
#' @title Get file base name
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @param file   File name
#' @returns      File base name
.file_dir <- function(file) {
    file <- .file_remove_vsi(file)
    gsub("[?].*$", "", gsub("^(.*/).*$", "\\1", file))
}
#' @title Get file name without extension
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @noRd
#' @param file   File name
#' @returns      File name without extension
.file_sans_ext <- function(file) {
    gsub("(.*)\\..+$", "\\1", .file_base(file))
}
#' @title Get file name extension
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @noRd
#' @param file   File name
#' @returns      File name extension
.file_ext <- function(file) {
    gsub(".*\\.(.+)$", "\\1", .file_base(file))
}
#' @title Apply a pattern to the file
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @noRd
#' @param file     File name
#' @param suffix   File suffix
#' @returns        File base name without extension with added suffix
.file_pattern <- function(file, suffix = "") {
    paste0(.file_sans_ext(.file_base(file)), suffix)
}
#' @title Expand the file path
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @noRd
#' @param file     File name
#' @returns        File base name with path expanded
.file_path_expand <- function(file) {
    path.expand(file)
}
#' @title Build a file path
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @noRd
#' @param ...      File name
#' @param ext      File extension
#' @param output_dir Directory where file will be saved
#' @param create_dir Should file directory be created?
#' @param sep      File name separator
#' @returns        File path
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
        output_dir <- .file_path_expand(output_dir)
        if (!dir.exists(output_dir) && create_dir) {
            dir.create(output_dir, recursive = TRUE)
        }
        filenames <- if (length(filenames) == 0) "" else filenames
        filenames <- file.path(output_dir, filenames)
    }
    filenames
}
#' @title Is the file local?
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @noRd
#' @param file     File name
#' @returns        TRUE/FALSE
.file_is_local <- function(file) {
    !all(grepl(pattern = "^(http[s]?|s3)://", x = file))
}
#' @title Remove vsi preamble for remote files
#'
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @noRd
#' @param file     File path
#' @returns        File path without vsi designators
.file_remove_vsi <- function(file) {
    gsub(pattern = "^(/vsicurl/|/vsis3/|/vsigs/)", replacement = "", x = file)
}

#' @title Create a file path for a block
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @noRd
#' @param pattern    Pattern to be used
#' @param block      Block (first row, first col, nrows, ncols)
#' @param output_dir Directory where the block will be saved
#' @param ext        file block extension
#' @returns File path for the block
.file_block_name <- function(pattern, block, output_dir, ext = "tif") {
    .file_path(
        pattern, "block", block[["row"]], block[["col"]],
        ext = ext, output_dir = file.path(output_dir, ".sits"),
        create_dir = TRUE
    )
}

#' @title Create a log file
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @noRd
#' @param output_dir Directory where the log  will be saved
#' @returns        File path for the log
.file_log_name <- function(output_dir) {
    .file_path(
        basename(tempdir()),
        ext = "log",
        output_dir = file.path(output_dir, ".sits"), create_dir = TRUE
    )
}

#' @title Create a file path for samples
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @noRd
#' @param hash       Character with the bundle hash value
#' @param output_dir Directory where the saves will be saved
#' @param ext        file block extension
#' @returns File path for the samples
.file_samples_name <- function(hash, output_dir, ext = "rds") {
    .file_path(
        "samples", hash, ext = ext, output_dir = file.path(output_dir, ".sits"),
        create_dir = TRUE
    )
}

#' @title Build a file path for a derived file
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @param tile       Tile of data cube
#' @param band       Spectral band
#' @param version    Version name
#' @param output_dir Directory where file will be saved
#' @param ext        file extension path
#' @returns File path for derived file
.file_derived_name <- function(tile, band, version, output_dir, ext = "tif") {
    .file_path(
        tile[["satellite"]], tile[["sensor"]], .tile_name(tile),
        .tile_start_date(tile), .tile_end_date(tile), band, version,
        ext = ext, output_dir = output_dir
    )
}

#' @title Build a file path for a mosaic of derived cubes
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @noRd
#' @param tile     Tile of data cube
#' @param band     Spectral band
#' @param version Version name
#' @param output_dir Directory where file will be saved
#' @returns        File path for mosaic
.file_mosaic_name_derived <- function(tile, band, version, output_dir) {
    .file_path(
        tile[["satellite"]], tile[["sensor"]], "MOSAIC",
        .tile_start_date(tile), .tile_end_date(tile), band, version,
        ext = "tif", output_dir = output_dir
    )
}
#' @title Build a file path for a mosaic of raster cubes
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @param tile     Tile of data cube
#' @param band     Spectral band
#' @param output_dir Directory where file will be saved
#' @returns        File path for mosaic
.file_mosaic_name_raster <- function(tile, band, output_dir) {
    .file_path(
        tile[["satellite"]], tile[["sensor"]], "MOSAIC",
        .tile_start_date(tile), band,
        ext = "tif", output_dir = output_dir
    )
}
#' @title Build a file path for a cropped file
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @param tile     Tile of data cube
#' @param band     Spectral band
#' @param version Version name
#' @param output_dir Directory where file will be saved
#' @returns        File path for cropped file
.file_crop_name <- function(tile, band, version, output_dir) {
    .file_path(
        tile[["satellite"]], tile[["sensor"]], .tile_name(tile),
        .tile_start_date(tile), band, "crop",
        version,
        ext = "tif", output_dir = file.path(output_dir, ".sits"),
        create_dir = TRUE
    )
}
#' @title Build a file path for a file in an eo_cube
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @noRd
#' @param tile     Tile of data cube
#' @param band     Spectral band
#' @param version Version name
#' @param output_dir Directory where file will be saved
#' @returns        File path for a file in an eo_cube
.file_eo_name <- function(tile, band, date, output_dir) {
    .file_path(
        tile[["satellite"]], tile[["sensor"]],
        .tile_name(tile), band, date,
        ext = ".tif", output_dir = output_dir
    )
}
