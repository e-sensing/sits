#' @title Chunks API
#' @noRd
#'
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @description
#' A chunk is a tibble of rectangular regions defining a matrix and
#' its corresponding geographical area. So, each  region contains a
#' block and a bbox information. chunks can be used to access
#' specific raster image regions and optimize memory usage.
#'
#' Generally, chunks are created from an actual image that is divided
#' into small blocks. The chunks also provide overlapping support, that is,
#' chunks that intersects its neighbors by some amount of pixels.
#'
#' @examples
#' if (sits_run_examples()) {
#'     chunks <- .chunks_create(
#'         block = c(ncols = 512, nrows = 512),
#'         overlap = 2,
#'         image_size = c(ncols = 4000, nrows = 4000),
#'         image_bbox = c(xmin = 1, xmax = 2, ymin = 3, ymax = 4, crs = 4326)
#'     )
#'     # remove overlaps from chunks
#'     cropped <- .chunks_no_overlap(chunks)
#'     # removing overlaps from a non overlapped chunks produces identical bbox
#'     identical(.bbox(cropped), .bbox(.chunks_no_overlap(cropped)))
#'     # blocks from 'cropped' can be used to remove any overlap from rasters
#'     # produced from 'chunks'.
#'     .chunks_filter_spatial(
#'         chunks = chunks,
#'         roi = c(lon_min = 1.3, lon_max = 1.7, lat_min = 3.3, lat_max = 3.7)
#'     )
#' }
NULL

#' @title Create chunks
#' @noRd
#' @param block  A block to represent the common chunk size.
#' @param overlap  An overlapping size in pixels.
#' @param image_size  A block with original image size.
#' @param image_bbox  A bbox with original image bbox.
#' @returns  A tibble with chunks.
.chunks_create <- function(block, overlap, image_size, image_bbox) {
    # Generate all starting block points (col, row)
    chunks <- tidyr::expand_grid(
        col = seq(1L, .ncols(image_size), .ncols(block)),
        row = seq(1L, .nrows(image_size), .nrows(block))
    )
    # Adjust col and row to do overlap
    chunks[["col"]] <- .as_int(pmax(1L, .col(chunks) - overlap))
    chunks[["row"]] <- .as_int(pmax(1L, .row(chunks) - overlap))
    # Adjust ncols and nrows to do overlap
    chunks[["ncols"]] <- .as_int(
        pmin(
            .ncols(image_size),
            .col(chunks) + .ncols(block) + overlap - 1L
        ) - .col(chunks) + 1L
    )
    chunks[["nrows"]] <- .as_int(
        pmin(
            .nrows(image_size),
            .row(chunks) + .nrows(block) + overlap - 1L
        ) - .row(chunks) + 1L
    )
    # Chunk of entire image
    entire_image <- c(image_size, image_bbox)
    # Prepare a raster as template to crop bbox
    t_obj <- .chunks_as_raster(chunk = entire_image, nlayers = 1L)
    # Generate chunks' bbox
    chunks <- slider::slide_dfr(chunks, function(chunk) {
        # Crop block from template
        rast <- .raster_crop_metadata(rast = t_obj, block = .block(chunk))
        # Add bbox information
        .xmin(chunk) <- .raster_xmin(rast = rast)
        .xmax(chunk) <- .raster_xmax(rast = rast)
        .ymin(chunk) <- .raster_ymin(rast = rast)
        .ymax(chunk) <- .raster_ymax(rast = rast)
        .crs(chunk) <- .raster_crs(rast = rast)
        chunk
    })
    # Overlapping support
    chunks[["overlap"]] <- .as_int(overlap)
    # Chunk size without overlap
    chunks[["crop_ncols"]] <- .as_int(pmin(
        .ncols(image_size) - .col(chunks) + 1L, .ncols(block)
    ))
    chunks[["crop_nrows"]] <- .as_int(pmin(
        .nrows(image_size) - .row(chunks) + 1L, .nrows(block)
    ))
    # Return chunks
    chunks
}
#' @title Convert chunk into raster
#' @noRd
#' @param chunk  A tibble with chunks
#' @param nlayers  Number of layers in the raster
#' @return  An empty raster object based on the on a chunk.
.chunks_as_raster <- function(chunk, nlayers) {
    .raster_new_rast(
        nrows = .nrows(chunk)[[1L]],
        ncols = .ncols(chunk)[[1L]],
        xmin = .xmin(chunk)[[1L]],
        xmax = .xmax(chunk)[[1L]],
        ymin = .ymin(chunk)[[1L]],
        ymax = .ymax(chunk)[[1L]],
        nlayers = nlayers,
        crs = .crs(chunk)[[1L]]
    )
}
#' @title Remove overlaps from chunks
#' @noRd
#' @param chunk  A tibble with chunks
#' @returns  A tibble with chunks without overlap.
.chunks_no_overlap <- function(chunks) {
    # Generate blocks
    cropped <- tibble::tibble(
        col = .as_int(pmin(chunks[["overlap"]] + 1L, .col(chunks))),
        row = .as_int(pmin(chunks[["overlap"]] + 1L, .row(chunks)))
    )
    # Adjust blocks size
    .ncols(cropped) <- pmin(
        .ncols(chunks) - .col(cropped) + 1L, .as_int(chunks[["crop_ncols"]])
    )
    .nrows(cropped) <- pmin(
        .nrows(chunks) - .row(cropped) + 1L, .as_int(chunks[["crop_nrows"]])
    )
    # Generate bbox for each chunk
    cropped <- slider::slide2_dfr(chunks, cropped, function(chunk, crop) {
        # Prepare a raster as template to crop bbox
        t_obj <- .chunks_as_raster(chunk = chunk, nlayers = 1L)
        # Crop block from template
        rast <- .raster_crop_metadata(rast = t_obj, block = .block(crop))
        # Add bbox information
        .xmin(crop) <- .raster_xmin(rast = rast)
        .xmax(crop) <- .raster_xmax(rast = rast)
        .ymin(crop) <- .raster_ymin(rast = rast)
        .ymax(crop) <- .raster_ymax(rast = rast)
        .crs(crop) <- .raster_crs(rast = rast)
        crop
    })
    # Finish cropped chunks
    cropped[["overlap"]] <- 0L
    cropped[["crop_ncols"]] <- chunks[["crop_ncols"]]
    cropped[["crop_nrows"]] <- chunks[["crop_nrows"]]
    # Return cropped chunks
    cropped
}
#' @title Filter chunks that intersects a given roi
#' @noRd
#' @param chunks  A data frame with chunks
#' @param roi  Region of interest
#' @returns  A tibble with filtered chunks
.chunks_filter_spatial <- function(chunks, roi) {
    chunks_sf <- .bbox_as_sf(.bbox(chunks, by_feature = TRUE))
    chunks[.intersects(chunks_sf, .roi_as_sf(roi)), ]
}
#' @title Filter chunks that are not within mask geometries
#' @noRd
#' @param chunks A data frame with chunks
#' @param mask Mask regions
#' @returns  A tibble with filtered chunks
.chunks_filter_mask <- function(chunks, mask) {
    # transform chunk to bbox
    chunks_sf <- .bbox_as_sf(.bbox(chunks, by_feature = TRUE))
    # remove chunks within mask
    chunks[!.within(chunks_sf, mask), ]
}
#' @title Crop chunk geometries by mask
#' @noRd
#' @param chunks A data frame with chunks
#' @param mask Mask regions
#' @returns  A sf object with cropped chunks geometries
.chunks_crop_mask <- function(chunks, mask) {
    # transform chunk to bbox
    chunks_sf <- .bbox_as_sf(.bbox(chunks, by_feature = TRUE))
    # crop the chunks
    .difference(chunks_sf, sf::st_union(mask))
}
#' @title Filter chunks that intersects segments
#' @noRd
#' @param chunks A data frame with chunks
#' @param tile   A cube tile
#' @param output_dir Output directory
#' @returns  A tibble with filtered segments
.chunks_filter_segments <- function(chunks, tile, output_dir) {
    # Read segments from tile
    segs <- .segments_read_vec(tile)
    # Transform each chunk in sf object
    sf_chunks <- .bbox_as_sf(
        .bbox(chunks, by_feature = TRUE, default_crs = .tile_crs(tile))
    )
    # Find segments in chunks
    idx_intersects <- sf::st_intersects(sf_chunks, segs, sparse = TRUE) |>
        purrr::imap_dfr(
            ~ dplyr::as_tibble(.x) |> dplyr::mutate(id = .y)
        ) |>
        dplyr::distinct(.data[["value"]], .keep_all = TRUE) |>
        dplyr::group_by(.data[["id"]]) |>
        tidyr::nest() |>
        tibble::deframe()
    idx_positions <- as.integer(names(idx_intersects))
    chunks <- chunks[idx_positions, ]
    chunks[["segments"]] <- purrr::map(seq_along(idx_intersects), function(i) {
        idx <- unname(as.vector(idx_intersects[[i]]))
        idx <- idx[[1L]]
        block_file <- .file_block_name(
            pattern = "chunk_seg",
            block = .block(chunks[i, ]),
            output_dir = output_dir,
            ext = "gpkg"
        )
        .vector_write_vec(segs[idx, ], block_file, append = TRUE)
        block_file
    })
    chunks
}
