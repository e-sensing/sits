#' @title Extract a four-digit year from a date argument
#' @keywords internal
#' @noRd
#' @return integer year, or NULL when \code{date} is NULL.
.aef_year <- function(date) {
    # check date
    if (is.null(date)) {
        return(NULL)
    }

    # extract year only
    as.integer(substr(as.character(date), 1L, 4L))
}

#' @title Resolve Earth-Engine image ids for the selected tiles
#' @keywords internal
#' @noRd
#' 
#' @description Queries the remote GeoParquet index with a numeric join on the
#' tile UTM origin and returns the image id of each matching (tile, year). Only 
#' the requested years are kept.
#'
#' @param index_url  URL of the remote GeoParquet index file.
#' @param tiles_sf   sf returned by \code{.grid_filter_tiles("ALPHAEARTH", ...)}.
#' @param start_year integer lower year bound, or NULL.
#' @param end_year   integer upper year bound, or NULL.
#'
#' @return tibble files information.
.aef_resolve_files <- function(index_url, tiles_sf, start_year, end_year) {
    # check packages
    .check_require_packages("duckdb")

    # open remote parquet file
    # we assume it is a remote parquet as it is huge and we 
    # are not bundling it
    con <- DBI::dbConnect(duckdb::duckdb())

    # effect - disconnect on exit
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

    # enable remote reads (range requests)
    DBI::dbExecute(con, "INSTALL httpfs; LOAD httpfs;")

    # prepare query: wanted footprints as a VALUES table (id, crs, UTM origin)
    want <- paste(
        sprintf(
            "(%s,%s,%s,%s)",
            DBI::dbQuoteString(con, tiles_sf[["tile_id"]]),
            DBI::dbQuoteString(con, tiles_sf[["crs"]]),
            format(tiles_sf[["xmin"]], scientific = FALSE, trim = TRUE),
            format(tiles_sf[["ymin"]], scientific = FALSE, trim = TRUE)
        ),
        collapse = ","
    )

    # optional filter: year bounds
    year_filter <- ""

    # start year - if available, add to the query
    if (!is.null(start_year)) {
        year_filter <- paste0(year_filter, sprintf(" AND i.year >= %d", start_year))
    }

    # end year - if available, add to the query
    if (!is.null(end_year)) {
        year_filter <- paste0(year_filter, sprintf(" AND i.year <= %d", end_year))
    }

    # prepare query: 
    # about the image id:
    #   it is the file base name without the trailing "-<offY>-<offX>",
    #   so we extract it using a regular expression
    sql <- sprintf(
        "WITH want(tile_id, crs, uw, us) AS (VALUES %s)
         SELECT w.tile_id AS tile_id,
                CAST(i.year AS INTEGER) AS year,
                regexp_replace(
                    regexp_extract(i.path, '([^/]+)[.]tiff?$', 1),
                    '-[0-9]+-[0-9]+$', ''
                ) AS image_id
         FROM read_parquet(%s) i
         JOIN want w
           ON i.crs = w.crs
          AND CAST(i.utm_west  AS BIGINT) = CAST(w.uw AS BIGINT)
          AND CAST(i.utm_south AS BIGINT) = CAST(w.us AS BIGINT)
         WHERE TRUE%s
         ORDER BY w.tile_id, i.year",
        want, DBI::dbQuoteString(con, index_url), year_filter
    )

    # return result as tibble
    tibble::as_tibble(DBI::dbGetQuery(con, sql))
}

#' @title Build a streamable single-band path for one tile/year/band
#' @keywords internal
#' @noRd
#' 
#' @description Constructs the file path as a GDAL connection string.
#' 
#' @param url       Base URL of the annual product.
#' @param tile_px   Tile side length in pixels.
#'
#' @return character GDAL connection string(s).
.aef_vrt_path <- function(url, tile_px, utm_zone, col, row, year,
                          image_id, band_idx) {
    # calculate file offset
    off_y <- (row %% 2L) * tile_px
    off_x <- (col %% 2L) * tile_px

    # build file name
    base <- sprintf("%s-%010d-%010d", image_id, off_y, off_x)

    # build file path
    url <- sprintf("/vsicurl/%s%d/%s/%s.tiff", url, year, utm_zone, base)

    # return GDAL connection string
    sprintf("vrt://%s?bands=%d", url, band_idx)
}

#' @title Build the file_info for an AlphaEarth cube
#' @keywords internal
#' @noRd
#' 
#' @param source      Data source.
#' @param collection  Image collection.
#' @param grid_system Grid system name.
#' @param tiles_sf    sf with tile geometry/CRS (UTM origin and bbox).
#' @param files       tibble(tile_id, year, image_id) from \code{.aef_resolve_files}.
#' @param bands       character vector of requested bands.
#' @param url         Base URL of the annual product.
#' 
#' @return tibble with file information.
.aef_file_info <- function(source, collection, grid_system, tiles_sf, files,
                           bands, url) {
    # get all bands
    all_bands <- .source_bands(source = source, collection = collection)

    # get VRT band index of each band (e.g., A00 -> 1, ..., A63 -> 64)
    band_idx <- stats::setNames(seq_along(all_bands), all_bands)

    # get tile side size
    tile_px <- as.integer(.conf("grid_systems", grid_system, "nrows"))

    # get tile resolution
    res <- .conf("grid_systems", grid_system, "xres")

    # parse tile id
    parts <- strsplit(files[["tile_id"]], "-", fixed = TRUE)

    # extract from tile id: UTM zone
    utm_zone <- purrr::map_chr(parts, 1L)

    # extract from tile id: tile column and row
    col <- as.integer(purrr::map_chr(parts, 2L))
    row <- as.integer(purrr::map_chr(parts, 3L))

    # tile-level geometry, joined into the (tile, year) rows
    tile_geom <- sf::st_drop_geometry(tiles_sf)
    tile_geom <- tile_geom[match(files[["tile_id"]], tile_geom[["tile_id"]]), ]

    # expand to one row per band
    file_info <- purrr::map_dfr(bands, function(band) {
        # build file path
        path <- .aef_vrt_path(
            url = url,
            tile_px = tile_px,
            utm_zone = utm_zone,
            col = col,
            row = row,
            year = files[["year"]],
            image_id = files[["image_id"]],
            band_idx = band_idx[[band]]
        )

        # define tile date
        tile_date <- as.Date(sprintf("%d-01-01", files[["year"]]))

        # build file_info
        tibble::tibble(
            tile = files[["tile_id"]],
            fid = files[["image_id"]],
            date = tile_date,
            band = band,
            path = path,
            xres = res,
            yres = res,
            xmin = tile_geom[["xmin"]],
            ymin = tile_geom[["ymin"]],
            xmax = tile_geom[["xmax"]],
            ymax = tile_geom[["ymax"]],
            nrows = tile_px,
            ncols = tile_px,
            crs = tile_geom[["crs"]],
            cloud_cover = 0
        )
    })

    # return
    file_info
}

#' @title Assemble AlphaEarth tiles into a sits cube
#' @keywords internal
#' @noRd
#' 
#' @description Nests the file_info by tile and builds one cube row per
#' tile via \code{.cube_create}, cloning the STAC operation.
#' 
#' @param source      Data source.
#' @param collection  Image collection.
#' @param file_info   file_info tibble.
#' 
#' @return sits cube.
.aef_cube_assemble <- function(source, collection, file_info) {
    # get satellite and sensor
    satellite <- .source_collection_satellite(source, collection)
    sensor <- .source_collection_sensor(source, collection)

    # nest file_info by tile
    cube <- file_info |>
        dplyr::mutate(crs2 = .data[["crs"]]) |>
        tidyr::nest(file_info = -dplyr::matches(c("tile", "crs2"))) |>
        dplyr::rename(crs = "crs2") |>
        slider::slide_dfr(function(tile) {
            # get file_info
            fi <- tile[["file_info"]][[1L]]

            # get bbox
            bbox <- .source_tile_get_bbox(
                source = source, file_info = fi, collection = collection
            )

            # create cube row
            .cube_create(
                source     = source,
                collection = collection,
                satellite  = satellite,
                sensor     = sensor,
                tile       = tile[["tile"]],
                xmin       = bbox[["xmin"]],
                xmax       = bbox[["xmax"]],
                ymin       = bbox[["ymin"]],
                ymax       = bbox[["ymax"]],
                crs        = tile[["crs"]],
                file_info  = fi
            )
        })
}


#' @title Test access to the AlphaEarth collection
#' @keywords internal
#' @noRd
#' @description AEF has no online catalogue service to probe, so the standard 
#' dry-run is a no-op. Reachability of the remote index/COGs is surfaced 
#' naturally when the cube is built.
#' @export
.source_collection_access_test.alphaearth_cube <- function(source, collection,
                                                           bands, ...) {
    return(invisible(source))
}

#' @title Create an AlphaEarth data cube
#' @keywords internal
#' @noRd
#' @description Builds an AEF cube. Resolves footprints from the ALPHAEARTH
#' grid, looks up the Earth-Engine image ids for the requested years from the
#' remote index, builds one streamable single-band VRT path per band, and
#' assembles the cube reusing the standard tile/file_info layout.
#' @export
.source_cube.alphaearth_cube <- function(source,
                                         collection,
                                         bands,
                                         tiles,
                                         roi,
                                         start_date,
                                         end_date,
                                         platform,
                                         multicores,
                                         progress, ...) {
    # set caller
    .check_set_caller(".source_cube_alphaearth_cube")
    
    # get grid system
    grid_system <- .source_collection_grid_system(source, collection)

    # get urls
    url <- .source_url(source)
    index_url <- .conf("sources", source, "index_url")

    # filter tiles
    tiles_sf <- .grid_filter_tiles(
        grid_system = grid_system, roi = roi, tiles = tiles
    )

    # check tiles
    .check_that(nrow(tiles_sf) > 0L,
        msg = "no AlphaEarth tiles match the requested roi/tiles."
    )

    # resolve the Earth-Engine image id for each (tile, year) from the index
    files <- .aef_resolve_files(
        index_url  = index_url,
        tiles_sf   = tiles_sf,
        start_year = .aef_year(start_date),
        end_year   = .aef_year(end_date)
    )

    # check files
    .check_that(nrow(files) > 0L,
        msg = "no AlphaEarth data found for the requested tiles/dates."
    )

    # build the file_info (one row per tile x year x band)
    file_info <- .aef_file_info(
        source = source, collection = collection, grid_system = grid_system,
        tiles_sf = tiles_sf, files = files, bands = bands,
        url = url
    )

    # assemble the cube (nest by tile, one row per tile)
    cube <- .aef_cube_assemble(
        source = source, collection = collection, file_info = file_info
    )

    # set class
    class(cube) <- .cube_s3class(cube)

    # return!
    cube
}