x <- sf::read_sf("~/Downloads/S2A_OPER_GIP_TILPAR_MPC__20151209T095117_V20150622T000000_21000101T000000_B00.kml")


s2 <- purrr::map_dfr(x$description, function(tile) {
    t <- rvest::read_html(tile) |>
        rvest::html_table(convert = FALSE)
    r <- t[[1]][[2]]
    names(r) <- t[[1]][[1]]
    tibble::as_tibble_row(r) |>
        dplyr::select(-MGRS_REF, -LL_WKT)
})

projs <- unique(s2$EPSG)
s2_lst <- purrr::map(projs, function(epsg) {
    dplyr::filter(s2, EPSG == {{epsg}}) |>
        sf::st_as_sf(
            wkt = "UTM_WKT",
            crs = paste0("EPSG:", epsg)
        )
})
names(s2_lst) <- projs

s2_f <- purrr::map_dfr(s2_lst, function(x) {
    dplyr::rowwise(x) |>
        dplyr::mutate(
            bbox = list(c(sf::st_bbox(UTM_WKT)))
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(
            bbox = dplyr::bind_rows(bbox)
        ) |>
        sf::st_segmentize(1098) |>
        sf::st_transform(crs = 4326) |>
        sf::st_set_geometry("geom") |>
        dplyr::transmute(
            tile_id = TILE_ID,
            epsg = EPSG,
            xmin = bbox$xmin,
            ymin = bbox$ymin,
            xmax = bbox$xmax,
            ymax = bbox$ymax
        )
})

s2_tb <- sf::st_drop_geometry(s2_f)
s2_tb2 <- s2_tb |>
    dplyr::transmute(tile_id, epsg, xmin = as.integer(xmin), ymin = as.integer(ymin))

saveRDS(s2_tb2, "inst/extdata/s2-tiles/tiles.rds")
s2_tb2 <- readRDS("inst/extdata/s2-tiles/tiles.rds")

system.time({
    s2_bb2 <- unique(s2_tb2$epsg) |>
        purrr::map(function(x) {
            dplyr::filter(s2_tb2, epsg == x) |>
                dplyr::rowwise() |>
                dplyr::mutate(geom = sf::st_as_sfc(sf::st_bbox(
                    c(xmin = xmin,
                      ymin = ymin,
                      xmax = xmin + 109800,
                      ymax = ymin + 109800)
                ))) |>
                dplyr::ungroup()
        })
})

system.time({
    s2_sf2 <- s2_bb2 |>
        purrr::map_dfr(function(df) {
            df |>
                sf::st_as_sf(
                    sf_column_name = "geom",
                    crs = paste0("EPSG:", df$epsg[[1]])
                ) |>
                sf::st_segmentize(10980) |>
                sf::st_transform(crs = 4326)
        })
})

