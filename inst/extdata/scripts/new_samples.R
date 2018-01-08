library (sits)

cerrado_13classes_new <- sits_tibble()

locs <- dplyr::distinct (cerrado_2classes, longitude, latitude)
locs %>%
    purrrlyr::by_row(function (r){
        long = as.double (r$longitude)
        lat  = as.double (r$latitude)
        # filter only those rows with the same label
        rows <- dplyr::filter (cerrado_13classes_modis_col6, longitude == long, latitude == lat)
        cerrado_13classes_new  <<- dplyr::bind_rows(cerrado_13classes_new, rows)
    })
