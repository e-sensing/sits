#' @title Creates a connection to an RSQLite database
#' @name sits_db_connect
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This functions creates a connection to an RSQLite database
#' to be able to store the data and metadata tables associated to time series
#' and data cubes associated to the "sits" suite of packages.
#'
#' @param  name     Name of the database (either a file or memory if empty)
#' @return          A connection to an RSQLite database
#' @examples
#' # name of file to store the RSQLite database
#' db_file <- paste0(Sys.getenv("HOME"), "/sits.sql")
#' # create RSQLite connection
#' conn <- sits_db_connect(db_file)
#' # clean up
#' unlink(db_file)
#' @export
sits_db_connect <- function(name = NULL) {
    # verifies if DBI package is installed
    if (!requireNamespace("DBI", quietly = TRUE)) {
        stop("DBI needed for this function to work. Please install it.",
            call. = FALSE
        )
    }
    # verifies if RSQLite package is installed
    if (!requireNamespace("RSQLite", quietly = TRUE)) {
        stop("RSQLite needed for this function to work. Please install it.",
            call. = FALSE
        )
    }
    if (purrr::is_null(name)) {
          name <- ":memory:"
      }

    conn <- DBI::dbConnect(RSQLite::SQLite(), name)

    return(conn)
}
#' @title List the time series and data cubes stored in an SQLite database
#' @name sits_db_info
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This functions describes the time series and data cubes
#' stored RSQLite database.
#'
#' @param  conn     SQLite connection
#' @return          Description
#' @examples
#' \dontrun{
#' # create a data base
#' db_file <- paste0(tempdir(), "/sits.sql")
#' conn <- sits_db_connect(db_file)
#' # write a set of time series
#' conn <- sits_db_write(conn, "cerrado_2classes", cerrado_2classes)
#' # describe the data available in the database
#' desc <- sits_db_info(conn)
#' # clean up
#' unlink(db_file)
#' }
#' @export
sits_db_info <- function(conn) {
    # connect to the database
    conn <- DBI::dbConnect(conn)
    # assert that the connection is valid
    assertthat::assert_that(DBI::dbIsValid(conn),
        msg = "Invalid database connection"
    )

    # list all tables available
    tables <- DBI::dbListTables(conn)

    # filter all extensions and leave only the original tables
    patterns <- c(".par", ".tim", ".lab", ".ts", ".fil")
    matches <- patterns %>%
      purrr::map(function(p) {stringr::str_detect(tables, stringr::fixed(p))})

    tables <- tables[!Reduce("|",matches)]

    tables_lst <- tables %>%
        purrr::map(function(tab) {
            data <- sits_db_read(conn, tab)
            # process the data
            if ("sits" %in% class(data)) {
                bbox <- .sits_bbox_time_series(data)
                desc <- tibble::tibble(
                    name = tab,
                    cube = data[1, ]$cube,
                    class = class(data)[1],
                    size = paste0(nrow(data), " samples"),
                    bands = paste(sits_bands(data), collapse = ", "),
                    b_box = paste0(
                        "(", round(bbox["xmin"], 2), ",",
                        round(bbox["ymin"], 2), "), ",
                        "(", round(bbox["xmax"], 2), ",",
                        round(bbox["ymax"], 2), ")"
                    ),
                    crs = "EPSG:3426",
                    labels = paste(sits_labels(data)$label, collapse = ", ")
                )
                return(desc)
            }
            else {

                # find how many instances are there
                timeline <- sits_timeline(data)
                start_date <- timeline[1]
                end_date <- timeline[length(timeline)]
                band_info <- data$bands[[1]]
                if (length(band_info) <= 3) {
                      bands <- paste(data$bands[[1]], collapse = ", ")
                  } else {
                      bands <- paste0(
                          "[", band_info[1], ",...,",
                          band_info[length(band_info)],
                          "] (", length(band_info), " bds)"
                      )
                  }

                ll_inf <- .sits_proj_to_latlong(
                    data$xmin, data$ymin,
                    as.character(data$crs)
                )
                ll_sup <- .sits_proj_to_latlong(
                    data$xmax, data$ymax,
                    as.character(data$crs)
                )

                desc <- tibble::tibble(
                    name = tab,
                    cube = data$name,
                    class = class(data)[1],
                    start_date = as.Date(start_date),
                    end_date = as.Date(end_date),
                    bands = bands,
                    b_box = paste0(
                        "(", signif(ll_inf[1, "longitude"], digits = 4), ",",
                        signif(ll_inf[1, "latitude"], digits = 4), "), ",
                        "(", signif(ll_sup[1, "longitude"], digits = 4), ",",
                        signif(ll_sup[1, "latitude"], digits = 4), ")"
                    ),
                    crs = data$crs,
                    labels = paste(data$labels[[1]], collapse = ", ")
                )
                return(desc)
            }
        })
    # collect all descriptions
    desc <- dplyr::bind_rows(tables_lst)

    # disconnect from database
    DBI::dbDisconnect(conn)

    return(desc)
}
#' @title Write time series and data cubes information on an SQLite database
#' @name sits_db_write
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This functions write a set of time series or data cubes
#' to an RSQLite database.
#'
#' @param  conn     SQLite connection
#' @param  name     Name of the object to be storeed
#' @param  data     Data to be stored
#' @return          A connection to an RSQLite database
#' @examples
#' \dontrun{
#' # create RSQLite connection
#' db_file <- paste0(tempdir(), "/sits.sql")
#' conn <- sits_db_connect(db_file)
#' # write a set of time series
#' conn <- sits_db_write(conn, "cerrado_2classes", cerrado_2classes)
#' }
#' @export
sits_db_write <- function(conn, name, data) {

    # does the data exist
    assertthat::assert_that(nrow(data) > 0, msg = "no data to save")


    assertthat::assert_that("sits" %in% class(data) |
                              "raster_cube" %in% class(data),
                            msg = "sits_db_write: data class not supported"
    )
    # write a set of time series stored as a sits tibble
    UseMethod("sits_db_write", data)

}
#' @title Write time series on an SQLite database
#' @name sits_db_write.sits
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This functions write a set of time series
#' to an RSQLite database.
#'
#' @param  conn     SQLite connection
#' @param  name     Name of the object to be stored
#' @param  data     Data to be stored
#' @return          A connection to an RSQLite database
#' @export
sits_db_write.sits <- function(conn, name, data) {
    # connect to the database
    conn <- DBI::dbConnect(conn)
    # assert that the connection is valid
    assertthat::assert_that(DBI::dbIsValid(conn),
                          msg = "invalid database connection"
    )
    # save the metadata on the database
    DBI::dbWriteTable(
        conn = conn, name = name, overwrite = TRUE,
        value = as.data.frame(data[, c(
                "longitude", "latitude",
                "start_date", "end_date",
                "label", "cube"
                )]
        )
    )
    # transform the time series into a list
    ts_lst <- purrr::map2(
        data$time_series, seq_len(nrow(data)),
            function(ts, i) {
                 ts <- dplyr::mutate(ts, row = i)
            }
    )
    # melt the list
    ts_melt <- dplyr::bind_rows(ts_lst)

    DBI::dbWriteTable(
        conn = conn, name = paste0(name, ".ts"),
        value = as.data.frame(ts_melt),
        overwrite = TRUE
    )
    DBI::dbDisconnect(conn)
    return(conn)
}
#' @title Write cube on an SQLite database
#' @name sits_db_write.raster_cube
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This functions write a tibble with metadata about a cube
#' to an RSQLite database.
#'
#' @param  conn     SQLite connection
#' @param  name     Name of the object to be storeed
#' @param  data     Data to be stored
#' @return          A connection to an RSQLite database
#' @export
#'
sits_db_write.raster_cube <- function(conn, name, data) {
    # connect to the database
    conn <- DBI::dbConnect(conn)
    # save the metadata on the database
    DBI::dbWriteTable(
        conn = conn, name = name,
        overwrite = TRUE,
        value = as.data.frame(data[, c(
            "type",
            "URL",
            "satellite",
            "sensor",
            "name",
            "cube",
            "tile",
            "nrows",
            "ncols",
            "xmin",
            "xmax",
            "ymin",
            "ymax",
            "xres",
            "yres",
            "crs"
        )])
    )

    # build the parameters and bands tibble
    params_data <- slider::slide(data, function(row) {
        # build the tibble with the parameters
        params <- purrr::pmap(
            list(
                row$bands[[1]], row$scale_factors,
                row$missing_values, row$minimum_values,
                row$maximum_values
            ),
            function(band, scale, miss, min, max) {
                par <- tibble::tibble(
                    band = band,
                    scale_factor = scale,
                    missing_value = miss,
                    minimum_value = min,
                    maximum_value = max,
                    name = row$name
                )
                return(par)
            }
        )
        # joint all lists of params into a single table
        pars_one_table <- dplyr::bind_rows(params)
        return(pars_one_table)
    })
    params <- dplyr::bind_rows(params_data)

    # save the params tibble
    DBI::dbWriteTable(
        conn = conn, name = paste0(name, ".par"),
        value = as.data.frame(params),
        overwrite = TRUE
    )

    # timelines tibble
    timelines_std <- tibble::tibble(
        date = as.Date(character()),
        name = character(),
        instance = integer()
    )
    # build the timelines tibble
    times_rows <- slider::slide(data, function(row) {
        # transform information about the timelines into a tibble
        timeline_row <- row$timeline[[1]]
        n_instances  <- length(timeline_row)
        assertthat::assert_that(n_instances > 0,
                                msg = "invalid timeline in cube"
        )

        # in a cube, a timeline is a list of timelines to account for the
        # classified image
        # build a tibble for each timeline

        timelines_inst_lst <- seq_len(n_instances) %>%
            purrr::map(function(n){
                times_instance <- lubridate::as_date(timeline_row[[n]])
                n_times <- length(times_instance)
                assertthat::assert_that(n_times > 0,
                                        msg = "timeline with no dates"
                )
                times <- seq_len(n_times) %>%
                    purrr::map(function(t) {
                        time <- tibble::tibble(
                            date = times_instance[t],
                            name = row$name,
                            instance = t
                        )
                        return(time)
                    })
                timeline_inst <- dplyr::bind_rows(times)
                return(timeline_inst)
            })
        timelines_std <- dplyr::bind_rows(timelines_inst_lst)
        return(timelines_std)
    })
    timelines <- dplyr::bind_rows(times_rows)
    # save the timelines tibble
    DBI::dbWriteTable(
        conn = conn, name = paste0(name, ".tim"),
        value = as.data.frame(timelines),
        overwrite = TRUE
    )

    # labels
    if (!purrr::is_null(data$labels)) {
        labels_std <- tibble::tibble(
            label = character(),
            name = character()
        )
        labs_rows <- slider::slide(data, function(row) {
            # build the labels table
            label_tabs <- purrr::pmap(row$labels, function(lab) {
                lb <- tibble::tibble(
                    label = lab,
                    name = row$name
                )
                return(lb)
            })
            # transform the list into a tibble
            lab <- dplyr::bind_rows(labels_std, label_tabs)
            return(lab)
        })
        labels <- dplyr::bind_rows(labs_rows)
        # save the labels tibble
        DBI::dbWriteTable(
            conn = conn, name = paste0(name, ".lab"),
            value = as.data.frame(labels),
            overwrite = TRUE
        )
    }
    # file_info
    file_info_std <- tibble::tibble(
        res = character(),
        band = character(),
        date = as.Date(character()),
        path = character()
    )
    fi_rows <- slider::slide(data, function(row) {
        fi <- row$file_info[[1]]
        fs <- purrr::pmap(list(fi$band, fi$date, fi$path),
                          function(b, d, p) {
                              f <- tibble::tibble(band = b,
                                                  date = d,
                                                  path = p
                              )
                              return(f)
                          })
        fi_row <- dplyr::bind_rows(file_info_std, fs)
        return(fi_row)
    })
    file_info <- dplyr::bind_rows(fi_rows)
    # save the files tibble
    DBI::dbWriteTable(
        conn = conn, name = paste0(name, ".fil"),
        value = as.data.frame(file_info),
        overwrite = TRUE
    )
    DBI::dbDisconnect(conn)
    return(conn)
}
#' @title Read time series and data cubes information on an SQLite database
#' @name sits_db_read
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This functions write a set of time series or data cubes
#' to an RSQLite database.
#'
#' @param  conn     SQLite connection
#' @param  name     Name of the object to be read
#' @return          Tibble with time series
#' @examples
#' \dontrun{
#' # create RSQLite connection
#' conn <- sits_db_connect()
#' # write a set of time series
#' conn <- sits_db_write(conn, "cerrado_2classes", cerrado_2classes)
#' # read a set of time series
#' ts <- sits_db_read(conn, "cerrado_2classes")
#' }
#' @export
sits_db_read <- function(conn, name) {

    conn <- DBI::dbConnect(conn)
    # assert that the connection is valid
    assertthat::assert_that(DBI::dbIsValid(conn),
        msg = "Invalid database connection"
    )
    # assert that the table exists
    assertthat::assert_that(DBI::dbExistsTable(conn, name),
        msg = "Table does not exist in the database"
    )

    # read the metadata on the database
    data <- tibble::as_tibble(DBI::dbReadTable(conn, name))

    # assert that there is data in the table
    assertthat::assert_that(nrow(data) > 0,
        msg = "Table contains no data"
    )
    if ("longitude" %in% names(data))
        class(data) <- c("sits", class(data))
    else
        class(data) <- c("raster_cube", class(data))

    UseMethod("sits_db_read", data)
}

#' @title Read time series from an SQLite database
#' @name sits_db_read.sits
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function reads a set of time series
#' to an RSQLite database.
#'
#' @param  conn     SQLite connection
#' @param  name     Name of the object to be storeed
#' @param  data     Data to be retrived
#' @return          A sits tibble
#' @export
sits_db_read.sits <- function(conn, name) {
    conn <- DBI::dbConnect(conn)
    # read the metadata on the database
    data <- tibble::as_tibble(DBI::dbReadTable(conn, name))
    n_rows <- nrow(data)
    # set the start and end dates
    data$start_date <- lubridate::as_date(data$start_date)
    data$end_date <- lubridate::as_date(data$end_date)
    data <- tibble::as_tibble(data)

    # read the melted time series
    ts_melt <- DBI::dbReadTable(conn, paste0(name, ".ts"))
    ts_melt$Index <- lubridate::as_date(ts_melt$Index)

    # reconstruct the time series list
    ts2_lst <- purrr::map(1:n_rows, function(i) {
        dplyr::filter(ts_melt, row == i) %>%
            dplyr::select(-c(row)) %>%
            tibble::as_tibble()
    })
    # insert the time series in the database
    data$time_series <- ts2_lst

    class(data) <- c("sits", class(data))
    # disconnect
    DBI::dbDisconnect(conn)

    return(data)
}


#' @title Read cube information from an SQLite database
#' @name sits_db_read.raster_cube
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This functions write a tibble with metadata about a cube
#' to an RSQLite database.
#'
#' @param  conn      SQLite connection
#' @param  name      Name of the cube table to be retrieved
#' @return  data     Tibble with metadata about the cube
#' @export
sits_db_read.raster_cube <- function(conn, name) {

    conn <- DBI::dbConnect(conn)
    # read the metadata on the database
    cubes <- tibble::as_tibble(DBI::dbReadTable(conn, name))
    # go through the rows of the table
    rows <- purrr::map(list(cubes), function(row) {
        meta <- dplyr::filter(cubes, name == row$name)

        # read the params tibble
        par <- tibble::as_tibble(
            DBI::dbReadTable(conn = conn, name = paste0(name, ".par"))
        )

        # retrieve params
        bands <- par %>%
            dplyr::filter(name == row$name) %>%
            dplyr::select(band) %>%
            dplyr::pull(.)

        missing_values <- par %>%
            dplyr::filter(name == row$name) %>%
            dplyr::select(missing_value) %>%
            dplyr::pull(.)

        names(missing_values) <- bands

        minimum_values <- par %>%
            dplyr::filter(name == row$name) %>%
            dplyr::select(minimum_value) %>%
            dplyr::pull(.)

        names(minimum_values) <- bands

        maximum_values <- par %>%
            dplyr::filter(name == row$name) %>%
            dplyr::select(maximum_value) %>%
            dplyr::pull(.)

        names(maximum_values) <- bands

        scale_factors <- par %>%
            dplyr::filter(name == row$name) %>%
            dplyr::select(scale_factor) %>%
            dplyr::pull(.)

        names(scale_factors) <- bands

        # read labels tibble
        if (DBI::dbExistsTable(conn, paste0(name, ".lab"))) {
            labs <- tibble::as_tibble(
                DBI::dbReadTable(conn = conn, name = paste0(name, ".lab"))
            )

            labels <- labs %>%
                dplyr::filter(name == row$name) %>%
                dplyr::select(label) %>%
                dplyr::pull(.) %>%
                as.character(.)
        }
        else {
              labels <- NA
          }


        # read timelines tibble
        times <- tibble::as_tibble(
            DBI::dbReadTable(conn = conn, name = paste0(name, ".tim"))
        )
        times$date <- lubridate::as_date(times$date)
        instances <- times %>%
            dplyr::filter(name == row$name) %>%
            dplyr::select(date, instance)
        # retrieve the timelines for the cube (there may be more than one)
        indexes <- unique(instances$instance)
        times_cube <- purrr::map(list(indexes), function(i) {
            timeline <- instances %>%
                dplyr::filter(instance == i) %>%
                dplyr::select(date)
            timeline <- lubridate::as_date(timeline$date)
            return(timeline)
        })

        if (DBI::dbExistsTable(conn, paste0(name, ".fil"))) {
            # read files tibble
            # save the files tibble
            file_info <- tibble::as_tibble(
                DBI::dbReadTable(conn = conn, name = paste0(name, ".fil"))
            )
            file_info$date <- lubridate::as_date(file_info$date)
        }

        # define the output cube
        cube <- tibble::tibble(
            type = meta$type,
            URL = meta$URL,
            satellite = meta$satellite,
            sensor = meta$sensor,
            name = meta$name,
            cube = meta$cube,
            tile = meta$tile,
            bands = list(bands),
            labels = list(labels),
            scale_factors = list(scale_factors),
            missing_values = list(missing_values),
            minimum_values = list(minimum_values),
            maximum_values = list(maximum_values),
            timeline = list(times_cube),
            nrows = meta$nrows,
            ncols = meta$ncols,
            xmin = meta$xmin,
            xmax = meta$xmax,
            ymin = meta$ymin,
            ymax = meta$ymax,
            xres = meta$xres,
            yres = meta$yres,
            crs = meta$crs,
            file_info = list(file_info)
        )

        return(cube)
    })

    data <- dplyr::bind_rows(rows)
    class_cube <- .sits_config_cube_class(data[1, ]$type)
    class(data) <- c(class_cube, "raster_cube", class(data))

    # disconnect
    DBI::dbDisconnect(conn)

    return(data)
}
