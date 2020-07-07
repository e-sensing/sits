#' @title Creates a connection to an RSQLite database
#' @name sits_db_create
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This functions creates a connection to an RSQLite database
#' to be able to store the data and metadata tables associated to time series
#' and data cubes associated to the "sits" suite of packages.
#'
#' @param  name     Name of the database
#' @return          A connection to an RSQLite database
#' @examples
#' # create RSQLite connection
#' conn <- sits_db_create()
#' @export
sits_db_create <- function(name = NULL){
    if (purrr::is_null(name))
        name <- ":memory:"

    conn <- DBI::dbConnect(RSQLite::SQLite(), name)

    return(conn)
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
#' \donttest{
#' # create RSQLite connection
#' conn <- sits_db_create("sits.sql")
#' # write a set of time series
#' conn <- sits_db_write(conn, "cerrado_2classes", cerrado_2classes)
#' }
#' @export
sits_db_write <- function(conn, name, data){

    # does the data exist
    assertthat::assert_that(nrow(data) > 0, msg = "no data to save")

    # connect to the database
    if(!as.logical(grep("memory", conn@dbname)))
        conn <-  DBI::dbConnect(conn)
    # assert that the connection is valid
    assertthat::assert_that(DBI::dbIsValid(conn),
                            msg = "Invalid database connection")

    # write a set of time series stored as a sits tibble
    if ("sits" %in% class(data))
        # save a set of time series
        .sits_db_write_ts(conn, name, data)
    else if (.sits_config_cube_classes_chk(class(data)[1]))
        # save a data cube
        .sits_db_write_cube(conn, name, data)
    else
        message("sits_db_write: data class not supported")

    # disconnect from database
    if(!as.logical(grep("memory", conn@dbname)))
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
#' \donttest{
#' # create RSQLite connection
#' conn <- sits_db_create()
#' # write a set of time series
#' conn <- sits_db_write(conn, "cerrado_2classes", cerrado_2classes)
#' # read a set of time series
#' ts <-  sits_db_read(conn, "cerrado_2classes")
#' }
#' @export
sits_db_read <- function(conn, name) {

    # connect to the database
    if(!as.logical(grep("memory", conn@dbname)))
        conn <-  DBI::dbConnect(conn)
    # assert that the connection is valid
    assertthat::assert_that(DBI::dbIsValid(conn),
                            msg = "Invalid database connection")
    # assert that the table exists
    assertthat::assert_that(DBI::dbExistsTable(conn, name),
                            msg = "Table does not exist in the database")


    # read the metadata on the database
    data <- tibble::as_tibble(DBI::dbReadTable(conn, name))

    # assert that there is data in the table
    assertthat::assert_that(nrow(data) > 0,
                            msg = "Table contains no data")

    # find out what is the object that is being read
    if ("latitude" %in% names(data)) {
        data <- .sits_db_read_ts(conn, name, data)
    }
    else if ("crs" %in% names(data)) {
        data <- .sits_db_read_cube(conn, name, data)
    }
    else {
        message("Table is unknown format to sits")
        return(NULL)
    }
    # disconnect from database
    if(!as.logical(grep("memory", conn@dbname)))
        DBI::dbDisconnect(conn)

    return(data)
}
#' @title Write time series on an SQLite database
#' @name .sits_db_write_ts
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This functions write a set of time series
#' to an RSQLite database.
#'
#' @param  conn     SQLite connection
#' @param  name     Name of the object to be storeed
#' @param  data     Data to be stored
#' @return          A connection to an RSQLite database
.sits_db_write_ts <- function(conn, name, data) {
    # save the metadata on the database
    DBI::dbWriteTable(conn = conn, name = name, overwrite = TRUE,
                      value = as.data.frame(data[, c("longitude", "latitude",
                                                     "start_date", "end_date",
                                                     "label", "cube")]))
    # transform the time series into a list
    # add a row number for retrieval
    ts.lst <- purrr::map2(data$time_series, 1:nrow(data),
                          function(ts, i) {
                              ts <-  dplyr::mutate(ts, row = i) })
    # melt the list
    ts_melt <- dplyr::bind_rows(ts.lst)

    DBI::dbWriteTable(conn = conn, name = paste0(name,".ts"),
                      value = as.data.frame(ts_melt),
                      overwrite = TRUE)

}
#' @title Read time series from an SQLite database
#' @name .sits_db_read_ts
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function reads a set of time series
#' to an RSQLite database.
#'
#' @param  conn     SQLite connection
#' @param  name     Name of the object to be storeed
#' @param  data     Data to be retrived
#' @return          A sits tibble
.sits_db_read_ts <- function(conn, name, data) {
    n_rows <- nrow(data)
    # set the start and end dates
    data$start_date <- lubridate::as_date(data$start_date)
    data$end_date   <- lubridate::as_date(data$end_date)
    data <- tibble::as_tibble(data)

    # read the melted time series
    ts_melt <- DBI::dbReadTable(conn, paste0(name, ".ts"))
    ts_melt$Index <- lubridate::as_date(ts_melt$Index)

    # reconstruct the time series list
    ts2.lst <- purrr::map(1:n_rows, function(i) {
        dplyr::filter(ts_melt, row == i) %>%
            dplyr::select(-c(row)) %>%
            tibble::as_tibble()
    })
    # insert the time series in the database
    data$time_series <- ts2.lst

    class(data) <- c("sits", class(data))

    return(data)
}
#' @title Write cube on an SQLite database
#' @name .sits_db_write_cube
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This functions write a tibble with metadata about a cube
#' to an RSQLite database.
#'
#' @param  conn     SQLite connection
#' @param  name     Name of the object to be storeed
#' @param  data     Data to be stored
#' @return          A connection to an RSQLite database
.sits_db_write_cube <- function(conn, name, data) {

    # save the metadata on the database
    DBI::dbWriteTable(conn = conn, name = name,
                      overwrite = TRUE,
                      value = as.data.frame(data[,c("type",
                                                    "URL",
                                                    "satellite",
                                                    "sensor",
                                                    "name",
                                                    "nrows",
                                                    "ncols",
                                                    "xmin",
                                                    "xmax",
                                                    "ymin",
                                                    "ymax",
                                                    "xres",
                                                    "yres",
                                                    "crs")]))


    # set the layouts of the additional tibbles to be saved
    # raster data parameters
    params.tb <- tibble::tibble(band           = character(),
                                scale_factor   = double(),
                                missing_value  = double(),
                                minimum_value  = double(),
                                maximum_value  = double(),
                                name           = character())
    # labels
    labels.tb <- tibble::tibble(label          = character(),
                                name           = character())

    # bands
    bands.tb <- tibble::tibble(band           = character(),
                               name           = character())

    # timelines
    timelines.tb <- tibble::tibble(date = as.Date(character()),
                                   name = character(),
                                   instance = integer())
    # files
    files.tb <- tibble::tibble(file = character(),
                               name = character())

    # go through the rows of the cube tibble
    nrows <- nrow(data)
    for (i in 1:nrows) {
        row <- data[i,]

        # build the tibble with the parameters
        params.lst <- purrr::pmap(list(row$bands, row$scale_factors,
                                       row$missing_values, row$minimum_values,
                                       row$maximum_values),
                                  function(band, scale, miss, min, max) {
                                      params <- tibble::tibble(
                                          band           = band,
                                          scale_factor   = scale,
                                          missing_value  = miss,
                                          minimum_value  = min,
                                          maximum_value  = max,
                                          name           = row$name)
                                      return(params)
                                  })
        # joint all lists of params into a single table
        params.tb <- dplyr::bind_rows(params.tb, params.lst)


        # build the labels table
        labels.lst <- purrr::pmap(row$labels, function(lab) {
            lab.tb <- tibble::tibble(label = lab,
                                     name  = row$name)
        })
        # transform the list into a tibble
        labels.tb <- dplyr::bind_rows(labels.tb, labels.lst)

        # transform information about the timelines into a tibble

        timelines <- row$timeline[[1]]

        n_instances <- length(timelines)

        # in a cube, a timeline is a list of timelines to account for the
        # classified image
        # build a tibble for each timeline

        for (i in 1:n_instances) {
            times_instance <- timelines[[i]]
            n_times <- length(times_instance)
            for (j in 1:n_times) {
                time.tb <- tibble::tibble(
                    date = as.Date(times_instance[j]),
                    name = row$name,
                    instance = j)
                timelines.tb <- dplyr::bind_rows(timelines.tb, time.tb)
            }
        }
        # build a list for all files of for each cube
        files.lst <- purrr::map(row$files, function(f) {
            f <- tibble::tibble(file = f,
                                name = row$name)
        })
        files.tb <- dplyr::bind_rows(files.tb, files.lst)

    }
    # save the params tibble
    DBI::dbWriteTable(conn = conn, name = paste0(name,".par"),
                      value = as.data.frame(params.tb),
                      overwrite = TRUE)

    # save the labels tibble
    DBI::dbWriteTable(conn = conn, name = paste0(name,".lab"),
                      value = as.data.frame(labels.tb),
                      overwrite = TRUE)

    # save the timelines tibble
    DBI::dbWriteTable(conn = conn, name = paste0(name,".tim"),
                      value = as.data.frame(timelines.tb),
                      overwrite = TRUE)

    # save the files tibble
    DBI::dbWriteTable(conn = conn, name = paste0(name,".fil"),
                      value = as.data.frame(files.tb),
                      overwrite = TRUE)
}

#' @title Read cube information from an SQLite database
#' @name .sits_db_read_cube
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This functions write a tibble with metadata about a cube
#' to an RSQLite database.
#'
#' @param  conn      SQLite connection
#' @param  name      Name of the cube table to be retrieved
#' @param  cubes     Tibble with cubes information
#' @return  data     Tibble with metadata about the cube
.sits_db_read_cube <- function(conn, name, cubes) {

    # read the basic information
    n_rows <- nrow(cubes)

    # go through the rows of the table
    rows.lst <-  purrr::map(list(cubes), function(row){

        meta <- dplyr::filter(cubes, name == row$name)

        # read the params tibble
        par <- tibble::as_tibble(
            DBI::dbReadTable(conn = conn, name = paste0(name,".par")))
        # retrieve params
        missing_values <- par %>%
            dplyr::filter(name == row$name) %>%
            dplyr::select(missing_value) %>%
            dplyr::pull(.)

        minimum_values <- par %>%
            dplyr::filter(name == row$name) %>%
            dplyr::select(minimum_value) %>%
            dplyr::pull(.)

        maximum_values <- par %>%
            dplyr::filter(name == row$name) %>%
            dplyr::select(maximum_value) %>%
            dplyr::pull(.)

        scale_factors <- par %>%
            dplyr::filter(name == row$name) %>%
            dplyr::select(scale_factor) %>%
            dplyr::pull(.)

        bands <- par %>%
            dplyr::filter(name == row$name) %>%
            dplyr::select(band) %>%
            dplyr::pull(.)

        # read labels tibble
        labs <- tibble::as_tibble(
            DBI::dbReadTable(conn = conn, name = paste0(name,".lab")))

        labels <- labs %>%
            dplyr::filter(name == row$name) %>%
            dplyr::select(label) %>%
            dplyr::pull(.) %>%
            as.character(.)

        # read timelines tibble
        times <- tibble::as_tibble(
            DBI::dbReadTable(conn = conn, name = paste0(name,".tim")))
        instances <- times %>%
            dplyr::filter(name == row$name) %>%
            dplyr::select(date, instance)
        # retrieve the timelines for the cube (there may be more than one)
        indexes <- unique(instances$instance)
        times_cube.lst <- purrr::map(list(indexes), function(i){
            timeline <- instances %>%
                dplyr::filter(instance == i) %>%
                dplyr::select(date)
            timeline <- lubridate::as_date(timeline$date)
        })


        # read files tibble
        # save the files tibble
        files.tb <- tibble::as_tibble(
            DBI::dbReadTable(conn = conn, name = paste0(name,".fil")))

        files <- files.tb %>%
            dplyr::filter(name == row$name) %>%
            dplyr::select(file) %>%
            dplyr::pull(.)

        # define the output cube
        cube <- tibble::tibble(type           = meta$type,
                               URL            = meta$URL,
                               satellite      = meta$satellite,
                               sensor         = meta$sensor,
                               name           = meta$name,
                               bands          = list(bands),
                               labels         = list(labels),
                               scale_factors  = list(scale_factors),
                               missing_values = list(missing_values),
                               minimum_values = list(minimum_values),
                               maximum_values = list(maximum_values),
                               timeline       = list(times_cube.lst),
                               nrows          = meta$nrows,
                               ncols          = meta$ncols,
                               xmin           = meta$xmin,
                               xmax           = meta$xmax,
                               ymin           = meta$ymin,
                               ymax           = meta$ymax,
                               xres           = meta$xres,
                               yres           = meta$yres,
                               crs            = meta$crs,
                               files          = list(files))

    })

    data <- dplyr::bind_rows(rows.lst)
    class_cube <- .sits_config_cube_class(data[1,]$type)
    if(purrr::is_null(class_cube)) {
        class(data) <- c("cube", class(data))
        message("Type of data cube not yet supported by sits")
    }
    else
        class(data) <- c(class_cube, class(data))
    return(data)
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
#' \donttest{
#' # create a data base
#' conn <- sits_db_create("sits.sql")
#' # write a set of time series
#' conn <- sits_db_write(conn, "cerrado_2classes", cerrado_2classes)
#' # describe the data available in the database
#' desc <-  sits_db_info(conn)
#' }
#' @export
sits_db_info <- function(conn){
    # connect to the database
    conn <-  DBI::dbConnect(conn)
    # assert that the connection is valid
    assertthat::assert_that(DBI::dbIsValid(conn),
                            msg = "Invalid database connection")
    # list all tables available
    tables <- DBI::dbListTables(conn)

    # filter all extensions and leave only the original tables
    tables <- tables[!grepl(".par|.tim|.lab|.ts|.fil", tables)]

    # describe the contents of the database
    desc <- tibble::tibble(name = character(),
                           cube   = character(),
                           type   = character(),
                           size   = character(),
                           bands  = character(),
                           b_box  = character(),
                           labels = character())
    # variable that controls

    tables.lst <- tables %>%
        purrr::map(function(tab){
            data <- sits_db_read(conn,tab)
            # is it a time series tibble?
            if ("sits_tibble" %in% class(data)) {
                bbox <- sits_bbox_time_series(data)
                desc <- tibble::tibble(
                    name   = tab,
                    cube   = data[1,]$cube,
                    type   = "time series",
                    size   = paste0(nrow(data), " samples"),
                    bands  = paste(sits_bands(data), collapse = ", "),
                    b_box  = paste0("(",round(bbox["xmin"], 2), ",",
                                    round(bbox["ymin"], 2), "), ",
                                    "(",round(bbox["xmax"], 2),",",
                                    round(bbox["ymax"], 2), ")"),
                    labels = paste(sits_labels(data)$label, collapse = ", ")
                )
                return(desc)
            }
            else if ("cube" %in% class(data)) {

                # find how many instances are there
                if (length(data$timeline[[1]]) == 1)
                    n_times <- length(data$timeline[[1]][[1]])
                else
                    n_times <- length(data$timeline[[1]])

                # get the bounding box in lat long
                xymin <- .sits_proj_to_latlong(data$xmin, data$ymin, data$crs)
                xmin <- xymin[1,"X"]
                ymin <- xymin[1,"Y"]

                xymax <- .sits_proj_to_latlong(data$xmax, data$ymax, data$crs)
                xmax <- xymax[1,"X"]
                ymax <- xymax[1,"Y"]

                desc <- tibble::tibble(
                    name   = tab,
                    cube   = data$name,
                    type   = "data cube",
                    size   = paste0(n_times, " instances"),
                    bands  = paste(data$bands[[1]], collapse = ", "),
                    b_box  = paste0("(",round(xmin, 2), ",",
                                    round(ymin, 2), "), ",
                                    "(",round(xmax, 2),",",
                                    round(ymax, 2), ")"),
                    labels = paste(data$labels[[1]], collapse = ", ")
                )
                return(desc)
            }
        })
    # collect all descriptions
    desc.tb <- dplyr::bind_rows(desc, tables.lst)

    message("-----------------------------------------------")
    message(paste0('Contents of database ', conn@dbname))

    print(knitr::kable(dplyr::select(desc.tb, name, cube, type, size),
                       padding = 0))
    print(knitr::kable(dplyr::select(desc.tb, name, bands, b_box),
                       padding = 0))
    print(knitr::kable(dplyr::select(desc.tb, name, labels), padding = 0))

    return(desc.tb)
}
