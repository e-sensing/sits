#' @title Obtain timeSeries from a JSON file.
#'
#' @name .sits_fromJSON
#'
#' @description reads a set of data and metadata for satellite image time series from a JSON file
#'
#' @param file      string  - name of a JSON file with sits data and metadata
#' @return data.tb   a SITS tibble
.sits_fromJSON <- function(file){
    # add the contents of the JSON file to a SITS tibble
    table <- tibble::as_tibble(jsonlite::fromJSON(file))
    # convert Indexes in time series to dates
    table1 <- sits_tibble()
    table %>%
        purrrlyr::by_row(function(r){
            tb <- tibble::as_tibble(r$time_series[[1]])
            tb$Index <- lubridate::as_date(tb$Index)
            r$time_series[[1]] <- tb
            r$start_date <- lubridate::as_date(r$start_date)
            r$end_date   <- lubridate::as_date(r$end_date)
            table1 <<- dplyr::bind_rows(table1, r)
        })
    return(table1)
}

#' @title Obtain timeSeries from a compressed JSON file.
#'
#' @name .sits_fromGZ
#'
#' @description reads a set of data and metadata for satellite image time series from a compressed JSON file
#'
#' @param  file       string  - name of a compressed JSON file with sits data and metadata
#' @return data.tb    a SITS tibble
.sits_fromGZ <- function(file){

    # uncompress the file
    json_file <- R.utils::gunzip(file, remove = FALSE)
    # retrieve the data
    data.tb <- .sits_fromJSON(json_file)
    # remove the uncompressed file
    file.remove(json_file)

    # return the JSON file
    return(data.tb)
}
