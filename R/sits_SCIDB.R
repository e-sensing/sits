#' @title Get unique SciDB columns and rows indexes
#' @name createColRowSequence
#'
#' @description This function returns a sits table with unique SciDB columns and rows indexes.
#'
#' @param col_id a integer index with SciDB column indexes
#' @param row_id a integer index with SciDB row indexes
#' @return sits.tb a sits table
#' @export
createColRowSequence <- function(col_id, row_id) {

  # create sits table
  sits.tb <- sits::sits_table()

  # choose unique col and row values
  col = unique(col_id)
  row = unique(row_id)

  # assign col and row values to a sits tibble
  k = 1
  for (c in col)
    for (r in row){
      sits.tb <- tibble::add_row(sits.tb,
                                 longitude    = c,
                                 latitude     = r)
      k = k + 1
    }

  return(sits.tb)

}

#' @title Create an indexed ordered obsevartion set within a sits_table.
#' @name createZooObject
#'
#' @description This function returns a sits table with an indexed ordered observation set as a zoo object
#'
#' @param  bands a character string vector with the interested bands of the satellite imagery product
#' @param  dates a string vector contaning time series dates
#' @param  scale_factor a real number representing the scale factor of the attribute values
#' @param  idx a integer vector containing the respective index of the attribute values
#' @return ts.tb
#' @export
createZooObject <- function(bands, dates, scale_factor, idx) {

  # create data.frame dynamically
  df <- data.frame(get(bands[1])[idx])
  for(i in 2:length(bands)) df <- cbind(df, get(bands[i])[idx])

  # change column names
  names(df) <- bands

  # create zoo object
  ts <- zoo::zoo(df*scale_factor, dates)

  # convert to a tibble object
  ts.tb <- tibble::as_tibble (zoo::fortify.zoo (ts))

  return(ts.tb)

}

#' @title Exit a connection from SciDB chunk processing.
#' @name exitConnection
#'
#' @description This function closes a SciDB streaming processing connection
#'
#' @param  out list or data.frame with classification data to return to SciDB
#' @param  connection connection object to flush
#' @export
exitConnection <- function(out, connection) {

  # binary data to connection
  writeBin(serialize(c(out), NULL, xdr=FALSE), connection)

  # flushes the output stream of an open connection
  flush(connection)

}
