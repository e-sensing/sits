#' @title Create an indexed ordered obsevartion set within a sits_table.
#' @name sits_createZooObject
#'
#' @description This function returns a sits table with an indexed ordered observation set as a zoo object
#'
#' @param  bands a character string vector with the interested bands of the satellite imagery product
#' @param  dates a string vector contaning time series dates
#' @param  scale_factor a real number representing the scale factor of the attribute values
#' @param  idx a integer vector containing the respective index of the attribute values
#' @param  missing_values the missing value of the coverage
#' @return ts.tb
#' @export
sits_createZooObject <- function(bands, dates, scale_factor, idx, missing_values) {

  # create data.frame dynamically
  df <- data.frame(get(bands[1])[idx])
  for(i in 2:length(bands)) df <- cbind(df, get(bands[i])[idx])

  # change scidb column names
  names(df) <- bands

  # create zoo object
  ts <- zoo::zoo(df*scale_factor, dates)

  # assign NA values to missing values
  ts[which(t(t(zoo::coredata(ts)) == missing_values))] <- NA

  # interpolate zoo object
  ts_spline <- zoo::na.spline(ts)

  # convert to a tibble object
  ts.tb <- tibble::as_tibble (zoo::fortify.zoo (ts_spline))

  return(ts.tb)

}

#' @title Exit a connection from SciDB chunk processing.
#' @name sits_exitConnection
#'
#' @description This function closes a SciDB streaming processing connection
#'
#' @param  out list or data.frame with classification data to return to SciDB
#' @param  connection connection object to flush
#' @export
sits_exitConnection <- function(out, connection) {

  # binary data to connection
  writeBin(serialize(c(out), NULL, xdr=FALSE), connection)

  # flushes the output stream of an open connection
  flush(connection)

}
