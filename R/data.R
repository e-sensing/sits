#' @title Samples of classes Cerrado and Pasture
#'
#' @description A dataset containing a tibble with time series samples
#'   for the Cerrado and Pasture areas of the Mato Grosso state.
#'   The time series come from MOD13Q1 collection 5 images.
#' @name cerrado_2classes
#' @docType data
#' @keywords datasets
#' @usage data(cerrado_2classes)
#' @format A tibble with 736 rows and 7 variables:
#'   longitude: East-west coordinate of the time series sample (WGS 84),
#'   latitude (North-south coordinate of the time series sample in WGS 84),
#'   start_date (initial date of the time series),
#'   end_date (final date of the time series),
#'   label (the class label associated to the sample),
#'   cube (the name of the cube associated with the data),
#'   time_series (list containing a tibble with the values of the time series).
"cerrado_2classes"

#' @title A time series sample with data from 2000 to 2016
#' @description A dataset containing a tibble with one time series samples
#'    in the Mato Grosso state of Brazil.
#'    The time series comes from MOD13Q1 collection 6 images.
#' @docType data
#' @keywords datasets
#' @name point_mt_6bands
#' @usage data(point_mt_6bands)
#' @format A tibble with 1 rows and 7 variables:
#'   longitude: East-west coordinate of the time series sample (WGS 84),
#'   latitude (North-south coordinate of the time series sample in WGS 84),
#'   start_date (initial date of the time series),
#'   end_date (final date of the time series),
#'   label (the class label associated to the sample),
#'   cube (the name of the cube associated with the data),
#'   time_series (list containing a tibble with the values of the time series).
NULL

#' @title Samples of nine classes for the state of Mato Grosso
#'
#' @description A dataset containing a tibble with time series samples
#'   for the Mato Grosso state in Brasil.
#'   The time series come from MOD13Q1 collection 6 images.
#'   The data set has the following classes:
#'   Cerrado(379 samples), Forest (131 samples),
#'   Pasture (344 samples), and Soy_Corn (364 samples).
#'
#' @docType data
#' @keywords datasets
#' @name samples_modis_4bands
#' @usage data(samples_modis_4bands)
#' @format A tibble with 1308 rows and 7 variables:
#'   longitude: East-west coordinate of the time series sample (WGS 84),
#'   latitude (North-south coordinate of the time series sample in WGS 84),
#'   start_date (initial date of the time series),
#'   end_date (final date of the time series),
#'   label (the class label associated to the sample),
#'   cube (the name of the cube associated with the data),
#'   time_series (list containing a tibble with the values of the time series).
NULL

#' @title Samples of Amazon tropical forest biome for deforestation analysis
#'
#'
#' @docType data
#' @name samples_l8_rondonia_2bands
#' @keywords datasets
#'
#' @description
#' A sits tibble with time series samples from Brazilian Amazonia rain forest.
#'
#' The labels are: "Deforestation", "Forest", "NatNonForest" and "Pasture".
#'
#' The time series were extracted from the Landsat-8 BDC data cube
#' (collection = "LC8_30_16D_STK-1", tiles = "038047").
#' These time series comprehends a period of 12 months
#' (25 observations) from "2018-07-12" to "2019-07-28".
#' The extracted bands are NDVI and EVI.
#' Cloudy values were removed and interpolated.
#'
#' @format
#' A \code{sits} tibble with 160 samples.
#'
#'
#' @usage data("samples_l8_rondonia_2bands")
#'
NULL
