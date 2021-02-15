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

#' @title A time series sample for the NDVI band from 2000 to 2016
#'
#' @description A dataset containing a tibble with one time series samples
#'    in the Mato Grosso state of Brazil.
#'    The time series comes from MOD13Q1 collection 5 images.
#'
#' @docType data
#' @keywords datasets
#' @name point_ndvi
#' @usage data(point_ndvi)
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
#'   Cerrado(400 samples), Fallow_Cotton (29 samples), Forest (131 samples),
#'   Pasture (344 samples),
#'   Soy_Corn (364 samples),  Soy_Cotton (352 samples), Soy_Fallow (87 samples),
#'   Soy_Millet (180 samples), and Soy_Sunflower (26 samples).
#'
#' @docType data
#' @keywords datasets
#' @name samples_mt_4bands
#' @usage data(samples_mt_4bands)
#' @format A tibble with 1892 rows and 7 variables:
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
#'   Cerrado, Fallow_Cotton, Forest, Pasture,
#'   Soy_Corn, Soy_Cotton, Soy_Fallow,
#'   Soy_Millet, and Soy_Sunflower.
#'
#' @docType data
#' @keywords datasets
#' @name samples_mt_6bands
#' @usage data(samples_mt_6bands)
#' @format A tibble with 425 rows and 7 variables:
#'   longitude: East-west coordinate of the time series sample (WGS 84),
#'   latitude (North-south coordinate of the time series sample in WGS 84),
#'   start_date (initial date of the time series),
#'   end_date (final date of the time series),
#'   label (the class label associated to the sample),
#'   cube (the name of the cube associated with the data),
#'   time_series (list containing a tibble with the values of the time series).
NULL

#' @title A time series in the ZOO format
#'
#' @description A dataset containing a one ZOO time series samples
#'    in the Mato Grosso state of Brazil.
#'    The time series comes from MOD13Q1 collection 5 images.
#'
#' @docType data
#' @keywords datasets
#' @name ts_zoo
#' @usage data(ts_zoo)
#' @format A ZOO time series with bands NDVI and EVI for a one year period
NULL

#' @title The timeline for the sequence of images for MOD13Q1 collection 5
#'
#' @description The timeline for the time series
#'    used in the examples of the sits package
#'    that use data from MODIS collection 5.
#'    There are 392 instances from 2000-02-18 until 2017-02-18.
#' @docType data
#' @keywords datasets
#' @name timeline_modis_392
#' @usage data(timeline_modis_392)
#' @format A vector with 392 dates in YYYY-MM-DD format.
NULL

#' @title The timeline for the sequence of images for MOD13Q1 collection 6
#'
#' @description The timeline for the time series used in the examples
#'    that use data from MODIS collection 6.
#'    There are 407 instances from 2000-02-18 until 2017-10-16.
#' @docType data
#' @keywords datasets
#' @name timeline_2000_2017
#' @usage data(timeline_2000_2017)
#' @format A vector with 407 dates in YYYY-MM-DD format.
NULL

#' @title The timeline for the sequence of images one year (2013 to 2014)
#'
#' @description The timeline for the time series used in the examples
#'    that use data for one year classification.
#'    There are 23 instances from 2013-09-14 until 2014-08-29.
#' @docType data
#' @keywords datasets
#' @name timeline_2013_2014
#' @usage data(timeline_2013_2014)
#' @format A vector with 23 dates in YYYY-MM-DD format.
NULL
