#' @title Samples of classes Cerrado and Pasture
#'
#' @description A dataset containing a tibble with time series samples for the Cerrado and Pasture areas of the Mato Grosso state.
#'   The time series come from MOD13Q1 collection 5 images.
#'
#' @docType data
#' @name cerrado_2classes
#' @usage data(cerrado_2classes)
#' @format A tibble with 736 rows and 7 variables: (a) longitude: East-west coordinate of the time series sample (WGS 84);
#'   latitude (North-south coordinate of the time series sample in WGS 84), start_date (initial date of the time series),
#'   end_date (final date of the time series), label (the class label associated to the sample),
#'   coverage (the name of the coverage associated with the data),
#'   time_series ( list containing a tibble with the values of the time series).
NULL

#' @title A time series sample with data from 2000 to 2016
#' @description A dataset containing a tibble with one time series samples in the Mato Grosso state of Brazil.
#'    The time series comes from MOD13Q1 collection 5 images.
#' @docType data
#' @keywords datasets
#' @name ts_2000_2016
#' @usage data(ts_2000_2016)
#' @format A tibble with 1 rows and 7 variables: (a) longitude: East-west coordinate of the time series sample (WGS 84);
#'   latitude (North-south coordinate of the time series sample in WGS 84), start_date (initial date of the time series),
#'   end_date (final date of the time series), label (the class label associated to the sample),
#'   coverage (the name of the coverage associated with the data),
#'   time_series ( list containing a tibble with the values of the time series).
NULL

#' @title A time series sample for the NDVI band from 2000 to 2016
#'
#' @description A dataset containing a tibble with one time series samples in the Mato Grosso state of Brazil.
#'    The time series comes from MOD13Q1 collection 5 images.
#'
#' @docType data
#' @keywords datasets
#' @name point_ndvi
#' @usage data(point_ndvi)
#' @format A tibble with 1 rows and 7 variables: (a) longitude: East-west coordinate of the time series sample (WGS 84);
#'   latitude (North-south coordinate of the time series sample in WGS 84), start_date (initial date of the time series),
#'   end_date (final date of the time series), label (the class label associated to the sample),
#'   coverage (the name of the coverage associated with the data),
#'   time_series ( list containing a tibble with the values of the time series).
NULL

#' @title Samples of nine classes for the state of Mato Grosso used for classification
#'
#' @description A dataset containing a tibble with time series samples for the Mato Grosso state in Brasil.
#'   The time series come from MOD13Q1 collection 5 images. The data set has the following classes:
#'   Cerrado(400 samples), Fallow_Cotton (34 samples), Forest (138 samples), Pasture (370 samples),
#'   Soy-Corn (398 samples),  Soy-Cotton (399 samples), Soy_Fallow (88 samples),
#'   Soy_Millet (235 samples), and Soy_Sunflower (53 samples).
#'
#' @docType data
#' @keywords datasets
#' @name samples_MT_9classes
#' @usage data(samples_MT_9classes)
#' @format A tibble with 2115 rows and 7 variables: (a) longitude: East-west coordinate of the time series sample (WGS 84);
#'   latitude (North-south coordinate of the time series sample in WGS 84), start_date (initial date of the time series),
#'   end_date (final date of the time series), label (the class label associated to the sample),
#'   coverage (the name of the coverage associated with the data),
#'   time_series ( list containing a tibble with the values of the time series).
NULL

#' @title Samples of 13 classes for the Cerrado biome in Brazil
#' @description A dataset containing a tibble with time series samples for the Cerrado biome in Brazil.
#'   The time series comes from MOD13Q1 collection 6 images with six bands
#'   (ndvi, evi, blue, red, nir, mir)
#'
#' @docType data
#' @keywords datasets
#' @name cerrado_13classes_modis_col6
#' @usage data(cerrado_13classes_modis_col6)
#' @format A tibble with 11,456 rows and 7 variables: (a) longitude: East-west coordinate of the time series sample (WGS 84);
#'   latitude (North-south coordinate of the time series sample in WGS 84), start_date (initial date of the time series),
#'   end_date (final date of the time series), label (the class label associated to the sample),
#'   coverage (the name of the coverage associated with the data),
#'   time_series ( list containing a tibble with the values of the time series).
NULL

#' @title Samples of nine classes for the state of Mato Grosso for the NDVI band
#' @description A dataset containing a tibble with time series samples for the Mato Grosso state in Brasil.
#'   The time series come from MOD13Q1 collection 5 images. The data set has the following classes:
#'   Cerrado(400 samples), Fallow_Cotton (34 samples), Forest (138 samples), Pasture (370 samples),
#'   Soy-Corn (398 samples),  Soy-Cotton (399 samples), Soy_Fallow (88 samples),
#'   Soy_Millet (235 samples), and Soy_Sunflower (53 samples).
#' @docType data
#' @keywords datasets
#' @name samples_MT_ndvi
#' @usage data(samples_MT_ndvi)
#' @format A tibble with 2115 rows and 7 variables: (a) longitude: East-west coordinate of the time series sample (WGS 84);
#'   latitude (North-south coordinate of the time series sample in WGS 84), start_date (initial date of the time series),
#'   end_date (final date of the time series), label (the class label associated to the sample),
#'   coverage (the name of the coverage associated with the data),
#'   time_series ( list containing a tibble with the values of the time series).
NULL

#' @title Samples of deforestation-related classes for the LANDSAT image WRS 226/064
#'
#' @description A dataset containing a tibble with time series samples for the combination
#'    of the LANDSAT images for WRS 226/04 with the MOD13Q1 collection 5 images, used
#'    to fill the gaps where there is too much cloud in the LANDSAT data.
#'    The data set has the following classes (and samples per class): Deforestation_2014 (146 samples),
#'    Deforestation_2015 (198 samples), Forest (128 samples), and  Pasture (145 samples).
#'
#' @docType data
#' @keywords datasets
#' @name prodes_226_064
#' @usage data(prodes_226_064)
#' @format A tibble with 617 rows and 7 variables
NULL

#' @title A time series in the ZOO format
#'
#' @description A dataset containing a one ZOO time series samples in the Mato Grosso state of Brazil.
#'    The time series comes from MOD13Q1 collection 5 images.
#'
#'
#' @docType data
#' @keywords datasets
#' @name ts_zoo
#' @usage data(ts_zoo)
#' @format A ZOO time series with two bands (NDVI and EVI) for a one year period
NULL

#' @title The timeline for the sequence of images for MOD13Q1 collection 5
#'
#' @description The timeline for the time series used in the examples of the SITS package.
#'    There are 392 instances from 2000-02-18 until 2017-02-18.
#' @docType data
#' @keywords datasets
#' @name timeline_mod13q1
#' @usage data(timeline_mod13q1)
#' @format A data frame with 392 lines and 1 column
NULL
