SITS - Satellite Image Time Series Analysis for Earth Observation Data
Cubes
================

### Overview

The SITS package provides a set of tools for analysis, visualization and
classification of satellite image time series. It includes methods for
filtering, clustering, classification, and post-processing. For a
general view of SITS, please see the vignette “SITS: Data Analysis and
Machine Learning for Data Cubes using Satellite Image Time Series”.

### Installation

Please install the SITS package from github, making sure you have the
latest version of the other packages it requires:

``` r
devtools::install_github("e-sensing/sits")
library(sits)

# Retrieve the data available in the "inSitu" package (used for some examples)
devtools::install_github("e-sensing/inSitu")
library(inSitu)
```

### Data Access

**sits** allows different methods of data input, including: (a) obtain
data from a time series web services such as INPE’s WTSS (Web Series
Time Service) or EMBRAPA’s SATVEG; (b) read data stored in a time series
in the ZOO format \[@Zeileis2005\]; (c) read a time series from a TIFF
RasterBrick; (d) Read images organized in data cubes using the EOCUbes
packages. More services will be added in future releases.

### Visualization

After a time series is imported, it is loaded in a tibble. The first six
columns contain the metadata: spatial and temporal location, label
assigned to the sample, and coverage from where the data has been
extracted. The spatial location is given in longitude and latitude
coordinates for the “WGS84” ellipsoid. For example, the first sample has
been labelled “Pasture”, at location (-55.1852, -10.8387), and is
considered valid for the period (2013-09-14, 2014-08-29). To display the
time series, use the `sits_plot()` function. For a large number of
samples, where the amount of individual plots would be substantial, the
default visualisation combines all samples together in a single temporal
interval.

``` r
# select the "ndvi" band
samples_ndvi <- sits_select_bands(samples_mt_6bands, ndvi)
# select only the samples with the cerrado label
samples_cerrado <- dplyr::filter(samples_ndvi, 
                  label == "Cerrado")
sits_plot(samples_cerrado)
```

<img src="man/figures/README-cerrado-15-1.png" style="display: block; margin: auto;" />

### Clustering

Clustering methods in SITS improve the quality of the samples and to
remove those that might have been wrongly labeled or that have low
discriminatory power. Good samples lead to good classification maps.
`sits` provides support for two clustering methods to test sample
quality: (a) Agglomerative Hierarchical Clustering (AHC); (b)
Self-organizing Maps (SOM). Full details of the cluster methods used in
SITS are available in the vignette ‘Clustering of Satellite Image Time
Series with SITS’. The following example shows how to create a
dendrogram and associated clusters for a dataset with two classes
(“pasture” and “cerrado”) for the Cerrado biome in Brasil.

``` r
# take a set of patterns for 2 classes
# create a dendrogram object with default clustering parameters
clusters.tb <- sits_cluster_dendro(cerrado_2classes)
```

<img src="man/figures/README-dendrogram-1.png" style="display: block; margin: auto;" />

## Filtering

Satellite image time series are contaminated by atmospheric influence
and directional effects. To make the best use of available satellite
data archives, methods for satellite image time series analysis need to
deal with data sets that are *noisy* and *non-homogeneous*. For data
filtering, `sits` supports Savitzky–Golay (`sits_sgolay()`), Whittaker
(`sits_whittaker()`), envelope (`sits_envelope()`) and the “cloud
filter” (`sits_cloud_filter()`). As an example, we show how to apply
the Whitakker smoother to a 16-year NDVI time series.

For more details, please see the vignette “Satellite Image Time Series
Filtering with
SITS”.

``` r
# apply Whitaker filter to a time series sample for the NDVI band from 2000 to 2016
# merge with the original data
# plot the original and the modified series
point_whit <- sits_filter(point_ndvi, sits_whittaker(lambda = 5))
point_whit %>% 
  sits_merge(point_ndvi) %>% 
  sits_plot()
```

<img src="man/figures/README-unnamed-chunk-4-1.png" title="Whittaker smoother filter applied on one-year NDVI time series. The example uses default $\lambda=1$ parameter." alt="Whittaker smoother filter applied on one-year NDVI time series. The example uses default $\lambda=1$ parameter." style="display: block; margin: auto;" />

## Time Series classification using machine learning

SITS provides support for the classification of both individual time
series as well as data cubes. The following machine learning methods are
available in SITS:

  - Linear discriminant analysis (`sits_lda`)
  - Quadratic discriminant analysis (`sits_qda`)
  - Multinomial logit and its variants ‘lasso’ and ‘ridge’ (`sits_mlr`)
  - Support vector machines (`sits_svm`)
  - Random forests (`sits_rfor`)
  - Extreme gradient boosting (`sits_xgboost`)
  - Deep learning (DL) using multi-layer perceptrons
    (`sits_deeplearning`)
  - DL with 1D convolutional neural networks (`sits_CNN`),
  - DL combining 1D CNN and multi-layer perceptron networks
    (`sits_tempCNN`)
  - DL using 1D version of ResNet (`sits_ResNet`).

The following example illustrate how to train a dataset and classify an
individual time series. First we use the `sits_train` function with two
parameters: the training dataset (described above) and the chosen
machine learning model (in this case, an XGBoost classifier). The
trained model is then used to classify a time series from Mato Grosso
Brazilian state, using `sits_classify`. The results can be shown in text
format using the function `sits_show_prediction` or graphically using
`sits_plot`.

``` r


#select the data for classification
mato_grosso_samples <- inSitu::br_mt_1_8K_9classes_6bands
mato_grosso_2bands  <- sits_select_bands(mato_grosso_samples, ndvi, evi)

# get a point to be classified
point_mt_2bands <- sits_select_bands(point_mt_6bands, ndvi, evi)

# Train a machine learning model for the mato grosso dataset using Random Forest
xgb_model <- sits_train(data = mato_grosso_2bands, ml_method = sits_xgboost())

# Classify using random forest model and plot the result
class.tb <- sits_classify(point_mt_2bands, xgb_model)
# plot the results of the prediction
sits_plot(class.tb)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" title="XGBoost classification of a $16$ years time series. The location (latitude, longitude) shown at the top of the graph is in geographic coordinate system (WGS84 {\it datum})." alt="XGBoost classification of a $16$ years time series. The location (latitude, longitude) shown at the top of the graph is in geographic coordinate system (WGS84 {\it datum})." style="display: block; margin: auto;" />

The following example shows how to use the model trained above to
classify a data cube organised as a set of raster bricks.

``` r

# Create a data cube from two raster bricks
evi_file <- system.file("extdata/Sinop", "Sinop_evi_2014.tif", package = "inSitu")
ndvi_file <- system.file("extdata/Sinop", "Sinop_ndvi_2014.tif", package = "inSitu")

# Obtain the associated timeline
time_file <- system.file("extdata/Sinop", "timeline_2014.txt", package = "inSitu")
timeline_2013_2014 <- scan(time_file, character(), quiet = TRUE)

# create a raster metadata file based on the information about the files
raster_cube <- sits_cube(name = "Sinop", timeline = timeline_2013_2014, bands = c("ndvi", "evi"), files = c(ndvi_file, evi_file))
#> satellite information not provided - assuming TERRA
#> sensor information not provided - assuming MODIS
# Classify the raster cube, generating a probability file
probs_cube <- sits_classify(raster_cube, ml_model = xgb_model)
#> Starting classification at 2019-09-11 13:18:11
#> Classification finished at 2019-09-11 13:18:30. Total elapsed time: 0.3 minute(s).

# label the probability file (by default selecting the class with higher probability)
# apply a bayesian smoothing to remove outliers
label_cube <- sits_label_classification(probs_cube, smoothing = "bayesian")

# plot the first raster object with a selected color pallete
# make a title, define the colors and the labels)
sits_plot_raster(label_cube, time = 1, title = "SINOP-MT - 2013/2014")
```

<img src="man/figures/README-unnamed-chunk-6-1.png" title="Image classified with XGBoost." alt="Image classified with XGBoost." style="display: block; margin: auto;" />

For more details, please see the vignette “Machine Learning for Data
Cubes using the SITS
package”.

#### Code status

| Job           | Status                                                                                                                                                                                      |
| ------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Build         | [<img src="http://www.dpi.inpe.br/jenkins/buildStatus/icon?job=sits-build-ubuntu-16.04">](http://www.dpi.inpe.br/jenkins/job/sits-build-ubuntu-16.04/lastBuild/consoleFull)                 |
| Check         | [<img src="http://www.dpi.inpe.br/jenkins/buildStatus/icon?job=sits-check-ubuntu-16.04">](http://www.dpi.inpe.br/jenkins/job/sits-check-ubuntu-16.04/lastBuild/consoleFull)                 |
| Documentation | [<img src="http://www.dpi.inpe.br/jenkins/buildStatus/icon?job=sits-documentation-ubuntu-16.04">](http://www.dpi.inpe.br/jenkins/job/sits-documentation-ubuntu-16.04/lastBuild/consoleFull) |
| Coverage      | [<img src="http://codecov.io/github/e-sensing/sits/coverage.svg?branch=master">](https://codecov.io/github/e-sensing/sits?branch=master)                                                    |

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/e-sensing/sits.svg?branch=master)](https://travis-ci.org/e-sensing/sits)
<!-- badges: end -->

[![DOI](https://zenodo.org/badge/98539507.svg)](https://zenodo.org/badge/latestdoi/98539507)

#### License

The **sits** package is licensed under the GPLv3
(<http://www.gnu.org/licenses/gpl.html>).
