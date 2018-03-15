sits
========

[![Build Status](https://travis-ci.org/rolfsimoes/sits.svg?branch=master)](https://travis-ci.org/rolfsimoes/) [![License](http://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html) 

# Satellite Image Time Series Analysis.
A set of tools for working with satellite image time series. Includes data retrieval from a WTSS (web time series service), different visualisation methods for image time series, smoothing methods for noisy time series, different clustering methods, including dendrograms and SOM. Matches noiseless patterns with noisy time series using the TWDTW method for shape recognition and provides machine learning methods for time series classification, including SVM, LDA, QDA, GLM, Lasso, Random Forests and Deep Learning.

### Overview

The **sits** package is a set of tools for working with satellite image time series, that:

- Supports for data retrieval from a web time series services, rasters files and other data sets.
- Provides different visualisation methods for image time series.
- Includes smoothing methods for noisy time series.
- Enables different clustering methods, including dendrograms and SOM (Kohonen maps).
- Matches noiseless patterns with noisy time series using the TWDTW method for shape recognition.
- Provides machine learning methods for time series classification, including SVM, LDA, QDA, GLM, Lasso, Random Forests and Deep Learning.

**sits** is a convenient front-end to many analysis package that are useful for satellite image time series analysis. The package organizes time series data using tibbles, which makes it quite simple for users to manage time series data sets. 

### Interface to CRAN data analysis packages

**sits** is a convenient front-end to many packages that are useful for satellite image time series analysis. Given data sets organized as time series tibbles, the package provides easy access to many packages for time series analysis:

- **keras** for Deep Learning classification.
- **e1071** for SVM models.
- **MASS** for LDA and QDA models.
- **nnet** for multinomial log-linear models.
- **glmnet** for generalized linear models.
- **gbm** for gradient boosting methods.
- **randomForest** for random forest methods.
- **dtwclust** for time series clustering.
- **kohonen** for clustering based on SOM.
- **dtwSat** for access to the time-weigthed dynamic time warping algorithm.
- **signal** for filtering.
    
**sits** also relies extensively on the **tidyverse**, **data.table**, **raster** and **sf** packages. The tight integration achieved by tha package makes it easier for users to perform different types of analysis on satellite image time series.   

### Installation

Please install the SITS package from github, making sure you have the latest version of the other packages it requires:

```{r, eval = FALSE}
devtools::install_github("e-sensing/sits")
```
 

### Basic data structure

After loading the library, users can print a **sits** tibble to see how the package organizes the data.

```{r, include = FALSE}
library(sits)
library(tibble)
library(dtwclust)
```


```{r}
samples_MT_9classes[1:3,]
```

The **sits** tibble contains data and metadata. The first six columns contain the metadata: spatial and temporal location, label assigned to the sample, and coverage from where the data has been extracted. The spatial location is given in longitude and latitude coordinates for the "WGS84" ellipsoid. For example, the first sample has been labelled "Pasture", at location (-55.1852, -10.8387), and is considered valid for the period (2013-09-14, 2014-08-29).

To display the time series, we provide `sits_plot()` function to display the time series. Given a small number of samples to display, the `sits_plot()` function tries to group as many spatial locations together. In the following example, the first 15 samples of the "Cerrado" class all refer to the same spatial location in consecutive time periods. For this reason, these samples are plotted together. 

```{r cerrado-15, fig.align="center", fig.height=3.1, fig.width=5}
# select the "ndvi" band
samples_ndvi.tb <- sits_select(samples_MT_9classes, bands = c("ndvi"))
# select only the samples with the cerrado label
samples_cerrado.tb <- dplyr::filter(samples_ndvi.tb, 
                  label == "Cerrado")
# plot the first 15 samples (different dates for the same points)
sits_plot(samples_cerrado.tb[1:15,])
```

For a large number of samples, where the amount of individual plots would be substantial, the default visualisation combines all samples together in a single temporal interval. This plot is useful to show the spread of values for the time series of each band. The strong red line in the plot shows the median of the values, and the two orange lines are the first and third interquartile ranges. 

```{r cerrado-all, fig.align="center", fig.height=3.1, fig.width=5}
# plot all cerrado samples together (shows the distribution)
sits_plot(samples_cerrado.tb)
```

### Importing Data into `sits`

**sits** allows different methods of data input, including: (a) obtain data from a time series web services such as INPE's WTSS (Web Series Time Service) or EMBRAPA's SATVEG; (b) read data stored in a time series in the ZOO format [@Zeileis2005]; (c) read a time series from a TIFF RasterBrick. More services will be added in future releases.

### Clustering

Clustering is a way to improve training data to use in machine learning classification models. In this regard, cluster analysis can assist the identification of structural patterns and anomalous samples. **sits** provides support for the agglomerative hierarchical clustering (AHC) using the DTW (dynamic time warping) distance measure. 

```{r dendrogram, cache=TRUE, fig.align="center", fig.height=4.1, fig.width=5}
# take a set of patterns for 2 classes
# create a dendrogram object with default clustering parameters
dendro <- sits_dendrogram(cerrado_2classes)
# plot the resulting dendrogram
sits_plot_dendrogram(cerrado_2classes, dendro)
```
After creating a dendrogram, we provide `sits_dendro_bestcut()`, a function that computes a validity index and returns the height where the cut of the dendrogram maximizes this index.

```{r}
# search for the best height to cut the dendrogram
sits_dendro_bestcut(cerrado_2classes, 
                    dendro)
```
This height optimises the ARI and generates $6$ clusters, which are then created by function `sits_cluster`. We can then see the cluster frequency using `sits_cluster_frequency`. In the example, we note that cluster $3$, unlike other clusters, includes a mix of two classes. Users can then remove this cluster with `sits_cluster_remove`to reduce the number of mixed-class samples.
```{r}
# create 6 clusters by cutting the dendrogram at 
# the linkage distance 20.39655
clusters.tb <- 
    sits_cluster(cerrado_2classes, dendro, k = 6)
# show clusters samples frequency
sits_cluster_frequency(clusters.tb)
# clear those samples with a high confusion rate in a cluster 
clean.tb <- sits_cluster_remove(clusters.tb, 
                        min_perc = 0.9)
# show clean clusters samples frequency
sits_cluster_frequency(clean.tb)
```

### Filtering

Satellite image time series are contaminated by atmospheric influence and directional effects. To make the best use of available satellite data archives, methods for satellite image time series analysis need to deal with data sets that are *noisy* and *non-homogeneous*. For data filtering, `sits` supports Savitzky–Golay (`sits_sgolay()`), Whittaker (`sits_whittaker()`), envelope (`sits_envelope()`) and the "cloud filter" (`sits_cloud_filter()`). As an example, we show how to apply the Whitakker smoother to the data.

```{r, fig.align="center", fig.height=3.1, fig.width=5}
# Take the NDVI band of the first sample data set
point.tb <- sits_select(prodes_226_064[1,], 
                bands = c("ndvi"))
# apply Whitaker filter
point_whit.tb <- sits_whittaker(point.tb)
# plot the series
sits_plot(sits_merge(point_whit.tb, 
                     point.tb))
```

### Machine Learning and Deep Learning

**sits** explores the full depth of satellite image time series data for classification. It treat time series as a feature vector, formed by all pixel "bands". The idea is to have as many temporal attributes as possible, increasing the dimension of the classification space. In this scenario, statistical learning models are the natural candidates to deal with high-dimensional data: learning to distinguish all land cover and land use classes from trusted samples exemplars, also known as training data, to infer classes of a larger data set. 

We support a number of machine learning techniques, including SVM (support vector machines), Random Forests, generalised linear models, and gradient boosting machines, and deep learning. We show an example of using the SVM and Deep Learning classifiers below.

```{r}
# Retrieve the set of samples for the Mato Grosso region 
# (provided by EMBRAPA) (samples_MT_ndvi) and 
# get a point to be classified (point_ndvi)
class.tb <- sits_classify(point_ndvi,
                          samples_MT_ndvi,
                          ml_method = sits_svm(kernel = "radial", 
                                               cost = 10))
sits_plot(class.tb)
```


#### License

The **sits** package is licensed under the GPLv3 (<http://www.gnu.org/licenses/gpl.html>).
