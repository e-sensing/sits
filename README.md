SITS - Satellite Image Time Series Analysis for Earth Observation Data
Cubes
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src="inst/extdata/sticker/sits_sticker.png" alt="SITS icon" align="right" height="150" width="150"/>

<!-- badges: start -->
<!-- [![Build Status](https://drone.dpi.inpe.br/api/badges/e-sensing/sits/status.svg)](https://drone.dpi.inpe.br/e-sensing/sits) -->

[![Status at rOpenSci Software Peer
Review](https://badges.ropensci.org/596_status.svg)](https://github.com/ropensci/software-review/issues/596)
[![CRAN
status](https://www.r-pkg.org/badges/version/sits)](https://cran.r-project.org/package=sits)
[![R-check-dev](https://github.com/e-sensing/sits/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/e-sensing/sits/actions/workflows/R-CMD-check.yaml)
[![Codecov](https://codecov.io/gh/e-sensing/sits/branch/dev/graph/badge.svg?token=hZxdJgKGcE)](https://codecov.io/gh/e-sensing/sits)
[![Documentation](https://img.shields.io/badge/docs-online-blueviolet)](https://e-sensing.github.io/sitsbook/)
[![Life
cycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![Software
License](https://img.shields.io/badge/license-GPL--2-green)](https://github.com/e-sensing/sits/blob/master/LICENSE)
<!-- badges: end -->

## Overview

`sits` is an open source R package for satellite image time series
analysis. It enables users to apply machine learning techniques for
classifying image time series obtained from earth observation data
cubes. The basic workflow in `sits` is:

1.  Select an image collection available on cloud providers AWS, Brazil
    Data Cube, Digital Earth Africa, Copernicus Data Space, Digital
    Earth Australia, Microsoft Planetary Computer, NASA Harmonized
    Landsat/Sentinel, and Swiss Data Cube.
2.  Build a regular data cube from analysis-ready image collections.
3.  Extract labelled time series from data cubes to be used as training
    samples.
4.  Perform samples quality control using self-organised maps.
5.  Train machine learning and deep learning models.
6.  Tune deep learning models for improved accuracy.
7.  Classify data cubes using machine learning and deep learning models.
8.  Run spatial-temporal segmentation methods for object-based time
    series classification.
9.  Post-process classified images with Bayesian smoothing to remove
    outliers.
10. Estimate uncertainty values of classified images.
11. Evaluate classification accuracy using best practices.
12. Improve results with active learning and self-supervised learning
    methods.

<div class="figure" style="text-align: center">

<img src="inst/extdata/markdown/figures/sits_general_view.jpg" alt="Conceptual view of data cubes (source: authors)" width="60%" height="60%" />
<p class="caption">
Conceptual view of data cubes (source: authors)
</p>

</div>

## Documentation

Detailed documentation on how to use `sits` is available in the e-book
[“Satellite Image Time Series Analysis on Earth Observation Data
Cubes”](https://e-sensing.github.io/sitsbook/).

## `sits` on Kaggle

Those that want to evaluate the `sits` package before installing are
invited to run the examples available on
[Kaggle](https://www.kaggle.com/esensing/code). If you are new to
Kaggle, please follow the
[instructions](https://gist.github.com/OldLipe/814089cc5792c9c0c989d870a22910f4)
to set up your account. These examples provide a fast-track introduction
to the package. We recommend running them in the following order:

1.  [Introduction to
    SITS](https://www.kaggle.com/esensing/introduction-to-sits)
2.  [Working with time series in
    SITS](https://www.kaggle.com/esensing/working-with-time-series-in-sits)
3.  [Creating data cubes in
    SITS](https://www.kaggle.com/esensing/creating-data-cubes-in-sits)
4.  [Improving the quality of training
    samples](https://www.kaggle.com/code/esensing/improving-quality-of-training-samples)
5.  [Machine learning for data
    cubes](https://www.kaggle.com/esensing/machine-learning-for-data-cubes)
6.  [Classification of raster data
    cubes](https://www.kaggle.com/code/esensing/classification-of-raster-data-cubes)
7.  [Bayesian smoothing for
    post-processing](https://www.kaggle.com/code/esensing/bayesian-smoothing-for-post-processing)
8.  [Uncertainty and active
    learning](https://www.kaggle.com/code/esensing/uncertainty-and-active-learning)
9.  [Object-based time series
    classification](https://www.kaggle.com/esensing/object-based-image-time-series-classification)

## Installation

### Pre-Requisites

The `sits` package relies on the geospatial packages `sf`, `stars`,
`gdalcubes` and `terra`, which depend on the external libraries GDAL and
PROJ. Please follow the instructions for installing `sits` from the
[Setup chapter of the on-line sits
book](https://e-sensing.github.io/sitsbook/setup.html).

### Obtaining `sits`

`sits` can be installed from CRAN:

``` r
install.packages("sits")
```

The latest supported version is available on github. It may have
additional fixes from the version available from CRAN.

``` r
devtools::install_github("e-sensing/sits", dependencies = TRUE)
```

``` r
# load the sits library
library(sits)
#> SITS - satellite image time series analysis.
#> Loaded sits v1.5.2.
#>         See ?sits for help, citation("sits") for use in publication.
#>         Documentation avaliable in https://e-sensing.github.io/sitsbook/.
```

### Support for GPU

Classification using torch-based deep learning models in `sits` uses
CUDA compatible NVIDIA GPUs if available, which provides up 10-fold
speed-up compared to using CPUs only. Please see the [installation
instructions](https://torch.mlverse.org/docs/articles/installation) for
more information on how to install the required drivers.

## Building Earth Observation Data Cubes

### Image Collections Accessible by `sits`

Users create data cubes from analysis-ready data (ARD) image collections
available in cloud services. The collections accessible in `sits` 1.5.2
are:

- Brazil Data Cube -
  [BDC](https://data.inpe.br/bdc/web/en/home-page-2/): Open data
  collections of Sentinel-2, Landsat-8 and CBERS-4 images.
- Copernicus Data Space Environment
  [CDSE](https://dataspace.copernicus.eu/): Open data collections from
  the EU Copernicus programme.
- Earth on AWS - [AWS](https://aws.amazon.com/earth/): Sentinel-2/2A
  level 2A collections.
- Digital Earth Africa -
  [DEAFRICA](https://www.digitalearthafrica.org/): Open data collection
  of Sentinel-2/2A and Landsat-8 for Africa.
- Digital Earth Australia -
  [DEAUSTRALIA](https://www.ga.gov.au/scientific-topics/dea): Open data
  collections for the Australian subcontinent.
- Microsoft Planetary Computer -
  [MPC](https://planetarycomputer.microsoft.com/catalog): Open data
  collection of Sentinel-2/2A and Landsat-8.
- NASA Harmonized Landsat/Sentinel Collection
  [HLS](https://hls.gsfc.nasa.gov/).
- Swiss Data Cube ([SDC](https://www.swissdatacube.org/)): Open data
  collection of Sentinel-2/2A and Landsat-8.
- [USGS](https://landsatlook.usgs.gov/stac-browser): Landsat-4/5/7/8
  collections, which are not open data.

Open data collections do not require payment of access fees. Except for
those in the Brazil Data Cube, these collections are not regular.
Irregular collections require further processing before they can be used
for classification using machine learning models.

### Building a Data Cube from an ARD Image Collection

The following code defines an irregular data cube of Sentinel-2/2A
images available in the Microsoft Planetary Computer, using the open
data collection `"SENTINEL-2-L2A"`. The geographical area of the data
cube is defined by the tiles `"20LKP"` and `"20LLKP"`, and the temporal
extent by a start and end date. Access to other cloud services works in
similar ways.

``` r
s2_cube <- sits_cube(
  source = "MPC",
  collection = "SENTINEL-2-L2A",
  tiles = c("20LKP", "20LLP"),
  bands = c("B03", "B08", "B11", "SCL"),
  start_date = as.Date("2018-07-01"),
  end_date = as.Date("2019-06-30"),
  progress = FALSE
)
```

This cube is irregular. The timelines of tiles `"20LKP"` and `"20LLKP"`
and the resolutions of the bands are different. Sentinel-2 bands `"B03"`
and `"B08"` have 10-meters resolution, while band `"B11"` and the cloud
band `"SCL"` have 20-meters resolution. Irregular collections need an
additional processing step to be converted to regular data cubes, as
described below.

<div class="figure" style="text-align: center">

<img src="inst/extdata/markdown/figures/datacube_conception.jpg" alt="Conceptual view of data cubes (source: authors)" width="90%" height="90%" />
<p class="caption">
Conceptual view of data cubes (source: authors)
</p>

</div>

After defining an irregular ARD image collection from a cloud service
using `sits_cube()`, users should run `sits_regularize()` to build a
regular data cube. This function uses the [gdalcubes R
package](https://github.com/appelmar/gdalcubes), described in [Appel and
Pebesma, 2019](https://www.mdpi.com/2306-5729/4/3/92).

``` r
gc_cube <- sits_regularize(
  cube          = s2_cube,
  output_dir    = tempdir(),
  period        = "P15D",
  res           = 60,
  multicores    = 4
)
```

The above command builds a regular data cube with all bands interpolated
to 60 m spatial resolution and 15-days temporal resolution. Regular data
cubes are the input to the `sits` functions for time series retrieval,
building machine learning models, and classification of raster images
and time series.

The cube can be shown in a leaflet using `sits_view()`.

``` r
# View a color composite on a leaflet
sits_view(s2_cube[1, ], green = "B08", blue = "B03", red = "B11")
```

## Working with Time Series in `sits`

### Accessing Time Series in Data Cubes

`sits` has been designed to use satellite image time series to derive
machine learning models. After the data cube has been created, time
series can be retrieved individually or by using CSV or SHP files, as in
the following example. The example below uses a data cube in a local
directory, whose images have been obtained from the `"MOD13Q1-6"`
collection of the Brazil Data Cube.

``` r
library(sits)
# this data cube uses images from the Brazil Data Cube that have
# downloaded to a local directory
data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
# create a cube from downloaded files
raster_cube <- sits_cube(
  source = "BDC",
  collection = "MOD13Q1-6.1",
  data_dir = data_dir,
  delim = "_",
  parse_info = c("X1", "X2", "tile", "band", "date"),
  progress = FALSE
)
# obtain a set of samples defined by a CSV file
csv_file <- system.file("extdata/samples/samples_sinop_crop.csv",
  package = "sits"
)
# retrieve the time series associated with the samples from the data cube
points <- sits_get_data(raster_cube, samples = csv_file)
# show the time series
points[1:3, ]
#> # A tibble: 3 × 7
#>   longitude latitude start_date end_date   label    cube        time_series
#>       <dbl>    <dbl> <date>     <date>     <chr>    <chr>       <list>     
#> 1     -55.8    -11.7 2013-09-14 2014-08-29 Cerrado  MOD13Q1-6.1 <tibble>   
#> 2     -55.8    -11.7 2013-09-14 2014-08-29 Cerrado  MOD13Q1-6.1 <tibble>   
#> 3     -55.7    -11.7 2013-09-14 2014-08-29 Soy_Corn MOD13Q1-6.1 <tibble>
```

After a time series has been obtained, it is loaded in a tibble. The
first six columns contain the metadata: spatial and temporal location,
label assigned to the sample, and coverage from where the data has been
extracted. The spatial location is given in longitude and latitude
coordinates. The first sample has been labelled “Pasture”, at location
(-55.65931, -11.76267), and is considered valid for the period
(2013-09-14, 2014-08-29).

## Time Series Classification

### Training Machine Learning Models

`sits` provides support for the classification of both individual time
series as well as data cubes. The following machine learning methods are
available in `sits`:

- Support vector machines (`sits_svm()`)
- Random forests (`sits_rfor()`)
- Extreme gradient boosting (`sits_xgboost()`)
- Multi-layer perceptrons (`sits_mlp()`)
- 1D convolution neural networks (`sits_tempcnn()`)
- Temporal self-attention encoder (`sits_tae()`)
- Lightweight temporal attention encoder (`sits_lighttae()`)

The following example illustrate how to train a dataset and classify an
individual time series. First we use the `sits_train()` function with
two parameters: the training dataset (described above) and the chosen
machine learning model (in this case, TempCNN). The trained model is
then used to classify a time series from Mato Grosso Brazilian state,
using `sits_classify()`. The results can be shown in text format using
the function `sits_show_prediction()` or graphically using `plot`.

``` r
# training data set
data("samples_modis_ndvi")
# point to be classified
data("point_mt_6bands")
# Train a deep learning model
tempcnn_model <- sits_train(
  samples = samples_modis_ndvi,
  ml_method = sits_tempcnn()
)
# Select NDVI band of the  point to be classified
# Classify using TempCNN model
# Plot the result
point_mt_6bands |>
  sits_select(bands = "NDVI") |>
  sits_classify(tempcnn_model) |>
  plot()
```

<div class="figure" style="text-align: center">

<img src="man/figures/README-unnamed-chunk-9-1.png" alt="Classification of NDVI time series using TempCNN"  />
<p class="caption">
Classification of NDVI time series using TempCNN
</p>

</div>

The following example shows how to classify a data cube organized as a
set of raster images. The result can also be visualized interactively
using `sits_view()`.

``` r
# Create a data cube to be classified
# Cube is composed of MOD13Q1 images from the Sinop region in Mato Grosso (Brazil)
data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
sinop <- sits_cube(
  source = "BDC",
  collection = "MOD13Q1-6.1",
  data_dir = data_dir,
  delim = "_",
  parse_info = c("X1", "X2", "tile", "band", "date"),
  progress = FALSE
)
# Classify the raster cube, generating a probability file
# Filter the pixels in the cube to remove noise
probs_cube <- sits_classify(
  data = sinop,
  ml_model = tempcnn_model,
  output_dir = tempdir()
)
# apply a bayesian smoothing to remove outliers
bayes_cube <- sits_smooth(
  cube = probs_cube,
  output_dir = tempdir()
)
# generate a thematic map
label_cube <- sits_label_classification(
  cube = bayes_cube,
  output_dir = tempdir()
)
# plot the the labelled cube
plot(label_cube,
  title = "Land use and Land cover in Sinop, MT, Brazil in 2018"
)
```

<div class="figure" style="text-align: center">

<img src="man/figures/README-unnamed-chunk-10-1.png" alt="Land use and Land cover in Sinop, MT, Brazil in 2018"  />
<p class="caption">
Land use and Land cover in Sinop, MT, Brazil in 2018
</p>

</div>

### References

#### Citable papers for sits

If you use `sits`, please cite the following paper:

- Rolf Simoes, Gilberto Camara, Gilberto Queiroz, Felipe Souza, Pedro R.
  Andrade, Lorena Santos, Alexandre Carvalho, and Karine Ferreira.
  “Satellite Image Time Series Analysis for Big Earth Observation Data”.
  Remote Sensing, 13: 2428, 2021. <doi:10.3390/rs13132428>.

Additionally, the sample quality control methods that use self-organized
maps are described in the following reference:

- Lorena Santos, Karine Ferreira, Gilberto Camara, Michelle Picoli, Rolf
  Simoes, “Quality control and class noise reduction of satellite image
  time series”. ISPRS Journal of Photogrammetry and Remote Sensing,
  177:75-88, 2021. <doi:10.1016/j.isprsjprs.2021.04.014>.

#### Papers that use sits to produce LUCC maps

- Rolf Simoes, Michelle Picoli, et al., “Land use and cover maps for
  Mato Grosso State in Brazil from 2001 to 2017”. Sci Data 7(34), 2020.
  <doi:10.1038/s41597-020-0371-4>.

- Michelle Picoli, Gilberto Camara, et al., “Big Earth Observation Time
  Series Analysis for Monitoring Brazilian Agriculture”. ISPRS Journal
  of Photogrammetry and Remote Sensing, 2018.
  <doi:10.1016/j.isprsjprs.2018.08.007>.

- Karine Ferreira, Gilberto Queiroz et al., “Earth Observation Data
  Cubes for Brazil: Requirements, Methodology and Products”. Remote
  Sens. 12:4033, 2020. <doi:10.3390/rs12244033>.

- Hadi, Firman, Laode Muhammad Sabri, Yudo Prasetyo, and Bambang
  Sudarsono. [Leveraging Time-Series Imageries and Open Source Tools for
  Enhanced Land Cover
  Classification](https://doi.org/10.1088/1755-1315/1276/1/012035). In
  IOP Conference Series: Earth and Environmental Science, 1276:012035.
  IOP Publishing, 2023.

- Bruno Adorno, Thales Körting, and Silvana Amaral, [Contribution of
  time-series data cubes to classify urban vegetation types by remote
  sensing](https://doi.org/10.1016/j.ufug.2022.127817). Urban Forest &
  Urban Greening, 79, 127817, 2023.

- Giuliani, Gregory. [Time-First Approach for Land Cover Mapping Using
  Big Earth Observation Data Time-Series in a Data Cube – a Case Study
  from the Lake Geneva Region
  (Switzerland)](https://doi.org/10.1080/20964471.2024.2323241). Big
  Earth Data, 2024.

- Werner, João, Mariana Belgiu et al., [Mapping Integrated
  Crop–Livestock Systems Using Fused Sentinel-2 and PlanetScope Time
  Series and Deep Learning](https://doi.org/10.3390/rs16081421). Remote
  Sensing 16, no. 8 (January 2024): 1421.

#### Papers that describe software used by the sits package

We thank the authors of these papers for making their code available to
be used in connection with sits.

- Marius Appel and Edzer Pebesma, “On-Demand Processing of Data Cubes
  from Satellite Image Collections with the Gdalcubes Library.” Data 4
  (3): 1–16, 2020. <doi:10.3390/data4030092>.

- Ron Wehrens and Johannes Kruisselbrink, “Flexible Self-Organising Maps
  in kohonen 3.0”. Journal of Statistical Software, 87(7), 2018.
  <doi:10.18637/jss.v087.i07>.

- Charlotte Pelletier, Geoffrey I. Webb, and Francois Petitjean.
  “Temporal Convolutional Neural Network for the Classification of
  Satellite Image Time Series.” Remote Sensing 11 (5), 2019.
  <doi:10.3390/rs11050523>.

- Vivien Garnot, Loic Landrieu, Sebastien Giordano, and Nesrine Chehata,
  “Satellite Image Time Series Classification with Pixel-Set Encoders
  and Temporal Self-Attention”, Conference on Computer Vision and
  Pattern Recognition, 2020. \<doi: 10.1109/CVPR42600.2020.01234\>.

- Vivien Garnot, Loic Landrieu, “Lightweight Temporal Self-Attention for
  Classifying Satellite Images Time Series”, 2020. \<arXiv:2007.00586\>.

- Maja Schneider, Marco Körner, “\[Re\] Satellite Image Time Series
  Classification with Pixel-Set Encoders and Temporal Self-Attention.”
  ReScience C 7 (2), 2021. <doi:10.5281/zenodo.4835356>.

- Jakub Nowosad, Tomasz Stepinski, “Extended SLIC superpixels algorithm
  for applications to non-imagery geospatial rasters”. International
  Journal of Applied Earth Observation and Geoinformation, 112, 102935,
  2022.

- Martin Tennekes, “tmap: Thematic Maps in R.” Journal of Statistical
  Software, 84(6), 1–39, 2018.

### Acknowledgements for community support

The authors are thankful for the contributions of Edzer Pebesma, Jakub
Nowosad. Marius Appel, Martin Tennekes, Robert Hijmans, Ron Wehrens, and
Tim Appelhans, respectively chief developers of the packages
`sf`/`stars`, `supercells`, `gdalcubes`, `tmap`, `terra`, `kohonen`, and
`leafem`. The `sits` package recognises the great work of the RStudio
team, including the `tidyverse`. Many thanks to Daniel Falbel for his
great work in the `torch` and `luz` packages. Charlotte Pelletier shared
the python code that has been reused for the TempCNN machine learning
model. We would like to thank Maja Schneider for sharing the python code
that helped the implementation of the `sits_lighttae()` and `sits_tae()`
model. We recognise the importance of the work by Chris Holmes and
Mattias Mohr on the STAC specification and API.

### Acknowledgements for Financial and Material Support

We acknowledge and thank the project funders that provided financial and
material support:

1.  Amazon Fund, established by the Brazilian government with financial
    contribution from Norway, through the project contract between the
    Brazilian Development Bank (BNDES) and the Foundation for Science,
    Technology and Space Applications (FUNCATE), for the establishment
    of the Brazil Data Cube, process 17.2.0536.1.

2.  Coordenação de Aperfeiçoamento de Pessoal de Nível Superior-Brasil
    (CAPES) and from the Conselho Nacional de Desenvolvimento Científico
    e Tecnológico (CNPq), for providing MSc and PhD scholarships.

3.  Sao Paulo Research Foundation (FAPESP) under eScience Program grant
    2014/08398-6, for for providing MSc, PhD and post-doc scholarships,
    equipment, and travel support.

4.  International Climate Initiative of the Germany Federal Ministry for
    the Environment, Nature Conservation, Building and Nuclear Safety
    (IKI) under grant 17-III-084- Global-A-RESTORE+ (“RESTORE+:
    Addressing Landscape Restoration on Degraded Land in Indonesia and
    Brazil”).

5.  Microsoft Planetary Computer under the GEO-Microsoft Cloud Computer
    Grants Programme.

6.  The Open-Earth-Monitor Cyberinfratructure project, which has
    received funding from the European Union’s Horizon Europe research
    and innovation programme under [grant agreement
    No. 101059548](https://cordis.europa.eu/project/id/101059548).

7.  [FAO-EOSTAT](https://www.fao.org/in-action/eostat) initiative, which
    uses next generation Earth observation tools to produce land cover
    and land use statistics.

### How to contribute

The `sits` project is released with a [Contributor Code of
Conduct](https://github.com/e-sensing/sits/blob/master/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.
