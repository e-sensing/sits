# SITS Release History

# What's new in SITS version 1.4

### New features in SITS version 1.4.1
* Updated access to collections in Brazil Data Cube, HLS, and AWS
* Corrected errors in labelling of classified cubes
* Created a factory of functions for segmentation


### New features in SITS version 1.4.0
* New function for image segementation based on `supercells` package
* New version of `sits_get_data()` to extract average values of time series based on segments
* Support for Harmonized Landsat Sentinel (HLS) collections from NASA
* Support for probability cubes and uncertainty cubes in `sits_view()`
* New `summary()` function to show details of data cubes and time series tibbles
* General big fixes


# What's new in SITS version 1.3

### New features in SITS version 1.3.0
* Remove NOTES and WARNINGS pointed out by CRAN
* New `sits_mosaic()` function for improving visualization of large data sets
* Add support to cubes with no cloud coverage information in `sits_regularize()`
* Improve `sits_cube_copy()` for downloading data from the internet
* Tested and validated GPU support for deep learning models in `sits`
* Added multithread support for deep learning models in `sits_train()`
* Improve `sits_combine_predictions()`
* Remove dependencies on `data.table` package
* Organize and clean internal APIs 
* General bug fixes 

# What's new in SITS version 1.2.0

### Hotfix version 1.2.0-4
* Fix `.raster_file_blocksize.terra()` bug (issue #918)

### Hotfix version 1.2.0-3
* Fix `stars` proxy bug (issue #902)
* Fix `purrr` cross deprecation
* Fix `ggplot2` aes_string deprecation

### Hotfix version 1.2.0-2
* Fix `tibble` subsetting bug (issue #893)

### Hotfix version 1.2.0-1
* Fix `sits_som_clean_samples()` bug (issue #890)

### New features in SITS version 1.2.0
* `sits_get_data()` can be used to retrieve samples in classified cube
*  Support for mixture models (`sits_mixture_model()`)
*  Joining cubes in a mosaic (`sits_mosaic_cubes()`)
*  Extract the trained ML model (`sits_model()`)
*  Downloading and copying data cubes (`sits_cube_copy()`) 
*  Combine prediction by average and entropy (`sits_combine_predictions()`)
*  Significant performance improvement when working with COG files
*  Allow plot of confusion matrix (`sits_plot`)
*  Support for operations on CLOUD band in `sits_apply()`
*  Bug fixes and internal re-engineering for better code maintenance

# What's new in SITS version 1.1.0

### Hotfix version 1.1.0-8
* Fix support to BDC cubes in `sits_regularize()` (issue #848)
* Fix support to classified_image cubes in `sits_labels()<-` (issue #846)

### Hotfix version 1.1.0-7
* Fix out of memory error in `sits_label_classification()` and `sits_smooth()` (issue #850)

### Hotfix version 1.1.0-6
* Fix resume feature in `sits_classify()` on BDC cubes (issue #844)

### Hotfix version 1.1.0-5
* Fix bound box issue in image blocks produced by `sits_apply()`

### Hotfix version 1.1.0-4
* Fix MPC cube time expiration bug

### Hotfix version 1.1.0-3
* Fix bound box issue in image blocks produced by `sits_apply()`

### Hotfix version 1.1.0-2
* Improve sits_values() function (issue #810)
* Fix sits_reduce_imbalance() function (issue #809)

### Hotfix version 1.1.0-1
* Fix sits_accuracy() function (issue #807)

### New features in SITS version 1.1.0
* Introduced support to kernel functions in `sits_apply`
* Introduced new function `sits_mixture_model` for spectral mixture analysis
* Support for the Swiss Data Cube ([swissdatacube.org](https://www.swissdatacube.org/))
* Support for mosaic visualization in `sits_view`
* Introduced new function `sits_as_sf` to convert sits objects to sf
* Export images as [COG](https://www.cogeo.org/) in `sits_regularize`
* Add `roi` parameter in `sits_regularize` function
* Add `crs` parameter in `sits_get_data`
* Change Microsoft Planetary Computer source name to `"MPC"`
* Fix several bugs and improve performance

# What's new in SITS version 1.0.0
* Available on CRAN.

### New features in SITS version 1.0.0
* Hotfix to improve `sits_whittaker()` function to process cube.
* Update documentation to match CRAN standards

# What's new in SITS version  0.17.0

### New features in SITS version 0.17.0
* Introduced new classifier model `sits_lighttae()`
(Lightweight Temporal Self-Attention)
* Introduced `sits_uncertainty_sampling()` for active learning
* Introduced `sits_confidence_samples()` for semi-supervised learning
* Introduced `sits_geo_dist()` to generate samples-samples and 
samples-predicted plot
* Introduced `sits_tuning()` for random search of machine learning parameters
* Introduced `sits_reduce_imbalance()` function to balance class samples
* Introduced `sits_as_sf()` to convert a sits tibble to a sf object
* Support to `torchopt` deep learning optimizer package
* New types of `sits_uncertainty()`: `least` confidence and `margin` of 
confidence

### Improvements in SITS version 0.17.0
* Implement parallel processing for `sits_kfold_validate()`
* Change `data` to `samples` in sits machine learning classifiers
(NOTE: models trained in previous versions is no longer supported)
* Change deep learning functions to snake case
* Remove `file` parameter in `sits_get_data()` function
* Update documentation
* Improve several internal functions performances
* Fix several bugs

# What's new in SITS version 0.16.3

### New features in SITS version 0.16.3
* reimplemented all deep learning functions using `torch` package and remove `keras` dependence
* Introduced `sits_TAE()` classification model
* Introduced `sits_lightgbm()` classification model
* Simplified `sits_regularize()` parameters
* Improve `sits_regularize()` to reach production level quality
* Improve `sits_regularize()` to use C++ internal functions
* Include improved version of gdalcubes
* Improve `sits_cube()` to open results cube
* Update `plot()` parameters on raster cubes
* Support multi-tile for classified cube in `sits_view()`

# What's new in SITS version 0.16.2

### New features in SITS version 0.16.2
* Improve `sits_get_data()` to accept tibbles
* Remove multiples progress bar from `sits_cube()`
* Improve `sits_regularize()` to process in parallel by tiles, bands, and dates
* Improve `sits_regularize()` to check malformed files

# What's new in SITS version 0.16.1

### New features in SITS version 0.16.1
* Update `AWS_NO_SIGN_REQUEST` environment variable
* Solved bug in `.gc_get_valid_interval()` function.
* Now `sits_regularize` has a fault tolerance system, so that if there is a processing error the function will delete the malformed files and create them again.
* `sits_regularize` function has a new parameter called `multithreads`.
* `sits_cube` function for `local cubes` has a new parameter called `multicores`.
* Print `F1 score` in `sits_kfold_validate` with more than 2 labels.

# What's new in SITS version 0.16.0

### New features in SITS version 0.16.0-1
* hotfix `sits_cube()` function to tolerate malformed paths from STAC service;

### New features in SITS version 0.16.0
* Include `sits_apply()` function to generate new bands from existing ones;
* Improve `sits_accuracy()` function to work with multiple cubes;
* Add band parameter to `sits_view()`
* Introduce `sits_uncertainty()` function to provide uncertainty measure to probability maps;
* Improve `sits_regularize()` by taking least cloud cover by default method to compose images
* Bug fixes;


# What's new in SITS version 0.15.1

### New features in SITS version 0.15.1-1
* Fix bug in `sits_regularize` that generated images with artifacts
* Fix wrong *bbox* in `sits_cube` from STAC AWS Sentinel-2

### New features in SITS version 0.15.1
* Update README.Rmd
* Support `sits_timeline()` to sits model objects
* Update drone image
* Simplify `config_colors.yml` by removing palette names
* Temporary python files are being generated in the check
* Organize color handling in SITS
* Organize configuration files
* Improve preconditions in `sits_regularize()`
* Compress external data with bzip2
* Update gdalcubes format files
* Update rstac version
* Check provided parameters in sits_regularize function
* Use default palette for SOM colors
* Remove `start_date` and `end_date` from validation csv file
* Use a default brewer palette to plot classified cube
* Improve package help pages
* Remove unused data sets
* Remove rarely used functions
* `sits_regularize()` is producing *Float64* images as output
* Full support for Microsoft Planetary Computing

# What's new in SITS version 0.15.0 

### New features in SITS version 0.15.0-4
* Change `gdalcubes_chunk_size` in "config.yml" to improve `sits_regularize()`.

### New features in SITS version 0.15.0-3
* Fix bug in `.source_collection_access_test` to pass ellipsis to `rstac::post_request` function.

### New features in SITS version 0.15.0-2
* Fix bug in `.source_collection_access_test` to pass ellipsis to `rstac::post_request` function.
* Update drone version

### New features in SITS version 0.15.0-1
* Fix bug in `sits_plot`
* Fix bug in `sits_timeline` for cubes that do not have the same temporal extent.

### New features in SITS version 0.15.0

* Support for regularization of collections in DEAFRICA and USGS improvement
* Collection `S2_10_16D_STK-1` removed from BDC source in config file
* Add a color for `NoClass` label improvement
* Change `mapview` to `leaflet` package
* Standardize cube creation parameters
* Remove `CLASSIFIED` and `PROBS` sources from config file
* Change minimal version requirement of `terra` package to 1.4-11
* Update `sits_list_collections()` to indicate open data collection
* Geographical visualization of samples
* Remove dependencies on packages `ptw`, `signal` and `MASS`
* Add support to `open_data` collections in config file
* Change default `output_dir` parameter
* Remove `sits_cube_clone()` function
* Plot RGB images from raster cubes
* Fixed error in `sits_select()` for bands in raster cube
* Update examples in demo
* Support open data collections of DEAFRICA and AWS
* Support USGS STAC Landsat 8 catalog
* User can provide resampling method to `sits_regularize()` function
* Add support to open data collections on 'AWS' source
* Remove `OPENDATA` source
* Update documentation
* Resolve ambiguity in "bands" parameter for data cubes
* Remove "sits_bands" assignment function
* Include "labels" information only on probs and labelled data cubes
* Remove `S2_10-1` BDC collection from config
* Other bug fixes

# What's new in SITS version 0.14.1

### New features in SITS version 0.14.1-1

* Bug in cube generated by sits_regularize() cannot have "CLOUD" band

### New features in SITS version 0.14.1

* Implement new function `sits_list_collections()`
* Update gdalcubes parameters
* Implement `.source_bands_resampling()`
* Remove name from demo file
* Improve `sits_som_clean_samples()` function
* Improve `sits_bands<-()` function
* Improve `sits_select()` function
* Error in cloud band of CBERS4 data example
* Include a function to list collections available in cloud services
* sits_cube_copy() does not include information on the tile
* Get spatial resolution from config file
* Fix partial merge configuration file
* Change bbox to roi in sits


# What's new in SITS version 0.14.0

### New features in SITS version 0.14.0-2

* fix `sits_bbox()` function 

### New features in SITS version 0.14.0-1

* fix duplicate link in AWS STAC 

### New features in SITS version 0.14.0

* Now the plot of a classified cube requires a legend or a palette if the labels are not in the default sits palette. 
* Support for `S2-SEN2COR_10_16D_STK-1` BDC collection
* Remove function name from msg in `check` function
* Add `satellite` and `sensor` info in config file
* Remove `imager`, `ranger`, `proto`, and `future` packages from sits
* Support for different providers to LOCAL sources
* LOCAL source is dynamically built
* Remove `sits_cube.local_cube()` function parameters `satellite` and `sensor`
* Add parameters `origin` and `collection` to `sits_cube.local_cube()` function
* Fix LOCAL source examples and tests

# What's new in SITS version 0.13.1

### New features in SITS version 0.13.1

* Update and add more tests in CI 
* Implement new check functions 
* Change error and warning messages 
* fix deprecated warnings in keras package 
* bug fixes 

# What's new in SITS version 0.13.0-3

### New features in SITS version 0.13.0-3

* Update documentation in Machine Learning methods
* Hotfix bug in neuron labelling

# What's new in SITS version 0.13.0-2

### New features in SITS version 0.13.0-2

* Bug fixes in BDC MODIS cube

# What's new in SITS version 0.13.0-1

### New features in SITS version 0.13.0-1

* Bug fixes in check STAC bands
* Change Landsat-8 (LC8_30-1) product metadata for BDC source

# What's new in SITS version 0.13

### New features in SITS version 0.13.0

* Create API for source cube
* Update auxiliary functions of the config file
* Update config file
* Add support to others bands values in config file
* Add support to bit mask in USGS cube
* Support to multiples directories in local cubes
* Support for MODIS cloud bands
* Dealing with invalid areas in SITS
* Support for WTSS

# What's new in SITS version 0.12

### New features in SITS version 0.12.1

* Update README
* Change docker image to new sits build
* Adjust CMASK bands values in BDC cubes
* Support for sits_config_sensor_bands accept more than one sensor
* sits cube selection by shapefile
* Problem - sits classify

### New features in SITS version 0.12.0

* Bugs fixed
* Documentation updated
* Support for multiple tile in local cubes
* Improve selection using `roi` parameter in `sits_classify()` function

# What's new in SITS version 0.11

### New features in SITS version 0.11.2

* Added keras serialisation to TempCNN and ResNet models

### New features in SITS version 0.11.1

* Removed LSTM and FCN deep learning models

### New features in SITS version 0.11.0

* Important improvements in classification performance
* Updated version of deep learning methods
* Support for STAC access to Brazil Data Cube, AWS and DE Africa
* Improved sits validation

# What's new in SITS version 0.10

### New features in SITS version 0.10.0
* Version update 0.10.0
* Continuous Integration (drone.io)
* Bayesian smoothing improvement
* Introduces Snow multiprocessing architecture
* cube plot allow region of interest (roi)
* Support for multiple tiles
* Update documentation
* Bugs fix

# What's new in SITS version 0.9

### New features in SITS version 0.9.8
* Access to Sentinel-2 level-2A images in AWS
* Access to the Brazil Data Cube using STAC
* Improved raster API 
* Code revision with lintr and good practices packages
* Improvement of assertions and code coverage
* Examples and tests generate output in tempdir()

### New features in SITS version 0.9.7
* Image classification using region of interest (ROI)

### New features in SITS version 0.9.6
* Access and processing of tiles of the Brazil Data Cube
* Plotting of data cube and probability images
* Examples of using SITS with SENTINEL-2 and CBERS-4 images 

### New features in SITS version 0.9.5
* Time series tibbles and data cube metadata can now be saved and read in SQLite
* Code coverage increased to 95%
* Vignettes have been moved to "sits-docs" to reduce building time

### New features in SITS version 0.9.4

* Filtering can be applied to classified images
* Band suffix in filtering is now set to ""
* Improvement in code coverage: most of the code has more than 90% coverage

### New features in SITS version 0.9.3

* Improvements in reading shapefiles: using sampling to retrieve time series inside polygons
* Improvement is plotting: uses overloading to the "plot" function

### New features in SITS version 0.9.2

* Raster classification results can now have versions: a new parameter "version" has been included in the `sits_classify` function. 

* Corrections to `sits_kohonen` and to the documentation.

### New Features in version 0.9.1

* New deep learning models for time series: 1D convolutional neural networks (`sits_FCN`), combining 1D CNN and multi-layer perceptron networks (`sits_TempCNN`), 1D version of ResNet (`sits_ResNet`), and combination of long-short term memory (LSTM) and 1D CNN (`sits_LSTM_FCN`).

* New version of area accuracy measures that include Olofsson metrics ()

# What's new in SITS version 0.8

### New Features

* From version 0.8 onwards, the package has been designed to work with data cubes. All references to "coverage" have been replaced by references to "cubes". 

* The classification of raster images using `sits_classify` now produces images with the information on the probability of each class for each pixel. This allows more flexibility in the options for labeling the resulting probability raster files.

* The function `sits_label_classification` has been introduced to generate a labelled image from the class probability files, with optional smoothing. The choices are `smoothing = none` (default), `smoothing = bayesian` (for bayesian smoothing) and `smoothing = majority` (for majority smoothing).

* To better define a cube, the metadata tibble associated to a cube requires four parameters to define the cube: (a) the web service that provides time series or cubes; (b) the URL of the web service; (c) the name of the satellite; (d) the name of the satellite sensor. If not provided, these parameters are inferred for the `sits` configuration file.

* The functions that do data transformations, such as `sits_tasseled_cap` and `sits_savi` now require a `sensor` parameter ("MODIS" is the default)

* Functions `sits_bands` and `sits_labels` now work for both tibbles with time series and data cubes.

### Configuration file

* The SITS configuration file has been improved to include information about web service providers, satellites and sensor parameters. Please use `sits_show_config()` to see the default contents. Users can override these parameters or add their own by creating a `config.yml` file in their home directory. 

### Examples and Demos

* Examples and demos that include classification of raster files now use the `inSitu` R package, available using `devtools::install_github(e-sensing/inSitu)`. 

* All examples have been tested and checked for correctness.

### Functions removed

* `sits_coverage` has been replaced by `sits_cube`.

* `sits_raster_classification` has been removed. Please use `sits_classify`.

* In `sits_classify`, the parameter `out_prefix` has been changed to `output_dir`, to allow better control of the directory on which to write.

* `sits_bayes_smooth` has been removed. Please use `sits_label_classification` with `smoothing = bayesian`. 

* To define a cube based on local files, `service = RASTER` has been replaced by `service = LOCALHOST`.  

### Improvements and fixes

*  For programmers only: The `sits_cube.R` file now includes many convenience functions to avoid using cumbersome indexes to files and vector: `.sits_raster_params`, `.sits_cube_all_robjs`, `.sits_class_band_name`, `.sits_cube_bands`, `.sits_cube_service`, `.sits_cube_file`, `.sits_cube_files`, `.sits_cube_labels`, `.sits_cube_timeline`, `.sits_cube_robj`, `.sits_cube_all_robjs`, `.sits_cube_missing_values`, `.sits_cube_minimum_values`, `.sits_cube_maximum_values`, `.sits_cube_scale_factors`, `.sits_files_robj`. Please look at the documentation provided in the `sits_cube.R` file.

* For programmers only: The metadata that describes the data cube no longer stores the raster objects associated to the files associated with the cube.
