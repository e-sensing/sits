# SITS (development version)

We are preparing to release the package on CRAN and are making relevant changes in the development version.


# What's new in SITS version 0.14.0-1

### New features in SITS version 0.14.0-1

* fix duplicate link in AWS STAC 

# What's new in SITS version 0.14.0

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

* New version of area accuracy measures that include Oloffson metrics ()

# What's new in SITS version 0.8

### New Features

* From version 0.8 onwards, the package has been designed to work with data cubes. All references to "coverage" have been replaced by references to "cubes". 

* The classification of raster images using `sits_classify` now produces images with the information on the probability of each class for each pixel. This allows more flexibility in the options for labeling the resulting probability raster files.

* The function `sits_label_classification` has been introduced to generate a labelled image from the class probability files, with optional smoothing. The choices are `smoothing = none` (default), `smoothing = bayesian` (for bayesian smoothing) and `smoothing = majority` (for majority smoothing).

* To better define a cube, the metadata tibble associated to a cube requires four parameters to define the cube: (a) the web service that provides time series or cubes; (b) the URL of the web service; (c) the name of the satellite; (d) the name of the satellite sensor. If not provided, these parameters are inferred for the `sits` configuration file.

* The functions that do data transformations, such as `sits_tasseled_cap` and `sits_savi` now require a `sensor` parameter ("MODIS" is the default)

* Functions `sits_bands` and `sits_labels` now work for both tibbles with time series and data cubes.

### Configuration file

* The SITS configuration file has been improved to include information about web service providers, satellites and sensor parameters. Plase use `sits_show_config()` to see the default contents. Users can override these parameters or add their own by creating a `config.yml` file in their home directory. 

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
