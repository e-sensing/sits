# SITS (development version)

We are preparing to release the package on CRAN and are making relevant changes in the development version.

# What's new in SITS version 0.9
# 
# ### New features in SITS version 0.9.5

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




