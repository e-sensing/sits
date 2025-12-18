# Get time series from data cubes and cloud services

Retrieve a set of time series from a data cube and and put the result in
a `sits tibble`, which contains both the satellite image time series and
their metadata.

There are five options for the specifying the input `samples` parameter:

- A CSV file: see
  [`sits_get_data.csv`](https://e-sensing.github.io/sits/reference/sits_get_data.csv.md).

- A shapefile: see
  [`sits_get_data.shp`](https://e-sensing.github.io/sits/reference/sits_get_data.shp.md).

- An `sf` object: see
  [`sits_get_data.sf`](https://e-sensing.github.io/sits/reference/sits_get_data.sf.md).

- A `sits` tibble: see
  [`sits_get_data.sits`](https://e-sensing.github.io/sits/reference/sits_get_data.sits.md).

- A data.frame: see
  [`sits_get_data.data.frame`](https://e-sensing.github.io/sits/reference/sits_get_data.data.frame.md).

## Usage

``` r
sits_get_data(cube, samples, ...)

# Default S3 method
sits_get_data(cube, samples, ...)
```

## Arguments

- cube:

  Data cube from where data is to be retrieved. (tibble of class
  "raster_cube").

- samples:

  Location of the samples to be retrieved. Either a tibble of class
  "sits", an "sf" object, the name of a shapefile or csv file, or a
  data.frame with columns "longitude" and "latitude".

- ...:

  Specific parameters for each input.

## Value

A tibble of class "sits" with set of time series \<longitude, latitude,
start_date, end_date, label, time_series\>.

## Note

The main `sits` classification workflow has the following steps:

1.  [`sits_cube`](https://e-sensing.github.io/sits/reference/sits_cube.md):
    selects a ARD image collection from a cloud provider.

2.  [`sits_cube_copy`](https://e-sensing.github.io/sits/reference/sits_cube_copy.md):
    copies an ARD image collection from a cloud provider to a local
    directory for faster processing.

3.  [`sits_regularize`](https://e-sensing.github.io/sits/reference/sits_regularize.md):
    create a regular data cube from an ARD image collection.

4.  [`sits_apply`](https://e-sensing.github.io/sits/reference/sits_apply.md):
    create new indices by combining bands of a regular data cube
    (optional).

5.  `sits_get_data`: extract time series from a regular data cube based
    on user-provided labelled samples.

6.  [`sits_train`](https://e-sensing.github.io/sits/reference/sits_train.md):
    train a machine learning model based on image time series.

7.  [`sits_classify`](https://e-sensing.github.io/sits/reference/sits_classify.md):
    classify a data cube using a machine learning model and obtain a
    probability cube.

8.  [`sits_smooth`](https://e-sensing.github.io/sits/reference/sits_smooth.md):
    post-process a probability cube using a spatial smoother to remove
    outliers and increase spatial consistency.

9.  [`sits_label_classification`](https://e-sensing.github.io/sits/reference/sits_label_classification.md):
    produce a classified map by selecting the label with the highest
    probability from a smoothed cube.

To be able to build a machine learning model to classify a data cube,
one needs to use a set of labelled time series. These time series are
created by taking a set of known samples, expressed as labelled points
or polygons. This `sits_get_data` function uses these samples to extract
time series from a data cube. It needs a `cube` parameter which points
to a regularized data cube, and a `samples` parameter that describes the
locations of the training set.

## Author

Felipe Carlos, <efelipecarlos@gmail.com>

Felipe Carvalho, <felipe.carvalho@inpe.br>

Gilberto Camara, <gilberto.camara@inpe.br>

Rolf Simoes, <rolfsimoes@gmail.com>

## Examples

``` r
if (sits_run_examples()) {
    # reading a lat/long from a local cube
    # create a cube from local files
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    raster_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir
    )
    samples <- tibble::tibble(longitude = -55.66738, latitude = -11.76990)
    point_ndvi <- sits_get_data(raster_cube, samples)
    #
    # reading samples from a cube based on a  CSV file
    csv_file <- system.file("extdata/samples/samples_sinop_crop.csv",
        package = "sits"
    )
    points <- sits_get_data(cube = raster_cube, samples = csv_file)

    # reading a shapefile from BDC (Brazil Data Cube)
    bdc_cube <- sits_cube(
        source = "BDC",
        collection = "CBERS-WFI-16D",
        bands = c("NDVI", "EVI"),
        tiles = c("007004", "007005"),
        start_date = "2018-09-01",
        end_date = "2018-10-28"
    )
    # define a shapefile to be read from the cube
    shp_file <- system.file("extdata/shapefiles/bdc-test/samples.shp",
        package = "sits"
    )
    # get samples from the BDC based on the shapefile
    time_series_bdc <- sits_get_data(
        cube = bdc_cube,
        samples = shp_file
    )
}
```
