# Create data cubes from image collections

Creates a data cube based on spatial and temporal restrictions in
collections available in cloud services or local repositories. Available
options are:

- To create data cubes from providers which support the STAC protocol,
  use
  [`sits_cube.stac_cube`](https://e-sensing.github.io/sits/reference/sits_cube.stac_cube.md).

- To create raster data cubes from local image files, use
  [`sits_cube.local_cube`](https://e-sensing.github.io/sits/reference/sits_cube.local_cube.md).

- To create vector data cubes from local image and vector files, use
  [`sits_cube.vector_cube`](https://e-sensing.github.io/sits/reference/sits_cube.vector_cube.md).

- To create raster data cubes from local image files which have been
  classified or labelled, use
  [`sits_cube.results_cube`](https://e-sensing.github.io/sits/reference/sits_cube.results_cube.md).

## Usage

``` r
sits_cube(source, collection, ...)
```

## Arguments

- source:

  Data source: one of `"AWS"`, `"BDC"`, `"CDSE"`, `"DEAFRICA"`,
  `"DEAUSTRALIA"`, `"HLS"`, `"PLANETSCOPE"`, `"MPC"`, `"SDC"` or
  `"USGS"`.

- collection:

  Image collection in data source. To find out the supported
  collections, use
  [`sits_list_collections()`](https://e-sensing.github.io/sits/reference/sits_list_collections.md)).

- ...:

  Other parameters to be passed for specific types.

## Value

A `tibble` describing the contents of a data cube.

## Note

The main `sits` classification workflow has the following steps:

1.  `sits_cube`: selects a ARD image collection from a cloud provider.

2.  [`sits_cube_copy`](https://e-sensing.github.io/sits/reference/sits_cube_copy.md):
    copies an ARD image collection from a cloud provider to a local
    directory for faster processing.

3.  [`sits_regularize`](https://e-sensing.github.io/sits/reference/sits_regularize.md):
    create a regular data cube from an ARD image collection.

4.  [`sits_apply`](https://e-sensing.github.io/sits/reference/sits_apply.md):
    create new indices by combining bands of a regular data cube
    (optional).

5.  [`sits_get_data`](https://e-sensing.github.io/sits/reference/sits_get_data.md):
    extract time series from a regular data cube based on user-provided
    labelled samples.

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

The following cloud providers are supported, based on the STAC protocol:
Amazon Web Services (AWS), Brazil Data Cube (BDC), Copernicus Data Space
Ecosystem (CDSE), Digital Earth Africa (DEAFRICA), Digital Earth
Australia (DEAUSTRALIA), Microsoft Planetary Computer (MPC), Nasa
Harmonized Landsat/Sentinel (HLS), Swiss Data Cube (SDC), TERRASCOPE and
USGS Landsat (USGS). Data cubes can also be created using local files.

In `sits`, a data cube is represented as a tibble with metadata
describing a set of image files obtained from cloud providers. It
contains information about each individual file.

A data cube in `sits` is:

- A set of images organized in tiles of a grid system (e.g., MGRS).

- Each tile contains single-band images in a unique zone of the
  coordinate system (e.g, tile 20LMR in MGRS grid) covering the period
  between `start_date` and `end_date`.

- Each image of a tile is associated to a unique temporal interval. All
  intervals share the same spectral bands.

- Different tiles may cover different zones of the same grid system.

A regular data cube is a data cube where:

- All tiles share the same set of regular temporal intervals.

- All tiles share the same spectral bands and indices.

- All images have the same spatial resolution.

- Each location in a tile is associated a set of multi-band time series.

- For each tile, interval and band, the cube is reduce to a 2D image.

## Author

Felipe Carlos, <efelipecarlos@gmail.com>

Felipe Carvalho, <felipe.carvalho@inpe.br>

Gilberto Camara, <gilberto.camara@inpe.br>

Rolf Simoes, <rolfsimoes@gmail.com>

## Examples

``` r
if (sits_run_examples()) {
    # --- Access to the Brazil Data Cube
    # create a raster cube file based on the information in the BDC
    cbers_tile <- sits_cube(
        source = "BDC",
        collection = "CBERS-WFI-16D",
        bands = c("NDVI", "EVI"),
        tiles = "007004",
        start_date = "2018-09-01",
        end_date = "2019-08-28"
    )
    # --- Access to Digital Earth Africa
    # create a raster cube file based on the information about the files
    # DEAFRICA does not support definition of tiles
    cube_deafrica <- sits_cube(
        source = "DEAFRICA",
        collection = "SENTINEL-2-L2A",
        bands = c("B04", "B08"),
        roi = c(
            "lat_min" = 17.379,
            "lon_min" = 1.1573,
            "lat_max" = 17.410,
            "lon_max" = 1.1910
        ),
        start_date = "2019-01-01",
        end_date = "2019-10-28"
    )
    # --- Create a cube based on a local MODIS data
    # MODIS local files have names such as
    # "TERRA_MODIS_012010_NDVI_2013-09-14.jp2"
    # see the parse info parameter as an example on how to
    # decode local files
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    modis_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        parse_info = c("satellite", "sensor", "tile", "band", "date")
    )
}
```
