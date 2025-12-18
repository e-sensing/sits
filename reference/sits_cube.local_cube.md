# Create sits cubes from cubes in flat files in a local

Creates data cubes based on files on local directory. Assumes users have
downloaded the data from a known cloud collection or the data has been
created by `sits`.

## Usage

``` r
# S3 method for class 'local_cube'
sits_cube(
  source,
  collection,
  ...,
  bands = NULL,
  tiles = NULL,
  start_date = NULL,
  end_date = NULL,
  data_dir,
  parse_info = c("X1", "X2", "tile", "band", "date"),
  delim = "_",
  multicores = 2L,
  progress = TRUE
)
```

## Arguments

- source:

  Data source: one of `"AWS"`, `"BDC"`, `"CDSE"`, `"DEAFRICA"`,
  `"DEAUSTRALIA"`, `"HLS"`, `"PLANETSCOPE"`, `"MPC"`, `"SDC"` or
  `"USGS"`. This is the source from which the data has been downloaded.

- collection:

  Image collection in data source. To find out the supported
  collections, use
  [`sits_list_collections()`](https://e-sensing.github.io/sits/reference/sits_list_collections.md)).

- ...:

  Other parameters to be passed for specific types.

- bands:

  Spectral bands and indices to be included in the cube (optional).

- tiles:

  Tiles from the collection to be included in the cube (see details
  below).

- start_date, end_date:

  Initial and final dates to include images from the collection in the
  cube (optional). (Date in YYYY-MM-DD format).

- data_dir:

  Local directory where images are stored.

- parse_info:

  Parsing information for local files.

- delim:

  Delimiter for parsing local files (default = "\_")

- multicores:

  Number of workers for parallel processing (integer, min = 1, max =
  2048).

- progress:

  Logical: show a progress bar?

## Value

A `tibble` describing the contents of a data cube.

## Note

To create a cube from local files, please inform:

- `source`: The data provider from which the data was downloaded (e.g,
  "BDC", "MPC");

- `collection`: The collection from which the data comes from. (e.g.,
  `"SENTINEL-2-L2A"` for the Sentinel-2 MPC collection level 2A);

- `data_dir`: The local directory where the image files are stored.

- `parse_info`: Defines how to extract metadata from file names by
  specifying the order and meaning of each part, separated by the
  `"delim"` character. Default value is
  `c("X1", "X2", "tile", "band", "date")`.

- `delim`: The delimiter character used to separate components in the
  file names. Default is `"_"`.

Please ensure that local files meet the following requirements:

- All image files must have the same spatial resolution and projection;

- Each file should represent a single image band for a single date;

- File names must include information about the `tile`, `date`, and
  `band` in their names.

- The `parse_info` parameter tells `sits` how to extract metadata from
  file names.

- By default the `parse_info` parameter is
  `c(satellite, sensor, tile, band, date)`.

Example of supported file names are:

- `"CBERS-4_WFI_022024_B13_2021-05-15.tif"`;

- `"SENTINEL-1_GRD_30TXL_VV_2023-03-10.tif"`;

- `"LANDSAT-8_OLI_198030_B04_2020-09-12.tif"`.

When you load a local data cube specifying the `source` (e.g., AWS, MPC)
and `collection`, `sits` assumes that the data properties (e.g., scale
factor, minimum, and maximum values) match those defined for the
selected provider. If you are working with custom data from an
unsupported source or data that does not follow the standard definitions
of providers in sits, refer to the Technical Annex of the `sits` online
book for guidance on handling such cases
(e-sensing.github.io/sitsbook/technical-annex.html).

## Examples

``` r
if (sits_run_examples()) {
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
