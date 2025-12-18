# Create data cubes from image collections accessible by STAC

Creates a data cube based on spatial and temporal restrictions in
collections accessible by the STAC protocol

## Usage

``` r
# S3 method for class 'stac_cube'
sits_cube(
  source,
  collection,
  ...,
  bands = NULL,
  tiles = NULL,
  roi = NULL,
  crs = NULL,
  start_date = NULL,
  end_date = NULL,
  orbit = "descending",
  platform = NULL,
  multicores = 2L,
  progress = TRUE
)
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

- bands:

  Spectral bands and indices to be included in the cube (optional). Use
  [`sits_list_collections()`](https://e-sensing.github.io/sits/reference/sits_list_collections.md)
  to find out the bands available for each collection.

- tiles:

  Tiles from the collection to be included in the cube (see details
  below).

- roi:

  Region of interest (see below).

- crs:

  The Coordinate Reference System (CRS) of the roi. (see details below).

- start_date, end_date:

  Initial and final dates to include images from the collection in the
  cube (optional). (Date in YYYY-MM-DD format).

- orbit:

  Orbit name ("ascending", "descending") for SAR cubes.

- platform:

  Optional parameter specifying the platform in case of "LANDSAT"
  collection. Options: `Landsat-5, Landsat-7, Landsat-8, Landsat-9`.

- multicores:

  Number of workers for parallel processing (integer, min = 1, max =
  2048).

- progress:

  Logical: show a progress bar?

## Value

A `tibble` describing the contents of a data cube.

## Note

Data cubes are identified on cloud providers using `sits_cube`. The
result of `sits_cube` is a description of the location of the requested
data in the cloud provider. No download is done.

To create data cube objects from cloud providers, users need to inform:

- `source`: Name of the cloud provider. One of "AWS", "BDC", "CDSE",
  "DEAFRICA", "DEAUSTRALIA", "HLS", "PLANETSCOPE", "MPC", "SDC",
  "TERRASCOPE", or "USGS";

- `collection`: Name of an image collection available in the cloud
  provider (e.g, "SENTINEL-1-RTC" in MPC). Use
  [`sits_list_collections()`](https://e-sensing.github.io/sits/reference/sits_list_collections.md)
  to see which collections are supported;

- `tiles`: A set of tiles defined according to the collection tiling
  grid (e.g, c("20LMR", "20LMP") in MGRS);

- `roi`: Region of interest (see below)

The parameters `bands`, `start_date`, and `end_date` are optional for
cubes created from cloud providers.

Either `tiles` or `roi` must be informed. The `tiles` should specify a
set of valid tiles for the ARD collection. For example, Landsat data has
tiles in `WRS2` tiling system and Sentinel-2 data uses the `MGRS` tiling
system. The `roi` parameter is used to select all types of images. This
parameter does not crop a region; it only selects images that intersect
it.

To define a `roi` use one of:

- A path to a shapefile with polygons;

- A `sfc` or `sf` object from `sf` package;

- A `SpatExtent` object from `terra` package;

- A named `vector` (`"lon_min"`, `"lat_min"`, `"lon_max"`, `"lat_max"`)
  in WGS84;

- A named `vector` (`"xmin"`, `"xmax"`, `"ymin"`, `"ymax"`) with XY
  coordinates.

Defining a region of interest using `SpatExtent` or XY values not in
WGS84 requires the `crs` parameter to be specified.

To get more details about each provider and collection available in
`sits`, please read the online sits book (e-sensing.github.io/sitsbook).
The chapter `Earth Observation data cubes` provides a detailed
description of all collections you can use with `sits`
(e-sensing.github.io/sitsbook/earth-observation-data-cubes.html).

## Examples

``` r
if (sits_run_examples()) {
    # --- Creating Sentinel cube from MPC
    s2_cube <- sits_cube(
        source = "MPC",
        collection = "SENTINEL-2-L2A",
        tiles = "20LKP",
        bands = c("B05", "CLOUD"),
        start_date = "2018-07-18",
        end_date = "2018-08-23"
    )

    # --- Creating Landsat cube from MPC
    roi <- c(
        "lon_min" = -50.410, "lon_max" = -50.379,
        "lat_min" = -10.1910, "lat_max" = -10.1573
    )
    mpc_cube <- sits_cube(
        source = "MPC",
        collection = "LANDSAT-C2-L2",
        bands = c("BLUE", "RED", "CLOUD"),
        roi = roi,
        start_date = "2005-01-01",
        end_date = "2006-10-28"
    )

    ## Sentinel-1 SAR from MPC
    roi_sar <- c(
        "lon_min" = -50.410, "lon_max" = -50.379,
        "lat_min" = -10.1910, "lat_max" = -10.1573
    )

    s1_cube_open <- sits_cube(
        source = "MPC",
        collection = "SENTINEL-1-GRD",
        bands = c("VV", "VH"),
        orbit = "descending",
        roi = roi_sar,
        start_date = "2020-06-01",
        end_date = "2020-09-28"
    )
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
    # --- Access to Digital Earth Australia
    cube_deaustralia <- sits_cube(
        source = "DEAUSTRALIA",
        collection = "GA_LS8CLS9C_GM_CYEAR_3",
        bands = c("RED", "GREEN", "BLUE"),
        roi = c(
            lon_min = 137.15991,
            lon_max = 138.18467,
            lat_min = -33.85777,
            lat_max = -32.56690
        ),
        start_date = "2018-01-01",
        end_date = "2018-12-31"
    )
    # --- Access to CDSE open data Sentinel 2/2A level 2 collection
    # --- remember to set the appropriate environmental variables
    # It is recommended that `multicores` be used to accelerate the process.
    s2_cube <- sits_cube(
        source = "CDSE",
        collection = "SENTINEL-2-L2A",
        tiles = c("20LKP"),
        bands = c("B04", "B08", "B11"),
        start_date = "2018-07-18",
        end_date = "2019-01-23"
    )

    ## --- Sentinel-1 SAR from CDSE
    # --- remember to set the appropriate environmental variables
    # --- Obtain a AWS_ACCESS_KEY_ID and AWS_ACCESS_SECRET_KEY_ID
    # --- from CDSE
    roi_sar <- c(
        "lon_min" = 33.546, "lon_max" = 34.999,
        "lat_min" = 1.427, "lat_max" = 3.726
    )
    s1_cube_open <- sits_cube(
        source = "CDSE",
        collection = "SENTINEL-1-RTC",
        bands = c("VV", "VH"),
        orbit = "descending",
        roi = roi_sar,
        start_date = "2020-01-01",
        end_date = "2020-06-10"
    )


    # -- Access to World Cover data (2021) via Terrascope
    cube_terrascope <- sits_cube(
        source = "TERRASCOPE",
        collection = "WORLD-COVER-2021",
        roi = c(
            lon_min = -62.7,
            lon_max = -62.5,
            lat_min = -8.83,
            lat_max = -8.70
        )
    )
}
```
