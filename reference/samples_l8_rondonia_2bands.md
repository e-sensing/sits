# Samples of Amazon tropical forest biome for deforestation analysis

A sits tibble with time series samples from Brazilian Amazonia rain
forest.

The labels are: "Deforestation", "Forest", "NatNonForest" and "Pasture".

The time series were extracted from the Landsat-8 BDC data cube
(collection = "LC8_30_16D_STK-1", tiles = "038047"). These time series
comprehends a period of 12 months (25 observations) from "2018-07-12" to
"2019-07-28". The extracted bands are NDVI and EVI. Cloudy values were
removed and interpolated.

## Usage

``` r
data("samples_l8_rondonia_2bands")
```

## Format

A `sits` tibble with 160 samples.
