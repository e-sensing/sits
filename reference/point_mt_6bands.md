# A time series sample with data from 2000 to 2016

A dataset containing a tibble with one time series samples in the Mato
Grosso state of Brazil. The time series comes from MOD13Q1 collection 6
images.

## Usage

``` r
data(point_mt_6bands)
```

## Format

A tibble with 1 rows and 7 variables: longitude: East-west coordinate of
the time series sample (WGS 84), latitude (North-south coordinate of the
time series sample in WGS 84), start_date (initial date of the time
series), end_date (final date of the time series), label (the class
label associated to the sample), cube (the name of the cube associated
with the data), time_series (list containing a tibble with the values of
the time series).
