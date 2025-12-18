# Export a a full sits tibble to the CSV format

Converts metadata and data from a sits tibble to a CSV file. The CSV
file will not contain the actual time series. Its columns will be the
same as those of a CSV file used to retrieve data from ground
information ("latitude", "longitude", "start_date", "end_date", "cube",
"label"), plus the all the time series for each data

## Usage

``` r
sits_timeseries_to_csv(data, file = NULL)
```

## Arguments

- data:

  Time series (tibble of class "sits").

- file:

  Full path of the exported CSV file (valid file name with extension
  ".csv").

## Value

Return data.frame with CSV columns (optional)

## Author

Gilberto Camara, <gilberto.camara@inpe.br>

## Examples

``` r
csv_ts <- sits_timeseries_to_csv(cerrado_2classes)
csv_file <- paste0(tempdir(), "/cerrado_2classes_ts.csv")
sits_timeseries_to_csv(cerrado_2classes, file = csv_file)
```
