# Export a sits tibble metadata to the CSV format

Converts metadata from a sits tibble to a CSV file. The CSV file will
not contain the actual time series. Its columns will be the same as
those of a CSV file used to retrieve data from ground information
("latitude", "longitude", "start_date", "end_date", "cube", "label"). If
the file is NULL, returns a data.frame as an object

## Usage

``` r
sits_to_csv(data, file = NULL)

# S3 method for class 'sits'
sits_to_csv(data, file = NULL)

# S3 method for class 'tbl_df'
sits_to_csv(data, file)

# Default S3 method
sits_to_csv(data, file)
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
csv_file <- paste0(tempdir(), "/cerrado_2classes.csv")
sits_to_csv(cerrado_2classes, file = csv_file)
#> # A tibble: 746 × 6
#>       id longitude latitude start_date end_date   label  
#>    <int>     <dbl>    <dbl> <date>     <date>     <chr>  
#>  1     1     -54.2    -14.0 2000-09-13 2001-08-29 Cerrado
#>  2     2     -54.2    -14.0 2001-09-14 2002-08-29 Cerrado
#>  3     3     -54.2    -14.0 2002-09-14 2003-08-29 Cerrado
#>  4     4     -54.2    -14.0 2003-09-14 2004-08-28 Cerrado
#>  5     5     -54.2    -14.0 2004-09-13 2005-08-29 Cerrado
#>  6     6     -54.2    -14.0 2005-09-14 2006-08-29 Cerrado
#>  7     7     -54.2    -14.0 2006-09-14 2007-08-29 Cerrado
#>  8     8     -54.2    -14.0 2007-09-14 2008-08-28 Cerrado
#>  9     9     -54.2    -14.0 2008-09-13 2009-08-29 Cerrado
#> 10    10     -54.2    -14.0 2009-09-14 2010-08-29 Cerrado
#> # ℹ 736 more rows
```
