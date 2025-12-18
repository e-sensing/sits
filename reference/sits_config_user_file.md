# Create a user configuration file.

Creates a user configuration file.

## Usage

``` r
sits_config_user_file(file_path, overwrite = FALSE)
```

## Arguments

- file_path:

  file to store the user configuration file

- overwrite:

  replace current configuration file?

## Value

Called for side effects

## Examples

``` r
user_file <- paste0(tempdir(), "/my_config_file.yml")
sits_config_user_file(user_file)
#> Warning: save default user configuratiin
#>  - please update your SITS_CONFIG_USER_FILE environmental variable 
#>  to point to the chosen file
```
