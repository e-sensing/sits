# Configure parameters for sits package

These functions load and show sits configurations.

The \`sits\` package uses a configuration file that contains information
on parameters required by different functions. This includes information
about the image collections handled by \`sits\`.

`sits_config()` loads the default configuration file and the user
provided configuration file. The final configuration is obtained by
overriding the options by the values provided by the user.

## Usage

``` r
sits_config(config_user_file = NULL)
```

## Arguments

- config_user_file:

  YAML user configuration file (character vector of a file with "yml"
  extension)

## Value

Called for side effects

## Details

Users can provide additional configuration files, by specifying the
location of their file in the environmental variable
`SITS_CONFIG_USER_FILE` or as parameter to this function.

To see the key entries and contents of the current configuration values,
use `link[sits]{sits_config_show()}`.

## Author

Rolf Simoes, <rolfsimoes@gmail.com>

## Examples

``` r
yaml_user_file <- system.file("extdata/config_user_example.yml",
    package = "sits"
)
sits_config(config_user_file = yaml_user_file)
```
