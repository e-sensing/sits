# Function to save color table as QML style for data cube

Saves a color table associated to a classified data cube as a QGIS style
file

## Usage

``` r
sits_colors_qgis(cube, file)

# S3 method for class 'class_cube'
sits_colors_qgis(cube, file)

# S3 method for class 'class_vector_cube'
sits_colors_qgis(cube, file)
```

## Arguments

- cube:

  a classified data cube

- file:

  a QGIS style file to be written to

## Value

No return, called for side effects

## Author

Gilberto Camara, <gilberto.camara@inpe.br>

## Examples

``` r
if (sits_run_examples()) {
    data_dir <- system.file("extdata/raster/classif", package = "sits")
    ro_class <- sits_cube(
        source = "MPC",
        collection = "SENTINEL-2-L2A",
        data_dir = data_dir,
        parse_info = c(
            "X1", "X2", "tile", "start_date", "end_date",
            "band", "version"
        ),
        bands = "class",
        labels = c(
            "1" = "Clear_Cut_Burned_Area",
            "2" = "Clear_Cut_Bare_Soil",
            "3" = "Clear_Cut_Vegetation",
            "4" = "Forest"
        )
    )
    qml_file <- paste0(tempdir(), "/qgis.qml")
    sits_colors_qgis(ro_class, qml_file)
}
```
