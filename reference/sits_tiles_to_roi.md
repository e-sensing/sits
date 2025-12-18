# Convert MGRS tile information to ROI in WGS84

Takes a list of MGRS tiles and produces a ROI covering them

## Usage

``` r
sits_tiles_to_roi(tiles, grid_system = "MGRS")
```

## Arguments

- tiles:

  Character vector with names of MGRS tiles

- grid_system:

  Grid system to be used

## Value

roi Valid ROI to use in other SITS functions

## Author

Gilberto Camara, <gilberto.camara@inpe.br>

Rolf Simoes, <rolf.simoes@gmail.com>
