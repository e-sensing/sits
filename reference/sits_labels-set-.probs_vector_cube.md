# Change the labels of a probs vector data cube

A "probs_vector_cube" is a data cube with a set of segments that contain
the probability values of each class for each polygon. When a user
changes the labels of the class, this function modifies the labels
associated to the cube's metadata and also changes the names in the
segments file. The GPKG file containing the segments and the probability
values is replace with a new file with the desired labels.

## Usage

``` r
# S3 method for class 'probs_vector_cube'
sits_labels(data) <- value
```

## Arguments

- data:

  Probs vector data cube.

- value:

  A character vector used to convert labels.

## Author

Gilberto Camara, <gilberto.camara@inpe.br>
