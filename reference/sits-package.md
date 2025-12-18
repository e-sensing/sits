# sits

Satellite Image Time Series Analysis for Earth Observation Data Cubes

## Note

The main `sits` classification workflow has the following steps:

1.  [`sits_cube`](https://e-sensing.github.io/sits/reference/sits_cube.md):
    selects a ARD image collection from a cloud provider.

2.  [`sits_cube_copy`](https://e-sensing.github.io/sits/reference/sits_cube_copy.md):
    copies an ARD image collection from a cloud provider to a local
    directory for faster processing.

3.  [`sits_regularize`](https://e-sensing.github.io/sits/reference/sits_regularize.md):
    create a regular data cube from an ARD image collection.

4.  [`sits_apply`](https://e-sensing.github.io/sits/reference/sits_apply.md):
    create new indices by combining bands of a regular data cube
    (optional).

5.  [`sits_get_data`](https://e-sensing.github.io/sits/reference/sits_get_data.md):
    extract time series from a regular data cube based on user-provided
    labelled samples.

6.  [`sits_train`](https://e-sensing.github.io/sits/reference/sits_train.md):
    train a machine learning model based on image time series.

7.  [`sits_classify`](https://e-sensing.github.io/sits/reference/sits_classify.md):
    classify a data cube using a machine learning model and obtain a
    probability cube.

8.  [`sits_smooth`](https://e-sensing.github.io/sits/reference/sits_smooth.md):
    post-process a probability cube using a spatial smoother to remove
    outliers and increase spatial consistency.

9.  [`sits_label_classification`](https://e-sensing.github.io/sits/reference/sits_label_classification.md):
    produce a classified map by selecting the label with the highest
    probability from a smoothed cube.

## Purpose

The SITS package provides a set of tools for analysis, visualization and
classification of satellite image time series. It includes methods for
filtering, clustering, classification, and post-processing.

## See also

Useful links:

- <https://github.com/e-sensing/sits/>

- <https://e-sensing.github.io/sitsbook/>

- <https://e-sensing.github.io/sits/>

- Report bugs at <https://github.com/e-sensing/sits/issues>

## Author

**Maintainer**: Gilberto Camara <gilberto.camara.inpe@gmail.com>
\[thesis advisor\]

Authors:

- Rolf Simoes <rolfsimoes@gmail.com>

- Felipe Souza <felipe.carvalho@inpe.br>

- Felipe Carlos <efelipecarlos@gmail.com>

Other contributors:

- Lorena Santos <lorena.santos@inpe.br> \[contributor\]

- Charlotte Pelletier <charlotte.pelletier@univ-ubs.fr> \[contributor\]

- Estefania Pizarro <eapizarroa@ine.gob.cl> \[contributor\]

- Karine Ferreira <karine.ferreira@inpe.br> \[contributor, thesis
  advisor\]

- Alber Sanchez <alber.ipia@inpe.br> \[contributor\]

- Alexandre Assuncao <alexcarssuncao@gmail.com> \[contributor\]

- Daniel Falbel <dfalbel@gmail.com> \[contributor\]

- Gilberto Queiroz <gilberto.queiroz@inpe.br> \[contributor\]

- Johannes Reiche <johannes.reiche@wur.nl> \[contributor\]

- Pedro Andrade <pedro.andrade@inpe.br> \[contributor\]

- Pedro Brito <pedro_brito1997@hotmail.com> \[contributor\]

- Renato Assuncao <assuncaoest@gmail.com> \[contributor\]

- Ricardo Cartaxo <rcartaxoms@gmail.com> \[contributor\]
