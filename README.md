sits
======

[![Build Status](https://travis-ci.org/rolfsimoes/sits.svg?branch=master)](https://travis-ci.org/rolfsimoes/) [![License](http://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html) 

### Satellite Image Time Series Analysis.
A set of tools for working with satellite image time series. Includes data retrieval from a WTSS (web time series service), different visualisation methods for image time series, smoothing methods for noisy time series, different clustering methods, including dendrograms and SOM. Matches noiseless patterns with noisy time series using the TWDTW method for shape recognition and provides machine learning methods for time series classification, including SVM, LDA, QDA, GLM, Lasso, Random Forests and Deep Learning.


###### For users
Install the sits package from github suing the following code:
```R
devtools::install_github("e-sensing/sits")
```

###### For developers
In order to setup a development environment for the sits package you need first to resolve its  dependencies and then clone its source code. The easiest way to resolve the dependencies is to install the sits from github (see above). 

Once the dependencies are resolved, the source code can be forked, cloned, and built. For example:

```bash
git clone https://github.com/e-sensing/sits.git
R CMD build sits
```
