#include <Rcpp.h>
using namespace Rcpp;

NumericVector build_inside_neigh(const NumericMatrix& data,
                                 const int& i, const int& j) {
    NumericMatrix neigh(3, 3);

    for (int k = 0; k < 3; ++k) {
        for (int l = 0; l < 3; ++l) {
            neigh(k, l) = data(i + k - 1, j + l - 1);
        }
    }

    return wrap(neigh);
}

NumericVector build_vborder_neigh(const NumericMatrix& data,
                                  const int& i, const int& j) {
    int nrows = data.nrow();
    int ncols = data.ncol();
    NumericMatrix neigh(3, 2);

    // check left border
    if (j == 0) {
        for (int k = 0; k < 3; ++k) {
            for (int l = 0; l < 2; ++l) {
                neigh(k, l) = data(i + k - 1, j + l);
            }
        }
    // check right border
    } else {
        for (int k = 0; k < 3; ++k) {
            for (int l = 0; l < 2; ++l) {
                neigh(k, l) = data(i + k - 1, j + l - 1);
            }
        }
    }

    return wrap(neigh);
}

NumericVector build_hborder_neigh(const NumericMatrix& data,
                                  const int& i, const int& j) {
    int nrows = data.nrow();
    int ncols = data.ncol();
    NumericMatrix neigh(2, 3);

    // check upper border
    if (i == 0) {
        for (int k = 0; k < 2; ++k) {
            for (int l = 0; l < 3; ++l) {
                neigh(k, l) = data(i + k, j + l - 1);
            }
        }
    // check lower border
    } else {
        for (int k = 0; k < 2; ++k) {
            for (int l = 0; l < 3; ++l) {
                neigh(k, l) = data(i + k - 1, j + l - 1);
            }
        }
    }

    return wrap(neigh);
}

NumericVector build_corner_neigh(const NumericMatrix& data,
                                 const int& i, const int& j) {
    int nrows = data.nrow();
    int ncols = data.ncol();
    NumericMatrix neigh(2, 2);

    // check upper-left corner
    if (i == 0 && j == 0) {
        for (int k = 0; k < 2; ++k) {
            for (int l = 0; l < 2; ++l) {
                neigh(k, l) = data(i + k, j + l);
            }
        }
    // check lower-left
    } else if (i == nrows - 1 && j == 0) {
        for (int k = 0; k < 2; ++k) {
            for (int l = 0; l < 2; ++l) {
                neigh(k, l) = data(i + k - 1, j + l);
            }
        }
    // check upper-right
    } else if (i == 0 && j == ncols - 1) {
        for (int k = 0; k < 2; ++k) {
            for (int l = 0; l < 2; ++l) {
                neigh(k, l) = data(i + k, j + l - 1);
            }
        }
    // check lower-right
    } else if (i == nrows - 1 && j == ncols - 1) {
        for (int k = 0; k < 2; ++k) {
            for (int l = 0; l < 2; ++l) {
                neigh(k, l) = data(i + k - 1, j + l - 1);
            }
        }
    }

    return wrap(neigh);
}

double smooth_estimator_pixel(const NumericVector& neigh,
                              const double& p,
                              const double& noise) {

    NumericVector log_neigh(neigh);
    log_neigh = log(neigh / (10000 - neigh));
    double x = log(p / (10000 - p));
    double v = var(log_neigh);
    double w1 = v / (noise + v);
    double w2 = noise / (noise + v);
    double sx = w1 * x + w2 * mean(log_neigh);

    return sx;
}

// [[Rcpp::export]]
NumericVector smooth_estimator_class(const NumericMatrix& data,
                                     const double& noise) {

    int nrows = data.nrow();
    int ncols = data.ncol();

    NumericVector result(nrows * ncols);
    NumericVector neigh;

    int k = 0;
    for (int i = 0; i < nrows; ++i) {
        for (int j = 0; j < ncols; ++j) {
            if (i > 0 && i < nrows - 1 && j > 0 && j < ncols - 1) {
                neigh = build_inside_neigh(data, i, j);
            } else if ((i == 0 && j == 0) ||
                       (i == nrows - 1 && j == 0) ||
                       (i == 0 && j == ncols - 1) ||
                       (i == nrows - 1 && j == ncols - 1)) {
                neigh = build_corner_neigh(data, i, j);
            } else if (i == 0 || i == nrows - 1) {
                neigh = build_hborder_neigh(data, i, j);
            } else if (j == 0 || j == ncols - 1) {
                neigh = build_vborder_neigh(data, i, j);
            }
            result(k++) = smooth_estimator_pixel(neigh, data(i, j), noise);
        }
    }

    return result;
}
