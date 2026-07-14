#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix C_normalize_data_inplace(NumericMatrix data,
                                       NumericVector min,
                                       NumericVector max) {
    const R_xlen_t nrow = data.nrow();
    const R_xlen_t ncol = data.ncol();

    if (ncol != min.size() || ncol != max.size()) {
        return data;
    }

    double* x = REAL(data);

    for (R_xlen_t j = 0; j < ncol; ++j) {
        const double mn = min[j];
        const double mx = max[j];
        const double denom = mx - mn;
        const R_xlen_t offset = j * nrow;

        if (denom == 0.0) {
            for (R_xlen_t i = 0; i < nrow; ++i) {
                const R_xlen_t k = offset + i;
                const double v = x[k];

                if (ISNAN(v)) {
                    continue;
                } else if (ISNAN(mn)) {
                    x[k] = R_NaN;
                } else if (v > mn) {
                    x[k] = 1.0;
                } else if (v < mn) {
                    x[k] = 0.0001;
                } else {
                    x[k] = R_NaN;
                }
            }
            continue;
        }

        const double inv_denom = 1.0 / denom;

        for (R_xlen_t i = 0; i < nrow; ++i) {
            const R_xlen_t k = offset + i;
            double v = x[k];

            if (ISNAN(v)) {
                continue;
            }

            if (ISNAN(mn) || ISNAN(mx)) {
                x[k] = R_NaN;
                continue;
            }

            v = (v - mn) * inv_denom;

            if (v < 0.0001) {
                v = 0.0001;
            } else if (v > 1.0) {
                v = 1.0;
            }

            x[k] = v;
        }
    }

    return data;
}
