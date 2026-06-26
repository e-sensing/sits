#include <vector>
#include <cmath>
#include <algorithm>

#include "./dtw2vec.h"

/*
 * Port of the `dtw2vec` algorithm from the `IncDTW` R package
 * (function `cpp_dtw2vec`, step pattern `symmetric2`).
 *
 * Upstream: https://github.com/cran/IncDTW  (src/dtw2vec.cpp, src/GCM.cpp)
 * Author:   Maximilian Leodolter
 * License:  GPL-2 | GPL-3
 *
 * The recurrence is preserved exactly; only the I/O signature is adapted to use
 * plain pointers (instead of `arma::vec`) so it carries no extra dependencies
 * and allocates nothing beyond the two rolling column buffers.
 *
 * `symmetric2` step (from IncDTW `GCM.cpp`, `gcm_step_symm2`)
 */

namespace incdtw {

double dtw2vec(const double *x, int nx, const double *y, int ny)
{
    std::vector<double> col_prev(nx);  // previous column (j - 1)
    std::vector<double> col_curr(nx);  // current  column (j)

    // first column (j = 0): cumulative cost against y[0]
    col_prev[0] = std::fabs(x[0] - y[0]);
    for (int i = 1; i < nx; ++i) {
        col_prev[i] = std::fabs(x[i] - y[0]) + col_prev[i - 1];
    }

    for (int j = 1; j < ny; ++j) {
        // first row (i = 0): cumulative cost against x[0]
        col_curr[0] = std::fabs(x[0] - y[j]) + col_prev[0];

        for (int i = 1; i < nx; ++i) {
            const double cm00 = std::fabs(x[i] - y[j]);
            const double diag = col_prev[i - 1];
            const double left = col_prev[i];
            const double up   = col_curr[i - 1];

            col_curr[i] = std::min({
                2.0 * cm00 + diag,
                cm00 + left,
                cm00 + up
            });
        }

        col_prev.swap(col_curr);
    }

    return col_prev[nx - 1];
}

}
