#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
SEXP C_as_array_inplace(SEXP x, IntegerVector dim) {
    if (dim.size() != 3) {
        stop("dim must have length 3");
    }

    R_xlen_t n = 1;
    for (int i = 0; i < dim.size(); ++i) {
        n *= dim[i];
    }

    if (XLENGTH(x) != n) {
        stop("product(dim) must match object length");
    }

    IntegerVector dim_copy = clone(dim);

    Rf_setAttrib(x, R_NamesSymbol, R_NilValue);
    Rf_setAttrib(x, R_DimNamesSymbol, R_NilValue);
    Rf_setAttrib(x, R_DimSymbol, dim_copy);

    return x;
}
