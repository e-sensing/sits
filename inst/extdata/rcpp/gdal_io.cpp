#include <Rcpp.h>
#include <gdal.h>
#include <gdal_alg.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericVector timesTwo(NumericVector x) {

    DatasetH RGDAL_Open(const char* file,
                        bool readonly = true,
                        bool shared = true)
    {
        GDALAccess access = readonly ? GA_ReadOnly : GA_Update;
        return shared ? GDALOpenShared(file, access) : GDALOpen(file, access);
    }

    err = GDALRasterIO(*hRB, GF_Write,
                       nDSXOff, nDSYOff,
                       nDSXSize, nDSYSize,
                       buf, nc, nr, GDT_Int32,
                       0, 0);
  return x * 2;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
timesTwo(42)
*/
