// Apply transitions rules to raster assuming Class(t) = f(Class(t-1), Class(t))
// subject to the initial condition Class(t=1) = Class(t=1)
// Author Victor Maus, \email{vwmaus1@@gmail.com}
// June, 2017

#include <Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export()]]
Rcpp::NumericMatrix apply_first_order_transition_rules(Rcpp::NumericMatrix v,
                                                       Rcpp::NumericMatrix A,
                                                       Rcpp::NumericVector I){

     int nrow = I.size(), ncol = v.ncol(), ii, jj;

     for (int i = 0; i < nrow; i++)
     {
          for (int j = 1; j < ncol; j++)
          {
               ii = (int) v(I(i)-1, j  );
               jj = (int) v(I(i)-1, j-1);
               v(I(i)-1, j) = A(ii, jj);
          }
     }

     return v;
}

