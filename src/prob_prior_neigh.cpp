#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]

double prob_prior_neigh(const NumericVector& probs) {
    return mean(probs);
}

/*** R
prob_prior_neigh(c(1, 2, 3, 4))
*/
