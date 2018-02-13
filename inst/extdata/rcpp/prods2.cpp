// [[Rcpp::depends(RcppArmadillo, RcppEigen, bigmemory, BH)]]
#include <RcppArmadillo.h>
#include <RcppEigen.h>
#include <bigmemory/MatrixAccessor.hpp>

using namespace Rcpp;


/******************************************************************************/

typedef Eigen::Matrix<char, Eigen::Dynamic, Eigen::Dynamic> MatrixXchar;

// [[Rcpp::export]]
Eigen::VectorXd prodEigen(XPtr<BigMatrix> bMPtr, 
                          const Eigen::Map<Eigen::VectorXd> x) {
  
  Eigen::Map<MatrixXchar> bM((char *)bMPtr->matrix(), 
                             bMPtr->nrow(), bMPtr->ncol());
  
  return bM.cast<double>() * x;
}

/******************************************************************************/

// [[Rcpp::export]]
arma::vec prodArma(XPtr<BigMatrix> xpA, const arma::vec& x) {
  
  arma::Mat<char> Am((char*) xpA->matrix(), xpA->nrow(), xpA->ncol(), false);
  
  return Am * x;
}

/******************************************************************************/

// [[Rcpp::export]]
arma::vec prodArmaSub(XPtr<BigMatrix> xpA, const arma::vec& x,
                      const arma::Row<uint32_t>& ind) {
  arma::Mat<char> Am((char *) xpA->matrix(), xpA->nrow(), xpA->ncol(), false);
  
  return Am.rows(ind) * x;
}

/******************************************************************************/
