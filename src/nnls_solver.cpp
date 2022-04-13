//[[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;
using namespace std;
//using namespace arma; //included for simple mat. multiplication

// The original implementation is available at
// https://github.com/bleutner/RStoolbox/blob/master/src/nnls_solver.cpp

//[[Rcpp::export]]
arma::mat nnls_solver(const arma::mat x,
                      const arma::mat A,
                      const int iterate = 400,
                      const float tolerance = 0.000001) {

    // pode ser resolvido no R
    // if( A_nbands != b_nbands) { // catch false inputs
    //     stop("A and b do not have equal column lengths.");
    // }

    int A_nEM = A.n_rows;
    int b_npix = x.n_rows;

    // adiciona mais uma coluna porque é adicionado uma coluna de probabilidade
    arma::mat sol(b_npix, A_nEM+1);

    for (int i = 0; i < b_npix; i++) {

        arma::vec xv(A_nEM, arma::fill::zeros);
        arma::vec xstore(A_nEM, arma::fill::value(-9999));

        // non-negative vector
        arma::vec xdiff = xv - xstore;

        // switching to arma here for nice matrix multiplication
        arma::vec nab = -A * x.row(i).t(); // negative A * b
        arma::mat ata = A * A.t(); // A * transposed A

        double temporary;
        int j = 0;

        //execute solving loop
        while(j < iterate && max(abs(xdiff)) > tolerance) {
            xstore = xv;

            for (int k = 0; k < A_nEM; k++) {

                temporary = xv[k] - nab[k] / ata(k,k);
                if (temporary < 0){
                    temporary = 0;
                }

                if (temporary != xv[k]){
                    nab += ((temporary - xv[k]) * ata.row(k).t());
                }

                xv[k] = temporary;
            }
            xdiff = xv-xstore;
            ++j;
        }

        //predict values
        arma::mat prob = xv.t();
        arma::mat pred = prob * A;

        //calculate RMSE
        arma::mat ppdiff = pred.row(0) - x.row(i);
        float rmsem = mean(mean(pow(ppdiff, 2)));
        float rmse = sqrt(rmsem);

        arma::mat ret(1, (A_nEM+1));

        for(int f = 0; f < A_nEM; f++) {
            ret(0,f) = prob(0,f);
        }

        //fill
        ret(0,A_nEM) = rmse;
        sol.row(i) = ret; //xv.t();
    }
    return(sol); //mat
}
