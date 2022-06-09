//[[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;
using namespace std;
//using namespace arma; //included for simple mat. multiplication

// This function was implemented by Jakob Schwalb-Willmann in the
// RStoolbox package (GPL>=3).
// The source code can be found in
// https://github.com/bleutner/RStoolbox/tree/v0.3.0

//[[Rcpp::export]]
arma::mat nnls_solver(const arma::mat x,
                      const arma::mat A,
                      const int iterate = 400,
                      const float tolerance = 0.000001) {

    int A_nEM = A.n_rows;
    int b_npix = x.n_rows;
    arma::mat sol(b_npix, A_nEM+1);

    for(int i = 0; i < b_npix; i++){ // parallelization with clusterR possible with this framework? --> test

        arma::vec xv(A_nEM), xstore(A_nEM);
        xv.fill(0);
        xstore.fill(-9999);

        // create a non-negative vector
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
