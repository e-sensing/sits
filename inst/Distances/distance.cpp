#include <Rcpp.h>

typedef double (*DistanceFunctionPtr)(double *, double *, int, int);

double EuclideanDTW(double x, double y)
{
    return std::sqrt(std::pow ((x-y),2));
}

double TimeSeriesDTW(double *data, double *codes, int ndata, int nCodes){

    std::vector<std::vector<double > > cost(ndata, std::vector<double>(ndata));
    cost[0][0]= EuclideanDTW(data[0],codes[0]);

    for (int i=1; i<ndata; ++i)
    {
        cost[i][0]=cost[i-1][0] + EuclideanDTW(data[i],codes[0]);
    }

    for (int j=1; j<ndata; ++j)
    {
        cost[0][j]=cost[0][j-1] + EuclideanDTW(data[0],codes[j]);
    }

    // Fill the matrix:
    for (int i = 1; i < ndata; i++)
    {
        for (int j = 1; j < ndata; j++)
        {
            cost[i][j] = std::min(cost[i - 1][j], std::min(cost[i][j - 1], cost[i - 1][j - 1])) + EuclideanDTW(data[i], codes[j]);
        }
    }
    return cost[ndata-1][ndata-1];
}

// [[Rcpp::export]]
Rcpp::XPtr<DistanceFunctionPtr> dtw() {
    return Rcpp::XPtr<DistanceFunctionPtr>(new DistanceFunctionPtr(&TimeSeriesDTW));
}
