#include <Rcpp.h>
#include <utility>
#include <vector>
#include <algorithm>
using namespace Rcpp;

std::vector< std::pair<int,int>> reverse(const int& i_out,
                                         const int& j_out,
                                         const double& ratio_in_out) {

    std::vector< std::pair<int,int> > points_in;
    std::pair<int,int> point_in;

    for (int i = 0; i < ratio_in_out; i++) {
        for (int j = 0; j < ratio_in_out; j++) {
            point_in = {(int)(ratio_in_out * i_out + i),
                        (int)(ratio_in_out * j_out + j)};
            points_in.push_back(point_in);
        }
    }
    return (points_in);
}

// [[Rcpp::export]]
IntegerMatrix reg_resample(const IntegerMatrix& band,
                           const IntegerMatrix& cloud,
                           const double& ratio_band_out,
                           const double& ratio_cloud_out,
                           const int& nrows_out,
                           const int& ncols_out,
                           IntegerVector& cloud_interp) {

    // output matrix
    IntegerMatrix band_out(nrows_out, ncols_out);
    band_out.fill(0);

    // a pair vector with corresponding points between two matrices
    std::vector< std::pair<int,int>> points_band(std::max(1, (int)(ratio_band_out*ratio_band_out)));
    std::vector< std::pair<int,int>> points_cloud(std::max(1, (int)(ratio_cloud_out*ratio_cloud_out)));
    std::vector< std::pair<int,int>>::iterator points_iter;

    // loop in out matrix
    for (int i = 0; i < nrows_out; i++) {
        for (int j = 0; j < ncols_out; j++) {

            // for each point of output matrix get the corresponding point in
            // input matrix
            points_band = reverse(i, j, ratio_band_out);
            points_cloud = reverse(i, j, ratio_cloud_out);
            std::vector< std::pair<int,int>>::iterator cloud_iter = points_cloud.begin();

            // mark uncleaned pixels with missing value (NA_INTEGER)
            while(cloud_iter !=  points_cloud.end()) {

                int cloud_value = cloud(cloud_iter->first, cloud_iter->second);

                IntegerVector::iterator f = std::find(cloud_interp.begin(),
                                                      cloud_interp.end(),
                                                      cloud_value);
                if (f != cloud_interp.end()){

                    band_out(i,j) = NA_INTEGER;
                    break;
                }
                cloud_iter++;
            }

            // for each valid value in band_out apply the bilinear method
            // in band_values
            if (band_out(i,j) != NA_INTEGER) {
                int band_sum = 0;
                int num_band = 0;
                std::vector< std::pair<int,int>>::iterator band_iter = points_band.begin();
                while(band_iter !=  points_band.end()) {
                    double band_val = band(band_iter->first, band_iter->second);

                    if (band_val != NA_INTEGER) {
                        band_sum += band_val;
                        num_band++;
                    }
                    band_iter++;
                }

                // equivalent to bilinear method
                band_out(i,j) = (int)(band_sum/num_band);
            }
        }
    }

    return band_out;
}

// [[Rcpp::export]]
IntegerMatrix reg_agg_first(const IntegerMatrix& band_dates){

    int num_bands = band_dates.ncol();

    IntegerMatrix band_out(band_dates.nrow() * band_dates.ncol(), 1);
    band_out.fill(NA_INTEGER);

    if (num_bands == 0)
        return band_out;

    // equivalent to first method
    for (int i = 0; i < band_out.nrow(); i++) {
        for (int k = 0; k < num_bands; k++) {
            if (band_dates(i, k) != NA_INTEGER) {
                band_out(i, 0) = band_dates(i, k);
                break;
            }
        }
    }
    return band_out;
}
