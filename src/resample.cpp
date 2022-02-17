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


IntegerMatrix reg_mask_band(const IntegerMatrix& band,
                            const IntegerMatrix& cloud,
                            const double& ratio_band_out,
                            const double& ratio_cloud_out,
                            const int& nrows_out,
                            const int& ncols_out,
                            IntegerVector& cloud_values,
                            const int& missing_value) {

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

            while(cloud_iter !=  points_cloud.end()) {

                int cloud_value = cloud(cloud_iter->first, cloud_iter->second);

                IntegerVector::iterator f = std::find(cloud_values.begin(),
                                                      cloud_values.end(),
                                                      cloud_value);
                if (f != cloud_values.end()){

                    band_out(i,j) = missing_value;
                    break;
                }
                cloud_iter++;
            }
        }
    }
    return band_out;
}


// [[Rcpp::export]]
IntegerMatrix reg_resample(const IntegerMatrix& band,
                           const IntegerMatrix& cloud,
                           const double& ratio_band_out,
                           const double& ratio_cloud_out,
                           const int& nrows_out,
                           const int& ncols_out,
                           IntegerVector& cloud_interp,
                           const int& missing_value) {

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

            // mark uncleaned pixels with missing value
            while(cloud_iter !=  points_cloud.end()) {

                int cloud_value = cloud(cloud_iter->first, cloud_iter->second);

                IntegerVector::iterator f = std::find(cloud_interp.begin(),
                                                      cloud_interp.end(),
                                                      cloud_value);
                if (f != cloud_interp.end()){

                    band_out(i,j) = missing_value;
                    break;
                }
                cloud_iter++;
            }

            // for each valid value in band_out apply the bilinear method
            // in band_values
            if (band_out(i,j) != missing_value) {
                int band_sum = 0;
                int num_band = 0;
                std::vector< std::pair<int,int>>::iterator band_iter = points_band.begin();
                while(band_iter !=  points_band.end()) {
                    double band_val = band(band_iter->first, band_iter->second);

                    if (band_val != missing_value) {
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
IntegerMatrix reg_merge_first(const List& band_block_dates,
                              const int& nrows,
                              const int& ncols,
                              const int& missing_value){
    int num_bands = band_block_dates.length();

    IntegerMatrix band_out (nrows, ncols);
    band_out.fill(missing_value);

    if (num_bands == 0)
        return band_out;

    // equivalent to first method
    for (int i = 0; i < nrows; i++) {
        for (int j = 0; j < ncols; j++) {
            for (int k = 0; k < num_bands; k++) {
                if (band_out(i,j) != missing_value) break;
                IntegerMatrix band_k = band_block_dates[k];
                band_out(i,j) = band_k(i,j);
            }
        }
    }
    return band_out;
}

// [[Rcpp::export]]
IntegerMatrix compose_first(const List& band_block_dates,
                            const List& cloud_block_dates,
                            IntegerVector& cloud_values,
                            const double& ratio_band_out,
                            const double& ratio_cloud_out,
                            const int& nrows_out,
                            const int& ncols_out,
                            const int& missing_value){

    int num_bands = band_block_dates.length();
    List bands_resampled;

    for (int k = 0; k < num_bands; k++) {
        IntegerMatrix band_k = band_block_dates[k];
        IntegerMatrix cloud_k = cloud_block_dates[k];

        IntegerMatrix band_k_out = reg_resample(band_k,
                                                cloud_k,
                                                ratio_band_out,
                                                ratio_cloud_out,
                                                nrows_out,
                                                ncols_out,
                                                cloud_values,
                                                missing_value);
        bands_resampled.push_back(band_k_out);
    }
    IntegerMatrix band_out = reg_merge_first(bands_resampled,
                                             nrows_out,
                                             ncols_out,
                                             missing_value);
    return band_out;
}
