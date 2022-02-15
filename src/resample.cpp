#include <Rcpp.h>
#include <utility>
#include <vector>
using namespace Rcpp;

std::vector< std::pair<int,int>> reverse(const int& i_out,
                                         const int& j_out,
                                         const int& ratio_in_out){


    std::vector< std::pair<int,int> > points_in;
    std::pair<int,int> point_in(0,0);

    for (int i = 0; i < ratio_in_out; i++){
        for (int j = 0; j < ratio_in_out; j++){
            point_in.first = ratio_in_out * i_out + i;
            point_in.second = ratio_in_out * j_out + j;
            points_in.push_back(point_in);
        }
    }
    return (points_in);
}

// [[Rcpp::export]]
IntegerMatrix reg_resample(const IntegerMatrix& band,
                           const IntegerMatrix& cloud,
                           const int& ratio_band_out,
                           const int& ratio_cloud_out,
                           const int& nrows_out,
                           const int& ncols_out,
                           IntegerVector& cloud_values,
                           const int& missing_value) {

    IntegerMatrix  band_out(nrows_out, ncols_out);
    band_out.fill(0);

    std::vector< std::pair<int,int>> points_band(ratio_band_out*ratio_band_out);
    std::vector< std::pair<int,int>> points_cloud(ratio_cloud_out*ratio_cloud_out);
    std::vector< std::pair<int,int>>::iterator points_iter;

    for (int i = 0; i < nrows_out; i++){
        for (int j = 0; j < ncols_out; j++){
            points_band = reverse(i, j, ratio_band_out);
            points_cloud = reverse(i, j, ratio_cloud_out);
            std::vector< std::pair<int,int>>::iterator cloud_iter = points_cloud.begin();

            std::cout << points_cloud.size() << std::endl;

             while(cloud_iter++ !=  points_cloud.end()){

                //std::cout << cloud_iter->first << " " << cloud_iter->second << std::endl;

                int cloud_value = cloud(cloud_iter->first, cloud_iter->second);
                IntegerVector::iterator f = std::find(cloud_values.begin(),cloud_values.end(),
                                                      cloud_value);
                if (f != cloud_values.end()){
                    band_out(i,j) = missing_value;
                    break;
                }
            }
            if (band_out(i,j) != missing_value) {
                int band_sum = 0;
                int num_band = 0;
                std::vector< std::pair<int,int>>::iterator band_iter = points_band.begin();
                while(band_iter++ !=  points_band.end()){
                    double band_val = band(band_iter->first, band_iter->second);
                    if (band_val != missing_value) {
                        band_sum += band_val;
                        num_band++;
                    }
                }
                band_out(i,j) = (int)(band_sum/num_band);
            }
        }
    }
    return band_out;
}
// [[Rcpp::export]]
IntegerMatrix reg_merge(const List& bands, const int& nrows, const int& ncols){
    int num_bands = bands.length();

    IntegerMatrix band_out (nrows, ncols);
    band_out.fill(NA_INTEGER);

    if (num_bands == 0)
        return band_out;

    for (int i = 0; i < nrows; i++){
        for (int j = 0; j < ncols; j++){
            for (int k = 0; k < num_bands; k++){
                if (band_out(i,j) != NA_INTEGER) break;
                IntegerVector band_k = bands[k];
                band_out(i,j) = band_k(i,j);
            }
        }
    }
    return band_out;
}

