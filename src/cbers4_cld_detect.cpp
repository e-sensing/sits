#include <Rcpp.h>
#include <stdio.h>
using namespace Rcpp;

// This function preprocesses the raster brick data, by removing missing values and those
// smaller than the minimum value

IntegerVector build_neigh(const IntegerMatrix& data,
                          const int& nrows,
                          const int& ncols,
                          const int& i,
                          const int& j) {

    IntegerVector neigh;

    IntegerMatrix window(nrows, ncols);

    window = 1;

    for (int k = 0; k < nrows; ++k) {
        for (int l = 0; l < ncols; ++l) {
            int data_i = i + k - nrows / 2, data_j = j + l - ncols / 2;

            if (data_i >= 0 && data_j >= 0 &&
                data_i < data.nrow() && data_j < data.ncol() && window(k, l) > 0 &&
                !std::isnan(data(data_i, data_j))) {
                neigh.push_back(data(data_i, data_j) * window(k, l));
            }
        }
    }

    return neigh;
}

IntegerMatrix median_neigh(const IntegerMatrix& data,
                           const int& nrows,
                           const int& ncols){

    IntegerMatrix median_data(nrows, ncols);

    for (int i = 0; i < data.nrow(); i++){
        for (int j = 0; j < data.ncol(); j++) {
            IntegerVector neigh = build_neigh(data, nrows, ncols, i, j);
            Rcout << "neigh is " << neigh << std::endl;
            median_data(i,j) = median(neigh);
        }
    }
    return(median_data);
}

// [[Rcpp::export]]
IntegerMatrix cbers4_cld_detect(const IntegerMatrix& b13, const IntegerMatrix& b14,
                                const IntegerMatrix& b15, const IntegerMatrix& b16,
                                const double& thres_1 = 1, const double& t2 = 0.125,
                                const double& t3 = 0.66, const double& t4 = 0.80,
                                const int& t5 = 40, const int& t6 = 5) {

    int nrows = b13.nrow();
    int ncols = b13.ncol();

    IntegerMatrix cld_band(nrows, ncols);
    IntegerMatrix shd_band(nrows, ncols);

    // calculate the mean band
    NumericMatrix mean_band(nrows, ncols);

    for (int i = 0; i < nrows; i++)
        for (int j = 0; j < ncols; j++)
            mean_band(i,j) = (b13(i,j) + b14(i,j) + b15(i,j) + b16(i,j))/4;



    // calculate the mean of the mean band
    int m1 = mean(mean_band);

    // calculate thres_2
    int thres_2 = m1 + t2 * (max(mean_band) - m1) ;
    Rcout << "Thres_2 is " << thres_2 << std::endl;

    // calculate thres_3
    int min_b16  = min(b16);
    int mean_b16 = mean(b16);
    int thres_3  = min_b16 + t3 * (mean_b16 - min_b16);

    Rcout << "Thres_3 is " << thres_3 << std::endl;

    // calculate thres_4
    int min_b13  = min(b13);
    int mean_b13 = mean(b13);
    int thres_4  = min_b13 + t4 * (mean_b13 - min_b13);

    Rcout << "Thres_4 is " << thres_4 << std::endl;

    for (int i = 0; i < nrows; i++){
        for (int j = 0; j < ncols; j++){
            double cld_idx_1 = 3 * b16(i,j)/(b13(i,j) + b14(i,j) + b15(i,j));
            cld_band(i,j) = ((abs(cld_idx_1 - 1.0) < thres_1) || mean_band(i,j) > thres_2);
            shd_band(i,j) = (b16(i,j) < thres_3) && (b13(i,j) < thres_4);
        }
    }


    // Conduct post-processing with the median filter to remove
    // noise/outliers for the cloud band
    int nrows_median_window = t6;
    int ncols_median_window = t6;
    cld_band = median_neigh(cld_band, nrows_median_window, ncols_median_window);

    // Refine the cloud shadows with spatial matching with the
    // help of the cloud detection result;
    int nrows_shd_window = t5;
    int ncols_shd_window = t5;

    for (int i = 0; i < nrows; i++) {
        for (int j = 0; j < ncols; j++){
            if (shd_band(i,j) == 1) {
                // assume there are no shadows.
                shd_band(i,j) = 0;
                // build a large neighborhood to find if there are clouds there
                IntegerVector neigh = build_neigh(cld_band, nrows_shd_window,
                                                  ncols_shd_window, i, j);
                // are there any clouds?
                // if so, then assume there are shadows
                IntegerVector::iterator it = neigh.begin();
                while (it != neigh.end()){
                    if (*it++ == 1){
                        shd_band(i,j) = 1;
                        break;
                    }
                }
            }
        }
    }
    // filter the shadow band by the median
    shd_band = median_neigh(cld_band, nrows_median_window, ncols_median_window);

    for (int i = 0; i < nrows; i++)
        for (int j = 0; j < ncols; j++)
            if (shd_band(i,j) == 1)
                cld_band(i,j) = 2;



    return cld_band;
}
