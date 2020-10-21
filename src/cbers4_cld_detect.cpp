#include <Rcpp.h>
#include <stdio.h>
using namespace Rcpp;

// This function finds a vector of neighbours from a window of size
// (nrows_window x ncols_window) of pixel (i,j)
IntegerVector neigh_i_j(const IntegerMatrix& data,
                        const int& nrows_window,
                        const int& ncols_window,
                        const int& i,
                        const int& j) {

    // number of rows and cols in the matrix
    int data_nrows = data.nrow();
    int data_ncols = data.ncol();

    // number of rows and cols in the neighbourhood vector
    // adjust for borders
    int first_row = i - (nrows_window + 1)/2 + 1;
    first_row = first_row > 0 ?  first_row : 0;
    int last_row  = i + (nrows_window + 1)/2 - 1;
    last_row  = last_row < data_nrows ? last_row : data_nrows - 1;

    int first_col = j - (ncols_window + 1)/2 + 1;
    first_col = first_col > 0 ?  first_col : 0;
    int last_col  = j + (ncols_window + 1)/2 - 1;
    last_col  = last_col < data_ncols ? last_col : data_ncols - 1;

    // number of points in the neighborhood
    //
    int npts_vec = (last_row - first_row + 1)*(last_col - first_col + 1);

    //Rcout << "window " << first_row << first_col << last_row << last_col << std::endl;

    IntegerVector neigh(npts_vec);

    IntegerVector::iterator it = neigh.begin();

    for (int k = first_row; k <= last_row; ++k) {
        for (int l = first_col; l <= last_col; ++l) {
            *it++ = data(k, l);
        }
    }

    return neigh;
}
// find the median value of the neighbours of a matrix
// uses a window of size (nrows_window x ncols_window)
IntegerMatrix median_neigh(const IntegerMatrix& data,
                           const int& nrows_window,
                           const int& ncols_window){

    IntegerMatrix new_data(data.nrow(), data.ncol());

    for (int i = 0; i < data.nrow(); i++){
        for (int j = 0; j < data.ncol(); j++) {
            new_data(i,j) = median(neigh_i_j(data, nrows_window, ncols_window, i, j));
        }
    }
    return(new_data);
}

bool cld_shd_neigh(IntegerMatrix& data,
                   const int& nrows_window,
                   const int& ncols_window,
                   const int& i,
                   const int& j){

    IntegerVector neigh = neigh_i_j(data, nrows_window, ncols_window, i, j);

    bool has_cld_neigh = is_true(any(neigh == 1));
    return(has_cld_neigh);
}

// [[Rcpp::export]]
IntegerMatrix cbers4_cld_detect(const IntegerMatrix& b13, const IntegerMatrix& b14,
                                const IntegerMatrix& b15, const IntegerMatrix& b16,
                                const double& thres_1, const double& t2,
                                const double& t3, const double& t4,
                                const int& t5, const int& t6) {

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
    if (0){
        Rcout << "cloud band before" << std::endl;
        for (int i = 1000; i < 1050; i++){
            for (int j = 1000; j < 1050; j++) {
                Rcout << cld_band(i,j);
            }
            Rcout << std::endl;
        }

        Rcout << "shadow band before" << std::endl;
        for (int i = 1000; i < 1050; i++){
            for (int j = 1000; j < 1050; j++) {
                Rcout << shd_band(i,j);
            }
            Rcout << std::endl;
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
                    bool has_cld_neigh = cld_shd_neigh(cld_band, nrows_shd_window,
                                                       ncols_shd_window, i, j);
                    if (!has_cld_neigh)
                        shd_band(i,j) = 0;
                }
            }
        }
        // filter the shadow band by the median
        shd_band = median_neigh(shd_band, nrows_median_window, ncols_median_window);

                Rcout << "cloud band" << std::endl;
                for (int i = 1000; i < 1050; i++){
                    for (int j = 1000; j < 1050; j++) {
                        Rcout << cld_band(i,j);
                    }
                    Rcout << std::endl;
                }

    }
    // join shadow band and cloud band
    for (int i = 0; i < nrows; i++)
        for (int j = 0; j < ncols; j++)
            if (shd_band(i,j) == 1)
                cld_band(i,j) = 2;
    return cld_band;
}
