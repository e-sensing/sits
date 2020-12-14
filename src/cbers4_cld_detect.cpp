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

    int nrows_data = data.nrow();
    int ncols_data = data.ncol();

    // number of rows and cols in the neighbourhood vector
    // adjust for borders
    int first_row = i - nrows_window/2;
    first_row = first_row > 0 ?  first_row : 0;
    int last_row  = i + nrows_window/2;
    last_row  = last_row < nrows_data ? last_row : nrows_data - 1;

    int first_col = j - ncols_window/2;
    first_col = first_col > 0 ?  first_col : 0;
    int last_col  = j + ncols_window/2;
    last_col  = last_col < ncols_data ? last_col : ncols_data - 1;

    // number of points in the neighborhood
    //
    int npts_vec = (last_row - first_row + 1)*(last_col - first_col + 1);

    //Rcout << "window " << first_row << first_col << last_row << last_col << std::endl;

    Rcpp::IntegerVector neigh(npts_vec);

    Rcpp::IntegerVector::iterator it = neigh.begin();

    int n_valid = 0;
    // iterate in the neighborhood and find all non_NA values
    for (int k = first_row; k <= last_row; ++k) {
        for (int l = first_col; l <= last_col; ++l) {
            if (data(k,l) != NA_INTEGER) {
                n_valid++;
                *it++ = data(k, l);
            }
        }
    }
    if (n_valid == 0) {
        Rcpp::IntegerVector valid_neigh(1);
        valid_neigh(0) = NA_INTEGER;
        return(valid_neigh);
    }
    else {
        Rcpp::IntegerVector valid_neigh(n_valid);
        Rcpp::IntegerVector::iterator it1 = neigh.begin();
        Rcpp::IntegerVector::iterator it2 = valid_neigh.begin();

        while (it2 != valid_neigh.end())
            *it2++ = *it1++;

        return (valid_neigh);
    }
}

// calculate the median value of a neighborhood accounting for NA
int median_neigh_i_j(const IntegerMatrix& data,
                         const int& nrows_window,
                         const int& ncols_window,
                         const int& i,
                         const int& j) {

    // return a vector of neighbours
    IntegerVector valid_neigh = neigh_i_j(data, nrows_window, ncols_window, i, j);

    // handle NA values
    if (valid_neigh(0) == NA_INTEGER)
        return (NA_INTEGER);
    else
        return (median(valid_neigh));

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
//
// [[Rcpp::export]]
IntegerMatrix median_neigh(const IntegerMatrix& data,
                           const int& nrows_window,
                           const int& ncols_window){

    IntegerMatrix new_data(data.nrow(), data.ncol());

    for (int i = 0; i < data.nrow(); i++){
        for (int j = 0; j < data.ncol(); j++) {
            new_data(i,j) = median_neigh_i_j(data, nrows_window, ncols_window, i, j);
        }
    }
    return(new_data);
}



// [[Rcpp::export]]
IntegerMatrix cbers4_cld_detect(const IntegerMatrix& b13, const IntegerMatrix& b14,
                                const IntegerMatrix& b15, const IntegerMatrix& b16,
                                const double& thres_1, const double& t2,
                                const double& t3, const double& t4,
                                const int& t5, const int& t6,
                                const IntegerVector& values) {

    int nrows = b13.nrow();
    int ncols = b13.ncol();

    IntegerMatrix cld_band(nrows, ncols);
    IntegerMatrix shd_band(nrows, ncols);
    // t1: (1,...,10) - default = 1
    // t2: (0.1,..,0.5) - default = 0.11
    // t3  (0.25,..,0.75) - default = 0.50
    // t4: (0.5,..,0.90) - default = 0.75
    // t5: (30,..,90) - default = 40
    // t6: (3,..,11) - default = 5

    //retrieve m1 and m2 from values
    int mean_mean_band = values(0);
    int max_mean_band = values(1);

    // calculate thres_2
    int thres_2 = mean_mean_band + t2 * (max_mean_band - mean_mean_band) ;

    //retrieve mean_b16 and min_b16 from values
    int mean_b16 = values(3);
    int min_b16  = values(4);

    // calculate thres_3
    int thres_3  = min_b16 + t3 * (mean_b16 - min_b16);

    // retrieve min_b13 and mean_b13 from values
    int mean_b13 = values(6);
    int min_b13  = values(7);

    // calculate thres_4
    int thres_4  = min_b13 + t4 * (mean_b13 - min_b13);

    // mean band
    IntegerMatrix mean_band(nrows, ncols);

    // skip all NA
    for (int i = 0; i < nrows; i++) {
        for (int j = 0; j < ncols; j++){
            if (b13(i,j) != NA_INTEGER && b14(i,j) != NA_INTEGER &&
                b15(i,j) != NA_INTEGER && b16(i,j) != NA_INTEGER) {
                mean_band(i,j) = (b13(i,j) + b14(i,j) + b15(i,j) + b16(i,j))/4;
            }
            else
                mean_band(i,j) = NA_INTEGER;
        }
    }

    // cloud and shadow estimate - step 1

    for (int i = 0; i < nrows; i++){
        for (int j = 0; j < ncols; j++){
            if (b13(i,j) != NA_INTEGER && b14(i,j) != NA_INTEGER
                    && b15(i,j) != NA_INTEGER && b16(i,j) != NA_INTEGER) {
                double cld_idx_1 = 3.0 * b16(i,j)/(b13(i,j) + b14(i,j) + b15(i,j));
                cld_band(i,j) = ((abs(cld_idx_1 - 1.0) < thres_1) || mean_band(i,j) > thres_2);
                shd_band(i,j) = (b16(i,j) < thres_3) && (b13(i,j) < thres_4);
            }
            else {
                cld_band(i,j) = NA_INTEGER;
                shd_band(i,j) = NA_INTEGER;
            }
        }
    }


    // Conduct post-processing with the median filter to remove
    // noise/outliers for the cloud band
    int nrows_median_window = t6;
    int ncols_median_window = t6;
    cld_band = median_neigh(cld_band, nrows_median_window,
                            ncols_median_window);

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

    // join shadow band and cloud band
    for (int i = 0; i < nrows; i++)
        for (int j = 0; j < ncols; j++)
            if (shd_band(i,j) == 1)
                cld_band(i,j) = 2;
    return cld_band;
}
// [[Rcpp::export]]
IntegerVector cbers4_cld_values(const IntegerMatrix& b13, const IntegerMatrix& b14,
                                const IntegerMatrix& b15, const IntegerMatrix& b16){

    //output
    IntegerVector values(9);

    // mean band
    int nrows = b13.nrow();
    int ncols = b13.ncol();
    IntegerMatrix mean_band(nrows, ncols);

    // skip all NA
    for (int i = 0; i < nrows; i++) {
        for (int j = 0; j < ncols; j++){
            if (b13(i,j) != NA_INTEGER && b14(i,j) != NA_INTEGER &&
                b15(i,j) != NA_INTEGER && b16(i,j) != NA_INTEGER) {
                mean_band(i,j) = (b13(i,j) + b14(i,j) + b15(i,j) + b16(i,j))/4;
            }
            else
                mean_band(i,j) = NA_INTEGER;
        }
    }
    // calculate mean band and its maximum
    double sum_mean = 0;
    int max_mean_band = 0;
    int n_valid_mean = 0;
    for (int i = 0; i < nrows; i++) {
        for (int j = 0; j < ncols; j++){
            if (mean_band(i,j) != NA_INTEGER) {
                n_valid_mean++;
                if (mean_band(i,j) > max_mean_band)
                    max_mean_band = mean_band(i,j);
                sum_mean += (double)mean_band(i,j);
            }
        }
    }
    // calculate the mean of the mean band
    int mean_mean_band = (int) std::round(sum_mean/n_valid_mean);
    // save the values
    values(0) = mean_mean_band;
    values(1) = max_mean_band;
    values(2) = n_valid_mean;

    // calculate the mean and minimum of b16
    int min_b16  = 32000;
    double sum_b16 = 0.;

    int n_valid_b16 = 0;
    for (int i = 0; i < nrows; i++) {
        for (int j = 0; j < ncols; j++){
            if (b16(i,j) != NA_INTEGER) {
                n_valid_b16++;
                if (b16(i,j) < min_b16)
                    min_b16 = b16(i,j);
                sum_b16 += b16(i,j);
            }
        }
    }
    // calculate the mean of band 16
    int mean_b16 = (int) std::round(sum_b16/n_valid_b16);

    // save the values
    values(3) = mean_b16;
    values(4) = min_b16;
    values(5) = n_valid_b16;

    int min_b13  = 32000;
    double sum_b13 = 0;
    int n_valid_b13 = 0;

    for (int i = 0; i < nrows; i++) {
        for (int j = 0; j < ncols; j++){
            if (b13(i,j) != NA_INTEGER) {
                n_valid_b13++;
                if (b13(i,j) < min_b13)
                    min_b13 = b13(i,j);
                sum_b13 += b13(i,j);
            }
        }
    }
    // calculate the mean of band 13
    int mean_b13 = (int) std::round(sum_b13/n_valid_b13);

    // save the values
    values(6) = mean_b13;
    values(7) = min_b13;
    values(8) = n_valid_b13;


    return(values);
}
