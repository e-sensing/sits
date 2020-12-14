#include <Rcpp.h>
#include <stdio.h>
#include <algorithm>
using namespace Rcpp;

// Adapted from Toby Speight's post below
// https://codereview.stackexchange.com/questions/208526/linearly-interpolate-na-values-in-rcppIntegervector
//
//

bool detect_start_na(const int& a, const int& b){
    return (!Rcpp::IntegerVector::is_na(a) && Rcpp::IntegerVector::is_na(b));
}

bool detect_end_na(const int& a, const int& b){
    return (Rcpp::IntegerVector::is_na(a) && !Rcpp::IntegerVector::is_na(b));
}

int na_interp(int& first_val, int& next_val, const int& gaps, const int& i){
    // the minimum value of gaps is 2
    return (first_val + (i*(next_val - first_val)/gaps));
}

Rcpp::IntegerVector na_linear_vector_interp(Rcpp::IntegerVector& x) {
    // This function linearly interpolates to fill sequences of NA
    // values surrounded by valid numbers.

    // detect leading NAs
    IntegerVector::iterator curr  = x.begin();
    IntegerVector::iterator first = x.begin();
    IntegerVector::iterator last  = x.end();
    while(Rcpp::IntegerVector::is_na(*curr)){
        ++curr;
    }
    if (curr == x.end())
        return x;

    while (first != curr) {
        *first++ = *curr;
    }

    // find the first NA after the first pixel
    while(true){
        // curr points to a non NA pixel
        IntegerVector::iterator num_to_na = std::adjacent_find(curr, last, detect_start_na);
        if (num_to_na == last)
            break;
        IntegerVector::iterator na_to_num = std::adjacent_find(num_to_na, last, detect_end_na);

        // account for trailing NAs
        if (na_to_num == last) {
            int val = *num_to_na;
            while (num_to_na != last) {
                *num_to_na++ = val;
            }
            break;
        }
        // At this point, num_to_na points to the last number before
        // an interpolation block, and na_to_num points to the last NA
        // of that block.

        ++na_to_num;            // Now, both iterators point to numbers.

        // find out the gaps
        int gaps = std::distance(num_to_na, na_to_num);
        // generate the missing values
        int i = 0;
        int base = *num_to_na;
        int target = *na_to_num;
        while (++num_to_na != na_to_num) {
            ++i;
            *num_to_na = na_interp(base, target, gaps, i);
        }
        // Advance onwards
        curr = na_to_num;
        if (curr == last)
            break;

    }
    return x;
}
// This function interpolates matrix with NA values using linear methods
// [[Rcpp::export]]
IntegerMatrix linear_interp(IntegerMatrix& mtx) {

    int nrows = mtx.nrow();
    int ncols = mtx.ncol();

    IntegerVector vec(ncols);

    for (int i = 0; i < nrows; i++) {
        IntegerVector vec = mtx(i, _);
        mtx(i, _) = na_linear_vector_interp(vec);
    }


    // fix all NA values by copying nearest non-NA pixel values
    for (int i = 0; i < nrows; i++) {

        if (IntegerVector::is_na(mtx(i, 0))) {

            // increase a linear window to look for non-NA pixel values
            for (int j = 1; j < nrows ; j++) {

                // check first left pixel
                if ((i - j >= 0) & !IntegerVector::is_na(mtx(i - j, 0))) {
                    mtx(i, _) = mtx(i - j, _);
                    break;
                    // else check right pixel
                } else if ((i + j < nrows) && !IntegerVector::is_na(mtx(i + j, 0))) {
                    mtx(i, _) = mtx(i + j, _);
                    break;
                } else if (i - j < 0 || i + j >= nrows)
                    stop("All values are NA.");
            }
        }
    }

    return mtx;
}

// [[Rcpp::export]]
IntegerVector linear_interp_vec(IntegerVector& vec) {

    return na_linear_vector_interp(vec);
}
