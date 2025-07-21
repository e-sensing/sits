#include <map>
#include <vector>
#include <string>
#include <numeric>
#include <algorithm>
#include <limits>
#include <cmath>
#include <unordered_set>
#include <random>
#include <set>

/**
 * Create a permutation vector representing the sorted order of elements.
 *
 * @description
 * Returns indices that would sort the input vector in ascending order.
 *
 * @note
 * This code was adapted from terra R package, version 1.8.60
 * (https://github.com/rspatial/terra/tree/a5c561230e1fd01e0b2a1c03ef410e91adbc7153)
 *
 * @param x A `std::vector<T>` with values to be sorted.
 * @return A `std::vector<std::size_t>` containing indices in sorted order.
 */
template <typename T>
std::vector<std::size_t> sort_order_a(const std::vector<T> &x){
    std::vector<std::size_t> p(x.size());
    std::iota(p.begin(), p.end(), 0);
    std::sort(p.begin(), p.end(),
              [&](std::size_t i, std::size_t j){ return (x[i] < x[j]); });
    return p;
}

/**
 * Create a permutation vector representing the sorted order of elements with NaN handling.
 *
 * @description
 * Returns indices that would sort the input vector in ascending order, placing
 * NaN values at the end.
 *
 * @note
 * This code was adapted from terra R package, version 1.8.60
 * (https://github.com/rspatial/terra/tree/a5c561230e1fd01e0b2a1c03ef410e91adbc7153)
 *
 * @param x A `std::vector<double>` with values to be sorted.
 * @return A `std::vector<std::size_t>` containing indices in sorted order.
 */
std::vector<std::size_t> sort_order_nan_a(const std::vector<double> &x){
    std::vector<std::size_t> p(x.size());
    std::iota(p.begin(), p.end(), 0);
    std::sort(p.begin(), p.end(),
              [&](std::size_t i, std::size_t j){ return (
                      std::isnan(x[i]) ? false :
                  std::isnan(x[j]) ? true :
                  x[i] < x[j]); });
    return p;
}

/**
 * Reorder vector elements according to a permutation vector.
 *
 * @description
 * Rearranges the elements of vector x using the permutation indices in p.
 *
 * @note
 * This code was adapted from terra R package, version 1.8.60
 * (https://github.com/rspatial/terra/tree/a5c561230e1fd01e0b2a1c03ef410e91adbc7153)
 *
 * @param x A `std::vector<T>` to be reordered.
 * @param p A `std::vector<std::size_t>` containing the permutation indices.
 */
template <typename T>
void permute(std::vector<T> &x, const std::vector<std::size_t> &p) {
    std::vector<bool> done(x.size());
    for (std::size_t i = 0; i < x.size(); ++i)  {
        if (done[i]) {
            continue;
        }
        done[i] = true;
        size_t prev_j = i;
        size_t j = p[i];
        while (i != j) {
            std::swap(x[prev_j], x[j]);
            done[j] = true;
            prev_j = j;
            j = p[j];
        }
    }
}

/**
 * Create a frequency table from a vector of doubles.
 *
 * @description
 * Counts the occurrences of each unique value in the input vector and returns
 * a map with values as keys and their counts as values.
 *
 * @note
 * This code was adapted from terra R package, version 1.8.60
 * (https://github.com/rspatial/terra/tree/a5c561230e1fd01e0b2a1c03ef410e91adbc7153)
 *
 * @param v A `std::vector<double>` with values to be counted.
 * @return A `std::map<double, size_t>` with value counts.
 */
 std::map<double, size_t> table(std::vector<double> &v) {
    std::map<double, size_t> count;
    for_each( v.begin(), v.end(), [&count]( double val ){
        if(!std::isnan(val)) count[val]++;
    }
    );
    return count;
}

/**
 * Convert a frequency table to a vector of vectors.
 *
 * @description
 * Converts a frequency table (map of values and counts) into a vector of vectors,
 * where the first vector contains the unique values (strata) and the second
 * vector contains the corresponding counts.
 *
 * @note
 * This code was adapted from terra R package, version 1.8.60
 * (https://github.com/rspatial/terra/tree/a5c561230e1fd01e0b2a1c03ef410e91adbc7153)
 *
 * @param x A `std::map<double, size_t>` with value counts.
 * @return A `std::vector<std::vector<double>>` with two vectors: one for values and one for counts.
 */
std::vector<std::vector<double>> table2vector2(std::map<double, size_t> &x) {
    std::vector<std::vector<double>> out(2);
    for( auto p : x ) {
        out[0].push_back(p.first);
        out[1].push_back(p.second);
    }
    return out;
}

/**
 * Generate weights for stratified sampling.
 *
 * @description
 * Generates weights for stratified sampling based on the input values and cells.
 *
 * @note
 * This code was adapted from terra R package, version 1.8.60
 * (https://github.com/rspatial/terra/tree/a5c561230e1fd01e0b2a1c03ef410e91adbc7153)
 *
 * @param values A `std::vector<double>` with values to be stratified.
 * @param cells A `std::vector<double>` with cells to be stratified.
 * @param size A `double` with the desired sample size.
 * @param seed A `unsigned` seed for the random number generator.
 *
 * @return A `std::vector<std::vector<double>>` with three vectors: values, weights, and cells.
 */
// [[Rcpp::export]]
std::vector<std::vector<double>> C_sampling_stratified_generate_weights(
    std::vector<double> &values,
    std::vector<double> &cells,
    double size,
    unsigned seed
) {
    // Initialize random number generator
    std::default_random_engine gen1(seed);

    // Initialize sample size
    size_t szz = size;

    // Initialize set
    std::set<double> add_set;

    // Sort values and cells
    std::vector<std::size_t> pm = sort_order_nan_a(values);

    // Permute values and cells
    permute(values, pm);
    permute(cells, pm);

    // Create frequency table
    std::map<double, size_t> tab = table(values);

    // Convert frequency table to vector of vectors
    std::vector<std::vector<double>> tv = table2vector2(tab);

    // Initialize vectors for values, weights, and cells
    std::vector<double> vals, vcell, vwght;

    // Initialize start index
    size_t start = 0;

    // Iterate over strata
    for (size_t j=0; j<tv[0].size(); j++) {
        // Check stratum size
        size_t size_j = add_set.count(tv[0][j]) ? szz + 1 : szz;

        // Skip if stratum size is 0
        if (size_j == 0) continue;

        // Create vector of indices for stratum
        std::vector<size_t> z;
        z.resize(tv[1][j]);
        std::iota(z.begin(), z.end(), 0);

        // If stratum has more cells than desired sample size, shuffle and truncate
        if (tv[1][j] > size_j) {
            std::shuffle(z.begin(), z.end(), gen1);
            z.erase(z.begin()+size_j, z.end());
        }

        // Calculate weight for stratum
        double weight_j = tv[1][j] / z.size();

        // Add values, weights, and cells to output vectors
        for (size_t k=0; k<z.size(); k++) {
            vals.push_back(tv[0][j]);
            vwght.push_back(weight_j);
            vcell.push_back(cells[start+z[k]]);
        }

        // Update start index for next stratum
        start += tv[1][j];
    }

    // Return output vectors
    return { vals,  vwght, vcell };
}

/**
 * Select cells for stratified sampling.
 *
 * @description
 * Selects cells for stratified sampling based on the input values, weights, and cells.
 *
 * @note
 * This code was adapted from terra R package, version 1.8.60
 * (https://github.com/rspatial/terra/tree/a5c561230e1fd01e0b2a1c03ef410e91adbc7153)
 *
 * @param vals A `std::vector<double>` with values to be stratified.
 * @param vwght A `std::vector<double>` with weights to be stratified.
 * @param vcell A `std::vector<double>` with cells to be stratified.
 * @param size A `size_t` with the desired sample size.
 * @param seed A `unsigned` seed for the random number generator.
 *
 * @return A `std::vector<std::vector<double>>` with two vectors: cells and values.
 */
// [[Rcpp::export]]
std::vector<std::vector<double>> C_sampling_stratified_select_cells(
    std::vector<double> &vals,
    std::vector<double> &vwght,
    std::vector<double> &vcell,
    size_t size,
    unsigned seed
) {
    // Initialize output vector
    std::vector<std::vector<double>> out;

    // Initialize sample size
    size_t szz=size;
    std::set<double> add_set;

    // Sort values
    std::vector<std::size_t> pm = sort_order_a(vals);

    // Permute values, weights, and cells
    permute(vals, pm);
    permute(vwght, pm);
    permute(vcell, pm);

    // Create frequency table
    std::map<double, size_t> tab = table(vals);

    // Convert frequency table to vector of vectors
    std::vector<std::vector<double>> tv = table2vector2(tab);

    std::vector<double> outvals, outcell;

    size_t start = 0;

    // Initialize random number generator
    std::mt19937 gen2(seed);

    // Iterate over strata
    for (size_t j=0; j<tv[0].size(); j++) {
        // Check stratum size
        size_t size_j = add_set.count(tv[0][j]) ? szz + 1 : szz;

        // Skip if stratum size is 0
        if (size_j == 0) continue;

        // Calculate end index
        size_t end = start + tv[1][j];

        // Create vector of indices for stratum
        std::vector<size_t> z;
        z.resize(tv[1][j]);
        std::iota(z.begin(), z.end(), 0);

        // If stratum has more cells than desired sample size, shuffle and truncate
        if (tv[1][j] > size_j) {
            // Create discrete distribution
            std::discrete_distribution<int> dist(vwght.begin() + start, vwght.begin() + end);

            // Create vector of indices for stratum
            std::vector<size_t> Z;

            // Create set of indices
            std::unordered_set<size_t> z;

            // Add indices to set until desired sample size is reached
            while (z.size() < size_j) {
                z.insert(dist(gen2));
            }

            // Convert set to vector
            Z = std::vector<size_t>(z.begin(), z.end());

            // Add values and cells to output vectors
            for (size_t k=0; k<z.size(); k++) {
                // Add value
                outvals.push_back(tv[0][j]);

                // Add cell
                outcell.push_back(vcell[Z[k] + start]);
            }
        } else {
            // Add values and cells to output vectors
            outvals.insert(outvals.end(), vals.begin() + start, vals.begin() + end);
            outcell.insert(outcell.end(), vcell.begin() + start, vcell.begin() + end);
        }

        // Update start index for next stratum
        start = end;
    }

    // Add cells and values to output vector
    out.push_back(outcell);
    out.push_back(outvals);

    return out;
}
