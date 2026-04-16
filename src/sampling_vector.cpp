// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
#include <random>      // std::default_random_engine, std::mt19937, distributions
#include <vector>
#include <cmath>       // std::abs, std::cos, M_PI, std::sqrt, std::floor
#include <algorithm>   // std::shuffle, std::sort, std::set_intersection, std::max, std::min, std::iota
#include <numeric>     // std::iota
#include <unordered_set>

// We decided to add all functions adapted from terra into a dedicated namespace
// to avoid symbol conflicts with any existing sits function
namespace terra_sampling_random {

/**
 * Generate a sequence of evenly spaced values.
 *
 * @description
 * Generates values: start, start+increment, start + 2 * increment, ...
 * stopping at or before `end`.
 *
 * @note
 * This code was adapted from terra R package, version 1.9-11
 * (https://github.com/rspatial/terra/tree/ca87f3eb638f20546011558d9266fe6ebd7e7eb4)
 *
 * @source
 * terra/src/recycle.h:105-118
 *
 * @param start A `T` with the starting value.
 * @param end A `T` with the ending value.
 * @param increment A `T` with the increment.
 *
 * @return A `std::vector<T>` with the sequence of values.
 */
template <typename T>
std::vector<T> seq(T start, T end, T increment) {
    std::vector<T> out;
    //if (increment <= 0) return out;
    if ((start > end) && (increment > 0)) return out;
    if ((start < end) && (increment < 0)) return out;
    if (start == end) return {start};
    size_t s = floor((end - start) / increment);
    out.reserve(s);
    for (size_t i = 0; i <= s; i++) {
        T val = start + i * increment;
        out.push_back(val);
    }
    return out;
}

/**
 * Draw `size` indices from [0, N-1] with replacement.
 *
 * @description
 * Draws `size` indices from [0, N-1] with replacement.
 *
 * @note
 * This code was adapted from terra R package, version 1.9-11
 * (https://github.com/rspatial/terra/tree/ca87f3eb638f20546011558d9266fe6ebd7e7eb4)
 *
 * @source
 * terra/src/sample.cpp:435-444
 *
 * @param size A `size_t` with the number of indices to draw.
 * @param N A `size_t` with the population size.
 * @param seed A `unsigned` with the random seed.
 *
 * @return A `std::vector<size_t>` with the sampled indices.
 */
std::vector<size_t> sample_replace(size_t size, size_t N, unsigned seed) {
    std::default_random_engine gen(seed);
    std::uniform_int_distribution<> U(0, N - 1);
    std::vector<size_t> sample;
    sample.reserve(size);
    for (size_t i = 0; i < size; i++) {
        sample.push_back(U(gen));
    }
    return sample;
}

/**
 * Draw `size` indices from [0, N-1] with replacement, weighted by `prob`.
 *
 * @description
 * Draws `size` indices from [0, N-1] with replacement, weighted by `prob`.
 *
 * @note
 * This code was adapted from terra R package, version 1.9-11
 * (https://github.com/rspatial/terra/tree/ca87f3eb638f20546011558d9266fe6ebd7e7eb4)
 *
 * @source
 * terra/src/sample.cpp:446-453
 *
 * @param size A `size_t` with the number of indices to draw.
 * @param N A `size_t` with the population size.
 * @param prob A `std::vector<double>` with the weights.
 * @param seed A `unsigned` with the random seed.
 *
 * @return A `std::vector<size_t>` with the sampled indices.
 */
std::vector<size_t> sample_replace_weights(size_t size, size_t N, std::vector<double> prob, unsigned seed) {
	std::discrete_distribution<int> dist(std::begin(prob), std::end(prob));
	std::mt19937 gen;
	gen.seed(seed);
	std::vector<size_t> sample(size);
	for(auto & i: sample) i = dist(gen);
	return sample;
}

/**
 * Draw `size` indices from [0, N-1] without replacement.
 *
 * @description
 * Draws `size` indices from [0, N-1] without replacement.
 *
 * @note
 * This code was adapted from terra R package, version 1.9-11
 * (https://github.com/rspatial/terra/tree/ca87f3eb638f20546011558d9266fe6ebd7e7eb4)
 *
 * @source
 * terra/src/sample.cpp:456-489
 *
 * @param size A `size_t` with the number of indices to draw.
 * @param N A `size_t` with the population size.
 * @param seed A `unsigned` with the random seed.
 *
 * @return A `std::vector<size_t>` with the sampled indices.
 */
std::vector<size_t> sample_no_replace(size_t size, size_t N, unsigned seed) {
	size_t one = 1;
	size = std::max(one, std::min(size, N));
	std::vector<size_t> sample;
	if (size == N) {
		sample.resize(size);
		std::iota(sample.begin(), sample.end(), 0);
		return sample;
	}
	std::default_random_engine gen(seed);

	if (size >= .66 * N) {
		sample.resize(N);
		std::iota(std::begin(sample), std::end(sample), 0);
		std::shuffle(sample.begin(), sample.end(), gen);
		if (size < N) {
			sample.erase(sample.begin()+size, sample.end());
		}
		return sample;
	}

	std::uniform_real_distribution<> U( 0, std::nextafter(1.0, std::numeric_limits<double>::max() ) );

	sample.reserve(size);
	for (size_t i=0; i<N; i++) {
		if ( ((N-i) * U(gen)) <  (size - sample.size()) ) {
			sample.push_back(i);
    		if (sample.size() == size ) {
				break;
			}
		}
	}
	return sample;
}

/**
 * Draw `size` indices from [0, N-1] without replacement, weighted by `prob`.
 *
 * @description
 * Draws `size` indices from [0, N-1] without replacement, weighted by `prob`.
 *
 * @note
 * This code was adapted from terra R package, version 1.9-11
 * (https://github.com/rspatial/terra/tree/ca87f3eb638f20546011558d9266fe6ebd7e7eb4)
 *
 * @source
 * terra/src/sample.cpp:492-556
 *
 * @param size A `size_t` with the number of indices to draw.
 * @param N A `size_t` with the population size.
 * @param prob A `std::vector<double>` with the weights.
 * @param seed A `unsigned` with the random seed.
 *
 * @return A `std::vector<size_t>` with the sampled indices.
 */
std::vector<size_t> sample_no_replace_weights(size_t size, size_t N, std::vector<double> prob, unsigned seed) {
	size_t one = 1;
	size = std::max(one, std::min(size, N));
	std::vector<size_t> sample;
	std::default_random_engine gen(seed);
	if (size == N) {
		sample.resize(size);
		std::iota(sample.begin(), sample.end(), 0);
		std::shuffle(sample.begin(), sample.end(), gen);
		return sample;
	}

	std::uniform_int_distribution<> U(0, std::numeric_limits<int>::max());
	std::unordered_set<size_t> sampleset;

	size_t isize = size;
	if (size > (0.8 * N)) {
		isize = N - size;
		for (double &d : prob) d = 1-d;
		size_t ssize = isize * (1.1 + isize / N);
		size_t cnt=0;
		while (sampleset.size() < isize) {
			seed = U(gen);
			std::vector<size_t> s = sample_replace_weights(ssize, N, prob, seed);
			for (size_t i=0; i<s.size(); i++) {
				sampleset.insert(s[i]);
			}
			cnt++;
			if (cnt > 10) break;
		}
		std::vector<size_t> invsamp;
		invsamp.insert(invsamp.begin(), sampleset.begin(), sampleset.end());
		std::sort(invsamp.begin(), invsamp.end());
		invsamp.push_back(N+1);
		size_t j=0;
		sample.reserve(size);
		for (size_t i=0; i<N; i++) {
			if (i != invsamp[j]) {
				sample.push_back(i);
			} else {
				j++;
			}
		}
		std::shuffle(sample.begin(), sample.end(), gen);

	} else {
		size_t ssize = size * (1.1 + size / N);
		size_t cnt=0;
		while (sampleset.size() < size) {
			seed = U(gen);
			std::vector<size_t> s = sample_replace_weights(ssize, N, prob, seed);
			for (size_t i=0; i<s.size(); i++) {
				sampleset.insert(s[i]);
			}
			cnt++;
			if (cnt > 10) break;
		}
		sample.insert(sample.begin(), sampleset.begin(), sampleset.end());
		if (sample.size() > size) {
			sample.resize(size);
		};
	}

	return(sample);
}

/**
 * Draw `size` indices from [0, N-1] with replacement or without replacement, weighted by `prob`.
 *
 * @description
 * Draws `size` indices from [0, N-1] with replacement or without replacement, weighted by `prob`.
 *
 * @note
 * This code was adapted from terra R package, version 1.9-11
 * (https://github.com/rspatial/terra/tree/ca87f3eb638f20546011558d9266fe6ebd7e7eb4)
 *
 * @source
 * terra/src/sample.cpp:559-586
 *
 * @param size A `size_t` with the number of indices to draw.
 * @param N A `size_t` with the population size.
 * @param replace A `bool` with replacement or without replacement.
 * @param prob A `std::vector<double>` with the weights.
 * @param seed A `unsigned` with the random seed.
 *
 * @return A `std::vector<size_t>` with the sampled indices.
 */
std::vector<size_t> sample(size_t size, size_t N, bool replace, std::vector<double> prob, unsigned seed) {
	if ((size == 0) || (N == 0)) {
		std::vector<size_t> s;
		return s;
	}
	bool w = prob.size() == N;
	if (replace) {
		if (N == 1) {
			std::vector<size_t> s(size,0);
			return s;
		}
		if (w) {
			return sample_replace_weights(size, N, prob, seed);
		} else {
			return sample_replace(size, N, seed);
		}
	} else {
		if (N == 1) {
			std::vector<size_t> s(1,0);
			return s;
		}
		if (w) {
			return sample_no_replace_weights(size, N, prob, seed);
		} else {
			return sample_no_replace(size, N, seed);
		}
	}
}

/**
 * Generate random points within a bounding box.
 *
 * @description
 * Generates random points within a bounding box. This is the core point generator for polygon sampling.
 *
 * In ``terra`` this is called at ``terra/src/sample.cpp:936`` as:
 *
 *      pxy = extent.sampleRandom(ssize, lonlat, seed);
 *
 * We reproduce the exact same logic but without requiring a ``SpatExtent`` object.
 *
 * @note
 * This code was adapted from terra R package, version 1.9-11
 * (https://github.com/rspatial/terra/tree/ca87f3eb638f20546011558d9266fe6ebd7e7eb4)
 *
 * @source
 * terra/src/sample.cpp:641-689
 *
 * @param size A `size_t` with the number of indices to draw.
 * @param xmin A `double` with the minimum x value.
 * @param xmax A `double` with the maximum x value.
 * @param ymin A `double` with the minimum y value.
 * @param ymax A `double` with the maximum y value.
 * @param lonlat A `bool` with the longitude/latitude flag.
 * @param seed A `unsigned` with the random seed.
 * @param out_lon A `std::vector<double>` with the sampled longitude values.
 * @param out_lat A `std::vector<double>` with the sampled latitude values.
 *
 * @return void
 */
void sampleRandom_extent(double xmin, double xmax, double ymin, double ymax, size_t size, bool lonlat, unsigned seed, std::vector<double>& out_lon, std::vector<double>& out_lat) {
    if (size == 0) return;
    std::default_random_engine gen(seed);

    if (lonlat) {
        double d = (ymax - ymin) / 1000.0;
        std::vector<double> r = seq(ymin, ymax, d);
        std::vector<double> w;
        w.reserve(r.size());
        for (size_t i = 0; i < r.size(); i++) {
            double ww = std::abs(cos(M_PI * r[i] / 180.0));
            w.push_back(ww);
        }

        std::vector<size_t> x = sample(size, r.size(), true, w, seed);
        out_lat.reserve(size);  // changed to use external value
        out_lon.reserve(size); // changed to use external value
        std::uniform_real_distribution<> U1(-0.5, 0.5);

        double dx = 0.5 * d;
        for (size_t i = 0; i < x.size(); i++) {
            double v = r[x[i]] + dx * U1(gen);
            out_lat.push_back(v);
        }
        std::uniform_real_distribution<> U2(xmin, xmax);
        for (size_t i = 0; i < size; i++) {
            out_lon.push_back(U2(gen));
        }
		// commented out to use external value and avoid a new copy
		// out[0] = lon;
		// out[1] = lat;
    } else {
        out_lon.reserve(size); // changed to use external value
        out_lat.reserve(size); // changed to use external value
        std::uniform_real_distribution<> runifx(xmin, xmax);
        std::uniform_real_distribution<> runify(ymin, ymax);
        for (size_t i = 0; i < size; i++) {
            out_lon.push_back(runifx(gen));
            out_lat.push_back(runify(gen));
        }
		// commented out to use external value and avoid a new copy
		// out[0] = x;
		// out[1] = y;
    }
}

} // namespace terra_sampling_random

/**
 * Generate oversampled candidates per polygon.
 *
 * @description
 * Generates oversampled candidates per polygon. This is the core point generator
 * for polygon sampling.
 *
 * @note
 * This code was adapted from terra R package, version 1.9-11
 * (https://github.com/rspatial/terra/tree/ca87f3eb638f20546011558d9266fe6ebd7e7eb4)
 *
 * @source
 * terra/src/sample.cpp:930-936
 *
 * @param bbox_xmin A `Rcpp::NumericVector` with the xmin of the bounding box per polygon.
 * @param bbox_xmax A `Rcpp::NumericVector` with the xmax of the bounding box per polygon.
 * @param bbox_ymin A `Rcpp::NumericVector` with the ymin of the bounding box per polygon.
 * @param bbox_ymax A `Rcpp::NumericVector` with the ymax of the bounding box per polygon.
 * @param areas A `Rcpp::NumericVector` with the area of the polygon.
 * @param n_points A `Rcpp::IntegerVector` with the number of points per polygon.
 * @param lonlat A `bool` with the longitude/latitude flag.
 * @param seed A `unsigned int` with the random seed.
 *
 * @return A `Rcpp::DataFrame` with the oversampled candidates.
 */
// [[Rcpp::export]]
Rcpp::DataFrame C_terra_sampling_random_candidates(Rcpp::NumericVector bbox_xmin,
                                    Rcpp::NumericVector bbox_xmax,
                                    Rcpp::NumericVector bbox_ymin,
                                    Rcpp::NumericVector bbox_ymax,
                                    Rcpp::NumericVector areas,
                                    Rcpp::IntegerVector n_points,
                                    bool lonlat = false,
                                    unsigned int seed = 42) {

    // Get the number of polygons
    int n_polys = n_points.size();

    // Initialize the output vectors
    std::vector<double> out_x, out_y;
    std::vector<int> out_id;

    // Initialize the unique random number generator
    // for all polygons
    std::default_random_engine rng(seed);

    // Initialize the uniform integer distribution
    // for the random number generator
    std::uniform_int_distribution<unsigned> seed_dist(
            0, std::numeric_limits<unsigned>::max());

    // Loop all polygons
    for (int pol_idx = 0; pol_idx < n_polys; pol_idx++) {
        // Get the number of points for the current polygon
        size_t target = n_points[pol_idx];

        // If, for any reason,the number of points is 0, skip the polygon
        // (terra/src/sample.cpp:929)
        if (target == 0) {
            continue;
        }
        // Oversampling ratio (terra/src/sample.cpp:927-935)
        // Step 1: Calculate bbox area (terra/src/sample.cpp:929)
        // (Note: In terra, a ``SpatVector`` is created for this)
        double vea = (bbox_xmax[pol_idx] - bbox_xmin[pol_idx]) * (bbox_ymax[pol_idx] - bbox_ymin[pol_idx]);

        // Step 2: Calculate polygon area (terra/src/sample.cpp:924)
        double suma = areas[pol_idx];

        // Step 3: Calculate adaptive oversampling formula (terra/src/sample.cpp:931-935)
        double m = vea / suma;

        // Comment from terra code: the larger the sample size, the fewer extra samples needed
        double smx = sqrt(std::max(9.0, 100.0 - (double)target));
        m = std::max(smx, std::min(m * m, 100.0));
        size_t ssize = target * m;

        // Step 4: Generate candidates in bbox (terra/src/sample.cpp:936) ---
        std::vector<double> cx, cy;
        unsigned rseed = seed_dist(rng);

        // Call function adapted from terra code
        terra_sampling_random::sampleRandom_extent(
            bbox_xmin[pol_idx],  bbox_xmax[pol_idx],
            bbox_ymin[pol_idx], bbox_ymax[pol_idx],
            ssize, lonlat, rseed,
            cx, cy
        );

        // Tag each candidate with its polygon ID (1-indexed for R)
        for (size_t i = 0; i < cx.size(); i++) {
            out_x.push_back(cx[i]);
            out_y.push_back(cy[i]);

            out_id.push_back(pol_idx + 1);
        }
    }

    // Return the output dataframe with the columns ``x``, ``y``, ``pol_id``
    return Rcpp::DataFrame::create(
        Rcpp::Named("x") = out_x,
        Rcpp::Named("y") = out_y,
        Rcpp::Named("pol_id") = out_id);
}

/**
 * Filter and trim the oversampled candidates.
 *
 * @description
 * Filters and trims the oversampled candidates. This is the core point generator
 * for polygon sampling.
 *
 * @note
 * This code was adapted from terra R package, version 1.9-11
 * (https://github.com/rspatial/terra/tree/ca87f3eb638f20546011558d9266fe6ebd7e7eb4)
 *
 * @source
 * This replaces terra/src/sample.cpp:943-951
 *
 * @param hit_mat A `Rcpp::List` with the hit matrix.
 * @param poly_ids A `Rcpp::IntegerVector` with the polygon IDs.
 * @param n_per_feature A `Rcpp::IntegerVector` with the number of points per feature.
 * @param seed A `unsigned int` with the random seed.
 *
 * @return A `Rcpp::IntegerVector` with the indices of the kept candidates.
 */
// [[Rcpp::export]]
Rcpp::IntegerVector C_terra_sampling_filter_and_trim(Rcpp::List hit_mat, Rcpp::IntegerVector poly_ids, Rcpp::IntegerVector n_per_feature, unsigned int seed = 777) {
    // Get the number of polygons
    int n_polys = hit_mat.size();

    // Get the number of points
    int n_pts = poly_ids.size();

    // Initialize the random number generator
    std::mt19937 gen(seed);

    // Initialize the vector to save the valid indices
    std::vector<int> valid_indices;

    // Step 1: Group candidate indices by polygon
    // We need to group the candidate indices by their polygon ID as here we are
    // processing multiple polygons at once.
    // In ``terra``, this is not required as they process polygons one by one (with lapply)
    std::vector<std::vector<int>> polygon_groups(n_polys);

    for (int i = 0; i < n_pts; i++) {
        // Get the polygon ID for the current point
        int pid = poly_ids[i] - 1;  // convert to 0-indexed

        // If the polygon ID is valid, add the point index to the polygon group
        if (pid >= 0 && pid < n_polys) {
            polygon_groups[pid].push_back(i + 1); // we use ``+1`` to facilitate R usage of this result
        }
    }

    // Step 2: For each polygon, we need to intersect the polygon groups with
    //        the confirmed indices for each polygon.
    // > Confirmed here referes to the indices of the points that were confirmed
    // > to intersect the polygon by ``sf``
    for (int p = 0; p < n_polys; p++) {
        // If not intersecting any polygon, skip
        if (polygon_groups[p].empty()) {
            continue;
        }

        // Otherwise, we assume there are some points to keep
        // for this polygon
        int n = n_per_feature[p];

        // Sanity check: If there is no points to keep, skip
        if (n == 0) {
            continue;
        }

        // Here we get the indices of the points that ``sf`` confirmed inside the polygon
        Rcpp::IntegerVector points_inside_polygon = hit_mat[p];

        // Convert the Rcpp::IntegerVector to a std::vector<int>
        std::vector<int> hits(points_inside_polygon.begin(), points_inside_polygon.end());

        // Sort both vectors so we can use std::set_intersection
        std::sort(hits.begin(), hits.end());
        std::sort(polygon_groups[p].begin(), polygon_groups[p].end());

        // valid = candidates that polygon_groups polygon p AND are inside it
        std::vector<int> valid;

        // The idea here is, if we get the intersection of the polygon groups
        // and the confirmed indices, we will get the indices of the points that
        // are both in the polygon groups and the confirmed indices.
        // https://en.cppreference.com/w/cpp/algorithm/set_intersection.html
        std::set_intersection(
            hits.begin(),
            hits.end(),
            polygon_groups[p].begin(),
            polygon_groups[p].end(),
            // This iterator is used to insert the elements of the intersection into the valid vector
            // https://en.cppreference.com/w/cpp/iterator/back_inserter.html
            std::back_inserter(valid)
        );

        // Step 3: Trim to target size
        // If we have more valid points than the target size, we need to shuffle and
        // trim the valid points to the target size.
        // This is the same logic as terra/src/sample.cpp:944-951
        if ((int)valid.size() > n) {
            // Shuffle the valid points
            std::shuffle(valid.begin(), valid.end(), gen);

            // Trim the valid points to the target size
            valid.resize(n);
        }

        // Add the valid indices to the valid indices vector
        valid_indices.insert(valid_indices.end(), valid.begin(), valid.end());
    }

    // Return!
    return Rcpp::wrap(valid_indices);
}
