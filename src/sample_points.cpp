#include <Rcpp.h>
#include <iostream>
#include <vector>
#include <cstdlib>
#include <ctime>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//
struct Point {
    double x, y;
};

bool is_inside(const std::vector<Point>& polygon, const Point& p) {
    int crossings = 0;
    size_t num_vertices = polygon.size();
    for (size_t i = 0; i < num_vertices; ++i) {
        size_t j = (i + 1) % num_vertices;
        if ((polygon[i].y <= p.y && polygon[j].y > p.y) ||
            (polygon[j].y <= p.y && polygon[i].y > p.y)) {
            double atX = polygon[i].x + (p.y - polygon[i].y) /
                (polygon[j].y - polygon[i].y) * (polygon[j].x - polygon[i].x);
            if (p.x < atX) crossings++;
        }
    }
    return (crossings % 2 > 0);
}
// [[Rcpp::export]]
NumericMatrix sample_points(const NumericMatrix& polymatrix,
                            const int n_sam_pol){

    std::srand(std::time(nullptr));  // Seed for random number generation

    NumericMatrix points(n_sam_pol, 2); // output

    int n_vertices = polymatrix.nrow();
    std::vector<Point> polygon(n_vertices);
    // build polygon from NumericMatrix
    for (size_t i = 0; i < n_vertices; ++i){
        polygon[i].x = polymatrix(i, 0);
        polygon[i].y = polymatrix(i, 1);
    }
    // find max and min
    double minX = polygon[0].x, maxX = polygon[0].x;
    double minY = polygon[0].y, maxY = polygon[0].y;
    for (const auto& vertex : polygon) {
        minX = std::min(minX, vertex.x);
        maxX = std::max(maxX, vertex.x);
        minY = std::min(minY, vertex.y);
        maxY = std::max(maxY, vertex.y);
    }

    // Generate points inside the polygon
    int i = 0;
    while (i < n_sam_pol) {
        Point p;
        p.x = minX + (maxX - minX) * (rand() / static_cast<double>(RAND_MAX));
        p.y = minY + (maxY - minY) * (rand() / static_cast<double>(RAND_MAX));
        if (is_inside(polygon, p)) {
            points(i, 0) = p.x;
            points(i, 1) = p.y;
        }
        i++;
    }
    return points;
}

