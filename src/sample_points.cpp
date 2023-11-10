#include <Rcpp.h>
#include <iostream>
#include <vector>
#include <list>
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
bool is_inside_inclusion(const std::vector<Point>& polygon, const Point& p) {
    int i, j, c = 0;
    int nvert = polygon.size();
    for (i = 0, j = nvert - 1; i < nvert; j = i++) {
        if ( ((polygon[i].y > p.y) != (polygon[j].y > p.y)) &&
             (p.x < (polygon[j].x - polygon[i].x) *
             (p.y - polygon[i].y) / (polygon[j].y - polygon[i].y) +
             polygon[i].x)
        )
            c = !c;
    }
    return c;
}
bool is_inside_crossings(const std::vector<Point>& polygon, const Point& p) {
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
// ======= Trapezoid (bin) algorithm =======================================

// Split polygons along set of y bins and sorts the edge fragments.  Testing
// is done against these fragments.

struct Edge {
    Point p1, p2;
};

class BinnedPolygon {
public:
    BinnedPolygon(const std::vector<Point>& vertices, int binSize)
        : binSize(binSize) {
        // Find the bounding box of the polygon
        for (const auto& v : vertices) {
            minX = std::min(minX, v.x);
            minY = std::min(minY, v.y);
            maxX = std::max(maxX, v.x);
            maxY = std::max(maxY, v.y);
        }

        // Create the bins
        int binsX = (maxX - minX) / binSize + 1;
        int binsY = (maxY - minY) / binSize + 1;
        bins.resize(binsX, std::vector<std::list<Edge>>(binsY));

        // Insert the edges into the bins
        for (size_t i = 0; i < vertices.size(); ++i) {
            size_t j = (i + 1) % vertices.size();
            insertEdge({vertices[i], vertices[j]});
        }
    }

    bool isInside(const Point& point) const {
        int binX = (point.x - minX) / binSize;
        int binY = (point.y - minY) / binSize;

        if (binX < 0 || binX >= bins.size() || binY < 0 || binY >= bins[0].size())
            return false;

        // Ray casting algorithm
        int intersections = 0;
        for (const auto& edge : bins[binX][binY]) {
            if ((edge.p1.y > point.y) != (edge.p2.y > point.y) &&
                point.x < (edge.p2.x - edge.p1.x) * (point.y - edge.p1.y) /
                    (edge.p2.y - edge.p1.y) + edge.p1.x) {
                ++intersections;
            }
        }
        return (intersections % 2) == 1;
    }

private:
    void insertEdge(const Edge& edge) {
        // Find the bins that this edge intersects with
        int minXBin = std::min(edge.p1.x, edge.p2.x);
        int maxXBin = std::max(edge.p1.x, edge.p2.x);
        int minYBin = std::min(edge.p1.y, edge.p2.y);
        int maxYBin = std::max(edge.p1.y, edge.p2.y);

        for (int x = minXBin; x <= maxXBin; x += binSize) {
            for (int y = minYBin; y <= maxYBin; y += binSize) {
                int binX = (x - minX) / binSize;
                int binY = (y - minY) / binSize;
                bins[binX][binY].push_back(edge);
            }
        }
    }

    double minX = std::numeric_limits<double>::infinity();
    double minY = std::numeric_limits<double>::infinity();
    double maxX = -std::numeric_limits<double>::infinity();
    double maxY = -std::numeric_limits<double>::infinity();
    int binSize;
    std::vector<std::vector<std::list<Edge>>> bins;
};

// [[Rcpp::export]]
NumericMatrix sample_points_inclusion(const NumericMatrix& polymatrix,
                            const int n_sam_pol){

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
        p.x = minX + (maxX - minX) * R::runif(0, static_cast<double>(RAND_MAX))
            / static_cast<double>(RAND_MAX);
        p.y = minY + (maxY - minY) * R::runif(0, static_cast<double>(RAND_MAX))
            / static_cast<double>(RAND_MAX);
        if (is_inside_inclusion(polygon, p)) {
            points(i, 0) = p.x;
            points(i, 1) = p.y;
            i++;
        }

    }
    return points;
}
// [[Rcpp::export]]
NumericMatrix sample_points_crossings(const NumericMatrix& polymatrix,
                            const int n_sam_pol){

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
        p.x = minX + (maxX - minX) * R::runif(0, static_cast<double>(RAND_MAX))
            / static_cast<double>(RAND_MAX);
        p.y = minY + (maxY - minY) * R::runif(0, static_cast<double>(RAND_MAX))
            / static_cast<double>(RAND_MAX);
        if (is_inside_crossings(polygon, p)) {
            points(i, 0) = p.x;
            points(i, 1) = p.y;
            i++;
        }
    }
    return points;
}
// [[Rcpp::export]]
NumericMatrix sample_points_bin(const NumericMatrix& polymatrix,
                                 const int n_sam_pol){

    int n_bins = 20;
    NumericMatrix points(n_sam_pol, 2); // output
    // internal data structures
    int n_vertices = polymatrix.nrow();
    std::vector<Point> polygon(n_vertices);
    // build polygon from NumericMatrix
    for (size_t i = 0; i < n_vertices; ++i){
        polygon[i].x = polymatrix(i, 0);
        polygon[i].y = polymatrix(i, 1);
    }
    BinnedPolygon binnedPolygon(polygon, n_bins);

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
        p.x = minX + (maxX - minX) * R::runif(0, static_cast<double>(RAND_MAX))
            / static_cast<double>(RAND_MAX);
        p.y = minY + (maxY - minY) * R::runif(0, static_cast<double>(RAND_MAX))
            / static_cast<double>(RAND_MAX);
        if (binnedPolygon.isInside(p)) {
            points(i, 0) = p.x;
            points(i, 1) = p.y;
            i++;
        }
    }
    return points;
}
