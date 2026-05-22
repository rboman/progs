#include "core.h"

Curve::Curve() : color() {}

void Curve::fill(std::function<double(double)> f, const std::pair<double, double>& rng, int n) {
    pts.clear();
    if (n <= 0) {
        return;
    }
    double x = rng.first;
    double dx = (rng.second - rng.first) / n;
    for (int i = 0; i < n; ++i) {
        pts.push_back(Point(x, f(x)));
        x += dx;
    }
}
