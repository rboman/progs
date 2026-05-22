#ifndef CORE_H
#define CORE_H

#include <vector>
#include <functional>
#include <utility>
#include <QColor>

struct Point {
    double x;
    double y;

    Point(double x = 0.0, double y = 0.0) : x(x), y(y) {}
};

class Curve {
public:
    std::vector<Point> pts;
    QColor color;

    Curve();

    void fill(std::function<double(double)> f, const std::pair<double, double>& rng, int n);
};

#endif // CORE_H
