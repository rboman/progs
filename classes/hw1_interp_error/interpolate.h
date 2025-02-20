#ifndef HW1_INTERPOLATE_H
#define HW1_INTERPOLATE_H

#include "hw1.h"
#include <vector>
#include <string>
#include <map>

void interpolate(Nodes &nodes, std::vector<Entity> &entities,
                 std::map<int, ElemPrp> &prps,
                 std::vector<std::size_t> &elems2D,
                 std::vector<std::vector<double>> &nodalF,
                 std::string &fct);

void approximate(std::vector<Entity> &entities,
                 std::map<int, ElemPrp> &prps,
                 std::vector<std::size_t> &elems2D,
                 std::vector<std::tuple<size_t, size_t, size_t>> &elemLOCs,
                 std::vector<std::vector<std::vector<double>>> &determinants,
                 std::vector<std::vector<std::vector<double>>> &gpcoords,
                 std::vector<std::vector<double>> &nodalF,
                 std::string &fct);

#endif // HW1_INTERPOLATE_H
