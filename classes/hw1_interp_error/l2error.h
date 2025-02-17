#ifndef HW1_L2ERROR_H
#define HW1_L2ERROR_H

#include "hw1.h"
#include <vector>
#include <string>
#include <map>
#include <tuple>
#include <cstddef>

double l2error(std::vector<Entity> &entities,
               std::map<int, ElemPrp> &prps,
               std::vector<std::size_t> &elems2D,
               std::vector<std::tuple<size_t, size_t, size_t>> &elemLOCs,
               std::vector<std::vector<std::vector<double>>> &determinants,
               std::vector<std::vector<std::vector<double>>> &gpcoords,
               std::vector<std::vector<double>> &nodalF,
               std::string &fct);

double totalArea(std::vector<Entity> &entities,
                 std::map<int, ElemPrp> &prps,
                 std::vector<std::size_t> &elems2D,
                 std::vector<std::tuple<size_t, size_t, size_t>> &elemLOCs,
                 std::vector<std::vector<std::vector<double>>> &determinants);

#endif // HW1_L2ERROR_H
