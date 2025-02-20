#ifndef HW1_ELEMENTS_H
#define HW1_ELEMENTS_H

#include "hw1.h"
#include <vector>
#include <map>
#include <tuple>
#include <cstddef>

// Elements of a given physical group

struct ElementData
{
    std::vector<std::vector<std::vector<double>>> jacobians;    // [entity][mesh][...]
    std::vector<std::vector<std::vector<double>>> determinants; // [entity][mesh][...]
    std::vector<std::vector<std::vector<double>>> gpcoords;     // [entity][mesh][...]
};

size_t countNbOfElements(std::vector<Entity> &entities);

void fillElementData(ElementData &eldata,
                     std::vector<Entity> &entities,
                     std::map<int, ElemPrp> &prps,
                     std::vector<std::size_t> &elems2D,
                     std::vector<std::tuple<size_t, size_t, size_t>> &elemLOCs);

#endif // HW1_ELEMENTS_H