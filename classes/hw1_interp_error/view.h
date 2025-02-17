#ifndef HW1_VIEW_H
#define HW1_VIEW_H

#include "hw1.h"
#include <vector>
#include <string>

void view(const std::vector<std::size_t> &elems2D,
          const std::vector<std::vector<double>> &nodalF,
          std::string const &vname);

#endif // HW1_VIEW_H