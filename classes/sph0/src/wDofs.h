/*
 * Copyright 2020 University of Liège
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#ifndef WDOFS_H
#define WDOFS_H

#include "sph.h"
#include <iostream>
#include <vector>
#include <Eigen/Core>

namespace sph
{

/**
 * @brief independant variables of the problem
 */

class SPH_API Dofs
{
public:
    Eigen::Vector3d x; //< position
    Eigen::Vector3d u; //< velocity
    double rho;        //< density
    Dofs(Eigen::Vector3d const &_x, Eigen::Vector3d const &_u, double _rho) : x(_x), u(_u), rho(_rho) {}

#ifndef SWIG
    friend SPH_API std::ostream &operator<<(std::ostream &out, Dofs const &obj);
#endif
};

} // namespace sph

#endif //WDOFS_H
