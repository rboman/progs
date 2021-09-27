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

#include "wDofs.h"
using namespace sph;

namespace sph
{
SPH_API std::ostream &
operator<<(std::ostream &out, Dofs const &obj)
{
    out << "x=" << obj.x.transpose() << " v=" << obj.u.transpose() << " rho=" << obj.rho;
    return out;
}
} // namespace sph
