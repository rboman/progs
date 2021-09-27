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

#ifndef WDISPLAYHOOK_H
#define WDISPLAYHOOK_H

#include "sph.h"
#include "wObject.h"
#include <vector>

namespace sph
{

/**
 * @brief Dispay Hook - links with the python GUI
 */

class SPH_API DisplayHook : public fwk::wObject
{
public:
    DisplayHook();
    virtual void display(int nt, double t, std::vector<double> &u);
    virtual void refresh();
};

} // namespace sph

#endif //WDISPLAYHOOK_H
