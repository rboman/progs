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

#ifndef WTIMEINTEGRATION_H
#define WTIMEINTEGRATION_H

#include "sph.h"
#include "wObject.h"
#include <iostream>
#include <vector>
#include <memory>

namespace sph
{

/**
 * @brief main sph TimeIntegration object
 */

class SPH_API TimeIntegration : public fwk::wObject
{
public:
    TimeIntegration();
    virtual ~TimeIntegration();

#ifndef SWIG
    virtual void write(std::ostream &out) const override;
#endif
};

} // namespace sph

#endif //WTIMEINTEGRATION_H
