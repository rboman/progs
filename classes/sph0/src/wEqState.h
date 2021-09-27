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

#ifndef WEQSTATE_H
#define WEQSTATE_H

#include "sph.h"
#include "wObject.h"
#include <iostream>
#include <vector>
#include <memory>

namespace sph
{

/**
 * @brief Virtual class for Equations of state
 */

class SPH_API EqState : public fwk::wSharedObject
{
public:
    EqState();
    virtual ~EqState();

#ifndef SWIG
    virtual void write(std::ostream &out) const override;
#endif
};

/**
 * @brief Ideal gas
 */

class SPH_API IdealGas : public EqState
{
public:
    IdealGas();
    virtual ~IdealGas();

#ifndef SWIG
    virtual void write(std::ostream &out) const override;
#endif
};

/**
 * @brief Quasi incompressible fluid
 */

class SPH_API QIncFluid : public EqState
{
public:
    QIncFluid();
    virtual ~QIncFluid();

#ifndef SWIG
    virtual void write(std::ostream &out) const override;
#endif
};

} // namespace sph

#endif //WEQSTATE_H
