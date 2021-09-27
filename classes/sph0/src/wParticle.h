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

#ifndef WPARTICLE_H
#define WPARTICLE_H

#include "sph.h"
#include "wObject.h"
#include <iostream>
#include <vector>
#include <memory>

using namespace tbox;

namespace sph
{

/**
 * @brief a particle
 */

class SPH_API Particle : public fwk::wObject
{
public:
    int no;                         //< label (debug/visu)
    std::vector<Dofs *> dofs;       //< variables that are time integrated
    double p;                       //< pressure
    double c;                       //< speed of sound
    std::vector<Particle *> neighs; //< list of neighbours

public:
    Particle(int _no, Dofs const &_dofs);
    virtual ~Particle();

#ifndef SWIG
    virtual void write(std::ostream &out) const override;
#endif
};

/**
 * @brief Fixed particle
 */

class SPH_API FixedParticle : public Particle
{
public:
    FixedParticle(int _no, Dofs const &_dofs);
    virtual ~FixedParticle();

#ifndef SWIG
    virtual void write(std::ostream &out) const override;
#endif
};

/**
 * @brief Mobile particle
 */

class SPH_API MobileParticle : public Particle
{
public:
    MobileParticle(int _no, Dofs const &_dofs);
    virtual ~MobileParticle();

#ifndef SWIG
    virtual void write(std::ostream &out) const override;
#endif
};

} // namespace sph

#endif //WPARTICLE_H
