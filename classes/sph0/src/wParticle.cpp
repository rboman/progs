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

#include "wParticle.h"
#include "wDofs.h"
using namespace sph;

Particle::Particle(int _no, Dofs const &_dofs) : no(_no)
{
    dofs.push_back(new Dofs(_dofs));
}

Particle::~Particle()
{
    std::cout << "~Particle()\n";
    for (auto d : dofs)
        delete d;
}

void Particle::write(std::ostream &out) const
{
    out << "p#" << no;
}

// ------------------------------------------------------------------------

FixedParticle::FixedParticle(int _no, Dofs const &_dofs) : Particle(_no, _dofs)
{
}

FixedParticle::~FixedParticle()
{
}

void FixedParticle::write(std::ostream &out) const
{
    out << "pF#" << no;
}

// ------------------------------------------------------------------------

MobileParticle::MobileParticle(int _no, Dofs const &_dofs) : Particle(_no, _dofs)
{
}

MobileParticle::~MobileParticle()
{
}

void MobileParticle::write(std::ostream &out) const
{
    out << "pM#" << no;
}
