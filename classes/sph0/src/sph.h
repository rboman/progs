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

// global header of the sph module

#ifndef SPH_H
#define SPH_H

#if defined(WIN32)
#ifdef sph_EXPORTS
#define SPH_API __declspec(dllexport)
#else
#define SPH_API __declspec(dllimport)
#endif
#else
#define SPH_API
#endif

#include "tbox.h"
#include <math.h>

namespace sph
{
class Dofs;
class Problem;
class Particle;
class FixedParticle;
class MobileParticle;
class EqState;
class IdealGas;
class QIncFluid;
class Kernel;
class CubicSplineKernel;
class QuadraticKernel;
class QuinticSplineKernel;
class DisplayHook;
} // namespace sph

#endif //SPH_H
