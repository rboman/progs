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

#ifndef WKERNEL_H
#define WKERNEL_H

#include "sph.h"
#include "wObject.h"
#include <iostream>
#include <vector>
#include <memory>

namespace sph
{

/**
 * @brief virtual Kernel class
 */

class SPH_API Kernel : public fwk::wSharedObject
{
public:
    Kernel();
    virtual ~Kernel();
    virtual double kappa() const = 0;

    virtual double value(double r, double h) const = 0;
    virtual double grad(double r, double h) const = 0;

#ifndef SWIG
    virtual void write(std::ostream &out) const override;
#endif
};

/**
 * @brief Cubic spline kernel
 */

class SPH_API CubicSplineKernel : public Kernel
{
public:
    CubicSplineKernel();
    virtual ~CubicSplineKernel();
    virtual double kappa() const override { return 2.0; }
    virtual double value(double r, double h) const override;
    virtual double grad(double r, double h) const override;

#ifndef SWIG
    virtual void write(std::ostream &out) const override;
#endif
};

/**
 * @brief Quadratic kernel
 */

class SPH_API QuadraticKernel : public Kernel
{
public:
    QuadraticKernel();
    virtual ~QuadraticKernel();
    virtual double kappa() const override { return 2.0; }
    virtual double value(double r, double h) const override;
    virtual double grad(double r, double h) const override;

#ifndef SWIG
    virtual void write(std::ostream &out) const override;
#endif
};

/**
 * @brief Quintic spline kernel
 */

class SPH_API QuinticSplineKernel : public Kernel
{
public:
    QuinticSplineKernel();
    virtual ~QuinticSplineKernel();
    virtual double kappa() const override { return 3.0; }
    virtual double value(double r, double h) const override;
    virtual double grad(double r, double h) const override;

#ifndef SWIG
    virtual void write(std::ostream &out) const override;
#endif
};

} // namespace sph

#endif //WKERNEL_H
