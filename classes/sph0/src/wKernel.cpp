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

#include "wKernel.h"
using namespace sph;

Kernel::Kernel()
{
}

Kernel::~Kernel()
{
}

void Kernel::write(std::ostream &out) const
{
    out << "sph::Kernel";
}

// -----------------------------------------------------------

CubicSplineKernel::CubicSplineKernel() : Kernel()
{
}

CubicSplineKernel::~CubicSplineKernel()
{
    std::cout << "~CubicSplineKernel()\n";
}

void CubicSplineKernel::write(std::ostream &out) const
{
    out << "sph::CubicSplineKernel";
}

double
CubicSplineKernel::value(double r, double h) const
{
    double h2 = h * h;
    double h3 = h2 * h;
    double alphad = 3.0 / (2.0 * M_PI * h3);
    double rh = r / h;

    if (rh < 1.0)
        return alphad * (3.0 / 2.0 - rh * rh + 0.5 * rh * rh * rh);
    else if (rh < 2.0)
        return alphad * (1.0 / 6.0 * pow(1.0 - rh, 3));
    else
        return 0.0;
}

double
CubicSplineKernel::grad(double r, double h) const
{
    double h2 = h * h;
    double h3 = h2 * h;
    double alphad = 3.0 / (2.0 * M_PI * h3);
    double rh = r / h;

    if (rh < 1.0)
        return alphad / h * (3.0 / 2.0 * rh * rh - 2.0 * rh);
    else if (rh < 2.0)
        return alphad / h * (-0.5 * (2.0 - rh) * (2.0 - rh));
    else
        return 0.0;
}

// -----------------------------------------------------------

QuadraticKernel::QuadraticKernel() : Kernel()
{
}

QuadraticKernel::~QuadraticKernel()
{
    std::cout << "~QuadraticKernel()\n";
}

void QuadraticKernel::write(std::ostream &out) const
{
    out << "sph::QuadraticKernel";
}

double
QuadraticKernel::value(double r, double h) const
{
    double h2 = h * h;
    double h3 = h2 * h;
    double alphad = 5.0 / (4.0 * M_PI * h3);
    double rh = r / h;

    if (rh < 2.0)
        return alphad * (3.0 / 16.0 * rh * rh - 3.0 / 4.0 * rh + 3.0 / 4.0);
    else
        return 0.0;
}

double
QuadraticKernel::grad(double r, double h) const
{
    double h2 = h * h;
    double h3 = h2 * h;
    double alphad = 5.0 / (4.0 * M_PI * h3);
    double rh = r / h;

    if (rh < 2.0)
        return alphad / h * (3.0 / 8.0 * rh - 3.0 / 4.0);
    else
        return 0.0;
}

// -----------------------------------------------------------

QuinticSplineKernel::QuinticSplineKernel() : Kernel()
{
}

QuinticSplineKernel::~QuinticSplineKernel()
{
    std::cout << "~QuinticSplineKernel()\n";
}

void QuinticSplineKernel::write(std::ostream &out) const
{
    out << "sph::QuinticSplineKernel";
}

double
QuinticSplineKernel::value(double r, double h) const
{
    double h2 = h * h;
    double h3 = h2 * h;
    double alphad = 3.0 / (359.0 * M_PI * h3);
    double rh = r / h;

    if (rh < 1.0)
        return alphad * (pow(3.0 - rh, 5) - 6.0 * pow(2.0 - rh, 5) + 15.0 * pow(1.0 - rh, 5));
    else if (rh < 2.0)
        return alphad * (pow(3.0 - rh, 5) - 6.0 * pow(2.0 - rh, 5));
    else if (rh < 3.0)
        return alphad * (pow(3.0 - rh, 5));
    else
        return 0.0;
}

double
QuinticSplineKernel::grad(double r, double h) const
{
    double h2 = h * h;
    double h3 = h2 * h;
    double alphad = 3.0 / (359.0 * M_PI * h3);
    double rh = r / h;

    if (rh < 1.0)
        return alphad / h * (-5.0 * pow(3.0 - rh, 4) + 30.0 * pow(2.0 - rh, 4) - 75.0 * pow(1.0 - rh, 4));
    else if (rh < 2.0)
        return alphad / h * (-5.0 * pow(3.0 - rh, 4) + 30.0 * pow(2.0 - rh, 4));
    else if (rh < 3.0)
        return alphad / h * (-5.0 * pow(3.0 - rh, 4));
    else
        return 0.0;
}
