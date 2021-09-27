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

#include "wTimeIntegration.h"
using namespace sph;

TimeIntegration::TimeIntegration()
{
}

TimeIntegration::~TimeIntegration()
{
    std::cout << "~TimeIntegration()\n";
}

void TimeIntegration::write(std::ostream &out) const
{
    out << "sph::TimeIntegration";
}
