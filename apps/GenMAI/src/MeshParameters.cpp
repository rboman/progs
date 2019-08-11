//   Copyright 2003-2017 Romain Boman
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//       http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.

#include "MeshParameters.h"

MeshParameters::MeshParameters()
{
    setToDefault();
}

/**
 * @brief Sets the parameters to the default values
 */

void MeshParameters::setToDefault()
{
    origin.x = -2.0;
    origin.y = -0.5175 / 2.0;
    dimension.x = 2.0;
    dimension.y = 0.5175 / 2.0;
    numberOfElementOnX = 40;
    numberOfElementOnY = 2;
    reductionCoefficient = 5.0;

    layers.push_back(REDUCTION);
    layers.push_back(REDUCTION);
    layers.push_back(REDUCTION);
    layers.push_back(CONSTANT);
}
