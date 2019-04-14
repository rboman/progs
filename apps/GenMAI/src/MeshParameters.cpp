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
#include "DoubleParam.h"
#include "IntegerParam.h"
#include "PointParam.h"
#include "LayersParam.h"

/**
 * @brief Constructor
 */

MeshParameters::MeshParameters()
{
    setToDefault();
}

MeshParameters::MeshParameters(const MeshParameters &obj) : Parameters(obj)
{
}

void MeshParameters::operator=(const MeshParameters &obj)
{
    Parameters::operator=(obj);
}

MeshParameters::~MeshParameters()
{
}

/**
 * @brief Sets the parameters to the default values
 */

void MeshParameters::setToDefault()
{
    addParam(PointParam(P_ORIG, "Origin", -2.0, -0.5175 / 2.0));
    addParam(PointParam(P_DIM, "Dimension", 2.0, 0.5175 / 2.0));
    addParam(IntegerParam(P_NOX, "Number Of Element On X", 40));
    addParam(IntegerParam(P_NBM, "Number Of Element On Y", 2));
    addParam(DoubleParam(P_COEF, "Reduction Coefficient", 5.0));
    addParam(LayersParam(P_TYPE, "Layers Type", REDUCTION));

    addLayer(REDUCTION);
    addLayer(REDUCTION);
    addLayer(REDUCTION);
    addLayer(CONSTANT);
}

/**
 * @brief Adds a Layer of a given type to the list of Layers
 */

void MeshParameters::addLayer(LayerType t)
{
    get(P_TYPE).add(t);
}
