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

#ifndef T_PARAM_H
#define T_PARAM_H

#include "genmai.h"
#include "Parameters.h"
#include <vector>
#include "Point.h"
#include "Layers.h"

/**
 * @brief Defines the Parameters for the MeshBuilder object, accessors and I/O routines
 */

class GENMAI_API MeshParameters : public Parameters
{
  public:
    MeshParameters();

    MeshParameters(const MeshParameters &obj);
    void operator=(const MeshParameters &obj);
    virtual ~MeshParameters();

    void setToDefault();

    // Get/Set

    Point const &getOrigin() const;
    void setOrigin(const Point &arg);
    void setOriginX(double arg);
    void setOriginY(double arg);

    Point const &getDimension() const;
    void setDimension(const Point &arg);
    void setDimensionX(double arg);
    void setDimensionY(double arg);

    int getNumberOfElementOnX() const;
    void setNumberOfElementOnX(int arg);

    int getNumberOfElementOnY() const;
    void setNumberOfElementOnY(int arg);

    double getReductionCoefficient() const;
    void setReductionCoefficient(double arg);

    int getNumberOfLayers() const;
    LayerType getLayerType(int i) const;
    void addLayer(LayerType t);
};

#include "MeshParameters.inl"

#endif
