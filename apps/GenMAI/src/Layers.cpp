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

#include "Global.h"
#include "Layers.h"

Layers *Layers::_instance = NULL;

/**
 * @brief Gets the unique instance object
 */

Layers *
Layers::Instance()
{
    if(_instance==NULL)
        _instance = new Layers();
    return _instance;
}

/**
 * @brief Adds a Layer (used by the constructor)
 */

void 
Layers::addLayer(const Layer &layer)
{
    layerMap[layer.getType()] = layer;
}

/**
 * @brief Private constructor : fills the singleton
 */

Layers::Layers()
{
    addLayer(Layer(CONSTANT,  "Constant"));
    addLayer(Layer(REDUCTION, "Reduction"));
}

/**
 * @brief Main function : gets a Layer objects from the singleton
 */

Layer const &
Layers::get(const LayerType t)
{
    return layerMap[t];
}
