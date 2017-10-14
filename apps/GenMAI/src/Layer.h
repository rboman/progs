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

#ifndef LAYER_H
#define LAYER_H

#include "Global.h"
#include "LayerType.h"
#include <string>

/**
 * @brief Defines a Layer in the Mesh
 */

class Layer
{
    LayerType type;
    std::string name;
public:
    Layer(const LayerType type=CONSTANT, const std::string &name="");
    LayerType   getType() const;
    std::string const & getName() const;
    bool operator!=(const Layer &l) const;
    bool operator==(const Layer &l) const;
    void operator=(const Layer &l);
};

#include "Layer.inl"

#endif
