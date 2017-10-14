//   Copyright 2017 Romain Boman
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

#ifndef LAYERSPARAM_H
#define LAYERSPARAM_H

#include "Param.h"
#include <vector>

/**
 * @brief Parameter that is a list of Layer objects
 */

class LayersParam : public Param
{
    LayerType defvalue;              ///< default value
    std::vector<LayerType> value;    ///< current value
public:
    LayersParam(ParamEnum id, const std::string &name="noname", LayerType defvalue=CONSTANT);

    virtual void setToDefault();

    virtual LayerType getLayer(int i) const;
    virtual void      add(LayerType value);
    virtual size_t    size() const;

    virtual Param * clone() const;
    virtual void print() const;
    virtual void load(FILE *file);
    virtual void save(FILE *file) const;
};

#endif
