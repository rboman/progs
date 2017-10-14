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

#include "Global.h"
#include "LayersParam.h"
#include "Layers.h"

LayersParam::LayersParam(ParamEnum id, const std::string &name, 
                         LayerType defvalue) : Param(id, name), defvalue(defvalue)
{
    setToDefault();
}

void 
LayersParam::setToDefault()
{
    value.resize(0);
}

LayerType
LayersParam::getLayer(int i) const
{
    return value[i];
}

void 
LayersParam::add(LayerType value)
{
    this->value.push_back(value);
}

Param * 
LayersParam::clone() const
{
    return new LayersParam(*this);
}

size_t 
LayersParam::size() const
{
    return this->value.size();
}

void 
LayersParam::print() const
{
    Param::print();
    std::cout << std::endl;

    int i;
    for(i=0; i<value.size(); ++i)
    {
        std::cout << "   couche #" << i+1 << " :";
        std::cout << Layers::Instance()->get(getLayer(i)).getName().c_str();
        std::cout << std::endl;
    }
    std::cout << std::endl;
}

void 
LayersParam::load(FILE *file)
{
    Param::load(file);
    int siz = loadInteger(file, true );

    setToDefault();
    int i;
    for(i=0; i<siz; ++i)
    {
       int tmp = loadInteger(file, true );
       add(tmp? REDUCTION : CONSTANT);
    }
}

void 
LayersParam::save(FILE *file) const
{ 
    Param::save(file);
    saveInteger(file, size(), true);

    for(auto i=0; i<size(); ++i)
       saveInteger(file, getLayer(i), true );
}
