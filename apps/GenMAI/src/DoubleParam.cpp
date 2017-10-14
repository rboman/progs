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
#include "DoubleParam.h"

DoubleParam::DoubleParam(ParamEnum id, const std::string &name, double defvalue) : Param(id, name)
{
    this->defvalue = defvalue;
    setToDefault();
}

void 
DoubleParam::setToDefault()
{
    value = defvalue;
}

double 
DoubleParam::getDouble() const
{
    return value;
}

void 
DoubleParam::set(double value)
{
    this->value = value;
}

Param * 
DoubleParam::clone() const
{
    return new DoubleParam(*this);
}

void 
DoubleParam::print() const
{
    Param::print();
    std::cout << " " << value;
}

void 
DoubleParam::load(FILE *file)
{
    Param::load(file);
    set(loadDouble(file, true));
}

void 
DoubleParam::save(FILE *file) const
{ 
    Param::save(file);
    saveDouble(file, getDouble(), true);
}
