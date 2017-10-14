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
#include "IntegerParam.h"

IntegerParam::IntegerParam(ParamEnum id, const std::string &name, int defvalue) : Param(id, name)
{
    this->defvalue = defvalue;
    setToDefault();
}

void 
IntegerParam::setToDefault()
{
    value = defvalue;
}

int 
IntegerParam::getInt() const
{
    return value;
}

void 
IntegerParam::set(int value)
{
    this->value = value;
}

Param * 
IntegerParam::clone() const
{
    return new IntegerParam(*this);
}

void 
IntegerParam::print() const
{
    Param::print();
    std::cout << " " << value;
}

void 
IntegerParam::load(FILE *file)
{
    Param::load(file);
    set(loadInteger(file, true));
}

void 
IntegerParam::save(FILE *file) const
{ 
    Param::save(file);
    saveInteger(file, getInt(), true);
}
