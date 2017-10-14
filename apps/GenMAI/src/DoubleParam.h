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

#ifndef DOUBLEPARAM_H
#define DOUBLEPARAM_H

#include "Param.h"

/**
 * @brief Parameter with a "double" value
 */

class DoubleParam : public Param
{
    double defvalue;   ///< default value
    double value;      ///< current value
public:
    DoubleParam(ParamEnum id, const std::string &name="noname", double defvalue=0);

    virtual void setToDefault();

    virtual double getDouble() const;
    virtual void set(double value);

    virtual Param * clone() const;
    virtual void print() const;
    virtual void load(FILE *file);
    virtual void save(FILE *file) const;
};

#endif
