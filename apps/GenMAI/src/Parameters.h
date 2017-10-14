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

#ifndef T_IOOBJECT_H
#define T_IOOBJECT_H

#include <map>
#include <string>
#include "Param.h"

/**
 * @brief Mother class for Parameters (I/O routines)
 */

class Parameters
{
private:
    typedef std::map<ParamEnum, Param*> MyMap ;   // ptr car on utilise fcts virtuelles
    MyMap paramMap;
    
protected:
    void addParam(const Param &param);

public:
    Parameters();

    Parameters(const Parameters &obj);
    void operator=(const Parameters &obj);
    virtual ~Parameters();

    void load(const std::string &file);
    void save(const std::string &file) const;
    void print() const;

    Param const &get(const ParamEnum t) const;
    Param &get(const ParamEnum t);
};

#endif

