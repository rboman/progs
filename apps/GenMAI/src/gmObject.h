//   Copyright 2003-2019 Romain Boman
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

#ifndef GMOBJECT_H
#define GMOBJECT_H

#include "genmai.h"
#include <iostream>

namespace genmai
{

/**
 * @brief Base class of all virtual objects
 *
 *        The only purpose of this class is that it avoids many copy/paste for
 *        the print function (__str__() fct) in SWIG/python
 */

class GENMAI_API Object
{
public:
    Object() = default;
    virtual ~Object() {}
    Object(const Object &) = delete;
    Object &operator=(const Object &) = delete;

#ifndef SWIG
    friend GENMAI_API std::ostream &operator<<(std::ostream &out,
                                               Object const &obj);
    virtual void write(std::ostream &out) const;
#endif
};

} // namespace genmai

#endif // GMOBJECT_H
