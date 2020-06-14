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

#ifndef GMMESH_H
#define GMMESH_H

#include "genmai.h"
#include "gmObject.h"
#include <vector>

namespace genmai
{

/**
 * @brief Defines a Mesh (list of Point and list of Element).
 *        A list of contact nodes is also included.
 */

class GENMAI_API Mesh : public Object
{
public:
    int firstContactNode;
    int lastContactNode;

    std::vector<Point *> nodes;
    std::vector<Element *> elements;

public:
    Mesh();

    virtual void write(std::ostream &out) const override;
    virtual void list() const;

    bool isEmpty() const;
    void clear();
};

} // namespace genmai

#endif // GMMESH_H
