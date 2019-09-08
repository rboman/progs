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

#ifndef GMTOOL_H
#define GMTOOL_H

#include "genmai.h"
#include "gmObject.h"
#include <vector>

namespace genmai {

/**
 * @brief Defines a Tool (list of Point and list of Curve). 
 */

class GENMAI_API Tool : public Object
{
public:
    int firstp;
    int firstc;

    std::vector<Point *> points;
    std::vector<Curve *> curves;

public:
    Tool();

    virtual void write(std::ostream &out) const override;
    virtual void list() const;

    bool isEmpty() const;
    void clear();
};

}

#endif //GMTOOL_H
