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

#ifndef GMLAYERTYPE_H
#define GMLAYERTYPE_H

#include "genmai.h"
#include <iostream>

namespace genmai
{

/**
 * @brief Id for the Layer objects
 */

enum LayerType
{
    CONSTANT = 0,
    REDUCTION
};

GENMAI_API std::ostream &operator<<(std::ostream &o, const LayerType &v);

} // namespace genmai

#endif // GMLAYERTYPE_H
