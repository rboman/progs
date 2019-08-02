//   Copyright 1996-2017 Romain Boman
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

#ifndef GRAPH_H
#define GRAPH_H

#include "ndh.h"

namespace ndh
{

NDH_API void titre();
NDH_API void ligne(double x1, double y1, double x2, double y2);
NDH_API void dot(double x1, double y1);
NDH_API void dot2(double x1, double y1);
NDH_API void visu();

} // namespace ndh

#endif //GRAPH_H
