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

#ifndef GENMAI_H
#define GENMAI_H

#if defined(WIN32)
#ifdef genmai_EXPORTS
#define GENMAI_API __declspec(dllexport)
#else
#define GENMAI_API __declspec(dllimport)
#endif
#else
#define GENMAI_API
#endif

#define _USE_MATH_DEFINES
#include <math.h>

#pragma warning( disable : 4251)  // DLL/templates non exportes
#pragma warning( disable : 4267)  // size_t => int

// namespace gauss
// {

class Curve;
class Element;
class Object;
class Mesh;
class MeshBuilder;
class Point;
class PolarPoint;
class Tool;
class ToolBuilder;

// }

#endif //GENMAI_H
