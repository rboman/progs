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

%module genmai
%{

// utils --
#include "Point.h"
#include "PolarPoint.h"
#include "Arc.h"
#include "Line.h"
#include "Element.h"

// builders --
#include "Mesh.h"
#include "MeshBuilder.h"
#include "Tool.h"
#include "ToolBuilder.h"

%}

%ignore operator<<;
%ignore *::operator=;
%ignore operator*;
%ignore Point::operator*(double, const Point &);
%ignore Point::atan2;
%ignore Point::cosin;

%rename(output) print; // Rename all `print' functions to `output'

// utils 

%include "genmai.h"
%include "Point.h"
%include "PolarPoint.h"
%include "Curve.h"
%include "Arc.h"
%include "Line.h"
%include "Element.h"
%include "LayerType.h"

// -- gestion des "std::string"

%include "std_string.i"

%include "std_vector.i"
// Instantiate some std templates
namespace std {
   %template(std_vector_LayerType) std::vector<LayerType>;
}

// builders

%include "TargetObject.h"
%include "Mesh.h"
%include "Tool.h"

%include "Builder.h"
%include "MeshBuilder.h"
%include "ToolBuilder.h"
