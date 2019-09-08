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

%module genmai
%{

// utils --
#include "Point.h"
#include "PolarPoint.h"
#include "Element.h"
#include "Curve.h"

// builders --
#include "Mesh.h"
#include "MeshBuilder.h"
#include "Tool.h"
#include "ToolBuilder.h"

#include <string>
#include <sstream>

%}

%ignore operator<<;
%ignore *::operator=;
%ignore operator*;
%ignore Point::operator*(double, const Point &);
%ignore Point::atan2;
%ignore Point::cosin;

%include "std_string.i"


// --------- EXCEPTIONS ---------------

%include "exception.i"

// from: http://swig.10945.n7.nabble.com/Trapping-Swig-DirectorException-td6013.html
// le code suivant permet de voir la call stack dans les appels C++ => python

%{ 
   static void handle_exception(void) { 
     try { 
       throw; 
     } catch (std::exception &e) { 
        std::stringstream txt; 
        txt << e.what(); // << ' ' << typeid(e).name();
        PyErr_SetString(PyExc_RuntimeError, e.what()); 
     } 
     catch(...) 
     {
        PyErr_SetString(PyExc_RuntimeError, "Unknown C++ Runtime Error");
     } 
   } 
%} 

%exception { 
   try { 
     $action 
   } catch (...) { 
     // Note that if a director method failed, the Python error indicator 
     // already contains full details of the exception, and it will be 
     // reraised when we go to SWIG_fail; so no need to convert the C++ 
     // exception back to a Python one 
     if (!PyErr_Occurred()) { 
       handle_exception(); 
     } 
     SWIG_fail; 
   } 
} 

%include "genmai.h"
%include "gmObject.h"

%extend Object {
    std::string __str__() {
        std::stringstream str; str << *self;
        return str.str();
    }
}

%include "std_vector.i"
// Instantiate some std templates
namespace std {
   %template(std_vector_int) std::vector<int>;
   %template(std_vector_size_t) std::vector<size_t>;
}


// utils 

%include "Point.h"
%include "PolarPoint.h"
%include "Curve.h"
%include "LayerType.h"
%include "Element.h"

// Instantiate some std templates
namespace std {
   %template(std_vector_LayerType) std::vector<LayerType>;
   %template(std_vector_Point) std::vector<Point *>;
   %template(std_vector_Element) std::vector<Element *>;
   %template(std_vector_Curve) std::vector<Curve *>;
}



// builders

%include "Mesh.h"
%include "Tool.h"

%include "MeshBuilder.h"
%include "ToolBuilder.h"
