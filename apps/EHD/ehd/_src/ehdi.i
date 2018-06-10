// SWIG input file of the 'ehd' module

%feature("autodoc","1");

%module(docstring=
"'ehdi' module: EHD
(c) ULg - A&M"
) ehdi
%{

#include "ehd.h"

#include <string>
#include <sstream>

%}
/*
%include "std_string.i"
%include "exception.i"

%include "std_vector.i"
// Instantiate some std templates
namespace std {
   %template(std_vector_double) std::vector<double>;
}
*/

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

// ----------- MODULES UTILISES ------------
%import "gaussi.i"
%import "skyi.i"

// ----------- EHD CLASSES ---------------
%include "ehd.h"
