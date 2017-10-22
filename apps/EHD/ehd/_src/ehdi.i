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
// ----------- MODULES UTILISES ------------
%import "gaussi.i"
%import "skyi.i"

// ----------- EHD CLASSES ---------------
%include "ehd.h"
