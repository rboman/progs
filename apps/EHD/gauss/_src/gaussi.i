// SWIG input file of the 'gauss' module

%feature("autodoc","1");

%module(docstring=
"'gaussi' module: SKYLIB
(c) ULg - A&M"
) gaussi
%{

#include "gausslib.h"
#include "el.h"
 
#include <string>
#include <sstream>

%}

%include "std_string.i"
%include "exception.i"

%include "std_vector.i"
// Instantiate some std templates
namespace std {
   %template(std_vector_double) std::vector<double>;
}

// ----------- SKY CLASSES ---------------
%include "gauss.h"
%include "gausslib.h"
%include "el.h"
