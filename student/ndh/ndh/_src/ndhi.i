// SWIG input file of the 'ndh' module

%feature("autodoc","1");

%module(docstring=
"'ndhi' module: NDH project 1996
(c) ULg - A&M"
) ndhi
%{

#include "ndh.h"
#include "BemSolver.h"
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

// ----------- DCM CLASSES ---------------
%include "ndh.h"

%include "BemSolver.h"

