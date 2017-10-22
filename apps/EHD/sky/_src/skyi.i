// SWIG input file of the 'sky' module

%feature("autodoc","1");

%module(docstring=
"'skyi' module: SKYLIB
(c) ULg - A&M"
) skyi
%{

#include "sky.h"
#include "skylib.h"
#include "TdiMat.h"
#include "mlab.h"

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
%include "sky.h"

%include "skylib.h"
%include "TdiMat.h"
%include "mlab.h"
