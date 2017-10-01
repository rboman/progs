// SWIG input file of the 'dcm' module

%feature("autodoc","1");

%module(docstring=
"'dcmi' module: DCM project 1994
(c) ULg - A&M"
) dcmi
%{

#include "dcm.h"
#include "Polynome.h"
#include "Plane.h"
#include <string>
#include <sstream>

%}

%include "std_string.i"
%include "exception.i"

// ----------- DCM CLASSES ---------------
%include "dcm.h"
%include "Polynome.h"
%include "Plane.h"

%extend dcm::Polynome {
    std::string __str__() {
        std::stringstream str; str << *self;
        return str.str();
    }
}
