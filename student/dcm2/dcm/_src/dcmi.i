// SWIG input file of the 'dcm' module

%feature("autodoc","1");

%module(docstring=
"'dcmi' module: DCM project 1994
(c) ULg - A&M",
directors="1",
threads="1"
) dcmi
%{

#include "dcm.h"
#include "Polynome.h"
#include "Plane.h"

%}

// ----------- DCM CLASSES ---------------
%include "dcm.h"
%include "Polynome.h"
%include "Plane.h"
