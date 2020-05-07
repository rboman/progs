// test unicode, swig, python2/3, and python-future

%feature("autodoc","1");

%module unic
%{

#include "unic.h"

%}

%include "std_string.i"
%include "unic.h"

