// test leaks with c++ class holding a PyObject*

//%feature("autodoc","1");

%module leak
%{

#include "leak.h"

%}

%include "leak.h"

