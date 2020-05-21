// test leaks with c++ class holding a PyObject*

%module(directors="1") leak
%{

#include "leak.h"

%}

%feature("director") Base;
//%pythonappend Base::Base "self.__disown__()"

%include "leak.h"

