
#include "leak.h"
#include <iostream>


ObjA::ObjA(PyObject *_pyFct)
{
    std::cout << "+++++ ObjA() " << this << std::endl;    
    // PyGILState_STATE gstate;
    // gstate = PyGILState_Ensure();

    pyFct = _pyFct;

    Py_XINCREF(pyFct);


    // PyGILState_Release(gstate);
}

ObjA::~ObjA()
{
    std::cout << "+++++ ~ObjA() " << this << std::endl;
    // PyGILState_STATE gstate;
    // gstate = PyGILState_Ensure();

    if(pyFct) Py_XDECREF(pyFct);

    // PyGILState_Release(gstate);
}
