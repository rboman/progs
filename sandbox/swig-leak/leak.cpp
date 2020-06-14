
#include "leak.h"
#include <iostream>

ObjA::ObjA(PyObject *_pyFct)
{
    std::cout << "+++++ ObjA() " << this << std::endl;
    pyFct = _pyFct;
    Py_XINCREF(pyFct);
}

ObjA::~ObjA()
{
    std::cout << "+++++ ~ObjA() " << this << std::endl;
    if (pyFct)
        Py_XDECREF(pyFct);
}

Base::Base() { std::cout << "+++++ Base() " << this << std::endl; }

Base::~Base() { std::cout << "+++++ ~Base() " << this << std::endl; }

void
Base::fct()
{
    std::cout << "+++++ Base::fct() " << this << std::endl;
}

void
Base::call()
{
    std::cout << "+++++ Base::call() " << this << std::endl;
    this->fct();
}