#ifndef LEAK_H
#define LEAK_H

#include <Python.h>

class ObjA
{
    PyObject *pyFct;

public:
    ObjA(PyObject *_pyFct);
    ~ObjA();
};

#endif //LEAK_H