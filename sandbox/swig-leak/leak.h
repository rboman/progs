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

class Base
{
public:
    Base();
    virtual ~Base();
    virtual void fct();
    void call();
};

#endif //LEAK_H