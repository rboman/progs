#ifndef F_ROUTINES_H
#define F_ROUTINES_H

#include "FC.h"

// missing prototypes of FORTRAN functions to be called from C++

extern "C"
{
    void FC_GLOBAL(froutine, FROUTINE)();
}

#endif

