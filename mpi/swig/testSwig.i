%module testSwig
%{
#include "testSwig.h"
%}

%include mpi4py/mpi4py.i
%mpi4py_typemap(Comm, MPI_Comm);

%include "testSwig.h"



