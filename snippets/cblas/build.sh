#!/bin/bash
# test cblas and lapacke in C and C++

# on ubuntu libcblas.so is merged into libblas.so
#   cblas.h is provided by blas, openblas and atlas.
#   the one from atlas does not have an 'extern "C"' section.
gcc test_cblas_lapake.c -llapacke -lblas

g++ test_cblas_lapake.c -llapacke -lblas
