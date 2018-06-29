#!/bin/bash

# compile
mkdir build
cd build
cmake ..
make -j2

# run tests
ctest --verbose
