#!/bin/bash

# compile
rm -rf build
mkdir build
cd build
cmake ..
make -j2

# run tests
#ctest --verbose
