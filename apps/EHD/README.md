# EHD

1D Elasto-Hydro-Dynamic simulation. This old code was part of my PhD research, written in 1998-99.
Solves the Reynolds' equation using cubic finite elements.
Inspired from [Vilaine Cahouet's PhD thesis](http://www.theses.fr/1997ISAL0054) "Contribution à l'étude de la lubrification plasto-hydrodynamique" (INSA Lyon, 1997). 

Also included:
  * a basic skyline solver packaged in a library,
  * a basic Gauss integration routines (in a library too).

## Compilation

This code should compile on Linux, Windows and macOS. 

### Dependencies
  * a C++ compiler: Visual Studio, mingw, g++, clang.
  * for the Python interface: [SWIG](http://www.swig.org/), [Python 2.7](https://www.python.org/)
  * build system: [CMake](https://cmake.org/)

### CMake
```
mkdir build
cd build
cmake -A x64 ..
cmake --build. --config Release
ctest
```
You may also double click on `build_and_run.py` and cross your fingers.

## Notes/TODO

  * convert to (real) C++
  * plot results using matplotlib
  * sign error in C_2 matrix?
