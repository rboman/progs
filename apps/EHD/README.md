# EHD

1D Elasto-Hydro-Dynamic simulation. This old code was part of my PhD research, written in 1998-99.
It solves the Reynolds' equation using cubic finite elements.
Inspired from [Violaine Cahouet's PhD thesis](http://www.theses.fr/1997ISAL0054) "Contribution à l'étude de la lubrification plasto-hydrodynamique" (INSA Lyon, 1997). 

Also included:
  * a basic skyline solver packaged in a library,
  * a basic set of Gauss integration routines (in a library too).

## Compilation

This code could be built on Linux, Windows and macOS. 

### Dependencies
  * a C++ compiler: Visual Studio, mingw, g++, clang.
  * for the Python interface: [SWIG](http://www.swig.org/), [Python 3](https://www.python.org/)
  * build/test system: [CMake/CTest](https://cmake.org/)

### CMake
```
mkdir build
cd build
cmake -A x64 ..
cmake --build . --config Release
ctest -C Release
```

On Windows, recent Python installations from `pymanager` may not provide
`python314_d.lib` or `python314_d.dll`. To debug the C++ code called from a
release Python interpreter, use the `PyRelDebug` configuration:

```
cmake -S . -B build -A x64
cmake --build build --config PyRelDebug
ctest --test-dir build -C PyRelDebug --output-on-failure
```

`PyRelDebug` builds the C++ code with debug symbols and no optimization, but
keeps the release MSVC runtime and links against the release Python library.
The regular `Debug` configuration is only useful with a matching debug build of
Python.

You may also double click on `build.py` and cross your fingers.

## Run a test
```
run.py ehd\tests\test1.py
```

To run a test against a specific CMake configuration:

```
python run.py --config PyRelDebug ehd\tests\test1.py
```

## Notes/TODO

  * convert to (real) C++
  * plot results using [matplotlib](https://matplotlib.org/)
  * Check the sign error in "C_2" matrix (?)
  * Use more python in the tests
