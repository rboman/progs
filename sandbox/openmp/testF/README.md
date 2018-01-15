# OpenMP test in Fortran

load the required compiler environment. e.g.

```
devenv-ifort.bat
```

Then,

```
mkdir build
cd build
cmake ..
cmake --build . --config Release
Release\omp_test.exe
```

Example Output:

```
 Hello World from thread =            1
 Hello World from thread =            2
 Hello World from thread =            9
 Hello World from thread =            3
 Hello World from thread =           10
 Hello World from thread =            0
 Hello World from thread =            6
 Number of threads =           12
 Hello World from thread =           11
 Hello World from thread =            5
 Hello World from thread =            4
 Hello World from thread =            8
 Hello World from thread =            7
```

use `set OMP_NUM_THREADS=n` to control the number of threads.