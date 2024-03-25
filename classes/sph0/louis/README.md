# SPH Louis Goffin

[Reference in ORBi](http://orbi.ulg.ac.be/handle/2268/156166)

![Screenshot](screenshot.png)

## Compilation (Windows or Linux)

```bash
./build.py
```
requires [Python](https://www.python.org/), [CMake](https://cmake.org/) and a fortran compiler (either [Intel](https://software.intel.com/en-us/fortran-compilers) or [gfortran](https://gcc.gnu.org/fortran/))

## Run

```bash
./run.py -k 12 tests/waterdrop.py
```
Results go to `workspace/test_waterdrop`. Conversion to paraview requires [VTK](http://www.vtk.org/).

Load `.vtu` files in [Paraview](http://www.paraview.org/).


## New run

Run C++ code in debug using 10 threads

mingw/msys2 
```
cmake . -B build -DCMAKE_BUILD_TYPE=Debug &&  cmake --build build && run.py --cpp -k 10 tests\waterdrop.py
cmake . -B build -DCMAKE_BUILD_TYPE=Release && cmake --build build && run.py --cpp -k 10 tests\waterdrop.py
```
msvc:
```
cmake . -B build && cmake --build build --config Debug && run.py --cpp -k 10 tests\waterdrop.py
cmake . -B build && cmake --build build --config Release && run.py --cpp -k 10 tests\waterdrop.py
```



## MSYS2

```
pacman -S mingw64/mingw-w64-x86_64-eigen3
pacman -S mingw64/mingw-w64-x86_64-vtk
pacman -S mingw64/mingw-w64-x86_64-python-pyqt6
# libs qui manquent pour "import vtk" sous python (debug avec python3 -c "import vtk" et "dependenciesGUI" jusqu'à ce que ça marche)
pacman -S mingw64/mingw-w64-x86_64-nlohmann-json
pacman -S mingw64/mingw-w64-x86_64-fast_float
pacman -S mingw64/mingw-w64-x86_64-gl2ps
pacman -S mingw64/mingw-w64-x86_64-openvr
pacman -S mingw64/mingw-w64-x86_64-ffmpeg
pacman -S mingw64/mingw-w64-x86_64-python-meshio  # pas sur
pacman -S mingw64/mingw-w64-x86_64-hdf5
pacman -S mingw64/mingw-w64-x86_64-postgresql
pacman -S mingw64/mingw-w64-x86_64-libexodus
pacman -S mingw64/mingw-w64-x86_64-pdal
pacman -S mingw64/mingw-w64-x86_64-openvdb
pacman -S mingw64/mingw-w64-x86_64-unixodbc
pacman -S mingw64/mingw-w64-x86_64-opencascade
pacman -S mingw64/mingw-w64-x86_64-liblas
pacman -S mingw64/mingw-w64-x86_64-seacas
pacman -S mingw64/mingw-w64-x86_64-cgns
pacman -S mingw64/mingw-w64-x86_64-adios2
pacman -S mingw64/mingw-w64-x86_64-openturns
pacman -S mingw64/mingw-w64-x86_64-openslide
CXX=g++ FC=gfortran cmake . -B build && cmake --build build && ./run.py --cpp -k 10 tests/small.py
```

## Win

```
cmake . -B build && cmake --build build --config Release && run.py -k 10 --cpp tests\waterdrop.py
```

## Linux

```
cmake . -DCMAKE_BUILD_TYPE=Release -B build && make -C build -j 10 && ./run.py tests/small.py -k 10 --cpp
```


## Notes résultat code C++

* les particules fixes ont une masse de 0 dans paraview
* la densité des particules fixes est constante et vaut rho0 dans paraview
* max(mu_ab) est nul dans paraview (c'est un bug dans les 2 codes où mu_ab est toujours negatif!)