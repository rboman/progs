# SPH Louis Goffin

[Reference in ORBi](http://orbi.ulg.ac.be/handle/2268/156166)

![Screenshot](doc/screenshot.png)

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
```
build & run 
```
CXX=g++ FC=gfortran cmake . -B build -DPython3_ROOT_DIR="c:/msys64/mingw64/bin" && cmake --build build && ./run.py --cpp -k 10 tests/julia.py
```
(check that msys version of python is used, otherwise "import _sphw" will fail)

## Win

release
```
cmake . -B build && cmake --build build --config Release && run.py -k 10 --cpp tests\waterdrop.py
```

debug
```
cmake . -B build -DSPH_USE_GUI=ON && cmake --build build --config Debug && python_d run.py -k 10 tests\julia.py --nogui --cpp
```

## Linux

```
cmake . -DCMAKE_BUILD_TYPE=Release -B build && make -C build -j 10 && ./run.py tests/waterdrop.py -k 10 --cpp --nogui --nosave
```

## Nic5

```
module load Python VTK  # required at runtime
module load SWIG Eigen  # required for build
cmake . -B build -DCMAKE_BUILD_TYPE=Release -DSPH_USE_GUI=OFF && make -C build -j 10
```


## Notes résultats / TODO


  - [x] `--nogui`
  - [x] pas de sauvegarde disque des résultats (pour benchs)
  - [x] c++: visu boite domaine
  - [x] démarrer le C++ sans passer par les fichiers fortran
  - [x] c++: visu de tous les champs dans GUI
  - [ ] affichage stats (nb min, max, moy part. dans les cellules.)
  - [ ] domaine non cubique 
  - [ ] sauvegarde direct en `.vtp`
  - [ ] g_timers en fortran
  - [x] imprimer parametres de Model au démarrage
  - [ ] faire un "print" des autres objets
  - [ ] fortran: ETA
  - [ ] ETA en hms si nécessaire
  - [ ] supprimer fichier `paths.txt`
  - [ ] fortran: faire plusieurs modules
  - [ ] implémenter Euler
  - [ ] probes: a given particle through time
  - [ ] probes: a field evaluated along a line at regular intervals
  - [ ] probes: max, min, dDx, Dy, Dy
  - [ ] probes: dt
  - [ ] initial pressure: p0(z) = rho g z

