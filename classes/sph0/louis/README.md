# SPH Louis Goffin

[Reference in ORBi](http://orbi.ulg.ac.be/handle/2268/156166)

![Screenshot](screenshot.png)

## Compilation (Windows or Linux)

```bash
build.py
```
requires [Python](https://www.python.org/), [CMake](https://cmake.org/) and a fortran compiler (either [Intel](https://software.intel.com/en-us/fortran-compilers) or [gfortran](https://gcc.gnu.org/fortran/))

## Run

```bash
run.py -k 12 tests/waterdrop.py
```
Results go to `workspace/test_waterdrop`. Conversion to paraview requires [VTK](http://www.vtk.org/).

Load `.vtu` files in [Paraview](http://www.paraview.org/).
