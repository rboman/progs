# TFE

This is the code related to my final year project about iterative solvers.

# windows

I make it work the fortran/python from msys2/mingw64

- install msys2
- double click on `mingw64.exe`
- type:
```
pacman -Ss gfortran | grep mingw64     # <= show available packages
pacman -S mingw64/mingw-w64-x86_64-gcc-libgfortran  # install
```
- you will also need:
    - `mingw64/mingw-w64-x86_64-python2-numpy`
    - `mingw64/mingw-w64-x86_64-python2-scipy`
    - `mingw64/mingw-w64-x86_64-python2-matplotlib`
    - `mingw64/mingw-w64-x86_64-python2-enum34`    <= missing dep. of matplotlib/Qt

If you have another `python` associated to `.py`, do not forget to run the scripts with `python script.py` instead of `script.py`

# run the solvers

```
cd BiCG
mkdir build
cd build
cmake ..
make
cd ..
python ../examples/savspar.py    # <= generates a system and solves it using scipy
build/exe/test_bicg
```
