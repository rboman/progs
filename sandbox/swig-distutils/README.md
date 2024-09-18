Examples from Romain Gaspar

using setup.py
```
swig -c++ -python example.i
./setup.py build_ext --inplace
./test.py
```

using CMake/pybind11
```
sudo apt install python3-pybind11
mkdir build
cd build
cmake .. && make
[pas testé]
```