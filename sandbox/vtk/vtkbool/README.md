# vtkbool

vtkBooleanOperationPolyDataFilter from VTK 7.1

The one from VTK is broken from [vtkIntersectionPolyDataFilter: use vtkPolygon::BoundedTriangulate · Kitware/VTK@38c2ba6 · GitHub](https://github.com/Kitware/VTK/commit/38c2ba64c955403ce1ec314a04d592ab87ca0bbe)

Build system adapted from 
* [GitHub - zippy84/vtkbool: A new boolean operations filter for VTK](https://github.com/zippy84/vtkbool)
* [Examples/Modules/Wrapping · master · VTK / VTK · GitLab](https://gitlab.kitware.com/vtk/vtk/-/tree/master/Examples/Modules/Wrapping)

```
mkdir build
cd build
cmake .. && cmake --build . --config release
..\test_vtkbool.py
```

TODO:
* [x] debug
* [x] linux
* [x] make install

