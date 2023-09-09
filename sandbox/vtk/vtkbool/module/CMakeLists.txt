set(classes vtkBooleanOperationPolyDataFilter71 vtkIntersectionPolyDataFilter71)

# set(srcs
#     ../vtkBooleanOperationPolyDataFilter71.cxx
#     ../vtkIntersectionPolyDataFilter71.cxx)

# set(headers
#     ../vtkBooleanOperationPolyDataFilter71.h
#     ../vtkIntersectionPolyDataFilter71.h)

vtk_module_add_module(vtkBool
CLASSES ${classes}
)
