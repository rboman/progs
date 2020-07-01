CMAKE_MINIMUM_REQUIRED(VERSION 3.1)
PROJECT(Versions)

FIND_PACKAGE(VTK)
vtk_module_config(VTK
    vtkCommonCore
)
INCLUDE(${VTK_USE_FILE})

ADD_EXECUTABLE(vtk_version vtk_version.cpp)
TARGET_LINK_LIBRARIES(vtk_version ${VTK_LIBRARIES})
