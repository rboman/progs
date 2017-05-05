cmake_minimum_required(VERSION 2.8.5 FATAL_ERROR)
if(POLICY CMP0025)
  cmake_policy(SET CMP0025 NEW) # CMake 3.0
endif()
if(POLICY CMP0053)
  cmake_policy(SET CMP0053 NEW) # CMake 3.1
endif()

project (Cylindre)

find_package(VTK REQUIRED)
vtk_module_config(VTK
    vtkGUISupportQt   
    vtkInteractionWidgets
    vtkFiltersHybrid
    vtkRenderingAnnotation
    vtkIOExport
    vtkIOXML
)
include(${VTK_USE_FILE})
add_executable(cylindre MACOSX_BUNDLE cylindre.cpp)
target_link_libraries(cylindre ${VTK_LIBRARIES})
