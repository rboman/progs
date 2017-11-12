# cmake -G "Visual Studio 14 Win64" ..
# cmake --build . --config Release

cmake_minimum_required(VERSION 2.8.5 FATAL_ERROR)
if(POLICY CMP0025)
  cmake_policy(SET CMP0025 NEW) # CMake 3.0
endif()
if(POLICY CMP0053)
  cmake_policy(SET CMP0053 NEW) # CMake 3.1
endif()

project (ShowVRML)

find_package(VTK REQUIRED)
vtk_module_config(VTK
    vtkGUISupportQt   
    vtkInteractionWidgets
    vtkFiltersHybrid
    vtkRenderingAnnotation
    vtkIOImport
    vtkIOXML
)
include(${VTK_USE_FILE})

IF(MSVC)
    add_definitions(-D_CRT_SECURE_NO_WARNINGS -D_SCL_SECURE_NO_DEPRECATE)
ENDIF(MSVC)

FILE(GLOB SRCS *.h *.cpp)
add_executable(showVRML MACOSX_BUNDLE ${SRCS})
target_link_libraries(showVRML ${VTK_LIBRARIES})