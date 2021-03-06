CMAKE_MINIMUM_REQUIRED(VERSION 3.1)
IF(POLICY CMP0025)
  CMAKE_POLICY(SET CMP0025 NEW) # CMake 3.0
ENDIF()
IF(POLICY CMP0053)
  CMAKE_POLICY(SET CMP0053 NEW) # CMake 3.1
ENDIF()

PROJECT(ShowVRML)

FIND_PACKAGE(VTK REQUIRED)
vtk_module_config(VTK
    vtkGUISupportQt   
    vtkInteractionWidgets
    vtkFiltersHybrid
    vtkRenderingAnnotation
    vtkIOImport
    vtkIOXML
)
INCLUDE(${VTK_USE_FILE})

IF(MSVC)
    ADD_DEFINITIONS(-D_CRT_SECURE_NO_WARNINGS -D_SCL_SECURE_NO_DEPRECATE)
ENDIF()

FILE(GLOB SRCS *.h *.cpp)
ADD_EXECUTABLE(showVRML MACOSX_BUNDLE ${SRCS})
TARGET_LINK_LIBRARIES(showVRML ${VTK_LIBRARIES})