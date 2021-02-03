CMAKE_MINIMUM_REQUIRED(VERSION 3.1)
IF(POLICY CMP0020)
    CMAKE_POLICY(SET CMP0020 NEW)   # Automatically link Qt executables to qtmain target on Windows. (OLD=NO)
ENDIF()
IF(POLICY CMP0025)
  CMAKE_POLICY(SET CMP0025 NEW)     # Compiler id for Apple Clang is now AppleClang.
ENDIF()
IF(POLICY CMP0053)
    CMAKE_POLICY(SET CMP0053 NEW)   # Simplify variable reference and escape sequence evaluation
ENDIF()

PROJECT(MailSph)

INCLUDE(CMakeToolsHelpers OPTIONAL)  # integration VSCode + CMake

# -- C++11
SET(CMAKE_CXX_STANDARD 11)
SET(CMAKE_CXX_STANDARD_REQUIRED ON)

FIND_PACKAGE(VTK REQUIRED)
vtk_module_config(VTK
    vtkGUISupportQt   
    vtkInteractionWidgets
    vtkFiltersHybrid
    vtkRenderingAnnotation
    vtkIOExport
    vtkIOXML
)
INCLUDE(${VTK_USE_FILE})

IF(MSVC)
    ADD_DEFINITIONS(-D_CRT_SECURE_NO_WARNINGS -D_SCL_SECURE_NO_DEPRECATE)
    SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /MP")  # parallel build with MSVC
ENDIF()

FILE(GLOB SRCS *.h *.cpp)
ADD_EXECUTABLE(mailsph MACOSX_BUNDLE ${SRCS})
TARGET_LINK_LIBRARIES(mailsph ${VTK_LIBRARIES})
