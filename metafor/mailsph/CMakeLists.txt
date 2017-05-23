# cmake -G "Visual Studio 14 Win64" ..
# cmake --build . --config Release

cmake_minimum_required(VERSION 2.8.5 FATAL_ERROR)
if(POLICY CMP0020)
    cmake_policy(SET CMP0020 NEW)   # Automatically link Qt executables to qtmain target on Windows. (OLD=NO)
endif()
if(POLICY CMP0025)
  cmake_policy(SET CMP0025 NEW)     # Compiler id for Apple Clang is now AppleClang.
endif()
if(POLICY CMP0053)
    cmake_policy(SET CMP0053 NEW)   # Simplify variable reference and escape sequence evaluation
endif()

project (MailSph)

include(CMakeToolsHelpers OPTIONAL)  # integration VSCode + CMake

include(CheckCXXCompilerFlag)
check_cxx_compiler_flag("-std=c++11" STD11CHECK)
IF(STD11CHECK)
    ADD_DEFINITIONS(-std=c++11 )
ENDIF()

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

IF(MSVC)
    add_definitions(-D_CRT_SECURE_NO_WARNINGS -D_SCL_SECURE_NO_DEPRECATE)
ENDIF(MSVC)

FILE(GLOB SRCS *.h *.cpp)
add_executable(mailsph MACOSX_BUNDLE ${SRCS})
target_link_libraries(mailsph ${VTK_LIBRARIES})
