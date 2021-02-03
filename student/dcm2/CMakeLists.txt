CMAKE_MINIMUM_REQUIRED(VERSION 3.12)
IF(POLICY CMP0078)
cmake_policy(SET CMP0078 OLD) # cmake 3.13 ``UseSWIG`` generates standard target names.
#                             NEW=> target_name devient dcmi au lieu de _dcmi (le "_" devient un prefix)
#                             ...=> MACRO_DebugPostfix(dcmi) au lieu de MACRO_DebugPostfix(_dcmi)
ENDIF()
IF(POLICY CMP0086)
cmake_policy(SET CMP0086 NEW) # cmake 3.14 ``UseSWIG`` honors ``SWIG_MODULE_NAME`` via ``-module`` flag.
#                             OLD/NEW => pas d'incidence
ENDIF()

PROJECT(DCM)

# send all binaries to ./bin
SET(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin CACHE PATH "")
SET(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin CACHE PATH "")
MARK_AS_ADVANCED(LIBRARY_OUTPUT_PATH EXECUTABLE_OUTPUT_PATH)

# on Linux, set optimisation by default
IF(NOT CMAKE_BUILD_TYPE)
    SET(CMAKE_BUILD_TYPE "Release" CACHE STRING "" FORCE)
ENDIF()

# compiler settings
IF(CMAKE_CXX_COMPILER_ID MATCHES "MSVC")
    ADD_DEFINITIONS(-D_CRT_SECURE_NO_WARNINGS -D_SCL_SECURE_NO_DEPRECATE)
    SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /MP")  # parallel build with MSVC
ENDIF()

LIST(APPEND CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/CMake")
INCLUDE(fwkMacros)

# -- GENERAL OPTIONS --

# print some info
MESSAGE(STATUS "CMAKE_SYSTEM_NAME=\"${CMAKE_SYSTEM_NAME}\"")

# -- C++11
SET(CMAKE_CXX_STANDARD 11)
SET(CMAKE_CXX_STANDARD_REQUIRED ON)

# -- Search for Python (requires cmake>=3.12)
# use Python3_ROOT_DIR if wrong python found (e.g. anaconda)
FIND_PACKAGE(Python3 COMPONENTS Interpreter Development)
SET(Python3_EXECUTABLE ${Python3_EXECUTABLE})
SET(Python3_LIBRARIES ${Python3_LIBRARIES})
SET(Python3_INCLUDE_DIRS ${Python3_INCLUDE_DIRS}) 
SET(PYTHONLIBS_VERSION_STRING ${Python3_VERSION})
MESSAGE(STATUS "Python3_EXECUTABLE=${Python3_EXECUTABLE}")
MESSAGE(STATUS "Python3_LIBRARIES=${Python3_LIBRARIES}")
MESSAGE(STATUS "Python3_INCLUDE_DIRS=${Python3_INCLUDE_DIRS}")
MESSAGE(STATUS "Python3_VERSION=${Python3_VERSION}")

# -- Search for SWIG
FIND_PACKAGE(SWIG REQUIRED)
IF(CMAKE_GENERATOR MATCHES "Visual Studio") # not MSVC because of nmake & jom
    SET(CMAKE_SWIG_OUTDIR "${EXECUTABLE_OUTPUT_PATH}/$(Configuration)/")
ELSE()
    SET(CMAKE_SWIG_OUTDIR "${EXECUTABLE_OUTPUT_PATH}")
ENDIF()
SET(CMAKE_SWIG_FLAGS -py3 -O)

# -- CTest
#ENABLE_TESTING()

# -- Sub directories

ADD_SUBDIRECTORY( dcm )
