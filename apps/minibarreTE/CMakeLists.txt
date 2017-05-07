# $Id: CMakeLists.txt 1086 2013-07-26 14:49:44Z Boman $


PROJECT(minibarreTE)
CMAKE_MINIMUM_REQUIRED(VERSION 2.6)

SET(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin CACHE PATH
                        "Single output directory for building all libraries.")
SET(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin CACHE PATH
                        "Single output directory for building all executables.")
MARK_AS_ADVANCED(LIBRARY_OUTPUT_PATH EXECUTABLE_OUTPUT_PATH)

#LIST(APPEND CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/CMake")

IF(WIN32)
    add_definitions(-D_SCL_SECURE_NO_DEPRECATE)
ENDIF(WIN32)

# -- Search for Python

FIND_PACKAGE(PythonLibs REQUIRED)

# -- Search for SWIG

FIND_PACKAGE(SWIG REQUIRED)
IF(WIN32)
    SET(CMAKE_SWIG_OUTDIR "${EXECUTABLE_OUTPUT_PATH}/$(OutDir)")
ENDIF(WIN32)

# -- Sub directories

ADD_SUBDIRECTORY( couplage )
ADD_SUBDIRECTORY( _couplage )

ADD_SUBDIRECTORY( minib )
ADD_SUBDIRECTORY( etage )

