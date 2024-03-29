PROJECT(GMSHAPI CXX)
CMAKE_MINIMUM_REQUIRED(VERSION 3.1)

# put all generated files in a single folder (required in windows for exe linked to dlls)
SET(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin CACHE PATH "")
SET(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin CACHE PATH "")
MARK_AS_ADVANCED(LIBRARY_OUTPUT_PATH EXECUTABLE_OUTPUT_PATH)

# build type is "" by default in Linux
IF(NOT CMAKE_BUILD_TYPE)
    SET( CMAKE_BUILD_TYPE "Release" CACHE STRING "" FORCE)
ENDIF()

# enable C++11
SET(CMAKE_CXX_STANDARD 11) # newer way to set C++11 (requires cmake>3.1)
SET(CMAKE_CXX_STANDARD_REQUIRED ON)

# compiler options
IF(UNIX AND CMAKE_CXX_COMPILER_ID STREQUAL GNU)
    # allows gcc7/ubuntu to link against the c++ interface if it is linked with an incompatible version
    # libgmsh is linked to libgcc4 (pbl with std::string) 
    # (this does not work with mingw)
    #ADD_DEFINITIONS(-D_GLIBCXX_USE_CXX11_ABI=0) # not required with ubuntu18.04/gcc-7
    # (should be tested on CECI clusters)
ELSEIF(CMAKE_CXX_COMPILER_ID MATCHES MSVC)
    ADD_DEFINITIONS(-D_CRT_SECURE_NO_WARNINGS -D_SCL_SECURE_NO_DEPRECATE -D_CRT_NONSTDC_NO_WARNINGS)
    ADD_DEFINITIONS(-D_USE_MATH_DEFINES) # for M_PI
ENDIF()

IF(APPLE)
    # on macOS, do not give priority to frameworks/apps
    SET(CMAKE_FIND_APPBUNDLE LAST)
    SET(CMAKE_FIND_FRAMEWORK LAST)
ENDIF()

# find gmsh-sdk
# gmsh.h
FIND_PATH(GMSH_INCLUDE_DIRS NAMES "gmsh.h")
MESSAGE(STATUS "GMSH_INCLUDE_DIRS=" ${GMSH_INCLUDE_DIRS})
if(NOT GMSH_INCLUDE_DIRS)
    MESSAGE(FATAL_ERROR "gmsh.h not found!")
ENDIF()
INCLUDE_DIRECTORIES(${GMSH_INCLUDE_DIRS})

# libgmsh.so
FIND_LIBRARY(GMSH_LIBRARIES gmsh)
MESSAGE(STATUS "GMSH_LIBRARIES=" ${GMSH_LIBRARIES})
IF(NOT GMSH_LIBRARIES)
    MESSAGE(FATAL_ERROR "gmsh library not found!")
ENDIF()

# gmsh.exe
FIND_PROGRAM(GMSH_EXECUTABLE gmsh)
MESSAGE(STATUS "GMSH_EXECUTABLE=" ${GMSH_EXECUTABLE})
IF(NOT GMSH_EXECUTABLE)
    MESSAGE(FATAL_ERROR "gmsh executable not found!")
ENDIF()

# OpenACC / OpenMP

SET(ACC_OPTIONS "")
IF(CMAKE_CXX_COMPILER_ID STREQUAL PGI) 
    SET(ACC_OPTIONS "-ta=tesla:cc70,managed -Minfo=accel")
ELSEIF(CMAKE_CXX_COMPILER_ID STREQUAL GNU)
    SET(ACC_OPTIONS "-fopenmp")
ENDIF()
SET(CMAKE_CXX_FLAGS  "${CMAKE_CXX_FLAGS} ${ACC_OPTIONS}")
SET(CMAKE_EXE_LINKER_FLAGS  "${CMAKE_EXE_LINKER_FLAGS} ${ACC_OPTIONS}")

ADD_SUBDIRECTORY( srcs )

ENABLE_TESTING()
ADD_SUBDIRECTORY( tests )

ADD_SUBDIRECTORY( sandbox )
ADD_SUBDIRECTORY( samples/invert_matrix )

ADD_SUBDIRECTORY( openacc/example )

