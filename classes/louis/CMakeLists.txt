CMAKE_MINIMUM_REQUIRED(VERSION 3.18...3.22)
PROJECT(LOUIS_CPP 
        VERSION 0.1
        DESCRIPTION "Rewriting of the SPH code of Louis GOFFIN in C++"
        HOMEPAGE_URL "https://github.com/rboman/progs"
        LANGUAGES CXX)
INCLUDE(CMakePrintHelpers)

SET(CMAKE_EXPORT_COMPILE_COMMANDS ON) # creates "compile_commands.json"


# send all binaries to ./bin
SET(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin CACHE PATH "")
SET(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin CACHE PATH "")
MARK_AS_ADVANCED(LIBRARY_OUTPUT_PATH EXECUTABLE_OUTPUT_PATH)


# build type is "" by default in Linux
IF(NOT CMAKE_BUILD_TYPE)
    SET( CMAKE_BUILD_TYPE "Release" CACHE STRING "" FORCE)
ENDIF()

IF(CMAKE_CXX_COMPILER_ID MATCHES "MSVC")
    SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /MP")  # parallel build with MSVC
    ADD_DEFINITIONS(-D_USE_MATH_DEFINES) # for M_PI
ENDIF()

# enable C++17
SET(CMAKE_CXX_STANDARD 17)
SET(CMAKE_CXX_STANDARD_REQUIRED ON)

# increase warning level
INCLUDE(CheckCXXCompilerFlag)
CHECK_CXX_COMPILER_FLAG("-Wall" WALL)
IF(WALL AND NOT MSVC)
    ADD_COMPILE_OPTIONS(-Wall)
ENDIF()

# Python
# use Python3_ROOT_DIR if wrong python found (e.g. anaconda)
FIND_PACKAGE(Python3 COMPONENTS Interpreter Development)
cmake_print_variables(Python3_EXECUTABLE Python3_LIBRARIES 
                      Python3_INCLUDE_DIRS Python3_VERSION)

MACRO(MACRO_DebugPostfix libname)
    IF(MSVC)
        SET_TARGET_PROPERTIES(${libname} PROPERTIES DEBUG_POSTFIX "_d")
    ENDIF(MSVC)
ENDMACRO()

ADD_SUBDIRECTORY( src_f )
ADD_SUBDIRECTORY( sph )
