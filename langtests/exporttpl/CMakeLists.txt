# ExportTPL
# sandbox for template instanciation in a DLL

CMAKE_MINIMUM_REQUIRED(VERSION 3.1)
# ----------------------------------------------------------------------------
PROJECT(ExportTPL)
# ----------------------------------------------------------------------------

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

# -- C++11
SET(CMAKE_CXX_STANDARD 11) # newer way to set C++11 (requires cmake>=3.1)
SET(CMAKE_CXX_STANDARD_REQUIRED ON)

# -- Sub directories
ADD_SUBDIRECTORY( mydll )
ADD_SUBDIRECTORY( myexe )
