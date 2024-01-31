CMAKE_MINIMUM_REQUIRED(VERSION 3.18)
PROJECT(SOLVER CXX)
INCLUDE(CMakePrintHelpers)

# ------------------------------------------------------------------------------
# Find libraries and setup compiler
# ------------------------------------------------------------------------------

# change "Debug" => "Release" for a faster executable (but less runtime checks)!
IF(NOT CMAKE_BUILD_TYPE)
    SET(CMAKE_BUILD_TYPE "Debug" CACHE STRING "" FORCE)
ENDIF()

# enable C++17
SET(CMAKE_CXX_STANDARD 17)
SET(CMAKE_CXX_STANDARD_REQUIRED ON)

IF(APPLE)
    # on macOS, do not give priority to frameworks/apps
    SET(CMAKE_FIND_APPBUNDLE LAST)
    SET(CMAKE_FIND_FRAMEWORK LAST)
    # target x86 arch even on Apple M1 (avoid arm64)
    # SET(CMAKE_OSX_ARCHITECTURES "x86_64" CACHE STRING "Build architectures for Mac OS X" FORCE)
ENDIF()

# find gmsh-sdk
# gmsh.h
FIND_PATH(GMSH_INCLUDE_DIRS NAMES "gmsh.h" REQUIRED)
cmake_print_variables(GMSH_INCLUDE_DIRS)

# libgmsh.so
FIND_LIBRARY(GMSH_LIBRARIES gmsh REQUIRED)
cmake_print_variables(GMSH_LIBRARIES)

# find Eigen
FIND_PATH(EIGEN_INCLUDE_DIRS "Eigen/Dense"
          PATHS "${PROJECT_SOURCE_DIR}/lib/eigen" "/usr/include/eigen3" REQUIRED)
cmake_print_variables(EIGEN_INCLUDE_DIRS)


# increase warning level
INCLUDE(CheckCXXCompilerFlag)
CHECK_CXX_COMPILER_FLAG("-Wall" WALL)
IF(WALL)
    ADD_COMPILE_OPTIONS(-Wall)
ENDIF()

# find OpenMP
FIND_PACKAGE(OpenMP)
IF(OPENMP_FOUND)
    MESSAGE (STATUS "OpenMP enabled (CXX_FLAGS=${OpenMP_CXX_FLAGS})")
ELSE()
    MESSAGE (STATUS "OpenMP disabled.")
ENDIF()

# ------------------------------------------------------------------------------

FILE(GLOB SRCS *.cpp)
ADD_EXECUTABLE(solver ${SRCS})
TARGET_INCLUDE_DIRECTORIES(solver PUBLIC ${EIGEN_INCLUDE_DIRS}
                                         ${GMSH_INCLUDE_DIRS})
TARGET_LINK_LIBRARIES(solver PUBLIC ${GMSH_LIBRARIES})
IF(OpenMP_CXX_FOUND)
    TARGET_LINK_LIBRARIES(solver PUBLIC OpenMP::OpenMP_CXX)
ENDIF()
