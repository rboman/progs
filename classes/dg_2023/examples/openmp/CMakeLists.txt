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

# increase warning level
INCLUDE(CheckCXXCompilerFlag)
CHECK_CXX_COMPILER_FLAG("-Wall" WALL)
IF(WALL)
    ADD_COMPILE_OPTIONS(-Wall)
ENDIF()

# find OpenMP
FIND_PACKAGE(OpenMP REQUIRED)
MESSAGE (STATUS "OpenMP enabled (CXX_FLAGS=${OpenMP_CXX_FLAGS})")

# ------------------------------------------------------------------------------

ADD_EXECUTABLE(omp_hello omp_hello.cpp)
TARGET_LINK_LIBRARIES(omp_hello PUBLIC OpenMP::OpenMP_CXX)
