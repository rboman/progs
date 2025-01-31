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
ENDIF()

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

# Find zlib for compressed output files
# set(CMAKE_FIND_DEBUG_MODE TRUE)
FIND_PACKAGE(ZLIB)

# ------------------------------------------------------------------------------

FILE(GLOB SRCS *.cpp)
ADD_EXECUTABLE(solver ${SRCS})
IF(OpenMP_CXX_FOUND)
    TARGET_LINK_LIBRARIES(solver PUBLIC OpenMP::OpenMP_CXX)
ENDIF()
IF(ZLIB_FOUND)
    TARGET_COMPILE_DEFINITIONS(solver PUBLIC -DUSE_ZLIB)
    TARGET_LINK_LIBRARIES(solver PUBLIC ZLIB::ZLIB)
ENDIF()
