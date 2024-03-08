CMAKE_MINIMUM_REQUIRED(VERSION 3.18...3.22)
PROJECT(LOUIS_CPP 
        VERSION 0.1
        DESCRIPTION "Rewriting of the SPH code of Louis GOFFIN in C++"
        HOMEPAGE_URL "https://github.com/rboman/progs"
        LANGUAGES CXX)

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
# IF(WALL)
#     ADD_COMPILE_OPTIONS(-Wall)
# ENDIF()

# find OpenMP
FIND_PACKAGE(OpenMP)
IF(OPENMP_FOUND)
    MESSAGE (STATUS "OpenMP enabled (CXX_FLAGS=${OpenMP_CXX_FLAGS})")
ELSE()
    MESSAGE (STATUS "OpenMP disabled.")
ENDIF()

# find Eigen
# FIND_PACKAGE(Eigen3 3.4 REQUIRED NO_MODULE)
# set(CMAKE_FIND_DEBUG_MODE TRUE)
FIND_PATH(EIGEN_INCLUDE_DIRS "Eigen/Dense" 
          PATHS "${PROJECT_SOURCE_DIR}/lib/eigen" "/usr/include/eigen3" "c:/msys64/mingw64/include/eigen3")
MESSAGE(STATUS "EIGEN_INCLUDE_DIRS=" ${EIGEN_INCLUDE_DIRS})
IF(NOT EIGEN_INCLUDE_DIRS)
    MESSAGE(FATAL_ERROR "Eigen include dir not found!")
ENDIF()


FILE(GLOB SRCS *.h *.cpp)
ADD_EXECUTABLE(louis++ ${SRCS})
TARGET_INCLUDE_DIRECTORIES(louis++ PUBLIC ${EIGEN_INCLUDE_DIRS})
#TARGET_LINK_LIBRARIES(louis++ Eigen3::Eigen)
IF(OpenMP_CXX_FOUND)
    TARGET_LINK_LIBRARIES(louis++ PUBLIC OpenMP::OpenMP_CXX)
ENDIF()


TARGET_COMPILE_DEFINITIONS(louis++ PUBLIC EIGEN_DONT_PARALLELIZE)
# try to increase Eigen performance in Debug mode (inlining and no assertions)
# TARGET_COMPILE_DEFINITIONS(louis++ PUBLIC $<$<CONFIG:Debug>: EIGEN_NO_DEBUG >)
# TARGET_COMPILE_DEFINITIONS(louis++ PUBLIC $<$<CONFIG:Debug>: "EIGEN_STRONG_INLINE=__attribute__((always_inline)) inline" > )
