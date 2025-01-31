PROJECT(SAXPY_GPU CXX)
CMAKE_MINIMUM_REQUIRED(VERSION 3.14)

IF(NOT CMAKE_BUILD_TYPE)
    SET(CMAKE_BUILD_TYPE "Release" CACHE STRING "" FORCE)
ENDIF()

ADD_EXECUTABLE(saxpy_gpu saxpy_gpu.cpp)

TARGET_COMPILE_OPTIONS(saxpy_gpu PUBLIC -fopenmp -fopenmp-targets=nvptx64) # --offload-arch=sm_75)
TARGET_LINK_OPTIONS(saxpy_gpu PUBLIC -fopenmp -fopenmp-targets=nvptx64)  #--offload-arch=sm_75)

FIND_LIBRARY(MATH_LIBRARY m)
IF(MATH_LIBRARY)
    TARGET_LINK_LIBRARIES(saxpy_gpu PUBLIC ${MATH_LIBRARY})
ENDIF()
