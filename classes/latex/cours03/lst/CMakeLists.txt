cmake_minimum_required(VERSION 2.7)

project(Codes CXX)

if(WIN32)
    add_definitions(-D_CRT_SECURE_NO_WARNINGS -D_SCL_SECURE_NO_DEPRECATE)
endif(WIN32)

include_directories(${PROJECT_SOURCE_DIR}/gmm)

add_executable(vectoradd_gmm vectoradd_gmm.cpp)
add_executable(matrixadd_gmm matrixadd_gmm.cpp)
