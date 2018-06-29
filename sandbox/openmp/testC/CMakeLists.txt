cmake_minimum_required(VERSION 3.0)
project(OpenMP_CXX CXX)

include(CheckCXXCompilerFlag)
check_cxx_compiler_flag("-std=c++11" STD11CHECK)
IF(STD11CHECK)
    ADD_DEFINITIONS(-std=c++11 )
ENDIF()

# OpenMP --
find_package(OpenMP REQUIRED)
if(OPENMP_FOUND)
    MESSAGE ("OpenMP_CXX_FLAGS=${OpenMP_CXX_FLAGS}")
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
endif()

FILE(GLOB SRCS *.h *.cpp *.inl *.hpp)
add_executable(omp ${SRCS})
target_link_libraries(omp ${TBB_LIBRARIES})

