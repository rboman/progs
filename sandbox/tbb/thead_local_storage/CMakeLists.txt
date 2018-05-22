cmake_minimum_required(VERSION 3.0)
project(TLS CXX)

include(CheckCXXCompilerFlag)
check_cxx_compiler_flag("-std=c++11" STD11CHECK)
IF(STD11CHECK)
    ADD_DEFINITIONS(-std=c++11 )
ENDIF()

# TBB

FIND_PATH(TBB_INCLUDE_DIRS NAMES  tbb/tbb.h HINTS "$ENV{TBBROOT}/include")
MESSAGE("TBB_INCLUDE_DIRS=${TBB_INCLUDE_DIRS}")
include_directories(${TBB_INCLUDE_DIRS})

FIND_LIBRARY(TBB_LIBRARIES tbb ENV LD_LIBRARY_PATH)  # serach paths in LD_LIBRARY_PATH
MESSAGE("TBB_LIBRARIES=${TBB_LIBRARIES}")

FILE(GLOB SRCS *.h *.cpp *.inl *.hpp)
add_executable(tls ${SRCS})
target_link_libraries(tls ${TBB_LIBRARIES})