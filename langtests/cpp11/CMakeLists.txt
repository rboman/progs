cmake_minimum_required(VERSION 2.6)
project(Cpp11 CXX)

#ADD_DEFINITIONS( /Qstd=c++11 )

include(CheckCXXCompilerFlag)
check_cxx_compiler_flag("-std=c++11" STD11CHECK)
IF(STD11CHECK)
    ADD_DEFINITIONS(-std=c++11 )
ENDIF()

set(SRCS main.cpp)
add_executable(cpp11 ${SRCS})
