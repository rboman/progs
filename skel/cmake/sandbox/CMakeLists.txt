# tests bidons de de cmake
# http://www.cmake.org/cmake/help/v2.8.10/cmake.html
#

CMAKE_MINIMUM_REQUIRED(VERSION 3.1)
MESSAGE(STATUS "hello")

PROJECT(Hello)

SET(HELLO_SRCS main.cpp)

ADD_EXECUTABLE(Hello ${HELLO_SRCS})

GET_SOURCE_FILE_PROPERTY(MYPRP main.cpp LOCATION)
MESSAGE(STATUS "MYPRP=" ${MYPRP})

