# cmake -G "Visual Studio 14 Win64" ..
# cmake --build . --config Release

cmake_minimum_required(VERSION 2.6)
project(Calc CXX)
IF(MSVC)
    add_definitions(-D_CRT_SECURE_NO_WARNINGS -D_SCL_SECURE_NO_DEPRECATE)
ENDIF(MSVC)
FILE(GLOB SRCS *.h *.cpp)
add_executable(calcpp ${SRCS})
