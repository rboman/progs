cmake_minimum_required(VERSION 2.6)
project(Calc C)
set(SRCS calc.c calc.h)
add_executable(calc ${SRCS})
target_link_libraries(calc m)

