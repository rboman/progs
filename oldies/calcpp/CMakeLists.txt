cmake_minimum_required(VERSION 2.6)
project(Calc CXX)
set(SRCS calc.cxx expMath.cxx expMath.hxx)
add_executable(calcpp ${SRCS})
