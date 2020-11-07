cmake_minimum_required(VERSION 2.6)

project(FEMcode CXX)

set(SRCS femcode.cpp 
         translateNode.cpp 
         printNode.cpp)

add_executable(femcode ${SRCS})
