cmake_minimum_required(VERSION 2.6)
project(Skel CXX)
file(GLOB SRCS *.h *.cpp *.inl *.hpp)
add_executable(skelcpp ${SRCS})
