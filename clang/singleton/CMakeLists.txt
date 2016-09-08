cmake_minimum_required(VERSION 2.6)
project(Singleton CXX)
file(GLOB SRCS *.h *.cpp *.inl *.hpp)
add_executable(singleton ${SRCS})
