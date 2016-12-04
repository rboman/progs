cmake_minimum_required(VERSION 2.6)
project(Md5Test CXX)
set(SRCS main.cpp md5.cpp md5.h)
add_executable(md5test ${SRCS})
